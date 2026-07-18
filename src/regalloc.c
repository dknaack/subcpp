/*
 * For each instruction, the register allocator ensures that all virtual
 * register operands are stored in machine registers and not on the stack. It
 * does so by inserting mov/spill/reload before the current instruction when
 * the specific register is not available. The code generator can then use a
 * peephole optimizer to combine loads/stores into one instruction, if
 * possible.
 */
static machine_location *
allocate_registers(inst *insts, isize inst_count,
	block *blocks, isize block_count, machine_info info, arena *arena)
{
	machine_location *locations = ALLOC(arena, inst_count, machine_location);
	arena_temp temp = arena_temp_begin(arena);

	// Allocate the bitsets
	bitset *gen = ALLOC(arena, block_count, bitset);
	bitset *kill = ALLOC(arena, block_count, bitset);
	bitset *live_in = ALLOC(arena, block_count, bitset);
	bitset *live_out = ALLOC(arena, block_count, bitset);
	for (isize i = 0; i < block_count; i++) {
		gen[i] = new_bitset(inst_count, arena);
		kill[i] = new_bitset(inst_count, arena);
		live_in[i] = new_bitset(inst_count, arena);
		live_out[i] = new_bitset(inst_count, arena);
	}

	// Initialize the gen and kill bitsets
	for (isize b = 1; b < block_count; b++) {
		for (isize i = blocks[b].begin; i < blocks[b].end; i++) {
			inst inst = insts[i];

			// Gen stores all registers which have been used before any def. We
			// track use first, since a register with both flags would count as
			// usage and then definition.

			i32 arg0 = inst.args[0];
			if ((inst.flags & INST_USE0) && !get_bit(kill[b], arg0)) {
				set_bit(gen[b], arg0, 1);
			}

			i32 arg1 = inst.args[1];
			if ((inst.flags & INST_USE1) && !get_bit(kill[b], arg1)) {
				set_bit(gen[b], arg1, 1);
			}

			i32 dest = i;
			if (inst.flags & INST_DEF) {
				set_bit(kill[b], dest, 1);
			}
		}
	}

	// Do the live variable analysis
	b32 has_changed = true;
	while (has_changed) {
		has_changed = false;

		for (isize i = block_count - 1; i >= 0; i--) {
			// Update the live_out matrix: Union all live_in successors
			for (isize j = 0; j < live_out[i].size; j++) {
				b32 bit = 0;

				for (isize k = 0; k < 2; k++) {
					isize succ = blocks[i].succ[k];
					if (succ > 0) {
						bit |= get_bit(live_out[succ], j);
					}
				}

				set_bit(live_out[i], j, bit);
			}

			// Update the live_in matrix
			for (isize j = 0; j < live_in[i].size; j++) {
				b32 o = get_bit(live_out[i], j);
				b32 k = get_bit(kill[i], j);
				b32 g = get_bit(gen[i], j);

				b32 old = get_bit(live_in[i], j);
				b32 new = g || (o && !k);

				set_bit(live_in[i], j, new);
				if (old != new) {
					has_changed = true;
				}
			}
		}
	}

	// Calculate the live ranges of the virtual registers
	live_range *ranges = ALLOC(arena, inst_count, live_range);
	for (isize i = 0; i < inst_count; i++) {
		ranges[i].virtual_register = i;
		ranges[i].start = i;
		ranges[i].end = i + 1;
	}

	for (isize i = 0; i < inst_count; i++) {
		if (insts[i].flags & INST_USE0) {
			i32 r = insts[i].args[0];
			ranges[r].end = MAX(ranges[r].end, i + 1);
		}

		if (insts[i].flags & INST_USE0) {
			i32 r = insts[i].args[0];
			ranges[r].end = MAX(ranges[r].end, i + 1);
		}
	}

	for (isize b = 0; b < block_count; b++) {
		for (isize r = 0; r < inst_count; r++) {
			if (get_bit(live_out[b], r)) {
				ranges[r].end = MAX(ranges[r].end, blocks[b].end);
			}
		}
	}

	// NOTE: Sort the ranges by their start
	// TODO: Replace with a more efficient sorting algorithm
	for (isize i = 0; i < inst_count; i++) {
		for (isize j = 0; j < inst_count - i - 1; j++) {
			if (ranges[j].start >= ranges[j + 1].start) {
				live_range tmp = ranges[j];
				ranges[j] = ranges[j + 1];
				ranges[j + 1] = tmp;
			}
		}
	}

	// Compute the interfering machine registers for each virtual register by
	// checking which registers with hints are live at the same time.
	bitset *blocked_registers = ALLOC(arena, inst_count, bitset);
	for (isize i = 0; i < inst_count; i++) {
		blocked_registers[i] = new_bitset(info.machine_register_count, arena);
	}

	for (isize b = 1; b < block_count; b++) {
		arena_temp block_temp = arena_temp_begin(arena);

		// Registers with hints are local to the basic block. Therefore, we can
		// skip initializing this with the live_out bitset and just use the
		// empty bitset.
		bitset live = new_bitset(info.machine_register_count, arena);

		for (isize i = blocks[b].end - 1; i >= blocks[b].begin; i--) {
			inst inst = insts[i];

			if (inst.flags & INST_DEF) {
				set_bit(live, inst.hint, 0);
			}

			for (isize r = 0; r < info.machine_register_count; r++) {
				b32 is_live = get_bit(live, r);
				set_bit(blocked_registers[i], r, is_live);
			}

			if (inst.flags & INST_USE0) {
				i32 arg0 = inst.args[0];
				set_bit(live, insts[arg0].hint, 1);
			}

			if (inst.flags & INST_USE1) {
				i32 arg1 = inst.args[1];
				set_bit(live, insts[arg1].hint, 1);
			}
		}

		arena_temp_end(block_temp);
	}

	b32 *is_active = ALLOC(arena, info.machine_register_count, b32);
	isize active_start = 0;
	for (isize i = 0; i < inst_count; i++) {
		i32 current_register = ranges[i].virtual_register;
		i32 current_start = ranges[i].start;
		i32 current_end = ranges[i].end;
		ASSERT(current_register >= info.machine_register_count);

		// Expire old ranges
		for (isize j = active_start; j < i; j++) {
			b32 has_expired = ranges[j].end < current_start;
			if (!has_expired) {
				continue;
			}

			i32 virtual_register = ranges[j].virtual_register;
			if (!locations[virtual_register].is_spilled) {
				i32 machine_register = locations[virtual_register].value;
				is_active[machine_register] = false;
			}

			// Swap the expired interval with the one at the start.
			ranges[j] = ranges[active_start++];
		}

		// Ignore empty or preallocated registers
		b32 is_empty = (current_start > current_end);
		b32 is_preallocated = (locations[current_register].value > 0);
		if (is_empty || is_preallocated || locations[current_register].is_spilled) {
			continue;
		}

		// Choose the required register pool for the virtual register
		i32 *class_registers = info.int_registers;
		isize class_register_count = info.int_register_count;
		b32 is_float = (insts[current_register].flags & INST_FLOAT);
		if (is_float) {
			class_register_count = info.float_register_count;
			class_registers = info.float_registers;
		}

		// Find a valid machine register that doesn't interfere with the
		// current register.
		i32 assigned_register = 0;
		for (isize j = 0; j < class_register_count; j++) {
			i32 machine_register = class_registers[j];
			b32 is_blocked = get_bit(blocked_registers[i], machine_register);
			if (!is_blocked && !is_active[machine_register]) {
				assigned_register = machine_register;
				break;
			}
		}

		machine_location *current_location = &locations[current_register];
		current_location->value = assigned_register;
		if (assigned_register == 0) {
			current_location->is_spilled = true;
		} else {
			is_active[assigned_register] = true;
		}
	}

	arena_temp_end(temp);
	return locations;
}
