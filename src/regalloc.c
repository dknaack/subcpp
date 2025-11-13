/*
 * For each instruction, the register allocator ensures that all virtual
 * register operands are stored in machine registers and not on the stack. It
 * does so by inserting mov/spill/reload before the current instruction when
 * the specific register is not available. The code generator can then use a
 * peephole optimizer to combine loads/stores into one instruction, if
 * possible.
 */
static mach_location *
regalloc(mach_token *tokens, isize token_count,
	block *blocks, isize block_count, mach_info mach, arena *arena)
{
	mach_location *result = NULL;

	return result;
}
