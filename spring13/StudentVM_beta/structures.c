/*******************************
    project: StudentVM_beta
    author: Alex Semin
    place: SPbU MM171
    date:  2013
*******************************/

#include "structures.h"

void printStateVM(FILE *output, VM *vm)
{
    fprintf(output, "VM state:\n");
    fprintf(output, "Next command");
    printCommand(output, vm->state->IP);
    printStackState(output, vm->state->stack);
    printMemoryState(output, vm->state->memory);
    //printCommands(output, vm->program->commands);
}
