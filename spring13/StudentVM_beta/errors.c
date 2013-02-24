/*******************************
    project: StudentVM_beta
    author: Alex Semin
    place: SPbU MM171
    date:  2013
*******************************/

#include "errors.h"

void printError(FILE *output, ErrorType errType, unsigned int step, unsigned int commandNum)
{
    switch (errType)
    {
    case NOTHING: fprintf(output, "There're no errors\n"); break;
    case FILE_NOT_FOUND: fprintf(output, "Sorry, file not found\n"); break;
    case MEMORY_SET_ERROR: fprintf(output, "Incorrent memory size\n"); break;
    case INCORRECT_COMMAND: fprintf(output, "Unknown command #%d\n", commandNum); break;
    case INCORRECT_PARAMS: fprintf(output, "Invalid parameter of command #%d\n", commandNum); break;
    case EMPTY_STACK: fprintf(output, "Pop from empty stack [command(#%d), step(#%d)]\n", step, commandNum); break;
    case OUT_OF_MEMORY: fprintf(output, "Invalid address of memory [command(#%d), step(#%d)]\n", step, commandNum); break;
    case NO_HLT: fprintf(output, "There's no HALT command\n"); break;
    case NO_RESULT: fprintf(output, "There's no result of performing\n"); break;
    case DIVISION_BY_ZERO: fprintf(output, "Division by zero [command(#%d), step(#%d)]\n", step, commandNum); break;
    default: fprintf(output, "Unknown error\n"); break;
    }
}
