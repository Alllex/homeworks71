/*******************************
    project: StudentVM_beta
    author: Alex Semin
    place: SPbU MM171
    date:  2013
*******************************/

#ifndef STRUCTURES_H
#define STRUCTURES_H

#include "stack.h"
#include "memory.h"
#include "commands.h"
#include "errors.h"

#define maxMemorySize (1 << 31);

typedef struct
{
    ErrorVM errorVM;
    pCommands commands;
} Program;

typedef struct
{
    pStack stack;
    pMemory memory;
    unsigned int IP;
} StateVM;

typedef struct
{
    Program *program;
    StateVM *state;
} VM;

void printStateVM(FILE *output, VM *vm);

#endif // STRUCTURES_H
