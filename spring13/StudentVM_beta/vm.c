/*******************************
    project: StudentVM_beta
    author: Alex Semin
    place: SPbU MM171
    date:  2013
*******************************/

#include "vm.h"

#include <stdio.h>

VM *createVM();
void deleteVM(VM *vm);
void setStartStateVM(VM *vm, unsigned int sizeOfMemory);
void clearVM(VM *vm);

void runVM(char *fileName, unsigned int sizeOfMemory)
{
    FILE *code;
    VM *vm;
    int performResult = 0;
    //FILE *log = fopen("log", "w");

    fprintf(stdout, "file name [%s]; memory size [%d]\n", fileName, sizeOfMemory);
    if ((code = fopen(fileName, "r")) == NULL)
    {
        printError(stderr, FILE_NOT_FOUND, 0, 0);
        fclose(code);
        return;
    }

    vm = createVM();
    setStartStateVM(vm, sizeOfMemory);
    parse(code, vm->program);
    if (vm->program->errorVM.type == NOTHING)
    {
        //printCommands(stdout, vm->program->commands);
        performResult = performProgram(stdout, vm);
        if (vm->program->errorVM.type != NOTHING)
        {
            printError(stderr, vm->program->errorVM.type, vm->program->errorVM.errorStep, vm->state->IP);
        }
        else
        {
            fprintf(stdout, "\n\n---------------------\n\nResult of preform: [%d]\n", performResult);
        }
    }
    else
    {
        printError(stderr, vm->program->errorVM.type, vm->program->errorVM.errorStep, vm->state->IP);
    }
    clearVM(vm);
    deleteVM(vm);

    fclose(code);
    //fclose(log);
}

VM *createVM()
{
    VM *vm = (VM *) malloc(sizeof(VM));
    vm->program = (Program *) malloc(sizeof(Program));
    vm->state = (StateVM *) malloc(sizeof(StateVM));
    return vm;
}

void deleteVM(VM *vm)
{
    free(vm->state);
    free(vm->program);
    free(vm);
}

void setStartStateVM(VM *vm, unsigned int sizeOfMemory)
{
    vm->program->errorVM.errorStep = 0;
    vm->program->errorVM.type = NOTHING;
    vm->state->IP = 0;
    vm->state->stack = createStack();
    vm->state->memory = createMemory(sizeOfMemory);
    vm->program->commands = createCommands();
}

void clearVM(VM *vm)
{
    vm->program->errorVM.errorStep = 0;
    vm->program->errorVM.type = NOTHING;
    vm->state->IP = 0;
    deleteStack(vm->state->stack);
    deleteMemory(vm->state->memory);
    deleteCommands(vm->program->commands);
}
