/*******************************
    project: StudentVM_beta
    author: Alex Semin
    place: SPbU MM171
    date:  2013
*******************************/

#include "interpreter.h"

void saveStep(unsigned int step, unsigned int IP);

int performProgram(FILE *log, VM *vm)
{
    unsigned int step = 0;
    pCommand command = getCommandsHead(vm->program->commands);
    vm->state->IP = 1;
    while (1)
    {
        Opcode opcode = getOpcode(command);
        CommandArg arg = getArg(command);
        int x = 0;
        int y = 0;

        saveStep(++step, vm->state->IP);
        fprintf(log, "Command ");
        printCommand(log, command);

        // Doesn't use pop stack

        if (opcode == HLT)
        {
            break;
        }
        else if (opcode == JMP)
        {
            command = arg.jump;
            vm->state->IP = getCommandID(command);
            printStateVM(stdout, vm);
            continue;
        }
        else if (opcode == LDC)
        {
            pushStack(vm->state->stack, arg.number);
        }
        else if (opcode == LD)
        {
            if (!isValidAddress(vm->state->memory, arg.address))
            {
                vm->program->errorVM.type = OUT_OF_MEMORY;
                vm->program->errorVM.errorStep = step;
                return 0;
            }
            pushStack(vm->state->stack, getItemMemory(vm->state->memory, arg.address));
        }
        else
        {
            if (!hasItemsStack(vm->state->stack, 1))
            {
                vm->program->errorVM.errorStep = step;
                vm->program->errorVM.type = EMPTY_STACK;
                return -1;
            }
            y = popStack(vm->state->stack);
            if (opcode == BR)
            {
                if (y != 0)
                {
                    command = arg.jump;
                    vm->state->IP = getCommandID(command);
                    printStateVM(stdout, vm);
                    continue;
                }
            }
            else if (opcode == ST)
            {
                if (!isValidAddress(vm->state->memory, arg.address))
                {
                    vm->program->errorVM.type = OUT_OF_MEMORY;
                    vm->program->errorVM.errorStep = step;
                    return -2;
                }
                setItemMemory(vm->state->memory, arg.address, y);
            }
            else if (opcode == DUP)
            {
                pushStack(vm->state->stack, y);
                pushStack(vm->state->stack, y);
            }
            else if (opcode == POP)
            {
                // already done pop
            }
            else
            {
                if (!hasItemsStack(vm->state->stack, 1))
                {
                    vm->program->errorVM.errorStep = step;
                    vm->program->errorVM.type = EMPTY_STACK;
                    return -1;
                }

                x = popStack(vm->state->stack);

                if (opcode == SWP)
                {
                    pushStack(vm->state->stack, y);
                    pushStack(vm->state->stack, x);
                }
                else if (opcode == ADD)
                {
                    pushStack(vm->state->stack, x + y);
                }
                else if (opcode == SUB)
                {
                    pushStack(vm->state->stack, x - y);
                }
                else if (opcode == MUL)
                {
                    pushStack(vm->state->stack, x * y);
                }
                else if (opcode == DIV)
                {
                    pushStack(vm->state->stack, (int) (x / y));
                }
                else if (opcode == MOD)
                {
                    pushStack(vm->state->stack, x % y);
                }
                else if (opcode == CMP)
                {
                    pushStack(vm->state->stack, (int) (x > y) - (x < y));
                }
            }
        }
        printStateVM(log, vm);
        command = getNextCommand(command);
        vm->state->IP = getCommandID(command);
    }
    fprintf(log, "############## The last state of VM ##############\n");
    printStateVM(log, vm);
    if (!hasItemsStack(vm->state->stack, 1))
    {
        vm->program->errorVM.errorStep = 0;
        vm->program->errorVM.type = NO_RESULT;
        return 0;
    }
    return popStack(vm->state->stack);
}

void saveStep(unsigned int step, unsigned int IP)
{

}
