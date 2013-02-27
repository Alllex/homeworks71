/*******************************
    project: StudentVM_beta
    author: Alex Semin
    place: SPbU MM171
    date:  2013
*******************************/

#include "interpreter.h"

int performProgram(FILE *log, VM *vm)
{
    unsigned int step = 0;
    vm->state->IP = getCommandsHead(vm->program->commands);
    while (1)
    {
        pCommand command = vm->state->IP;
        Opcode opcode = getOpcode(command);
        CommandArg arg = getArg(command);
        int x = 0;
        int y = 0;

        if (command == NULL)
        {
            //error
        }
        step++;
        fprintf(log, "Command ");
        printCommand(log, command);
//        fprintf(stdout, "Command ");
//        printCommand(stdout, command);

        // Doesn't use pop stack

        if (opcode == HLT)
        {
            break;
        }
        else if (opcode == JMP)
        {
            vm->state->IP = arg.jump;
            //printStateVM(log, vm);
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
        {           // needs 1 stack item
            if (!hasItemsStack(vm->state->stack, 1))
            {
                vm->program->errorVM.errorStep = step;
                vm->program->errorVM.type = EMPTY_STACK;
                return 0;
            }
            y = popStack(vm->state->stack);
            if (opcode == BR)
            {
                if (y != 0)
                {
                    vm->state->IP = arg.jump;
                    //printStateVM(log, vm);
                    continue;
                }
            }
            else if (opcode == ST)
            {
                if (!isValidAddress(vm->state->memory, arg.address))
                {
                    vm->program->errorVM.type = OUT_OF_MEMORY;
                    vm->program->errorVM.errorStep = step;
                    return 0;
                }
                setItemMemory(vm->state->memory, arg.address, y);
            }
            else if (opcode == LDI)
            {
                unsigned int address = (unsigned int) y;
                if (!isValidAddress(vm->state->memory, address))
                {
                    vm->program->errorVM.type = OUT_OF_MEMORY;
                    vm->program->errorVM.errorStep = step;
                    return 0;
                }
                pushStack(vm->state->stack, getItemMemory(vm->state->memory, address));
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
                // needs another 1 stack item
                if (!hasItemsStack(vm->state->stack, 1))
                {
                    vm->program->errorVM.errorStep = step;
                    vm->program->errorVM.type = EMPTY_STACK;
                    return 0;
                }

                x = popStack(vm->state->stack);

                if (opcode == SWP)
                {
                    pushStack(vm->state->stack, y);
                    pushStack(vm->state->stack, x);
                }
                else if (opcode == STI)
                {
                    unsigned int address = (unsigned int) y;
                    if (!isValidAddress(vm->state->memory, address))
                    {
                        vm->program->errorVM.type = OUT_OF_MEMORY;
                        vm->program->errorVM.errorStep = step;
                        return 0;
                    }
                    setItemMemory(vm->state->memory, address, x);
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
                    if (y == 0)
                    {
                        vm->program->errorVM.errorStep = step;
                        vm->program->errorVM.type = DIVISION_BY_ZERO;
                        return 0;
                    }
                    pushStack(vm->state->stack, (int) (x / y));
                }
                else if (opcode == MOD)
                {
                    if (y == 0)
                    {
                        vm->program->errorVM.errorStep = step;
                        vm->program->errorVM.type = DIVISION_BY_ZERO;
                        return 0;
                    }
                    pushStack(vm->state->stack, x % y);
                }
                else if (opcode == CMP)
                {
                    pushStack(vm->state->stack, (int) (x > y) - (x < y));
                }
            }
        }
        vm->state->IP = getNextCommand(command);
        //printStateVM(log, vm);
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
