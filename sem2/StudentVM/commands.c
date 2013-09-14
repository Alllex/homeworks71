/*******************************
    project: StudentVM_beta
    author: Alex Semin
    place: SPbU MM171
    date:  2013
*******************************/

#include "commands.h"

typedef struct Command
{
    int id;
    Opcode opcode;
    CommandArg arg;
    char cleared;

    pCommand next;
} Command;

typedef struct Commands
{
    pCommand head;
    pCommand tail;
    int amountCommands;
    int hasHLT;
} Commands;

void deleteContains(pCommands commands);
void freeCommand(pCommand command);
void clearCommand(pCommand command);

pCommands createCommands(void)
{
    pCommands commands = (pCommands) malloc(sizeof(Commands));
    commands->head = NULL;
    commands->tail = NULL;
    commands->amountCommands = 0;
    commands->hasHLT = 0;
    return commands;
}

pCommand createCommand(void)
{
    pCommand command = (pCommand) malloc(sizeof(Command));
    command->id = 0;
    command->opcode = ERR;
    command->arg.number = 0;
    command->next = NULL;
    command->cleared = 0;
    return command;
}

CommandArg createCommandArg(void)
{
    CommandArg arg;
    arg.number = 0;
    return arg;
}

void deleteCommands(pCommands commands)
{
    deleteContains(commands);
    free(commands);
}

void deleteContains(pCommands commands)
{
    freeCommand(commands->head);
}

void freeCommand(pCommand command)
{
    if (command != NULL)
    {
        clearCommand(command);
        freeCommand(command->next);
        free(command);
    }
}

void clearCommand(pCommand command)
{
    if (command != NULL && (command->opcode == BR || command->opcode == JMP || command->opcode == LBL)
                            && (!command->cleared))
    {
        free(command->arg.label);
    }
}

pCommand getCommandsHead(pCommands commands)
{
    return commands->head;
}

void eraseCommand(pCommands commands, pCommand command, pCommand previous)
{
    if (command == NULL)
    {
        return;
    }
    clearCommand(command);
    commands->amountCommands--;
    if (previous == NULL)
    {
        commands->head = command->next;
    }
    else
    {
        previous->next = command->next;
    }
    free(command);
}

void pushCommand(pCommands commands, pCommand command)
{
    if (commands->tail == NULL)
    {
        commands->head = command;
        commands->tail = command;
        command->id = 1;
        return;
    }
    command->id = commands->tail->id + ((commands->tail->opcode == LBL) ? 0 : 1);
    commands->tail->next = command;
    commands->tail = command;
}

void setNextCommand(pCommand command, pCommand next)
{
    if (command != NULL)
    {
        command->next = next;
    }
}

pCommand getNextCommand(pCommand command)
{
    return (command != NULL) ? command->next : NULL;
}

int getCommandID(pCommand command)
{
    return (command != NULL) ? command->id : 0;
}

Opcode getOpcode(pCommand command)
{
    return (command != NULL) ? command->opcode : ERR;
}

CommandArg getArg(pCommand command)
{
    CommandArg tempArg;
    return (command != NULL) ? command->arg : tempArg;
}

void setOpcode(pCommand command, Opcode opcode)
{
    if (command != NULL)
    {
        command->opcode = opcode;
    }
}

void setCommandArg(pCommand command, CommandArg arg)
{
    if (command != NULL)
    {
        command->arg = arg;
    }
}

void printCommands(FILE *output, pCommands commands)
{
    pCommand com = commands->head;
    while (com != NULL)
    {
        printCommand(output, com);
        com = com->next;
    }
}

void printCommand(FILE *output, pCommand command)
{
    int jumpID = 0;
    if (command->opcode == BR || command->opcode == JMP)
    {
        pCommand to = command->arg.jump;
        if (to != NULL)
        {
            jumpID = to->id;
        }
    }
    fprintf(output, "#%d", command->id);
    switch (command->opcode)
    {
    case LDC: fprintf(output, "\t\t[LDC] (%d)\n", command->arg.number); break;
    case LD:  fprintf(output, "\t\t[LD]  (%d)\n", command->arg.address); break;
    case ST:  fprintf(output, "\t\t[ST]  (%d)\n", command->arg.address); break;
    case STI: fprintf(output, "\t\t[STI]\n"); break;
    case LDI: fprintf(output, "\t\t[LDI]\n"); break;
    case SWP: fprintf(output, "\t\t[SWP]\n"); break;
    case DUP: fprintf(output, "\t\t[DUP]\n"); break;
    case POP: fprintf(output, "\t\t[POP]\n"); break;
    case ADD: fprintf(output, "\t\t[ADD]\n"); break;
    case SUB: fprintf(output, "\t\t[SUB]\n"); break;
    case MUL: fprintf(output, "\t\t[MUL]\n"); break;
    case DIV: fprintf(output, "\t\t[DIV]\n"); break;
    case MOD: fprintf(output, "\t\t[MOD]\n"); break;
    case CMP: fprintf(output, "\t\t[CMP]\n"); break;
    case JMP: fprintf(output, "\t\t[JMP] (%d)\n", jumpID); break;
    case BR:  fprintf(output, "\t\t[BR]  (%d)\n", jumpID); break;
    case HLT: fprintf(output, "\t\t[HLT]\n"); break;
    case LBL: fprintf(output, "\t\t[LBL]\n"); break;
    case ERR: fprintf(output, "ERROR\n"); break;
    default:  fprintf(output, "ERROR\n"); break;
    }
}

void setHasHLT(pCommands commands, int has)
{
    commands->hasHLT = has;
}

int hasHLT(pCommands commands)
{
    return commands->hasHLT;
}

void makeCleared(pCommand command)
{
    command->cleared = 1;
}
