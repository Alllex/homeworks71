/*******************************
    project: StudentVM_beta
    author: Alex Semin
    place: SPbU MM171
    date:  2013
*******************************/

#include "parser.h"

#include <string.h>
#include <ctype.h>

void setCommands(FILE *code, Program *program);
void setLabels(Program *program);
pCommand readNextCommand(FILE *code);

char *getString(FILE *input);
void clearString(char *line);
void goToNextLine(FILE *input);
void strToLower(char *s);

int parse(FILE *code, Program *program)
{
    setCommands(code, program);
    if (program->errorVM.type != NOTHING)
    {
        return 1;
    }
    setLabels(program);
    return 0;
}

void setCommands(FILE *code, Program *program)
{
    while (!feof(code))
    {
        pCommand command = readNextCommand(code);
        if (command == NULL)
        {
            continue;
        }
        if (getOpcode(command) == ERR)
        {
            program->errorVM.type = INCORRECT_COMMAND;
            freeCommand(command);
            return;
        }
        pushCommand(program->commands, command);
    }
}

pCommand readNextCommand(FILE *code)
{
    pCommand com = createCommand();
    char *command = getString(code);
    CommandArg arg;
    if (strlen(command) == 0)
    {
        clearString(command);
        return NULL;
    }
    if (command[strlen(command) - 1] == ':')
    {
        command[strlen(command) - 1] = '\0';
        setLabel(com, command);
        setOpcode(com, LBL);
        return com;
    }
    strToLower(command);
    if (strcmp(command, "ldc") == 0)
    {
        char *num = getString(code);
        int value = atoi(num);
        clearString(num);
        setOpcode(com, LDC);
        arg.number = value;
        setCommandArg(com, arg);
    }
    else if (strcmp(command, "ld") == 0)
    {
        char *num = getString(code);
        unsigned int address = (unsigned int) atoi(num);
        clearString(num);
        setOpcode(com, LD);
        arg.address = address;
        setCommandArg(com, arg);
    }
    else if (strcmp(command, "st") == 0)
    {
        char *num = getString(code);
        unsigned int address = (unsigned int) atoi(num);
        clearString(num);
        setOpcode(com, ST);
        arg.address = address;
        setCommandArg(com, arg);
    }
    else if (strcmp(command, "swp") == 0)
    {
        setOpcode(com, SWP);
    }
    else if (strcmp(command, "dup") == 0)
    {
        setOpcode(com, DUP);
    }
    else if (strcmp(command, "pop") == 0)
    {
        setOpcode(com, POP);
    }
    else if (strcmp(command, "add") == 0)
    {
        setOpcode(com, ADD);
    }
    else if (strcmp(command, "sub") == 0)
    {
        setOpcode(com, SUB);
    }
    else if (strcmp(command, "mul") == 0)
    {
        setOpcode(com, MUL);
    }
    else if (strcmp(command, "div") == 0)
    {
        setOpcode(com, DIV);
    }
    else if (strcmp(command, "mod") == 0)
    {
        setOpcode(com, MOD);
    }
    else if (strcmp(command, "cmp") == 0)
    {
        setOpcode(com, CMP);
    }
    else if (strcmp(command, "jmp") == 0)
    {
        char *label = getString(code);
        setOpcode(com, JMP);
        arg.label = label;
        setCommandArg(com, arg);
    }
    else if (strcmp(command, "br") == 0)
    {
        char *label = getString(code);
        setOpcode(com, BR);
        arg.label = label;
        setCommandArg(com, arg);
    }
    else if (strcmp(command, "hlt") == 0)
    {
        setOpcode(com, HLT);
    }
    else
    {
        setOpcode(com, ERR);
    }
    clearString(command);
    return com;
}

void setLabels(Program *program)
{
    pCommand prev = NULL;
    pCommand lbl = getCommandsHead(program->commands);
    while (lbl != NULL)
    {
        if (isLabel(lbl))
        {
            pCommand temp = getCommandsHead(program->commands);
            while (temp != NULL)
            {
                char *label = getArg(temp).label;
                if ((getOpcode(temp) == JMP || getOpcode(temp) == BR) && (label != NULL)
                        && strcmp(getLabel(lbl), label) == 0)
                {
                    CommandArg arg = getArg(temp);
                    free(label);
                    arg.jump = getNextCommand(lbl);
                    setCommandArg(temp, arg);
                }
                temp = getNextCommand(temp);
            }
            temp = getNextCommand(lbl);
            eraseCommand(program->commands, lbl, prev);
            lbl = temp;
        }
        else
        {
            prev = lbl;
            lbl = getNextCommand(lbl);
        }
    }
}

void strToLower(char *s)
{
    int i = 0;
    while (s[i])
    {
        s[i] = tolower(s[i]);
        i++;
    }
}

int isDivide(char c)
{
    return !isalpha(c) && !isdigit(c) && c != ':';
}

// this function provide reading very long string to \n
char *getString(FILE *input)
{
    const int sizeStep = 256;
    int countChars = 0;
    int currentSize = sizeStep;
    char *string = (char *) malloc(currentSize * sizeof(char));
    char c = '0';
    int first = 1;
    while (!feof(input))
    {
        c = fgetc(input);
        if (!isDivide(c))
        {
            break;
        }
    }
    while (!feof(input))
    {
        if (first)
        {
            first = 0;
        }
        else
        {
            c = fgetc(input);
        }
        if (isDivide(c))
        {
            break;
        }
        if (countChars == currentSize)
        {
            currentSize += sizeStep;
            string = realloc(string, currentSize * sizeof(char));
        }
        string[countChars++] = c;
    }
//    if (feof(input))
//    {
//        string[countChars - 1] = '\0';
//    }
//    else
//    {
//        string[countChars] = '\0';
//    }
    string[countChars] = '\0';
    return string;
}

// not nessery to include malloc libraries to another files
void clearString(char *line)
{
    free(line);
}

void goToNextLine(FILE *input)
{
    char c = '0';
    while (!feof(input))
    {
        c = fgetc(input);
        if (c == '\n')
        {
            break;
        }
    }
}
