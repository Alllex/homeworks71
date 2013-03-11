/*******************************
    project: StudentVM_beta
    author: Alex Semin
    place: SPbU MM171
    date:  2013
*******************************/

#ifndef ERRORS_H
#define ERRORS_H

#include <stdio.h>

typedef enum
{
    NOTHING,
    FILE_NOT_FOUND,
    MEMORY_SET_ERROR,
    INCORRECT_COMMAND,
    INCORRECT_PARAMS,
    EMPTY_STACK,
    OUT_OF_MEMORY,
    NO_HLT,
    NO_RESULT,
    DIVISION_BY_ZERO,
    EQUALS_LABELS,
    UNEXPECTED_QUIT,
    UNKNOWN_ERROR
} ErrorType;

typedef struct
{
    ErrorType type;
    unsigned int errorStep;
} ErrorVM;

void printError(FILE *output, ErrorType errType, unsigned int step, unsigned int commandNum);

#endif // ERRORS_H
