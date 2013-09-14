/*
*   Program: Calulate Math Expression
*   Author: Alex Semin Math-Mech 171 2012
*   2012 (c)
*/

#ifndef ERRORS_H
#define ERRORS_H

#include <stdio.h>

typedef enum
{
    NOTHING,
    MISSED_ARGUMENT,
    MISSED_BRACE,
    DIVISION_BY_ZERO,
    UNKNOWN_SIGN,
    BAD_NUMBER,
    UNKNOWN_ERROR
} MathError;

#endif // ERRORS_H
