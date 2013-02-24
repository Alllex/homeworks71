/*
*   Program: Calulate Math Expression
*   Author: Alex Semin Math-Mech 171 2012
*   2012 (c)
*/

#ifndef MATHEXPRESSION_H
#define MATHEXPRESSION_H

#include "errors.h"

typedef enum
{
    START,
    PLUS,
    MINUS,
    MULT,
    DIV,
    NUMBER,
    OBRACE,
    CBRACE,
    EOEXP,
    ERROR
} LexemType;

typedef struct
{
    float value;
    MathError err;
} MathResult;

typedef struct
{
    char *line;
    int pointer;
    int errorPointer;
    LexemType lexem;
    MathResult temp;
} Lexer;

void initMathExpression(Lexer *exp);
void clearMathExpression(Lexer *exp);
MathResult calcExpression(Lexer *exp);
void showErr(MathError err, int position, FILE *output);



#endif // MATHEXPRESSION_H
