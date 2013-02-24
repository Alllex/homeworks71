/*
*   Program: Calulate Math Expression
*   Author: Alex Semin Math-Mech 171 2012
*   2012 (c)
*/

#include <stdio.h>
#include <malloc.h>

#include "mathExpression.h"

int main(void)
{
    fprintf(stdout, "Type math expession:\n");
    Lexer *exp = (Lexer *) malloc(sizeof(Lexer));
    initMathExpression(exp);
    MathResult result = calcExpression(exp);
    if (result.err != NOTHING)
    {
        // show place of error
        showErr(result.err, experrorPointer, stderr);
        clearMathExpression(exp);
        return 1;
    }
    printf("Result = %g\n", result.value);
    clearMathExpression(exp);
    free(exp);
    return 0;
}

