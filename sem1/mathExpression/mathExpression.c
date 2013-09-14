/*
*   Program: Calulate Math Expression
*   Author: Alex Semin Math-Mech 171 2012
*   2012 (c)
*/

#include <stdio.h>

#include "mathExpression.h"
#include "inputMethods.h"

#define dDEBUG

void initMathExpression(Lexer *exp);
void clearMathExpression(Lexer *exp);
MathResult calcExpression(Lexer *exp);
void showErr(MathError err, int position, FILE *output);

//-------------------------DECLARATION PRIVATE FUNCTION---------------------------

int isDigit(char c);
int equalZero(float x);

MathResult getNumber(Lexer *exp);
MathResult getNextLexem(Lexer *exp);
void nextLexem(Lexer *exp);

MathResult factor(Lexer *exp);
MathResult term(Lexer *exp);
MathResult expression(Lexer *exp);

#ifdef DEBUG
    void printMathObj(Lexer *exp);
    void printMathObjDebug(Lexer *exp, const char *head);
    void printHeadDebug(const char *head);
#endif

//-------------------------DECLARATION PRIVATE FUNCTION---------------------------

void initMathExpression(Lexer *exp)
{
    FILE* input = fopen("exp", "r");
    if (input != NULL)
    {
        expline = getString(input);
        fclose(input);
    }
    else
    {
        expline = getString(stdin);
    }
    exppointer = 0;
    experrorPointer = 0;
    explexem = START;
    exptemp.value = 0.0;
    exptemp.err = NOTHING;
}

void clearMathExpression(Lexer *exp)
{
    clearString(expline);
}

int equalZero(float x)
{
    const float eps = 1e-10;
    if (x > 0.0)
    {
        return (x - 0.0 < eps);
    }
    else
    {
        return (-x - 0.0 < eps);
    }
}

int isDigit(char c)
{
    return (c >= '0' && c <= '9');
}

MathResult getNumber(Lexer *exp)
{
    MathResult result;
    result.err = NOTHING;
    result.value = 0.0;

    float number = 0.0;
    int hasDot = 0;
    float pow = 1.0;
    int ptr = exppointer;
    char c = expline[ptr];

    while (isDigit(c) || (c == '.' && !hasDot))
    {
        float oldNumber = number;
        if (c == '.')
        {
            hasDot = 1;
            c = expline[++ptr];
            continue;
        }
        int digit = c - '0';
        if (hasDot)
        {
            pow /= 10.0;
            number += digit * pow;
        }
        else
        {
            number = 10.0 * number + digit;
        }
        if (oldNumber > number) // overflow
        {
            result.err = BAD_NUMBER; //check infinity
            return result;
        }
        c = expline[++ptr];
    }
    exppointer = ptr;
    result.value = number;

#ifdef DEBUG
    printMathObjDebug(exp, "\t\t\t read number: ");
#endif

    return result;
}

void nextLexem(Lexer *exp)
{
    if (exptemp.err != NOTHING)
    {
        return;
    }
    experrorPointer = exppointer;
    char c = expline[exppointer];

    if (c == ' ') // whitespace
    {
        exppointer++;
        nextLexem(exp);
    }
    else if (c == '\0') // end of expression
    {
        explexem = EOEXP;
    }
    else if (c == '+' || c == '-' || c == '*' || c == '/')
    {
        if (c == '+')
        {
            explexem = PLUS;
        }
        else if (c == '-')
        {
            explexem = MINUS;
        }
        else if (c == '*')
        {
            explexem = MULT;
        }
        else
        {
            explexem = DIV;
        }
        (exppointer)++;
    }
    else if (c == '(' || c == ')')
    {
        explexem = (c == '(') ? OBRACE : CBRACE;
        exppointer++;
    }
    else if (isDigit(c))
    {
        explexem = NUMBER;
        exptemp = getNumber(exp);
    }
    else
    {
        experrorPointer = exppointer;
        exptemp.err = UNKNOWN_SIGN;
    }

#ifdef DEBUG
    printMathObjDebug(exp, "\t\t\t read lexem: ");
    printMathObjDebug(exp, "Next lexem: ");
#endif
}

MathResult getNextLexem(Lexer *exp)
{
    return exptemp;
}

MathResult expression(Lexer *exp)
{
#ifdef DEBUG
    printHeadDebug("in (e) ");
#endif
    MathResult result = term(exp);
    if (result.err != NOTHING)
    {
        return result;
    }
    while (explexem == PLUS || explexem == MINUS)
    {
        LexemType operation = explexem;
        nextLexem(exp);
        MathResult temp = term(exp);
        if (temp.err != NOTHING)
        {
            return temp;
        }
        result.value = (operation == PLUS) ? result.value + temp.value : result.value - temp.value;
    }
#ifdef DEBUG
    printMathObjDebug(exp, "result (e): ");
    printHeadDebug("out (e)\n");
#endif

    return result;
}

MathResult term(Lexer *exp)
{
#ifdef DEBUG
    printHeadDebug("in (t) ");
#endif
    MathResult result = factor(exp);
    if (result.err != NOTHING)
    {
        return result;
    }
    while (explexem == MULT || explexem == DIV)
    {
        int position = experrorPointer;
        LexemType operation = explexem;
        nextLexem(exp);
        MathResult temp = factor(exp);
        if (temp.err != NOTHING)
        {
            return temp;
        }
        if (operation == MULT)
        {
            result.value *= temp.value;
        }
        else
        {
            if (equalZero(temp.value))
            {
                experrorPointer = position;
                result.err = DIVISION_BY_ZERO;
                return result;
            }
            result.value /= temp.value;
        }
    }

#ifdef DEBUG
    printMathObjDebug(exp, "result (t): ");
    printHeadDebug("out (t)\n");
#endif

    return result;
}

MathResult factor(Lexer *exp)
{
#ifdef DEBUG
    printHeadDebug("in (f)\n");
#endif

    float sign = 1.0;
    MathResult result = getNextLexem(exp);

    if (explexem == MINUS)
    {
        sign = -1.0;
        nextLexem(exp);
        result = getNextLexem(exp);
    }

    if (explexem == OBRACE)
    {
        nextLexem(exp);
        result = expression(exp);
        if (explexem != CBRACE)
        {
            if (result.err == NOTHING)
            {
                result.err = MISSED_BRACE;
                return result;
            }
        }
    }
    else if (explexem != NUMBER)
    {
        if (result.err == NOTHING)
        {
            result.err = MISSED_ARGUMENT;
            return result;
        }
    }
    result.value *= sign;
    nextLexem(exp);

#ifdef DEBUG
    printMathObjDebug(exp, "result (f): ");
    printHeadDebug("out (f)\n");
#endif

    return result;
}

MathResult calcExpression(Lexer *exp)
{
#ifdef DEBUG
    //const char *debugFile = "out.debug";
    //freopen(debugFile, "w", stderr);
    //fprintf(stdout, "Debug file \"%s\"\n", debugFile);
    printHeadDebug("Expression: ");
    printHeadDebug(expline);
    printHeadDebug("\nStart calculating\n");
#endif
    MathResult result;
    if (explexem != START)
    {
        explexem = ERROR;
        result.err = UNKNOWN_ERROR;
    }
    else
    {
        nextLexem(exp);
        result = expression(exp);
        if (explexem != EOEXP || result.err != NOTHING)
        {
            explexem = ERROR;
            if (exptemp.err != NOTHING)
            {
                result.err = exptemp.err;
            }
            else if (result.err == NOTHING)
            {
                result.err = UNKNOWN_ERROR;
            }
        }
    }

#ifdef DEBUG
    printHeadDebug("Correct expression calculated\n");
    printMathObjDebug(exp, "Expression result = ");
    fclose(stderr);
#endif
    return result;
}

void showErr(MathError err, int position, FILE *output)
{
    fprintf(output, "Error in (%d) position: ", position + 1);
    if (err == MISSED_ARGUMENT)
    {
        fprintf(output, "There is missed argument");
    }
    else if (err == MISSED_BRACE)
    {
        fprintf(output, "There is missed brace");
    }
    else if (err == DIVISION_BY_ZERO)
    {
        fprintf(output, "Division by zero");
    }
    else if (err == BAD_NUMBER)
    {
        fprintf(output, "Some number is incorrect");
    }
    else if (err == UNKNOWN_SIGN)
    {
        fprintf(output, "Unknown sign in expression");
    }
    else if (err == UNKNOWN_ERROR)
    {
        fprintf(output, "Waiting for ending of expression");
    }
    else if (err == NOTHING)
    {
        fprintf(output, "Nothing error?");
    }
    fprintf(output, "\n");
}

void printMathObj(Lexer *exp)
{
    //fprintf(stderr, "number (%0.5g)\n", exptemp.value);
    LexemType lex = explexem;
    if (lex == ERROR || exptemp.err != NOTHING)
    {
        showErr(exptemp.err, experrorPointer, stderr);
    }
    else if (lex == PLUS)
    {
        fprintf(stderr, "operation: +\n");
    }
    else if (lex == MINUS)
    {
        fprintf(stderr, "operation: -\n");
    }
    else if (lex == MULT)
    {
        fprintf(stderr, "operation: *\n");
    }
    else if (lex == DIV)
    {
        fprintf(stderr, "operation: /\n");
    }
    else if (lex == NUMBER)
    {
        fprintf(stderr, "number (%0.5g)\n", exptemp.value);
    }
    else if (lex == EOEXP)
    {
        fprintf(stderr, "End of expression\n");
    }
    else if (lex == OBRACE)
    {
        fprintf(stderr, "brace: (\n");
    }
    else if (lex == CBRACE)
    {
        fprintf(stderr, "brace: )\n");
    }
}

void printMathObjDebug(Lexer *exp, const char *head)
{
    fprintf(stderr, "%s", head);
    printMathObj(exp);
}

void printHeadDebug(const char* head)
{
    fprintf(stderr, "%s", head);
}
