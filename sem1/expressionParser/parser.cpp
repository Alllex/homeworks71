#include "parser.h"

Parser::Parser():
    pointer(0),
    currentLexem(unknownLexem),
    error(false)
{
    numbersParser = new StateAutomate("rules");
}

Parser::~Parser()
{
    delete numbersParser;
}

bool Parser::calc(string &expr)
{
    pointer = 0;
    line = expr;
    currentLexem = unknownLexem;
    error = false;
    nextLexem();
    return (calcExpression() && !error && currentLexem == endOfExpLexem);
}

bool Parser::calcExpression()
{
    if (error)
    {
        return false;
    }
    bool result = calcTerm();
    while (currentLexem == plusLexem || currentLexem == minusLexem)
    {
        nextLexem();
        if (!calcTerm())
        {
            return false;
        }
    }
    return result;
}

bool Parser::calcTerm()
{
    if (error)
    {
        return false;
    }
    bool result = calcFactor();
    while (currentLexem == multLexem || currentLexem == divLexem)
    {
        nextLexem();
        if (!calcFactor())
        {
            return false;
        }
    }
    return result;
}

bool Parser::calcFactor()
{
    if (error)
    {
        return false;
    }
    bool result = true;
    if (currentLexem == oBraceLexem)
    {
        nextLexem();
        result = calcExpression();
        if (currentLexem != cBraceLexem)
        {
            return false;
        }
    }
    else if (currentLexem != numberLexem)
    {
        return false;
    }
    nextLexem();
    return result;
}

bool Parser::calcNumber()
{
    double result = 0.0;
    return numbersParser->run(getNumber(), result);
}

string Parser::getInt()
{
    string result = "";
    int length = line.length();
    while (pointer < length && line[pointer] >= '0' && line[pointer] <= '9')
    {
        result += line[pointer++];
    }
    return result;
}

string Parser::getNumber()
{
    string result = getInt();
    int length = line.length();
    if (pointer < length && line[pointer] == '.')
    {
        pointer++;
        result += '.' + getInt();
    }
    if (pointer < length && (line[pointer] == 'e' || line[pointer] == 'E'))
    {
        pointer++;
        result += 'e';
        if (pointer < length && (line[pointer] == '+' || line[pointer] == '-'))
        {
            result += line[pointer++];
        }
        result += getInt();
    }
    return result;
}

void Parser::nextLexem()
{
    char c = line[pointer];

    if (c == ' ') // whitespace
    {
        pointer++;
        nextLexem();
    }
    else if (c == '\0') // end of expression
    {
        currentLexem = endOfExpLexem;
    }
    else if (c == '+' || c == '-' || c == '*' || c == '/')
    {
        if (c == '+')
        {
            currentLexem = plusLexem;
        }
        else if (c == '-')
        {
            currentLexem = minusLexem;
        }
        else if (c == '*')
        {
            currentLexem = multLexem;
        }
        else
        {
            currentLexem = divLexem;
        }
        pointer++;
    }
    else if (c == '(' || c == ')')
    {
        currentLexem = (c == '(') ? oBraceLexem : cBraceLexem;
        pointer++;
    }
    else if (c >= '0' || c <= '9')
    {
        currentLexem = numberLexem;
        if (!calcNumber())
        {
            error = true;
        }
    }
    else
    {
        error = true;
    }
}
