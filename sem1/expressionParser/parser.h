#ifndef PARSER_H
#define PARSER_H

#include <iostream>
#include <string>

using namespace std;

#include "stateAutomate.h"

enum MathLexem
{
    numberLexem,
    plusLexem,
    minusLexem,
    multLexem,
    divLexem,
    oBraceLexem,
    cBraceLexem,
    endOfExpLexem,
    unknownLexem
};

class Parser
{
public:
    Parser();
    ~Parser();
    bool calc(string &expr);
    double getResult() const;

private:
    string line;
    int pointer;
    MathLexem currentLexem;
    StateAutomate *numbersParser;

    bool result;
    bool error;

    bool calcExpression();
    bool calcTerm();
    bool calcFactor();
    bool calcNumber();

    string getInt();
    string getNumber();
    void nextLexem();
};

#endif // PARSER_H
