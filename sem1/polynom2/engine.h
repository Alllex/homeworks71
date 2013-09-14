#pragma once

using namespace std;

#include "polynom.h"

enum Command
{
    cExit = 0,
    cAdd,
    cCompare,
    cCalc,
    cSum,
    cViewList,
    cRepeat,
    cUnknown
};

class Engine
{
public:
    Engine(): size(0) {}
    Command printMainMenu();
    void printAddMenu();
    void printSumMenu();
    void printCalculateMenu();
    void printViewMenu();
    void printCompareMenu();
    Command userAction(Command command);
    void clearScreen();
private:
    static const int maxSize = 100;
    int size;
    Polynom list[maxSize];

    void addPolynomial(Polynom p);
    void printPolynomials();
};

