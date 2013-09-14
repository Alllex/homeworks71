#ifndef STATEAUTOMATE_H
#define STATEAUTOMATE_H

#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <cmath>

using namespace std;

enum Lexem
{
    digit,
    dot,
    exponent,
    sign,
    exit,
    start,
    finish,
    unknown
};

struct States
{
    int amountCases;
    vector<Lexem> lexem;
    vector<int> jump;
    States(): amountCases(0) {}
};

class StateAutomate
{
public:
    StateAutomate(char *rulesFile);
    bool run(string number, double &result);

    void printRules() const;
private:
    static const int maxSize = 10;
    int size;
    States rules[maxSize];

    int getDestination(const int state, const Lexem word) const;
    void setRules(char *rulesFile);
    static Lexem getLexem(string &word);
    static string getName(Lexem word);
};

#endif // STATEAUTOMATE_H
