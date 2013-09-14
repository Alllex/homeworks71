#include "stateAutomate.h"

StateAutomate::StateAutomate(char *rulesFile):
    size(0)
{
    setRules(rulesFile);
}

bool StateAutomate::run(string number, double &result)
{
    int currentState = rules[0].jump[0];
    Lexem currentLexem = start;
    unsigned int pointer = 0;

    int powerSign = 1;
    result = 0.0;
    int expPower = 0;
    int addPower = 0;
    bool wasDot = false;
    bool wasExp = false;

    while (currentLexem != exit)
    {
        if (pointer == number.length())
        {
            currentLexem = exit;
        }
        else
        {
            char c = number[pointer++];
            if (c >= '0' && c <= '9')
            {
                currentLexem = digit;

                if (wasExp)
                {
                    expPower = expPower * 10 + (c - '0');
                }
                else if (wasDot)
                {
                    result += (c - '0') * pow(10.0, --addPower);
                }
                else
                {
                    result = result * 10.0 + (c - '0');
                }
            }
            else if (c == '.')
            {
                currentLexem = dot;
                wasDot = true;
            }
            else if (c == 'e' || c == 'E')
            {
                currentLexem = exponent;
                wasExp = true;
            }
            else if (c == '+' || c == '-')
            {
                powerSign = (c == '-') ? -1 : 1;
                currentLexem = sign;
            }
        }

        int next = getDestination(currentState, currentLexem);
        if (next < 0)
        {
            return false;
        }
        currentState = next;
    }
    result *= pow(10.0, powerSign * expPower);
    return true;
}

void StateAutomate::printRules() const
{
    for (int i = 0; i < size; ++i)
    {
        cout << "state " << i << "\n";
        for (int j = 0; j < rules[i].amountCases; ++j)
        {
            cout << "\t\t";
            cout << "case " << getName(rules[i].lexem[j]) << " jump " << rules[i].jump[j] << endl;
        }
        cout << endl;
    }
}

int StateAutomate::getDestination(const int state, const Lexem word) const
{
    for (int i = 0; i < rules[state].amountCases; ++i)
    {
        if (rules[state].lexem[i] == word)
        {
            return rules[state].jump[i];
        }
    }
    return -state;
}

void StateAutomate::setRules(char *rulesFile)
{
    ifstream input;
    input.open(rulesFile);
    if (!input.is_open())
    {
        return;
    }
    string s = "";
    while (true)
    {
        input >> s;
        if (s == "state")
        {
            States state;
            int id = 0;
            int jumpTo = 0;
            string word = "";
            input >> id >> word;
            if (word == "start")
            {
                input >> s >> jumpTo;
                state.lexem.push_back(getLexem(word));
                state.jump.push_back(jumpTo);
                state.amountCases++;
            }
            else if (word == "finish")
            {
                state.lexem.push_back(getLexem(word));
                state.jump.push_back(jumpTo);
                state.amountCases++;
                rules[size++] = state;
                break;
            }
            else if (word == "switch")
            {
                int amountCases = 0;
                input >> amountCases;
                for (int i = 0; i < amountCases; ++i)
                {
                    input >> s >> word >> s >> jumpTo;
                    state.lexem.push_back(getLexem(word));
                    state.jump.push_back(jumpTo);
                    state.amountCases++;
                }
            }
            rules[size++] = state;
        }
    }
    input.close();
}

Lexem StateAutomate::getLexem(string &word)
{
    if (word == "digit")
    {
        return digit;
    }
    else if (word == "dot")
    {
        return dot;
    }
    else if (word == "exp")
    {
        return exponent;
    }
    else if (word == "sign")
    {
        return sign;
    }
    else if (word == "exit")
    {
        return exit;
    }
    else if (word == "start")
    {
        return start;
    }
    else if (word == "finish")
    {
        return finish;
    }
    return unknown;
}

string StateAutomate::getName(Lexem word)
{
    if (word == digit)
    {
        return "digit";
    }
    else if (word == dot)
    {
        return "dot";
    }
    else if (word == exponent)
    {
        return "exp";
    }
    else if (word == sign)
    {
        return "sign";
    }
    else if (word == exit)
    {
        return "exit";
    }
    else if (word == start)
    {
        return "start";
    }
    else if (word == finish)
    {
        return "finish";
    }
    return "unknown";
}
