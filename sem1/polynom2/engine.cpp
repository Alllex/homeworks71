#include "engine.h"

#include <iostream>
#include <cmath>

using namespace std;

void Engine::clearScreen()
{
    //magic clear console
    cout<<"\E[H\E[J";
}

void Engine::addPolynomial(Polynom p)
{
    if (size < maxSize)
    {
        list[size++] = p;
    }
}

void Engine::printPolynomials()
{
    cout << "Saved polynomials:\n";
    for (int i = 0; i < size; ++i)
    {
        cout << i + 1 << ") " << list[i] << endl;
    }
    if (size == 0)
    {
        cout << "Nothing\n";
    }
}

void Engine::printCalculateMenu()
{
    clearScreen();
    cout << "Calculating value of polynomial in point\n"
            "Type number of polinomial from list.\n"
            "And enter double value of point.\n";
    printPolynomials();
    int number = 0;
    double point = 0.0;
    cin >> number >> point;
    cout << number << ") p(" << point << ") = " << list[number - 1](point) << endl;
    cout << "Type '0' to return or '1' repeat\n";
    int command = 0;
    cin >> command;
    if (command == 1)
    {
        printCalculateMenu();
    }
}

void Engine::printViewMenu()
{
    clearScreen();
    printPolynomials();
    cout << "Type 0 to return\n";
    int command = 0;
    cin >> command;
}

void Engine::printCompareMenu()
{
    clearScreen();
    cout << "Compare menu\nChoose two polinomials from list to compare (type their numbers)\n";
    printPolynomials();
    int first = 0;
    int second = 0;
    cin >> first >> second;
    cout << ((list[--first] == list[--second]) ? "They are equals\n" : "They are different\n");
    cout << "Type '0' to return or '1' repeat\n";
    int command = 0;
    cin >> command;
    if (command == 1)
    {
        printCompareMenu();
    }
}

Command Engine::userAction(Command command)
{
    if (command == cAdd)
    {
        printAddMenu();
    }
    else if (command == cCompare)
    {
        printCompareMenu();
    }
    else if (command == cSum)
    {
        printSumMenu();
    }
    else if (command == cCalc)
    {
        printCalculateMenu();
    }
    else if (command == cViewList)
    {
        printViewMenu();
    }
    else
    {
        return cExit;
    }
    return cRepeat;
}

void Engine::printSumMenu()
{
    clearScreen();
    cout << "Sum menu\nChoose two polinomials from list to sum (type their numbers)\n";
    printPolynomials();
    int first = 0;
    int second = 0;
    cin >> first >> second;
    addPolynomial(Polynom(list[--first] + list[--second]));
    cout << "New Polynom p(x) = " << list[size - 1] << endl;
    cout << "Type '0' to return or '1' repeat\n";
    int command = 0;
    cin >> command;
    if (command == 1)
    {
        printSumMenu();
    }
}

Command Engine::printMainMenu()
{
    clearScreen();
    cout << "Type command number\n"
            "Main menu:\n"
            "1) Add new polynomial\n"
            "2) Compare polynomials\n"
            "3) Get polynomial's value in point\n"
            "4) Sum polynomials\n"
            "5) View list of polynomials\n"
            "0) Exit\n";
    int command = 0;
    cin >> command;
    Command result = userAction((Command) command);
    return result;
}

void Engine::printAddMenu()
{
    clearScreen();
    cout << "Type pairs of factor and placeholder power: (k, p) == k * x^p\n"
            "k = 0 is end of input\n"
            "k is double, p is int\n";
    const double eps = 1e-10;
    double k = 0.0;
    int p = 0;
    Polynom q;
    while (true)
    {
        cin >> k;
        if (std::abs(k) < eps)
        {
            break;
        }
        cin >> p;
        q += Term(k, p);
    }
    addPolynomial(q);
    cout << "New Polynom p(x) = " << list[size - 1] << endl;
    cout << "Type '0' to return or '1' repeat\n";
    int command = 0;
    cin >> command;
    if (command == 1)
    {
        printAddMenu();
    }
}
