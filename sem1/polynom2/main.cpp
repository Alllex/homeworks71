#include <iostream>

using namespace std;

#include "polynom.h"
#include "engine.h"

int main()
{
    Engine menu;
    while (menu.printMainMenu() != cExit);
    return 0;
}

