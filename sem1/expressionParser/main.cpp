#include <iostream>
#include <string>

using namespace std;

#include "parser.h"

void test(Parser &parser, string s)
{
    cout << "Exp: " << s << endl;
    cout << "\t\t\t\t\tResult = " << parser.calc(s) << endl;
}

int main()
{
    Parser parser;
    test(parser, "1e-1.1");
    test(parser, "1e+1");
    return 0;
}

