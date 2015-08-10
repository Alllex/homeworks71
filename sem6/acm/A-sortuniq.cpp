#include <iostream>
#include <stdio.h>

using namespace std;

#define PROBLEM_NAME "sortuniq"

int main()
{
    FILE *in  = fopen (PROBLEM_NAME".in","r");
    FILE *out = fopen (PROBLEM_NAME".out","w");

    int n = 0;
    fscanf(in, "%d", &n);
    for (int i = 0; i < n; ++i) {
        fprintf(out, "%d ", i + 1);
    }

    fclose(in);
    fclose(out);
    return 0;
}

