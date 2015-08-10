#include <iostream>
#include <stdio.h>

using namespace std;

#define PROBLEM_NAME "sort"

void heapify(int a[], int k, int n) {
    while (2 * k <= n) {
        int j = 2 * k;
        if (j < n && a[j] < a[j+1]) j++;
        if (a[k] >= a[j]) break;
        swap(a[k], a[j]);
        k = j;
    }
}

void sort(int a[], int n) {
    for (int i = n/2; i >= 1; --i) heapify(a, i, n);
    while (n > 1) {
        swap(a[1], a[n--]);
        heapify(a, 1, n);
    }
}

int main()
{
    FILE *in  = fopen (PROBLEM_NAME".in","r");
    FILE *out = fopen (PROBLEM_NAME".out","w");

    int n = 0;
    fscanf(in, "%d", &n);
    int a[n+1];
    for (int i = 1; i <= n; ++i) {
        fscanf(in, "%d", &a[i]);
    }
    sort(a, n);
    for (int i = 1; i <= n; ++i) {
        fprintf(out, "%d ", a[i]);
    }

    fclose(in);
    fclose(out);
    return 0;
}
