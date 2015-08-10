#include <iostream>
#include <stdio.h>
#include <string.h>

using namespace std;

typedef unsigned long long ull;

#define PROBLEM_NAME "sum"

//void print(ull a[], int l, int r,
//           const char *fmt,
//           const char *end,
//           FILE* out)
//{
//    for (int i = l; i < r; i++) {
//        fprintf(out, fmt, a[i]);
//    }
//    fprintf(out, end);
//}

const int N = 100 * 1000;
ull a[N], t[N];
int n;

ull sumr(int r)
{
    ull res = 0;
    while (r >= 0) {
        res += t[r];
        r = (r & (r + 1)) - 1;
    }
    return res;
}

ull sum(int l, int r)
{
    return sumr(r) - sumr(l - 1);
}

void upd(int k, ull val)
{
    ull d = val - a[k];
    a[k] = val;
    while (k < n) {
        t[k] += d;
        k = (k | (k + 1));
    }
}

int main()
{
    FILE *in  = fopen (PROBLEM_NAME".in","r");
    FILE *out = fopen (PROBLEM_NAME".out","w");

    int k;
    fscanf(in, "%d %d ", &n, &k);
    for (int i = 0; i < n; ++i) {
        a[i] = 0LL;
        t[i] = 0LL;
    }

    for (int i = 0; i < k; ++i) {
        char c;
        int x, y;
        fscanf(in, "%c %d %d ", &c, &x, &y);
        if (c == 'A') {
            upd(x - 1, y);
        } else if (c == 'Q') {
            ull res = sum(x - 1, y - 1);
//            fprintf(stdout, "%llu\n", res);
            fprintf(out, "%I64u\n", res);
        }
    }

    fclose(in);
    fclose(out);
    return 0;
}
