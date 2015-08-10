#include <iostream>
#include <stdio.h>
#include <string.h>

using namespace std;

typedef unsigned long long ull;

#define PROBLEM_NAME "sum"

//void print(int a[], int l, int r,
//           const char *fmt,
//           const char *end,
//           FILE* out)
//{
//    for (int i = l; i < r; i++) {
//        fprintf(out, fmt, a[i]);
//    }
//    fprintf(out, end);
//}

ull sum(ull t[], long v,
        int tl, int tr,
        int l, int r)
{
    if (l > r) return 0;
//    printf("v(%d) tl(%d) tr(%d) l(%d) r(%d)\n", v, tl, tr, l, r);
    if (l == tl && r == tr) return t[v];
    int tm = (tl + tr) / 2;
    return sum(t, v*2, tl, tm, l, min(r, tm)) +
     sum(t, v*2+1, tm+1, tr, max(l, tm+1), r);
}

void update (ull t[], long v,
             int tl, int tr,
             int p, int val)
{
//    printf("v(%d) tl(%d) tr(%d)\n", v, tl, tr);
    if (tl == tr) t[v] = val;
    else {
        int tm = (tl + tr) / 2;
        if (p <= tm) update (t, v*2, tl, tm, p, val);
        else update (t, v*2+1, tm+1, tr, p, val);
        t[v] = t[v*2] + t[v*2+1];
    }
}

int main()
{
    FILE *in  = fopen (PROBLEM_NAME".in","r");
    FILE *out = fopen (PROBLEM_NAME".out","w");

    int n, k;
    fscanf(in, "%d %d ", &n, &k);
    int N = n * 4;
    ull t[N];
    for (int i = 0; i < N; t[i++] = 0LL);

    for (int i = 0; i < k; ++i) {
        char c;
        int x, y;
        fscanf(in, "%c %d %d ", &c, &x, &y);
        if (c == 'A') {
            update(t, 1, 0, n-1, x-1, y);
//            fprintf(stdout, "a[%d] := %d;\n", x, y);
//            print(t, 0, N, "%d ", "\n", stdout);
        } else if (c == 'Q') {
            ull res = sum(t, 1, 0, n-1, x-1, y-1);
            fprintf(out, "%I64u\n", res);
        }
    }

    fclose(in);
    fclose(out);
    return 0;
}
