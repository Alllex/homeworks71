#include <iostream>
#include <stdio.h>
#include <vector>
#include <queue>
#include <cstdlib>
#include <algorithm>
#include <string>
#include <cstring>

using namespace std;

#ifdef LOCALLY
    #define IN "input"
    #define OUT "output"
#else
    #define IN "basis.in"
    #define OUT "basis.out"
#endif

typedef vector<int> vi;
typedef vector<vi> vvi;
typedef vector<bool> vb;
typedef unsigned long long ull;

#define pb(x) push_back(x)
#define loop(i, n) for(int i = 0; i < (n); ++i)
#define pool(i, n) for(int i = (n)-1; i >= 0; --i)
#define looop(i, j, n) loop(i, n) loop(j, n)
#define filter(i, n, cond) loop(i, n) if (cond)

#define outs(fmt) fprintf(out, fmt)
#define out1(fmt, arg) fprintf(out, fmt, (arg))
#define outl outs("\n")
#define outi(i) out1("%d", i)
#define outis(i) out1("%d ", i)
#define outloop(i, n, fmt, ith) loop(i, n) out1(fmt, ith)
#define outarr(n, arr) outloop(_i_, n, "%d ", arr[_i_])
#define coutarr(n, arr) loop(_i_, n) cout << arr[_i_] << " "

#define in1i(i) fscanf(in, "%d", &i)
#define in2i(i, j) fscanf(in, "%d %d", &i, &j)


const int MAXLEN = 50007;

int n;
char s[MAXLEN];
int pf[MAXLEN]{};

int main()
{
    FILE *in  = fopen(IN,"r");
    FILE *out = fopen(OUT,"w");
    fscanf(in, "%s", s);
    n = strlen(s);
    loop(i, n-1) {
        int j;
        for (j = pf[i]; j > 0 && s[i+1] != s[j]; j = pf[j-1]);
        pf[i+1] = (s[i+1] != s[j]) ? j : j+1;
    }
    outi(n - pf[n - 1]);
    fclose(in);
    fclose(out);
    return 0;
}