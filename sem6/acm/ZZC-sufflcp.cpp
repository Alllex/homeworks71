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
    #define IN "sufflcp.in"
    #define OUT "sufflcp.out"
#endif

#define pb(x) push_back(x)
#define loop(i, n) for(int i = 0; i < (n); ++i)
#define loop1(i, n) for(int i = 1; i < (n); ++i)
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

const int MAXLEN = 100007;
int n;
char s[MAXLEN];
int p[MAXLEN], rp[MAXLEN], lcp[MAXLEN];

void mklcp() {
    int k = 0;
    loop(i, n) {
        if (rp[i] == n - 1) { k = 0; continue; }
        int j = p[1+rp[i]];
        if (k > 0) k--;
        while (n - k > max(i, j) && s[j+k] == s[i+k]) k++;
        lcp[rp[i]] = k;
    }
}

void read(FILE *in) {
    in1i(n);
    fscanf(in, "%s", s);
    loop(i, n) in1i(p[i]), rp[--p[i]] = i;
}

int main()
{
    FILE *in  = fopen(IN,"r");
    FILE *out = fopen(OUT,"w");
    read(in);
    mklcp();
    outloop(i, n-1, "%d ", lcp[i]);
    fclose(in);
    fclose(out);
    return 0;
}
