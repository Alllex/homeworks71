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
    #define IN "suffarray.in"
    #define OUT "suffarray.out"
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
const int A = 32;
const int Z = 127;
const int CAZ = Z - A + 1;

int n;
char s[MAXLEN];

int p[MAXLEN]{},
    pn[MAXLEN]{},
    clr[MAXLEN]{},
    clrd[MAXLEN]{},
    cnt[MAXLEN]{};

void prepare() {
    for (int i = 0; i < n; ++cnt[(int)s[i++]]);
    for (int i = 0; i < CAZ; cnt[i+1] += cnt[i], ++i);
    for (int i = 0; i < n; p[--cnt[(int)s[i]]] = i, ++i);
    int color = 1;
    clr[p[0]] = 0;
    for (int i = 0; i < n-1; ++i) {
        if (s[p[i]] != s[p[i+1]]) ++color;
        clr[p[i+1]] = color-1;
    }
    for (int h = 0;; ++h) {
        int hh = 1 << h;
        if (hh >= n) return;
        for (int i = 0; i < n; ++i) {
            pn[i] = p[i] - hh;
            if (pn[i] < 0)  pn[i] += n;
            if (i < color) cnt[i] = 0;
        }
        for (int i = 0; i < n; ++cnt[clr[pn[i++]]]);
        for (int i = 0; i < color-1; cnt[i+1] += cnt[i], ++i);
        for (int i = n-1; i >= 0; p[--cnt[clr[pn[i]]]] = pn[i], --i);
        color = 1; clrd[p[0]] = 0;
        for (int i = 1; i < n; ++i) {
            if (clr[(p[i-1]+hh) % n] != clr[(p[i]+hh) % n]
                || clr[p[i]] != clr[p[i-1]])
                ++color;
            clrd[p[i]] = color-1;
        }
        for (int i = 0; i < n; clr[i] = clrd[i], ++i);
    }
}

void read(FILE *in) {
    fgets(s, MAXLEN, in);
    n = strlen(s);
    if (s[n-1] == '\n') s[n-1] = 0; else s[n++] = 0;
    for (int i = 0; i < n-1; s[i++] -= A - 1);
}

int main()
{
    FILE *in  = fopen(IN,"r");
    FILE *out = fopen(OUT,"w");
    read(in);
    prepare();
    for (int i = 0; i < n-1; fprintf(out, "%d ", 1+p[1+i++])) ;
    fclose(in);
    fclose(out);
    return 0;
}
