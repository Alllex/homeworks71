#include <iostream>
#include <stdio.h>
#include <vector>
#include <queue>
#include <cstdlib>
#include <algorithm>
#include <string>
#include <cstring>

using namespace std;

#ifdef WIN
  #define LLD "%I64d"
#else
  #define LLD "%lld"
#endif

#ifdef LOCALLY
    #define IN "input"
    #define OUT "output"
#else
    #define IN "invtrans.in"
    #define OUT "invtrans.out"
#endif

typedef vector<int> vi;
typedef vector<vi> vvi;
typedef vector<bool> vb;
typedef unsigned long long ull;

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


const int MAXLEN = 50007;

int n;
vi pf, s, zf;

int main()
{
    FILE *in  = fopen(IN,"r");
    FILE *out = fopen(OUT,"w");
    in1i(n);
    pf.assign(n, 0);
    zf.assign(n, 0);
    s.assign(n, 0);
    loop(i, n) in1i(pf[i]);
    int ch = 'a';
    loop(i, n) s[i] = pf[i] > 0 ? s[pf[i]-1] : ch++;
    int l = 0, r = 0;
    loop1(i, n) {
        int &z = zf[i] = min(r-i, zf[i-l]);
        if (z < 0) z = 0;
        while (i+z < n && s[z] == s[z+i]) z++;
        if (z+i >= r) l=i, r=z+i;
    }
    zf[0] = n;
    outarr(n, zf);
    fclose(in);
    fclose(out);
    return 0;
}
