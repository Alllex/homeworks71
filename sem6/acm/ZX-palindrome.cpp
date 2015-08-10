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
    #define IN "palindrome.in"
    #define OUT "palindrome.out"
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

const int MAXLEN = 100007;

int n;
char s[MAXLEN];

int main()
{
    FILE *in  = fopen(IN,"r");
    FILE *out = fopen(OUT,"w");

    fscanf(in, "%s", s);
    n = strlen(s);

    vvi d(2); d[0].assign(n, 0); d[1].assign(n, 0);
    loop (p, 2) {
        int l = 0, r = -1;
        loop(i, n) {
            int k = p;
            if (i <= r) k += min(r - i + p, d[p][l + r - i + p]);
            while (i + k - p < n && i - k >= 0 && s[i + k - p] == s[i - k]) ++k;
            d[p][i] = p ? --k : k--;
            if (i + k - p > r) l = i - k, r = i + k - p;
        }
    }
    ull res = 0LL;
    loop(i, n) res += d[0][i]+d[1][i]-1;
    out1(LLD, res);

    fclose(in);
    fclose(out);
    return 0;
}
