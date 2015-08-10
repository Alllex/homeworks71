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
    #define IN "substrcmp.in"
    #define OUT "substrcmp.out"
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


const int MAXLEN = 1000007;
const int WMAXLEN = 256;

int n, m;
char s[MAXLEN];

int p[MAXLEN]{},
    c[MAXLEN]{},
    pn[MAXLEN]{};
int cn[20][MAXLEN]{};

void prepare() {
    loop(i, n) ++c[(int)s[i]-'a'];
    loop(i, 256) c[i+1] += c[i];
    loop(i, n) p[--c[(int)s[i]-'a']] = i;
    cn[0][p[0]] = 0;
    int color = 1;
    loop(i, n-1) {
        if (s[p[i+1]] != s[p[i]]) ++color;
        cn[0][p[i+1]] = color - 1;
    }
    int pwr = 1;
    for (int h = 0; pwr < n; ++h, pwr *= 2) {
        loop(i, n) {
            pn[i] = p[i] - pwr;
            if (pn[i] < 0)  pn[i] += n;
        }
        memset(c, 0, color*sizeof(int));
        loop(i, n) ++c[cn[h][pn[i]]];
        loop(i, color-1) c[i+1] += c[i];
        pool(i, n) p[--c[cn[h][pn[i]]]] = pn[i];
        cn[h+1][p[0]] = 0;
        color = 1;
        loop(i, n-1) {
            int m1 = (p[i+1] + pwr) % n, m2 = (p[i] + pwr) % n;
            if (cn[h][p[i+1]] != cn[h][p[i]] || cn[h][m1] != cn[h][m2]) ++color;
            cn[h+1][p[i+1]] = color - 1;
        }
    }
}

bool query(int a1, int b1, int a2, int b2) {
    int l = b1 - a1;
    if (l++ != b2 - a2) return false;
    int k = 0, pwr = 1;
    while (pwr <= l) k++, pwr *= 2;
    k--;
    return cn[k][a1] == cn[k][a2] && cn[k][a1+l-(1<<k)] == cn[k][a2+l-(1<<k)];
}

void solve_online(FILE *in, FILE *out) {
    prepare();
    loop(o, m) {
        int a1, b1, a2, b2; in2i(a1, b1); in2i(a2, b2);
        outs(query(a1-1,b1-1,a2-1,b2-1) ? "Yes" : "No"); // out1("\t%s", w);
        outl;
    }
}

void read(FILE *in) {
    fscanf(in, "%s", s);
    n = strlen(s);
    in1i(m);
}

int main()
{
    FILE *in  = fopen(IN,"r");
    FILE *out = fopen(OUT,"w");
    read(in); solve_online(in, out);
    fclose(in);
    fclose(out);
    return 0;
}
