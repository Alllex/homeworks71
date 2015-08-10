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
    #define IN "dictionary.in"
    #define OUT "dictionary.out"
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
#define filter1(i, n, cond) loop1(i, n) if (cond)

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

const int N = 1000007;
const int NN = 2*N + 1;
const int WN = 37;
const int A = 27;

struct Node {
    int l, r, p, link;
    int nxt[A];
    Node() { memset(nxt, -1, A * sizeof(int)); }
    int len() { return r - l + 1; }
    inline void lr(int ll, int rr) { l = ll; r = rr; }
    inline void lp(int ll, int pp) { l = ll; p = pp; }
};

int n, m;
char s[N], w[WN];
Node st[NN];
int tv,tp,ts;

inline int si(int i) { return s[i] - 'a'; }
inline void mv(int tvv, int tpp) { tv = tvv; tp = tpp; }

void append(int la) {
    for(int c = si(la);;) {
        if (st[tv].r < tp) {
            if (st[tv].nxt[c] == -1) {
                st[tv].nxt[c] = ts;
                st[ts++].lp(la, tv);
                mv(st[tv].link, st[tv].r+1);
                continue;
            }
            tv = st[tv].nxt[c]; tp = st[tv].l;
        }
        if (tp == -1 || c == si(tp)) { ++tp; return; }
        st[ts+1].lp(la, ts);
        st[ts].lr(st[tv].l, tp-1);
        st[ts].p = st[tv].p;
        st[ts].nxt[c] = ts+1;
        st[ts].nxt[si(tp)] = tv;
        st[tv].lp(tp, ts);
        st[st[ts].p].nxt[si(st[ts].l)] = ts;
        mv(st[st[ts].p].link, st[ts].l);
        while (st[ts].r >= tp) tv = st[tv].nxt[si(tp)], tp += st[tv].len();
        st[ts].link = tp == st[ts].r+1 ? tv : ts+2;
        tp = st[tv].r-tp+st[ts].r+2;
        ts+=2;
    }
}

void prepare() {
    n = strlen(s);
    s[n++] = 'z'+1;
    ts=2; tv=0; tp=0;
    loop(i, NN) st[i].r = n-1;
    st[0].link = 1;
    st[0].lr(-1, -1); st[1].lr(-1, -1);
    loop(i, A) st[1].nxt[i] = 0;
    loop(i, n) append(i);
}

bool query() {
    int c = 0, wl = strlen(w);
    for (int i = 0; i < wl;) {
        if ((c = st[c].nxt[w[i] - 'a']) == -1) return false;
        for (int j = st[c].l; i < wl && j <= st[c].r; j++)
            if (w[i++] != s[j]) return false;
    }
    return true;
}


int main()
{
    FILE *in  = fopen(IN,"r");
    FILE *out = fopen(OUT,"w");

    fscanf(in, "%s", s);
    in1i(m);
    prepare();
    loop(o, m) {
        fscanf(in, "%s", w);
        outs(query() ? "Yes" : "No"); outl;
    }
    fclose(in);
    fclose(out);
    return 0;
}