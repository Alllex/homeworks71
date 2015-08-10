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
    #define IN "suftree.in"
    #define OUT "suftree.out"
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

const int N = 1000000;
const int A = 27;

struct Node {
    int l, r, p, link;
    int nxt[A];
    Node() { loop(i, A) nxt[i] = -1; }
    int len() { return r - l + 1; }
    void lr(int ll, int rr) { l = ll; r = rr; }
    void lp(int ll, int pp) { l = ll; p = pp; }
};

int n;
char s[N];
Node st[N];
int tv,tp,ts;

int si(int i) { return s[i] - 'a'; }
void mv(int tvv, int tpp) { tv = tvv; tp = tpp; }

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

int vc = 0;
void pdfs(FILE *out, int v) {
    if (v != 0) fprintf(out, "%d %d %d\n", st[v].p, st[v].l, st[v].r+1);
    int par = vc++;
    loop(i, A) {
        int to = st[v].nxt[i ? i-1 : A-1];
        if (to != -1) st[to].p = par, pdfs(out, to);
    }
}

int main()
{
    FILE *in  = fopen(IN,"r");
    FILE *out = fopen(OUT,"w");

    fscanf(in, "%s", s);
    n = strlen(s);
    s[n-1] = 'z' + 1;
    ts=2; tv=0; tp=0;
    loop(i, N) st[i].r = n-1;
    st[0].link = 1;
    st[0].lr(-1, -1); st[1].lr(-1, -1);
    loop(i, A) st[1].nxt[i] = 0;

    loop(i, n) append(i);
    outi(ts-1); outl;
    pdfs(out, 0);

    fclose(in);
    fclose(out);
    return 0;
}
