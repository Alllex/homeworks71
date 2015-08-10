#include <iostream>
#include <stdio.h>
#include <vector>
#include <queue>
#include <cstdlib>
#include <algorithm>

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
    #define IN "lca.in"
    #define OUT "lca.out"
#endif

typedef vector<int> vi;
typedef vector<vi> vvi;
typedef vector<bool> vb;
typedef unsigned long long ull;

#define pb(x) push_back(x)
#define loop(i, n) for(int i = 0; i < (n); ++i)
#define looop(i, j, n) loop(i, n) loop(j, n)
#define filter(i, n, cond) loop(i, n) if (cond)

#define outs(fmt) fprintf(out, fmt)
#define out1(fmt, arg) fprintf(out, fmt, (arg))
#define outl outs("\n")
#define outi(i) out1("%d", i)
#define outis(i) out1("%d ", i)
#define outn(i, n, fmt, ith) loop(i, n) out1(fmt, ith)

#define in1i(i) fscanf(in, "%d", &i)
#define in2i(i, j) fscanf(in, "%d %d", &i, &j)

const int INF = 1000000000;

int n, m;
vvi g;
vi ord, fst, h, t;
int len;

void dfs(int v, int hh) {
    h[v] = hh; ord.pb(v);
    for (auto to : g[v]) dfs(to, hh+1), ord.pb(v);
}

void bld(int v, int l, int r) {
    if (r == l) t[v] = ord[r];
    else {
        int mid = (l + r) / 2;
        bld(2*v, l, mid);
        bld(2*v+1, mid+1, r);
        t[v] = (h[t[2*v]] > h[t[2*v+1]]) ? t[2*v+1] : t[2*v];
    }
}

void prepare() {
    h.assign(n+1, -1);
    dfs(1, 0);
    len = (int)ord.size();
    fst.assign(n+1, -1);
    filter(i, len, fst[ord[i]] < 0) fst[ord[i]] = i;
    t.assign(4*len+1, -1);
    bld(1, 0, len-1);
}

int find_min(int v, int sl, int sr, int l, int r)
{
    if (sl == l && sr == r) return t[v];
    int sm = (sl + sr) / 2;
    if (r <= sm) return find_min(2*v, sl, sm, l, r);
    if (l > sm) return find_min(2*v+1, sm+1, sr, l, r);
    int v1 = find_min(2*v, sl, sm, l, sm);
    int v2 = find_min(2*v+1, sm+1, sr, sm+1, r);
    return h[v1] > h[v2] ? v2 : v1;
}

int query(int u, int v) {
    int l = fst[u], r = fst[v];
    if (l > r)  swap(l, r);
    return find_min(1, 0, len-1, l, r);
}

void solve_online(FILE *in, FILE *out) {
    prepare();
    loop(o, m) {
        int u, v; in2i(u, v);
        outi(query(u, v)); outl;
    }
}

void read(FILE *in) {
    in1i(n);
    g.assign(n+1, vi());
    for (int i = 2; i <= n; ++i) {
        int p; in1i(p);
        g[p].pb(i);
    }
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
