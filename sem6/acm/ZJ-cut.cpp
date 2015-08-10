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
    #define IN "cut.in"
    #define OUT "cut.out"
#endif

typedef vector<int> vi;
typedef vector<vi> vvi;
typedef vector<bool> vb;
typedef unsigned long long ull;

#define pb(x) push_back(x)
#define loop(i,n) for(int i = 0; i < n; ++i)

const int INF = 1000000000;

struct Edge {
    int a, b, c, f;
    Edge(int a, int b, int c = 0, int f = 0): a(a), b(b), c(c), f(f) {}
};

int n, s, t;
vi d, p;
vvi g;
vector<Edge> es;
vb u;

int dfs(int v, int flow) {
    if (flow == 0) return 0; else if (v == t) return flow;
    int sz = (int)g[v].size();
    while (p[v] < sz) {
        int id = g[v][p[v]], to = es[id].b;
        if (d[to] != d[v] + 1) { p[v]++; continue; }
        int pd = dfs(to, min(flow, es[id].c - es[id].f));
        if (pd) {
            es[id].f   += pd;
            es[id^1].f -= pd;
            return pd;
        }
        p[v]++;
    }
    return 0;
}

void dfs2(int v) {
    u[v] = true;
    for (auto id : g[v])
      if (!u[es[id].b] && es[id].c - es[id].f > 0)
        dfs2(es[id].b);
}

int main()
{
    FILE *in  = fopen(IN,"r");
    FILE *out = fopen(OUT,"w");
    int m, k = 0;
    fscanf(in, "%d %d", &n, &m);
    s = 0; t = n - 1;
    g.assign(n, vi());
    vector<Edge> ges;
    loop(i, m) {
        int fr, to, c;
        fscanf(in, "%d %d %d ", &fr, &to, &c);
        fr--; to--;
        g[fr].pb(k++); es.pb(Edge(fr, to, c));
        g[to].pb(k++); es.pb(Edge(to, fr));
        g[to].pb(k++); es.pb(Edge(to, fr, c));
        g[fr].pb(k++); es.pb(Edge(fr, to));
        ges.pb(Edge(fr, to));
    }

    int max_flow = 0;
    while (true) {
        queue<int> q; q.push(s);
        d.assign(n, -1); d[s] = 0;
        while (!q.empty() && d[t] == -1) {
            int v = q.front(); q.pop();
            for (auto id : g[v]) {
                int to = es[id].b;
                if (d[to] == -1 && es[id].f < es[id].c) {
                    q.push(to); d[to] = d[v] + 1;
                }
            }
        }
        if (d[t] == -1) break;
        p.assign(n, 0);
        for(;;) {
            int portion = dfs(s, INF);
            if (portion == 0) break;
            max_flow += portion;
        }
    }

    u.assign(n, false);
    dfs2(s);
    vi cut;
    loop(i, m) if (u[ges[i].a]^u[ges[i].b]) cut.pb(i);
    sort(cut.begin(), cut.end());
    fprintf(out, "%d %d\n", cut.size(), max_flow);
    for (auto i : cut) fprintf(out, "%d ", i+1);

    fclose(in);
    fclose(out);
    return 0;
}
