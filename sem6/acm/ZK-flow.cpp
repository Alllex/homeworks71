#include <iostream>
#include <stdio.h>
#include <vector>
#include <queue>
#include <cstdlib>

using namespace std;

#define PROBLEM_NAME "flow"

typedef vector<int> vi;
typedef vector<vi> vvi;
typedef vector<bool> vb;
typedef unsigned long long ull;

#define pb(x) push_back(x)
#define loop(i,n) for(int i = 0; i < n; ++i)

const int INF = 1000000000;

struct Edge {
    int a, b, c, f;
    bool rv;
    Edge(int a, int b,
         bool rv = false,
         int c = 0, int f = 0): a(a), b(b), c(c), f(f), rv(rv) {}
};

int n, s, t;
vi d, p;
vvi g;
vector<Edge> es;

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

int main()
{
    FILE *in  = fopen (PROBLEM_NAME".in","r");
    FILE *out = fopen (PROBLEM_NAME".out","w");

    int m, k = 0;
    fscanf(in, "%d %d ", &n, &m);
    s = 0; t = n-1;
    g.assign(n, vi());
    loop(i, m) {
        int fr, to, cap;
        fscanf(in, "%d %d %d ", &fr, &to, &cap);
        fr--; to--;
        g[fr].pb(k++); es.pb(Edge(fr, to, false, cap));
        g[to].pb(k++); es.pb(Edge(to, fr, false));
        g[to].pb(k++); es.pb(Edge(to, fr, true, cap));
        g[fr].pb(k++); es.pb(Edge(fr, to, true));
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
    fprintf(out, "%d\n", max_flow);
    for (int i = 0; i<(int)es.size(); i+=4) {
        int f = 0;
        if (es[i].f > 0 && es[i+2].f == 0) f = es[i].f;
        if (es[i+2].f > 0 && es[i].f == 0) f = -es[i+2].f;
        fprintf(out, "%d\n", f);
    }

    fclose(in);
    fclose(out);
    return 0;
}