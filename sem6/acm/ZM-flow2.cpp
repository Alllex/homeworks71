#include <iostream>
#include <stdio.h>
#include <vector>
#include <cstdlib>

using namespace std;

#define PROBLEM_NAME "flow2"

typedef vector<int> vi;
typedef vector<vi> vvi;
typedef vector<bool> vb;
typedef unsigned long long ull;

#define pb(x) push_back(x)
#define loop(i,n) for(int i = 0; i < n; ++i)

const int INF = 1000000000;

struct Edge { int a, b, c, f; };
int n, s, t;
vi d, p;
vvi g;
vector<Edge> es;

void add(int from, int to, int cap) {
    Edge e1 { from, to, cap, 0 };
    Edge e2 { to, from, 0, 0 };
    g[from].pb((int) es.size()); es.pb(e1);
      g[to].pb((int) es.size()); es.pb(e2);
}

bool bfs() {
    int qh = 0, qt = 0;
    vi q(n); q[qt++] = s;
    d.assign(n, -1); d[s] = 0;
    while (qh < qt && d[t] == -1) {
        int v = q[qh++];
        for (auto id : g[v]) {
            int to = es[id].b;
            if (d[to] == -1 && es[id].f < es[id].c) {
                q[qt++] = to;
                d[to] = d[v] + 1;
            }
        }
    }
    return d[t] != -1;
}

int dfs(int v, int flow) {
    if (!flow)   return 0;
    if (v == t)  return flow;
    for (; p[v]<(int)g[v].size(); ++p[v]) {
        int id = g[v][p[v]],
            to = es[id].b;
        if (d[to] != d[v] + 1)  continue;
        int pushed = dfs(to, min(flow, es[id].c - es[id].f));
        if (pushed) {
            es[id].f   += pushed;
            es[id^1].f -= pushed;
            return pushed;
        }
    }
    return 0;
}

ull dinic() {
    ull flow = 0LL;
    for (;;) {
        if (!bfs()) break;
        p.assign(n, 0);
        while (int pushed = dfs(s, INF)) flow += pushed;
    }
    return flow;
}

int main()
{
    FILE *in  = fopen (PROBLEM_NAME".in","r");
    FILE *out = fopen (PROBLEM_NAME".out","w");

    int m;
    fscanf(in, "%d %d ", &n, &m);
    s = 0; t = n-1;
    g.assign(n, vi());
    loop(i, m) {
        int from, to, cap;
        fscanf(in, "%d %d %d ", &from, &to, &cap);
        add(from-1, to-1, cap);
    }

    ull max_flow = dinic();
//    cout << max_flow << endl;
    fprintf(out, "%I64u\n", max_flow);
    for (auto e : es) if (e.c > 0) {
//        cout << e.f << endl;
        fprintf(out, "%d\n", e.f);
    }

    fclose(in);
    fclose(out);
    return 0;
}
