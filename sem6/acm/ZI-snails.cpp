#include <iostream>
#include <stdio.h>
#include <vector>
#include <queue>
#include <cstdlib>

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
    #define IN "snails.in"
    #define OUT "snails.out"
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
    FILE *in  = fopen(IN,"r");
    FILE *out = fopen(OUT,"w");
    int m, k = 0;
    fscanf(in, "%d %d %d %d", &n, &m, &s, &t);
    s--; t--;
    g.assign(n, vi());
    loop(i, m) {
        int fr, to;
        fscanf(in, "%d %d ", &fr, &to);
        if (fr == to) continue;
        fr--; to--;
        g[fr].pb(k++); es.pb(Edge(fr, to, 1));
        g[to].pb(k++); es.pb(Edge(to, fr));
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

    if (max_flow < 2) {
        fprintf(out, "NO\n");
    } else {
        fprintf(out, "YES\n");
        loop(o, 2) {
            int cur = s;
            fprintf(out, "%d ", cur+1);
            while (cur != t) {
                for (auto id : g[cur]) {
                    if (es[id].f > 0) {
                        es[id].f = 0;
                        cur = es[id].b;
                        fprintf(out, "%d ", cur+1);
                        break;
                    }
                }
            }
            fprintf(out, "\n");
        }
    }

    fclose(in);
    fclose(out);
    return 0;
}
