#include <iostream>
#include <stdio.h>
#include <vector>
#include <queue>
#include <algorithm>

using namespace std;

#define PROBLEM_NAME "maze"
#define mp(a, b) make_pair(a, b)

typedef unsigned long long ull;
typedef vector<bool> vb;
typedef vector<int> vi;
typedef vector<vi> vvi;
typedef pair<int, int> pii;
typedef vector<pii> vpii;

const int INF = 1000000000;

vvi g;
vb used;

void dfs(int v) {
    used[v] = true;
    for (auto &to : g[v])
        if (!used[to])
            dfs(to);
}

int main()
{
    FILE *in  = fopen (PROBLEM_NAME".in","r");
    FILE *out = fopen (PROBLEM_NAME".out","w");

    int n, m;
    fscanf(in, "%d %d", &n, &m);
    vpii e;
    vi c;
    g.assign(n, vi());
    for (int i = 0; i < m; ++i) {
        int x, y, w;
        fscanf(in, "%d %d %d ", &x, &y, &w);
        e.push_back(mp(x-1, y-1));
        c.push_back(w);
        g[x-1].push_back(y-1);
    }
    vi d(n, -INF);
    vi p(n, -1);
    d[0] = 0;
    int x;
    for (int i = 0; i < n; ++i) {
        x = -1;
        for (int j = 0; j < m; ++j)
            if (d[e[j].first] > -INF)
                if (d[e[j].second] < d[e[j].first] + c[j]) {
                    d[e[j].second] = min(INF, d[e[j].first] + c[j]);
                    p[e[j].second] = e[j].first;
                    x = e[j].second;
                }
    }
    bool useCycle = false;
    if (x != -1) {
        used.assign(n, false);
        for (int i=0; i<n; ++i) x = p[x];
        dfs(x);
        useCycle = used[n-1];
    }
    if (useCycle)
        fprintf(out, ":)");
    else if (d[n-1] == -INF)
        fprintf(out, ":(");
    else
        fprintf(out, "%d", d[n-1]);

    fclose(in);
    fclose(out);
    return 0;
}
