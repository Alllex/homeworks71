#include <iostream>
#include <stdio.h>
#include <vector>
#include <queue>
#include <algorithm>

using namespace std;

#define PROBLEM_NAME "distance"
#define mp(a, b) make_pair(a, b)

typedef unsigned long long ull;
typedef vector<bool> vb;
typedef vector<int> vi;
typedef vector<vi> vvi;
typedef pair<int, int> pii;
typedef vector<pii> vpii;

const int INF = 1000000000;

int main()
{
    FILE *in  = fopen (PROBLEM_NAME".in","r");
    FILE *out = fopen (PROBLEM_NAME".out","w");

    int n, m, s, f;
    fscanf(in, "%d %d %d %d", &n, &m, &s, &f);
    s--; f--;
    vector< vpii > g(n, vpii());
    for (int i = 0; i < m; ++i) {
        int x, y, w;
        fscanf(in, "%d %d %d ", &x, &y, &w);
        g[x-1].push_back(mp(y-1, w));
        g[y-1].push_back(mp(x-1, w));
    }
    vi d(n, INF);
    vi p(n, 0);
    vb used(n, false);

    d[s] = 0;
    p[s] = -1;
    for (int i=0; i<n; ++i) {
        int v = -1;
        for (int j=0; j<n; ++j)
            if (!used[j] && (v == -1 || d[j] < d[v])) v = j;
        if (d[v] == INF) break;
        used[v] = true;
        for (auto &pair : g[v]) {
            int to = pair.first;
            int len = pair.second;
            if (d[v] + len < d[to]) {
                d[to] = d[v] + len;
                p[to] = v;
            }
        }
    }

    if (d[f] == INF) {
        fprintf(out, "-1");
    } else {
        vi path;
        for (int v = f; v != s; v = p[v]) path.push_back (v);
        path.push_back(s);
        reverse (path.begin(), path.end());
        fprintf(out, "%d\n", d[f]);
        for (auto &v : path) {
            fprintf(out, "%d ", v+1);
        }
    }

    fclose(in);
    fclose(out);
    return 0;
}
