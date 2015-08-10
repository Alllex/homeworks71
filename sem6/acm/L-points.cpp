#include <iostream>
#include <stdio.h>
#include <vector>
#include <algorithm>

using namespace std;

#define PROBLEM_NAME "points"

typedef vector<bool> vb;
typedef vector<int> vi;
typedef vector<vi> vvi;

vvi g;
vi ans, tin, fup;
vb used;
int n, timer;

void dfs (int v, int p = -1) {
    used[v] = true;
    tin[v] = fup[v] = timer++;
    int children = 0;
    for (size_t i=0; i<g[v].size(); ++i) {
        int to = g[v][i];
        if (to == p)  continue;
        if (used[to])
            fup[v] = min (fup[v], tin[to]);
        else {
            dfs (to, v);
            fup[v] = min (fup[v], fup[to]);
            if (fup[to] >= tin[v] && p != -1) ans.push_back(v);
            ++children;
        }
    }
    if (p == -1 && children > 1)
        ans.push_back(v);
}

int main()
{
    FILE *in  = fopen (PROBLEM_NAME".in","r");
    FILE *out = fopen (PROBLEM_NAME".out","w");

    int k;
    fscanf(in, "%d %d ", &n, &k);
    for (int i = 0; i < n; ++i) {
        used.push_back(false);
        tin.push_back(0);
        fup.push_back(0);
        vi l;
        g.push_back(l);
    }
    for (int i = 0; i < k; ++i) {
        int x, y;
        fscanf(in, "%d %d ", &x, &y);
        g[x-1].push_back(y-1);
        g[y-1].push_back(x-1);
    }

    timer = 0;
    dfs(0);

    sort(ans.begin(), ans.end());
    ans.erase( unique( ans.begin(), ans.end() ), ans.end() );
    fprintf(out, "%d\n", (int)ans.size());
    for (size_t i = 0; i < ans.size(); ++i) {
        fprintf(out, "%d\n", ans[i]+1);
    }

    fclose(in);
    fclose(out);
    return 0;
}
