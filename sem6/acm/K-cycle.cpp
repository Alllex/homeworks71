#include <iostream>
#include <stdio.h>
#include <vector>
#include <algorithm>

using namespace std;

#define PROBLEM_NAME "cycle"

typedef vector<bool> vb;
typedef vector<int> vi;
typedef vector<vi> vvi;

vvi g;
vi color, ans;
bool hasCycle = false;
int n, c;

void dfs(int v) {
    if (hasCycle) return;
    ans.push_back(v);
    color[v] = 1;
    for (size_t i = 0; i < g[v].size(); ++i) {
        if (hasCycle) return;
        int u = g[v][i];
        if (color[u] == 1) {
            hasCycle = true;
            ans.push_back(u);
            c = u;
            return;
        }
        if (!color[u]) {
            dfs(u);
        }
    }
    if (hasCycle) return;
    color[v] = 2;
    ans.pop_back();
}

int main()
{
    FILE *in  = fopen (PROBLEM_NAME".in","r");
    FILE *out = fopen (PROBLEM_NAME".out","w");

    int k;
    fscanf(in, "%d %d ", &n, &k);
    for (int i = 0; i < n; ++i) {
        color.push_back(0);
        vi l;
        g.push_back(l);
    }
    for (int i = 0; i < k; ++i) {
        int x, y;
        fscanf(in, "%d %d ", &x, &y);
        g[x-1].push_back(y-1);
    }

    for (int i = 0; i < n; ++i) {
        if (!color[i] && !hasCycle) dfs(i);
    }

    if (!hasCycle) {
        fprintf(out, "NO\n");
    } else {
        fprintf(out, "YES\n");
        size_t i = 0;
        while (ans[i++] != c);
        for (; i < ans.size(); ++i) {
            fprintf(out, "%d ", ans[i]+1);
        }
    }
    fclose(in);
    fclose(out);
    return 0;
}
