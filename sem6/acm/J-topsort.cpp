#include <iostream>
#include <stdio.h>
#include <vector>
#include <algorithm>

using namespace std;

#define PROBLEM_NAME "topsort"

typedef vector<bool> vb;
typedef vector<int> vi;
typedef vector<vi> vvi;

vvi g;
vi color, ans;
bool hasCycle = false;
int n;

//void print() {
//    for (int i = 0; i < n; ++i) {
//        printf("%d:", i+1);
//        for (size_t j = 0; j < g[i].size(); ++j) {
//            printf(" %d", g[i][j]+1);
//        }
//        printf("\n");
//    }
//    printf("------------\n");
//}

void dfs(int v) {
    if (hasCycle) return;
    color[v] = 1;
    for (size_t i = 0; i < g[v].size(); ++i) {
        int u = g[v][i];
        if (color[u] == 1) {
            hasCycle = true;
            return;
        }
        if (!color[u]) dfs(u);
    }
    color[v] = 2;
    ans.push_back(v+1);
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
        if (!color[i]) dfs(i);
    }

    if (hasCycle) {
        fprintf(out, "-1\n");
    } else {
        reverse(ans.begin(), ans.end());
        for (int i = 0; i < n; ++i) {
            fprintf(out, "%d ", ans[i]);
        }
    }
    fclose(in);
    fclose(out);
    return 0;
}
