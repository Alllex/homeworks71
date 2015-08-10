#include <iostream>
#include <stdio.h>
#include <vector>
#include <algorithm>

using namespace std;

#define PROBLEM_NAME "longpath"

typedef vector<bool> vb;
typedef vector<int> vi;
typedef vector<vi> vvi;
typedef vector<vb> vvb;

vvi g;
vi length;
vb used;
int n;

//void print(vvi g) {
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
    used[v] = true;
    for (size_t i=0; i<g[v].size(); ++i) {
        int u = g[v][i];
        if (!used[u]) {
            dfs(u);
        }
        length[v] = max(length[v], length[u] + 1);
    }
}

int main()
{
    FILE *in  = fopen (PROBLEM_NAME".in","r");
    FILE *out = fopen (PROBLEM_NAME".out","w");

    int k;
    fscanf(in, "%d %d ", &n, &k);
    g.assign(n, vi());
    for (int i = 0; i < k; ++i) {
        int x, y;
        fscanf(in, "%d %d ", &x, &y);
        g[x-1].push_back(y-1);
    }

    used.assign(n, false);
    length.assign(n, 0);
    for (int i = 0; i < n; ++i)
        if (!used[i]) dfs(i);

    int ans = *max_element(length.begin(), length.end());
    fprintf(out, "%d", ans);

    fclose(in);
    fclose(out);
    return 0;
}
