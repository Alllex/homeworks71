#include <iostream>
#include <stdio.h>
#include <vector>

using namespace std;

#define PROBLEM_NAME "connect"

typedef vector<int> vi;
typedef vector<vi> vvi;

vvi g;
vi color;
int n;

void print() {
    for (int i = 0; i < n; ++i) {
        printf("%d:", i+1);
        for (size_t j = 0; j < g[i].size(); ++j) {
            printf(" %d", g[i][j]+1);
        }
        printf("\n");
    }
    printf("------------\n");
}

void dfs(int c, int v) {
    color[v] = c;
    for (int i = 0, len = (int)g[v].size(); i < len; ++i) {
        int u = g[v][i];
        if (!color[u]) dfs(c, u);
    }
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
        x--; y--;
        g[x].push_back(y);
        g[y].push_back(x);
    }
    int c = 0;
    for (int i = 0; i < n; ++i) {
        if (!color[i]) dfs(++c, i);
    }
    fprintf(out, "%d\n", c);
    for (int i = 0; i < n; ++i) {
        fprintf(out, "%d ", color[i]);
    }
//    printf("\n");
    fclose(in);
    fclose(out);
    return 0;
}
