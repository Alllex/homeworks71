#include <iostream>
#include <stdio.h>
#include <vector>
#include <algorithm>

using namespace std;

#define PROBLEM_NAME "condense2"

typedef vector<bool> vb;
typedef vector<int> vi;
typedef vector<vi> vvi;
typedef vector<vb> vvb;

vvi g, gr;
vi order, comp, color;
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

void dfs1(int v) {
    used[v] = true;
    for (size_t i=0; i<g[v].size(); ++i)
        if (!used[ g[v][i] ])
            dfs1(g[v][i]);
    order.push_back(v);
}

void dfs2(int v) {
    used[v] = true;
    comp.push_back(v);
    for (size_t i=0; i<gr[v].size(); ++i)
        if (!used[ gr[v][i] ])
            dfs2(gr[v][i]);
}

int main()
{
    FILE *in  = fopen (PROBLEM_NAME".in","r");
    FILE *out = fopen (PROBLEM_NAME".out","w");

    int k;
    fscanf(in, "%d %d ", &n, &k);
    g.assign(n, vi());
    gr.assign(n, vi());
//    for (int i = 0; i < n; ++i) {
//        vi l;
//        g.push_back(l);
//        gr.push_back();
//    }
    for (int i = 0; i < k; ++i) {
        int x, y;
        fscanf(in, "%d %d ", &x, &y);
        g[x-1].push_back(y-1);
        gr[y-1].push_back(x-1);
    }

//    print(g);
//    print(gr);

    used.assign(n, false);
    for (int i = 0; i < n; ++i)
        if (!used[i]) dfs1(i);

    used.assign(n, false);
    color.assign(n, -1);
    int c = 0;
    for (int i = 0; i < n; ++i) {
        int v = order[n-1-i];
        if (!used[v]) {
            dfs2(v);
            for (size_t j = 0; j < comp.size(); ++j)
                color[comp[j]] = c;
            comp.clear();
            ++c;
        }
    }

    vvb m(c, vb(c, false));
    for (size_t i = 0; i < g.size(); ++i) {
        int from = color[i];
        for (size_t j = 0; j < g[i].size(); ++j) {
            int to = color[ g[i][j] ];
            m[from][to] = true;
        }
    }

    int ans = 0;
    for (int i = 0; i < c; ++i) {
        for (int j = 0; j < c; ++j) {
            if (i != j && m[i][j]) ans++;
        }
    }

    fprintf(out, "%d", ans);

    fclose(in);
    fclose(out);
    return 0;
}
