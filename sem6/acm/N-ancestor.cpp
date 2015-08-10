#include <iostream>
#include <stdio.h>
#include <vector>
#include <algorithm>

using namespace std;

#define PROBLEM_NAME "ancestor"

typedef vector<bool> vb;
typedef vector<int> vi;
typedef vector<vi> vvi;

vvi g;
vi tin, tout;
vb used;
int n, root, timer;

//void print() {
//    for (int i = 0; i < n; ++i) {
//        printf("%d(in(%d), out(%d)):", i+1, tin[i], tout[i]);
//        for (size_t j = 0; j < g[i].size(); ++j) {
//            printf(" %d", g[i][j]+1);
//        }
//        printf("\n");
//    }
//    printf("------------\n");
//}

void dfs(int v) {
    tin[v] = ++timer;
    used[v] = true;
    for (size_t i = 0; i < g[v].size(); ++i) {
        int u = g[v][i];
        if (!used[u]) dfs(u);
    }
    tout[v] = ++timer;
}

bool upper (int a, int b) {
    return tin[a] <= tin[b] && tout[a] >= tout[b];
}

int main()
{
    FILE *in  = fopen (PROBLEM_NAME".in","r");
    FILE *out = fopen (PROBLEM_NAME".out","w");

    fscanf(in, "%d", &n);
    for (int i = 0; i < n; ++i) {
        tin.push_back(0);
        tout.push_back(0);
        used.push_back(false);
        vi l;
        g.push_back(l);
    }

    for (int i = 0; i < n; ++i) {
        int p;
        fscanf(in, "%d", &p);
        if (p == 0) {
            root = i;
        } else {
            g[p-1].push_back(i);
        }
    }

    timer = 0;
    dfs(root);

//    print();

    int k;
    fscanf(in, "%d", &k);
    for (int i = 0; i < k; ++i) {
        int a, b;
        fscanf(in, "%d %d", &a, &b);
        int res = (upper(a-1, b-1)) ? 1 : 0;
        fprintf(out, "%d\n", res);
    }

    fclose(in);
    fclose(out);
    return 0;
}
