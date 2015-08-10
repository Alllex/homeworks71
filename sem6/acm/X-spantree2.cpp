#include <iostream>
#include <stdio.h>
#include <vector>
#include <queue>
#include <algorithm>

using namespace std;

#define PROBLEM_NAME "spantree2"
#define mp(a, b) make_pair(a, b)
#define pb(x) push_back(x)
#define pbp(a, b) pb(mp(a,b))

typedef unsigned long long ull;
typedef vector<bool> vb;
typedef vector<int> vi;
typedef vector<double> vd;
typedef vector<vi> vvi;
typedef pair<int, int> pii;
typedef vector<pii> vpii;

const double INF = 1000000000;

int main()
{
    FILE *in  = fopen (PROBLEM_NAME".in","r");
    FILE *out = fopen (PROBLEM_NAME".out","w");

    int n, m;
    fscanf(in, "%d %d", &n, &m);
    vector < pair < int, pii > > g;
    for (int i = 0; i < m; ++i) {
        int x, y, w;
        fscanf(in, "%d %d %d", &x, &y, &w);
        g.pbp(w, mp(x-1, y-1));
    }
    ull cost = 0;
    vector<pii> res;
    vector<int> c(n);
    sort(g.begin(), g.end());
    for (int i=0; i<n; ++i) c[i] = i;
    for (int i=0; i<m; ++i) {
        int a = g[i].second.first,
            b = g[i].second.second;
        if (c[a] != c[b]) {
            cost += g[i].first;
            res.pbp(a, b);
            int oldc = c[b],  newc = c[a];
            for (int j = 0; j < n; ++j) if (c[j] == oldc) c[j] = newc;
        }
    }
    fprintf(out, "%I64d", cost);
    fclose(in);
    fclose(out);
    return 0;
}