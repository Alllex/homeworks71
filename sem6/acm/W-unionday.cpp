#include <iostream>
#include <stdio.h>
#include <vector>
#include <queue>
#include <algorithm>

using namespace std;

#define PROBLEM_NAME "unionday"
#define mp(a, b) make_pair(a, b)

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

    int n;
    fscanf(in, "%d", &n);
    vpii ps;
    for (int i = 0; i < n; ++i) {
        int x, y;
        fscanf(in, "%d %d ", &x, &y);
        ps.push_back(mp(x, y));
    }
    vector<vd> g(n, vd(n, 0));
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            if (i == j) g[i][j] = INF;
            else if (i < j) {
                double xs = ps[i].first - ps[j].first;
                double ys = ps[i].second - ps[j].second;
                double d = sqrt(xs * xs + ys * ys);
                g[i][j] = d;
                g[j][i] = d;
            }
        }
    }

    vb used(n, false);
    vd min_e(n, INF);
    vi sel_e(n, -1);
    min_e[0] = 0;
    double ans = 0;
    for (int i=0; i<n; ++i) {
        int v = -1;
        for (int j=0; j<n; ++j) if (!used[j] && (v == -1 || min_e[j] < min_e[v])) v = j;
        used[v] = true;
        if (sel_e[v] != -1) {
            ans += g[v][sel_e[v]];
//            cout << v << " " << sel_e[v] << endl;
        }

        for (int to = 0; to < n; ++to)
            if (!used[to] && g[v][to] < min_e[to]) {
                min_e[to] = g[v][to];
                sel_e[to] = v;
            }
    }

    fprintf(out, "%f", ans);

    fclose(in);
    fclose(out);
    return 0;
}
