#include <iostream>
#include <stdio.h>
#include <vector>
#include <queue>
#include <algorithm>

using namespace std;

#define PROBLEM_NAME "tickets"
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

int n, m;
vb g;
vi a;

int gf(int x)
{
    vb u(n+1, false);
    for (int i = 0; i < m; ++i) {
        int j = x-a[i];
        if (j >= 0) u[g[j]] = true;
    }
    for (int i = 0; i <= n; ++i) {
        if (!u[i]) return i;
    }
    return n+1;
}

int main()
{
    FILE *in  = fopen (PROBLEM_NAME".in","r");
    FILE *out = fopen (PROBLEM_NAME".out","w");

    fscanf(in, "%d %d", &n, &m);
    a.resize(m);
    for (int i = 0; i < m; ++i) {
        int x;
        fscanf(in, "%d", &x);
        a[i] = x;
    }
    g.resize(n+1, false);
    for (int i = 1; i <= n; ++i) {
        for (int j = 0; j < m; ++j) {
            int d = i - a[j];
            if (d < 0 || !g[d]) {
                g[i] = true;
                break;
            }
        }
    }

//    for (auto b : g) cout << (b?1:0) << " ";
//    cout << endl;
//    cout << (g[n] ? "PETYA" : "MISHA") << endl;
    fprintf(out, g[n] ? "PETYA" : "MISHA");

    fclose(in);
    fclose(out);
    return 0;
}
