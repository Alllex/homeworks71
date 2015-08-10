#include <iostream>
#include <stdio.h>
#include <vector>
#include <queue>
#include <algorithm>

using namespace std;

#define PROBLEM_NAME "island2"
#define mp(a, b) make_pair(a, b)

typedef vector<bool> vb;
typedef vector<int> vi;
typedef vector<vi> vvi;
typedef pair<int, int> pii;
typedef vector<pii> vpii;

const int INF = 1000 * 1000;

vvi g;
vb fst;
vi p, c;
int n;

int getlen(int from, int to)
{
    int len = 0;
    if (fst[from] ^ fst[to]) len++;
    if (from % 2) len*=2;
    return len;
}

void bfs()
{
    deque< int > q1;
    queue< int > q2;
    q1.push_back(0);
    p[0] = -1;
    c[0] = 0;
    while (!q1.empty() || !q2.empty()) {
        int v;
        if (!q1.empty()) {
            v = q1.front();
            q1.pop_front();
        } else {
            v = q2.front();
            q2.pop();
        }
        for (auto &to : g[v]) {
            int l = getlen(v, to);
            if (c[v] + l < c[to]) {
                if (l == 0) q1.push_front(to);
                else if (l == 1) q1.push_back(to);
                else q2.push(to);
                c[to] = c[v] + l;
                p[to] = v;
            }
        }
    }
}

int main()
{
    FILE *in  = fopen (PROBLEM_NAME".in","r");
    FILE *out = fopen (PROBLEM_NAME".out","w");

    int m;
    fscanf(in, "%d %d ", &n, &m);
    g.assign(n, vi());
    fst.assign(n, false);
    for (int i = 0; i < n; ++i) {
        int first;
        fscanf(in, "%d ", &first);
        fst[i] = first == 1;
    }
    for (int i = 0; i < m; ++i) {
        int x, y;
        fscanf(in, "%d %d ", &x, &y);
        g[x-1].push_back(y-1);
        g[y-1].push_back(x-1);
    }
    p.assign(n, 0);
    c.assign(n, INF);

    bfs();

    if (c[n-1] == INF) {
        fprintf(out, "impossible\n");
    } else {
        vi path;
        for (int v = n-1; v != -1; v = p[v]) path.push_back(v);
        reverse(path.begin(), path.end());
        fprintf(out, "%d %d\n", c[n-1], (int)path.size());
        for (auto &v : path) {
            fprintf(out, "%d ", v+1);
        }
    }

    fclose(in);
    fclose(out);
    return 0;
}
