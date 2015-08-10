#include <iostream>
#include <stdio.h>
#include <vector>
#include <queue>
#include <algorithm>

using namespace std;

#define PROBLEM_NAME "divgame"
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

//int gf(int x)
//{
//    vb u(n+1, false);
//    for (int i = 0; i < m; ++i) {
//        int j = x-a[i];
//        if (j >= 0) u[g[j]] = true;
//    }
//    for (int i = 0; i <= n; ++i) {
//        if (!u[i]) return i;
//    }
//    return n+1;
//}

int main()
{
    FILE *in  = fopen (PROBLEM_NAME".in","r");
    FILE *out = fopen (PROBLEM_NAME".out","w");

    int w, h;
    fscanf(in, "%d %d", &w, &h);
    int all = w*h;
    vvi g(w + 1, vi(h + 1, 0));
    int cx, cy, ck;
    for (int x = 2; x <= w; ++x) for (int y = 2; y <= h; ++y) {
        // dymamic: calc g[x][y]
        vb used(all+1, false);
        int lcx, lcy, lck;
        for (int dx=1;dx<x;++dx) for (int dy=1;dy<y;++dy) {
            int sg[4];
            sg[0] = g[dx][dy] ^ g[dx][y-dy] ^ g[x-dx][y];
            sg[1] = g[dx][y]  ^ g[x-dx][dy] ^ g[x-dx][y-dy];
            sg[2] = g[x][dy]  ^ g[dx][y-dy] ^ g[x-dx][y-dy];
            sg[3] = g[dx][dy] ^ g[x-dx][dy] ^ g[x][y-dy];
            for (int k=0;k<4;++k) {
                used[sg[k]] = true; // add to mex set
                if ((x==w && y==h) && !sg[k]) {
                   lcx = dx; lcy = dy; lck = k;
                }
            }
        }
        g[x][y] = all+1;
        for (int r=0;r<=all;++r) if (!used[r]) {
            g[x][y] = r; break;
        }
        if (x == w && y == h && g[x][y]) {
            cx = lcx; cy = lcy; ck = lck;
        }
        // g[x][y] is ready
    }

    if (g[w][h]) {
        fprintf(out, "Alice\n");
        if (ck == 0) {
            fprintf(out, "%d %d %d %d\n%d %d %d %d",
                    cx, 0, cx, h,
                    cx, cy, 0, cy);
        } else if (ck == 1) {
            fprintf(out, "%d %d %d %d\n%d %d %d %d",
                    cx, 0, cx, h,
                    cx, cy, w, cy);
        } else if (ck == 2) {
            fprintf(out, "%d %d %d %d\n%d %d %d %d",
                    0, cy, w, cy,
                    cx, cy, cx, h);
        } else if (ck == 3) {
            fprintf(out, "%d %d %d %d\n%d %d %d %d",
                    0, cy, w, cy,
                    cx, cy, cx, 0);
        }
    } else {
        fprintf(out, "Bob");
    }

//    cout << g[w][h] << endl;
//    if (g[w][h] != 0) cout << "(" << cx << ", " << cy << ") :: " << ck+1 << endl;

//    cout << "--------------" << endl;
//    for (int j=0;j<=h;++j) cout << j << " ";
//    cout << endl;
//    for (int i = 1; i <= w; ++i) {
//        cout << i << " ";
//        for (int j=1;j<=h;++j) cout << g[i][j] << " ";
//        cout << endl;
//    }

    fclose(in);
    fclose(out);
    return 0;
}
