#include <iostream>
#include <stdio.h>
#include <vector>
#include <queue>
#include <cstdlib>
#include <algorithm>

using namespace std;

#ifdef WIN
  #define LLD "%I64d"
#else
  #define LLD "%lld"
#endif

#ifdef LOCALLY
    #define IN "input"
    #define OUT "output"
#else
    #define IN "assignment.in"
    #define OUT "assignment.out"
#endif

typedef vector<int> vi;
typedef vector<vi> vvi;
typedef vector<bool> vb;
typedef unsigned long long ull;

#define pb(x) push_back(x)
#define loop(i, n) for(int i = 0; i < (n); ++i)
#define looop(i, j, n) loop(i, n) loop(j, n)

const int INF = 1000000000;

vvi c;      // cost matrix
int n;      // dimension
vi  lw, lj, mw, mv, w2j, j2w, par;
vb  mark;
int result = 0;

int fst() {
    loop(w, n) if (w2j[w] < 0) return w;
    return n;
}

void step() {
    for(;;) {
        int bw = -1, bj = -1, bv = INF;
        loop(j, n) if (par[j] < 0 && mv[j] < bv) {
            bv = mv[j]; bw = mw[j]; bj = j;
        }
        if (bv > 0) loop(i, n) {
            if (mark[i]) lw[i] += bv;
            if (par[i] < 0) mv[i] -= bv; else lj[i] -= bv;
        }
        par[bj] = bw;
        if (j2w[bj] < 0) {
            for (; bj >= 0; bw = par[bj]) {
                int tj = w2j[bw];
                w2j[bw] = bj; j2w[bj] = bw;
                bj = tj;
            }
            break;
        } else {
            int w = j2w[bj]; mark[w] = true;
            loop(j, n) if (par[j] < 0) {
                int d = c[w][j] - lw[w] - lj[j];
                if (mv[j] > d) mv[j] = d, mw[j] = w;
            }
        }
    }
}

void solve() {
    looop(w, j, n) if (c[w][j] < lj[j]) lj[j] = c[w][j];
    looop(w, j, n) if (w2j[w] < 0 && j2w[j] < 0 && lw[w]+lj[j] == c[w][j])
        w2j[w] = j, j2w[j] = w;
    for (int w = fst(); w < n; w = fst()) {
        mark.assign(n, false); mark[w] = true; par.assign(n, -1);
        loop(j, n) mw[j] = w, mv[j] = c[w][j]-lw[w]-lj[j];
        step();
    }
    loop(w, n) result += c[w][w2j[w]];
}

void read(FILE *in) {
    fscanf(in, "%d", &n);
    c.assign(n, vi(n, 0));
    looop(i, j, n) fscanf(in, "%d", &c[i][j]);
    lw.assign(n, 0); lj.assign(n, INF);
    mw.assign(n, 0); mv.assign(n, 0);
    w2j.assign(n, -1); j2w.assign(n, -1);
}

void print(FILE *out) {
    fprintf(out, "%d\n", result);
    loop(w, n) fprintf(out, "%d %d\n", w+1, w2j[w]+1);
}

int main()
{
    FILE *in  = fopen(IN,"r");
    FILE *out = fopen(OUT,"w");
    read(in); solve(); print(out);
    fclose(in);
    fclose(out);
    return 0;
}
