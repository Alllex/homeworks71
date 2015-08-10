#include <iostream>
#include <stdio.h>
#include <vector>
#include <queue>
#include <set>
#include <algorithm>

using namespace std;

#define PROBLEM_NAME "varnim"
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

int ans(int x) {
    if (x==0) return 0;
    int m = x%4;
    if (m==1 || m==2) return x;
    else return m == 3 ? x+1 : x-1;
}

int main()
{
    FILE *in  = fopen (PROBLEM_NAME".in","r");
    FILE *out = fopen (PROBLEM_NAME".out","w");

    int t;
    fscanf(in, "%d", &t);
    for (int o = 0; o < t; ++o) {
        int n, a, res = 0;
        fscanf(in, "%d", &n);
        for (int i = 0; i < n; i++) {
            fscanf(in, "%d", &a);
            res ^= ans(a);
        }
        fprintf(out, res ? "FIRST\n" : "SECOND\n");
    }

    fclose(in);
    fclose(out);
    return 0;
}