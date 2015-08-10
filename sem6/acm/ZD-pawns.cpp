#include <iostream>
#include <stdio.h>
#include <vector>
#include <queue>
#include <set>
#include <algorithm>

using namespace std;

#define PROBLEM_NAME "pawns"
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

    const int N = 52+34+1;
    vi g(N+1, 0);
    g[1] = g[2] = 1;
    for (int n=3;n<N;++n) {
        set<int> u;
        u.insert(g[n-2]);
        for (int i=2;i<n;++i) {
            u.insert(g[i-2]^g[n-i-1]);
        }
        for (int j=0;j<N;++j) if (u.find(j) == u.end()) {
            g[n] = j;
            break;
        }
    }


    int k;
    fscanf(in, "%d", &k);
    if (k > 52) {
        k = 52 + ((k-52)%34);
    }
    fprintf(out, (g[k] ? "White" : "Black"));
//    cout << (g[k] ? "White" : "Black") << endl;

//    for (int i = 1; i < N; ++i) {
//        if (i <= 52) cout << i << " " << g[i] << endl;
//        else {
//            cout << g[i] << " ";
//            if ((i-52)%34==0) cout << endl;
//        }
//    }
//    cout << "----------------" << endl;
//    int k = 51;
//    while (k < 500) {
//        for (int i = 0; i < 34; ++i)
//            cout << g[k++] << " ";
//        cout << endl;
//    }

    fclose(in);
    fclose(out);
    return 0;
}
