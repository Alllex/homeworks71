#include <iostream>
#include <stdio.h>
#include <vector>
#include <cstdlib>

using namespace std;

typedef vector<int> vi;
typedef vector<vi> vvi;
typedef vector<bool> vb;

#define PROBLEM_NAME "pairs"
#define pb(x) push_back(x)
#define loop(i,n) for(int i = 0; i < n; ++i)

const int INF = 1000000000;

int n, m;
vvi g;
vi mt;
vb used;

bool kuhn(int v) {
    if (used[v])  return false;
    used[v] = true;
    for (auto to : g[v]) if (mt[to] == -1 || kuhn(mt[to])) {
        mt[to] = v;
        return true;
    }
    return false;
}

int main()
{
    FILE *in  = fopen (PROBLEM_NAME".in","r");
    FILE *out = fopen (PROBLEM_NAME".out","w");

    fscanf(in, "%d %d ", &n, &m);
    g.resize(n);
    loop(from, n) {
        for(;;) {
            int to;
            fscanf(in, "%d ", &to);
            if (to == 0) break;
            g[from].pb(--to);
        }
    }

    mt.assign(m, -1);
    int size = 0;
    loop(v, n) {
        used.assign(n, false);
        if (kuhn(v)) size++;
    }

//    cout << size << endl;
    fprintf(out, "%d\n", size);
    loop(i, m) if (mt[i] != -1) {
        int from = mt[i]+1, to = i+1;
//        cout << from << " " << to << endl;
        fprintf(out, "%d %d\n", from, to);
    }

    fclose(in);
    fclose(out);
    return 0;
}
