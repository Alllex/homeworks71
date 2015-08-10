#include <iostream>
#include <stdio.h>
#include <vector>
#include <cstdlib>

using namespace std;

#define PROBLEM_NAME "terminator"
#define pb(x) push_back(x)
#define loop(i,n) for(int i = 0; i < n; ++i)

const int S = 8;
const int SS = S*S;

const int dx[8] = { 1, 0, -1,  0, 1, -1, -1,  1 };
const int dy[8] = { 0, 1,  0, -1, 1, -1,  1, -1 };

struct State
{
    int r, t;
    bool rstep;
};

vector<State> g[SS][SS][2];
bool w[SS][SS][2] {};
bool l[SS][SS][2] {};
bool u[SS][SS][2] {};
int  d[SS][SS][2] {};

vector<string> a(S);

bool vp(int x, int y)
{
    return x>=0 && x<S && y>=0 && y<S && a[x][y]!='1';
}

bool kill(int rx, int ry, int tx, int ty)
{
    if (rx==tx && ry==ty) return true;
    bool onFire = rx==tx || ry==ty || (abs(rx-tx) == abs(ry-ty));
    if (!onFire) return false;
    int dx = rx - tx, dy = ry - ty;
    if (dx!=0) dx = dx>0 ? 1 : -1;
    if (dy!=0) dy = dy>0 ? 1 : -1;
    while (vp(tx, ty)) {
        if (rx==tx && ry==ty) return true;
        tx+=dx; ty+=dy;
    }
    return false;
}

void dfs(int r, int t, bool rstep)
{
    u[r][t][rstep] = true;
    for (auto s : g[r][t][rstep]) if (!u[s.r][s.t][s.rstep]) {
        if (l[r][t][rstep])
            w[s.r][s.t][s.rstep] = true;
        else if (--d[s.r][s.t][s.rstep] == 0)
            l[s.r][s.t][s.rstep] = true;
        else
            continue;
        dfs(s.r, s.t, s.rstep);
    }
}

int main()
{
    FILE *in  = fopen (PROBLEM_NAME".in","r");
    FILE *out = fopen (PROBLEM_NAME".out","w");

    loop(i,S) {
        char tmp[S];
        fscanf(in, "%8s ", tmp);
        a[i] = tmp;
    }

    loop(r, SS) loop(t, SS) loop(rstep, 2) {
        w[r][t][rstep] = false;
        l[r][t][rstep] = false;
        u[r][t][rstep] = false;
        d[r][t][rstep] = 0;
        g[r][t][rstep].clear();
    }

    loop(r, SS) loop(t, SS) loop(rstep, 2) {
        int rx=r/S, ry=r%S, tx=t/S, ty=t%S;
        if (!vp(rx, ry) || !vp(tx, ty)) continue;
        bool ww, ll;
        bool kk = kill(rx, ry, tx, ty);
        bool esc = rx == S-1;
        if (rstep) {
            ww = esc && !kk; ll = kk;
        } else {
            ww = kk; ll = false;
        }
        w[r][t][rstep] = ww; l[r][t][rstep] = ll;
        if (ww || ll) continue;
        State cur { r, t, rstep!=0 };
        loop(i, 8) {
            int rrx=rx, rry=ry, ttx=tx, tty=ty;
            if (rstep) {
                rrx+=dx[i]; rry+=dy[i];
            } else {
                ttx+=dx[i]; tty+=dy[i];
            }
            if (vp(rrx, rry) && vp(ttx, tty)) {
                g[rrx*S+rry][ttx*S+tty][!rstep].pb(cur);
                ++d[r][t][rstep];
            }
        }
    }

    loop(r, SS) loop(t, SS) loop(rstep, 2)
        if ((w[r][t][rstep] || l[r][t][rstep]) && !u[r][t][rstep])
            dfs(r, t, rstep!=0);

    int rstart = 0, tstart = 0;
    loop(i, S) loop(j, S)
        if (a[i][j] == '2') rstart = i*8+j;
        else if (a[i][j] == '3') tstart = i*8+j;

    bool run = w[rstart][tstart][true];
    int  res = run ? 1 : -1;
    cout << res << endl;
    fprintf(out, "%d", res);
    fclose(in);
    fclose(out);
    return 0;
}
