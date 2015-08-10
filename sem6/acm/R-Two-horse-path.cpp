#include <iostream>
#include <stdio.h>
#include <vector>
#include <algorithm>
#include <queue>

using namespace std;

typedef vector<int> vi;
typedef vector<vi> vvi;
typedef pair<int, int> pii;
typedef vector<pii> vpii;

#define PROBLEM_NAME "knight2"

const int INF = 1000 * 1000;

struct Cell {
    int len;
    int p1;
    int p2;
    Cell(): len(INF), p1(-1), p2(-1) {}
};

vector<vector<Cell> > table;
vpii moves;

bool valid(int x, int y)
{
    return 0 <= x && x < 8 && 0 <= y && y < 8;
}

void set(int i1, int i2, int p1, int p2, int len) {
    table[i1][i2].p1 = p1;
    table[i1][i2].p2 = p2;
    table[i1][i2].len = len;
}

vpii bfz(int from_x1, int from_y1,
         int from_x2, int from_y2,
         int to_x1, int to_y1,
         int to_x2, int to_y2)
{
    int sp1 = from_x1*8 + from_y1, sp2 = from_x2*8+from_y2;
    int fp1 = to_x1*8+to_y1, fp2 = to_x2*8+to_y2;
    queue< pii > q;
    q.push(make_pair(sp1, sp2));
    table[sp1][sp2].len = 0;
    while (!q.empty() && table[fp1][fp2].len == INF) {
        pii p = q.front(); q.pop();
        int p1 = p.first, p2 = p.second;
        int x1 = p1 / 8, y1 = p1 % 8;
        int x2 = p2 / 8, y2 = p2 % 8;
        int nextlen = table[p1][p2].len + 1;
        for (pii &d : moves) {
            int to_x, to_y, to_p;
            to_x = x1 + d.first;
            to_y = y1 + d.second;
            to_p = to_x*8+to_y;
            if (valid(to_x, to_y) &&
                to_p != p2 &&
                table[to_p][p2].len == INF)
            {
                set(to_p, p2, p1, p2, nextlen);
                q.push(make_pair(to_p, p2));
//                printf("(%c%d -> %c%d) (%c%d)\n",
//                       (char)('a'+x1), y1+1,
//                       (char)('a'+to_x), to_y+1,
//                       (char)('a'+x2), y2+1);
            }
            to_x = x2 + d.first;
            to_y = y2 + d.second;
            to_p = to_x*8+to_y;
            if (valid(to_x, to_y) &&
                to_p != p1 &&
                table[p1][to_p].len == INF)
            {
                set(p1, to_p, p1, p2, nextlen);
                q.push(make_pair(p1, to_p));
//                printf("(%c%d) (%c%d -> %c%d)\n",
//                       (char)('a'+x1), y1+1,
//                       (char)('a'+x2), y2+1,
//                       (char)('a'+to_x), to_y+1);
            }
        }
    }

    vpii path;
    int p1 = fp1, p2 = fp2;
    while (p1 != sp1 || p2 != sp2) {
        path.push_back(make_pair(p1, p2));
        int to_p1 = table[p1][p2].p1;
        int to_p2 = table[p1][p2].p2;
        p1 = to_p1;
        p2 = to_p2;
    }
    reverse(path.begin(), path.end());
    return path;
}


int main()
{
    FILE *in  = fopen (PROBLEM_NAME".in","r");
    FILE *out = fopen (PROBLEM_NAME".out","w");

    moves.push_back(make_pair(1, 2));
    moves.push_back(make_pair(2, 1));
    moves.push_back(make_pair(2, -1));
    moves.push_back(make_pair(1, -2));
    moves.push_back(make_pair(-1, -2));
    moves.push_back(make_pair(-2, -1));
    moves.push_back(make_pair(-2, 1));
    moves.push_back(make_pair(-1, 2));

    int a[8];
    for (int i = 0; i < 4; ++i) {
        char c1, c2;
        fscanf(in, "%c%c ", &c1, &c2);
        a[2*i] = (int) (c1 - 'a');
        a[2*i+1] = (int) (c2 - '1');
    }

    for (int i = 0; i < 64; ++i) {
        table.push_back(vector<Cell>(64));
    }

    vpii path = bfz(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
    for (pii &st : path) {
        if (st.first != table[st.first][st.second].p1) {
            fprintf(out, "1 %c%d\n", (char)('a' + st.first / 8), st.first % 8 + 1);
        } else {
            fprintf(out, "2 %c%d\n", (char)('a' + st.second / 8), st.second % 8 + 1);
        }
    }

    fclose(in);
    fclose(out);
    return 0;
}
