#include <iostream>
#include <stdio.h>
#include <math.h>

using namespace std;

//typedef unsigned long long ull;
typedef long long ll;

#define PROBLEM_NAME "middle"

//void print(ull a[], int size,
//           const char *fmt,
//           const char *end,
//           FILE* out)
//{
//    for (int i = 0; i < size; i++) {
//        fprintf(out, fmt, a[i]);
//    }
//    fprintf(out, end);
//}

typedef struct T
{
    ll sum;
    ll val;
    bool mark;
    int c;
} T;

const int N = 30 * 1000 + 1;
ll a[N];
T t[4 * N];
int n;
ll isum = 0LL;

void bld(int v, int tl, int tr)
{
    if (tl == tr) {
        t[v].sum = a[tl];
        t[v].val = a[tl];
        t[v].mark = true;
        t[v].c = 1;
    } else {
        int tm = (tl + tr) / 2;
        bld(v*2, tl, tm);
        bld(v*2+1, tm+1, tr);
        t[v].sum = t[v*2].sum + t[v*2 + 1].sum;
        t[v].val = 0;
        t[v].mark = false;
        t[v].c = t[v*2].c + t[v*2+1].c;
    }
}

void push(int v) {
    if (t[v].mark) {
        t[v*2].mark = t[v*2 + 1].mark = true;
        t[v*2].val = t[v*2 + 1].val = t[v].val;
        t[v*2].sum = t[v].val * t[v*2].c;
        t[v*2+1].sum = t[v].val * t[v*2+1].c;
        t[v].mark = false;
    }
}

ll sum(int v, int tl, int tr,
       int l, int r)
{
    if (l > r) return 0;
    if (l == tl && r == tr) return t[v].sum;
//    if (tl == tr) {
//        printf("WTF?!@#$%^$#\n");
//    }
    int tm = (tl + tr) / 2;
    int nr = min(r,tm);
    int nl = max(l, tm+1);
    push(v);
    ll left = sum(v*2,   tl,   tm, l,  nr);
    ll right = sum(v*2+1, tm+1, tr, nl, r);
    return left + right;
}


void upd(int v, int tl, int tr,
         int l, int r, ll val)
{
    if (l > r) return;
    if ((l == tl && r == tr) || tl == tr) {
        t[v].mark = true;
        t[v].val = val;
        t[v].sum = val * t[v].c;
    } else {
        int tm = (tl + tr) / 2;
        int nr = min(r,tm);
        int nl = max(l, tm+1);
        push(v);
        upd(v*2,   tl,   tm, l,  nr, val);
        upd(v*2+1, tm+1, tr, nl, r, val);
        t[v].sum = t[v*2].sum + t[v*2+1].sum;
    }
}

void update(int l, int r)
{
    if (l > r) return;
    ll sumlr = sum(1, 0, n-1, l, r);
    int c = r - l + 1;
    long double tmp = sumlr / (long double) c;
    bool upwards = t[1].sum <= isum;
    ll mid = (ll) (upwards ? ceil(tmp) : floor(tmp));
    upd(1, 0, n-1, l, r, mid);
}

ll get (int v, int tl, int tr, int pos) {
    if (t[v].mark || tl == tr) return t[v].val;
//    if (tl > tr) {
//        printf("FUUCK!!!\n");
//        return -1;
//    }
    int tm = (tl + tr) / 2;
    if (pos <= tm)
        return get(v*2, tl, tm, pos);
    else
        return get(v*2+1, tm+1, tr, pos);
}

//void printt() {
//    for (int i = 0; i < n; ++i) {
//        fprintf(stdout, "%llu ", get(1, 0, n-1, i));
//    }
//    printf("\n");
//}

int main()
{
    FILE *in  = fopen (PROBLEM_NAME".in","r");
    FILE *out = fopen (PROBLEM_NAME".out","w");

    int k;
    fscanf(in, "%d %d ", &n, &k);
    for (int i = 0; i < n; ++i) {
        int x;
        fscanf(in, "%d ", &x);
        a[i] = x;
        isum += x;
    }

    bld(1, 0, n - 1);
//    printt();

    for (int i = 0; i < k; ++i) {
        int l, r;
        fscanf(in, "%d %d ", &l, &r);
        update(l - 1, r - 1);
//        printf("upd %d to %d\n", l, r);
//        printt();
    }


    for (int i = 0; i < n; ++i) {
        fprintf(out, "%I64d ", get(1, 0, n-1, i));
    }
//    print(a, n, "%llu ", "", stdout);
//    print(a, n, "%I64u ", "", out);

    fclose(in);
    fclose(out);
    return 0;
}
