#include <iostream>
#include <stdio.h>
#include <cstdlib>

using namespace std;

typedef unsigned long long ull;

#define PROBLEM_NAME "reverse"

typedef struct item * pitem;
struct item {
    int cnt, val, prior;
    ull sum;
    bool rev;
    pitem l, r;
    item(int value = 0):
        cnt(0), val(value),
        prior(rand() % 1000000),
        sum(0),
        rev(false), l(NULL), r(NULL) {}
};

int cnt (pitem it) {
    return it ? it->cnt : 0;
}

void upd_cnt (pitem it) {
    if (it) it->cnt = cnt(it->l) + cnt(it->r) + 1;
}

ull sum (pitem it) {
    return it ? it->sum : 0LL;
}

void upd_sum (pitem it) {
    if (it) it->sum = sum(it->l) + sum(it->r) + it->val;
}

void push (pitem it) {
    if (it && it->rev) {
        it->rev = false;
        swap (it->l, it->r);
        if (it->l)  it->l->rev ^= true;
        if (it->r)  it->r->rev ^= true;
    }
}

void split (pitem t, pitem & l, pitem & r, int key, int add = 0) {
    if (!t)
        return void( l = r = NULL );
    push(t);
    int cur_key = add + cnt(t->l); // вычисляем неявный ключ
    if (key <= cur_key)
        split (t->l, l, t->l, key, add),  r = t;
    else
        split (t->r, t->r, r, key, add + 1 + cnt(t->l)),  l = t;
    upd_cnt (t);
    upd_sum (t);
}

void merge (pitem & t, pitem l, pitem r) {
    push (l);
    push (r);
    if (!l || !r)
        t = l ? l : r;
    else if (l->prior > r->prior)
        merge (l->r, l->r, r),  t = l;
    else
        merge (r->l, l, r->l),  t = r;
    upd_cnt (t);
    upd_sum (t);
}

void insert (pitem & t, pitem nt, int i) {
    pitem t1, t2;
    split(t, t1, t2, i);
    merge(t1, t1, nt);
    merge(t, t1, t2);
}

void reverse (pitem &t, int l, int r) {
    pitem t1, t2, t3;
    split (t, t1, t2, l);
    split (t2, t2, t3, r-l+1);
    t2->rev ^= true;
    merge (t, t1, t2);
    merge (t, t, t3);
}

ull sum(pitem &t, int l, int r) {
    pitem t1, t2, t3;
    split (t, t1, t2, l);
    split (t2, t2, t3, r-l+1);
    ull res = t2->sum;
    merge (t, t1, t2);
    merge (t, t, t3);
    return res;
}

void output (pitem t) {
    if (!t)  return;
    push (t);
    output (t->l);
    printf ("%d ", t->val);
    output (t->r);
}

void clear(pitem &t) {
    if (!t) return;
    clear(t->l);
    clear(t->r);
    delete t;
}

int main()
{
    FILE *in  = fopen (PROBLEM_NAME".in","r");
    FILE *out = fopen (PROBLEM_NAME".out","w");

    int n, k;
    pitem root = NULL;
    fscanf(in, "%d %d ", &n, &k);
    for (int i = 0; i < n; ++i) {
        int x;
        fscanf(in, "%d ", &x);
        pitem n = new item(x);
        insert(root, n, i);
    }

    for (int i = 0; i < k; ++i) {
        int q, l, r;
        fscanf(in, "%d %d %d ", &q, &l, &r);
        if (q == 1) {
            reverse(root, l-1, r-1);
        } else if (q == 0) {
            ull s = sum(root, l-1, r-1);
            fprintf(out, "%I64d\n", s);
//            fprintf(out, "%llu\n", s);
        }
    }

//    output(root);
    clear(root);

    fclose(in);
    fclose(out);
    return 0;
}
