
def ext(xs):
    ys = []
    for x in xs: ys += x
    return ys

def print_grammar(g):
    for r in g:
        print('{0} -> {1}'.format(r[0], ' '.join(r[1])))
    print('-------------------------')

def is_nonterm(a): return a.istitle() 
def is_term(a): return not is_nonterm(a)

def is_eps_rule(r): return len(r[1]) == 0
def is_chain_rule(r): return len(r[1]) == 1 and is_nonterm(r[1][0]) and r[1] != r[1][0]

def read_input():
#     return """
# S -> A
# S -> B
# S -> C
# A -> a A
# A -> 
# B -> b B
# B -> 
# C -> D
# D -> d
#     """.split('\n')
    return """
S -> A
A -> a
B -> A A
C -> D d D
D -> D D d
    """.split('\n')

def parse(lines):
    rules = []
    for line in lines:
        lexemes = line.split()
        if [] == lexemes: continue
        head = lexemes[0]
        body = lexemes[2:]
        rules.append((head, body))
    return rules

def rm_epses(g):
    return [rule for rule in g if not is_eps_rule(rule)]

def rm_chains(g):
    changed = True
    while changed:
        changed = False
        for r in g:
            if is_chain_rule(r):
                changed = True
                gg = []
                for rr in g:
                    if rr != r:
                        if rr[0] == r[1][0]: gg.append((r[0], rr[1]))
                        gg.append(rr)
                g = gg
                break
    return g

def rm_nonproductive(g):
    found = True
    prod = []
    nonterms = list(set([r[0] for r in g]))
    while found:
        found = False
        for nt in nonterms:
            if not nt in prod:
                bodies = [r[1] for r in g if r[0] == nt]
                is_prod = True
                for b in bodies:
                    nts = [a for a in b if is_nonterm(a)]
                    for a in nts:
                        if not a in prod:
                            is_prod = False
                            break
                    if not is_prod: break
                if is_prod:
                    prod.append(nt)
                    found = True
    nonprod = [a for a in nonterms if not a in prod]
    print('Nonproductive:')
    print(nonprod)
    filtered = []
    for r in g:
        if not r[0] in nonprod:
            nts = [a for a in r[1] if is_nonterm(a)]
            is_prod = True
            for nt in nts:
                if nt in nonprod:
                    is_prod = False
                    break
            if is_prod: filtered.append(r)
    return filtered


g = parse(read_input())
g_eps = rm_epses(g) 
g_chains = rm_chains(g_eps)
g_nonprod = rm_nonproductive(g_chains)

print('Source grammar:')
print_grammar(g)
print('Eps removed:')
print_grammar(g_eps)
print('Chains removed:')
print_grammar(g_chains)
print('Nonproductive removed:')
print_grammar(g_nonprod)
