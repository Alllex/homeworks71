
# Project: Translator to CNF
# Author:  alllex
# Date  :  2014-10-01


def is_nonterm(a): return a.istitle()

class Rule:

    def __init__(self, head, body):
        self.head = head
        self.body = body

    # def __repr__(self):
    #     def mk_line(b): return '{0} -> {1}'.format(self.head, ' '.join(b))
    #     return '\n'.join([mk_line(b) for b in self.bodies])

    # def __repr__(self):
    #     return '{0} -> {1}'.format(self.head, ' |'.join(map(lambda b: ' '.join(b), self.bodies)))

    def __repr__(self):
        return '{0} -> {1}'.format(self.head, ' '.join(self.body))
        # return '{0} -> {1}'.format(self.head, self.body)

    def is_eps(self): return len(self.body) == 0
    def is_chain(self): return len(self.body) == 1 and is_nonterm(self.body[0]) and self.head != self.body[0]

    def has_in_body(self, a): return a in self.body
    # def append(self, body):
    #     Rule(self.head, self.bodies.append(body))

class Grammar:

    def __init__(self, lines):
        rules = self.__parse(lines)
        if len(rules) == 0: raise Exception('Empty grammar!')
        self.axiom = rules[0][0]
        self.nonterms = self.__get_nonterms(rules)
        self.rules = self.__mk_grammar(rules)

    # def __repr__(self):
    #     return '\n'.join(['Axiom: %s' % self.axiom] + [str(r) for r in self.rules])

    def __parse(self, lines):
        rules = []
        for line in lines:
            lexemes = line.split()
            if [] == lexemes: continue
            rules.append((lexemes[0], lexemes[2:]))
        return rules

    def __get_nonterms(self, rules): return list(set([r[0] for r in rules]))

    def __mk_grammar(self, rules):
        return map(lambda r: Rule(r[0], r[1]), rules)

    def rm_epses(self):
        changed = True
        g = self.rules
        while changed:
            changed = False
            for r in g:
                if r.is_eps():
                    changed = True
                    gg = []
                    for rr in g:
                        if rr != r:
                            new_rules = [rr]
                            i = 0
                            for a in rr.body:
                                if a == r.head:
                                    new_body = rr.body[:i] + rr.body[(i + 1):]
                                    new_rules.append(Rule(rr.head, new_body))
                                i += 1
                            gg += new_rules
                    g = gg
                    break
        self.rules = g

    # DOESN'T WORK!!
    def rm_chains(self):
        changed = True
        g = self.rules
        while changed:
            changed = False
            for r in g:
                # print(r)
                if r.is_chain():
                    changed = True
                    gg = []
                    for rr in g:
                        if rr != r:
                            if rr.head == r.body[0]: gg.append(Rule(r.head, rr.body))
                            gg.append(rr)
                    g = list(set(gg))
                    print(g)
                    break
        self.rules = g

# def rm_epses(g): return [rule for rule in g if not is_eps_rule(rule)]

# def rm_chains(g):
#     changed = True
#     while changed:
#         changed = False
#         for r in g:
#             if is_chain_rule(r):
#                 changed = True
#                 gg = []
#                 for rr in g:
#                     if rr != r:
#                         if rr[0] == r[1][0]: gg.append((r[0], rr[1]))
#                         gg.append(rr)
#                 g = gg
#                 break
#     return g

# def rm_nonproductive(g):
#     found = True
#     prod = []
#     nonterms = list(set([r[0] for r in g]))
#     while found:
#         found = False
#         for nt in nonterms:
#             if not nt in prod:
#                 bodies = [r[1] for r in g if r[0] == nt]
#                 is_prod = True
#                 for b in bodies:
#                     nts = [a for a in b if is_nonterm(a)]
#                     for a in nts:
#                         if not a in prod:
#                             is_prod = False
#                             break
#                     if not is_prod: break
#                 if is_prod:
#                     prod.append(nt)
#                     found = True
#     nonprod = [a for a in nonterms if not a in prod]
#     filtered = []
#     for r in g:
#         if not r[0] in nonprod:
#             nts = [a for a in r[1] if is_nonterm(a)]
#             is_prod = True
#             for nt in nts:
#                 if nt in nonprod:
#                     is_prod = False
#                     break
#             if is_prod: filtered.append(r)
#     return filtered

# def read_input(gfile):
#     with open(gfile, 'r') as f:
#         return f.readlines()

# def translate_to_CNF(lines):
#     g = parse(lines)
#     g_eps = rm_epses(g) 
#     g_chains = rm_chains(g_eps)
#     g_nonprod = rm_nonproductive(g_chains)
#     print_grammar('Source grammar:', g)
#     print_grammar('Removed eps:', g_eps)
#     print_grammar('Removed chains:', g_chains)
#     print_grammar('Removed nonproductive:', g_nonprod)

def default_grammar():
    return """
S -> A
A -> a A
A -> 
B -> C
C -> C C
C -> 
    """.split('\n')

def main():
    import sys
    if len(sys.argv) > 1:
        gfile = sys.argv[1]
        # translate_to_CNF(read_input(gfile))        
    else: 
        # translate_to_CNF(default_grammar())
        g = Grammar(default_grammar())
        print(g)
        g.rm_epses()
        print(g)
        g.rm_chains()
        print(g)

if __name__ == '__main__':
    main()

# xs = range(1, 6)
# print(xs[0:])
# print(xs[2:])
# print(xs[4:])
# for i in range(0, 5):
#     ys = xs[:i] + xs[(i+1):]
#     print(ys)