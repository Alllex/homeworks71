
# Project: Translator to CNF
# Author:  alllex
# Date  :  2014-10-01


def is_nonterm(a): return a.istitle()
def is_term(a): return not is_nonterm(a)

class Rule:

    def __init__(self, head, body):
        self.head = head
        self.body = body

    def __eq__(self, other):
        if isinstance(other, Rule):
            return self.head == other.head and self.body == other.body
        return NotImplemented

    def __ne__(self, other):
        result = self.__eq__(other)
        if result is NotImplemented:
            return result
        return not result

    # def __repr__(self):
    #     def mk_line(b): return '{0} -> {1}'.format(self.head, ' '.join(b))
    #     return '\n'.join([mk_line(b) for b in self.bodies])

    # def __repr__(self):
    #     return '{0} -> {1}'.format(self.head, ' |'.join(map(lambda b: ' '.join(b), self.bodies)))

    def __repr__(self): return '{0} -> {1}'.format(self.head, ' '.join(self.body))
        # return '{0} -> {1}'.format(self.head, self.body)

    def __hash__(self):
        return hash(str([self.head] + self.body))

    def is_eps(self): return len(self.body) == 0
    def is_chain(self): return len(self.body) == 1 and is_nonterm(self.body[0]) and self.head != self.body[0]
    def is_self_prod(self): return len(self.body) == 1 and self.head == self.body[0]
    def is_finite(self): return len(self.body) == 1 and is_term(self.body[0])
    def has_in_body(self, a): return a in self.body

    def reachable(self): return list(set([a for a in self.body if is_nonterm(a) and a != self.head]))

    # def append(self, body):
    #     Rule(self.head, self.bodies.append(body))

    def rename(self, old, new):
        if self.head == old: self.head = new
        self.body = map(lambda a: new if a == old else a, self.body)

class Grammar:

    def __init__(self, lines):
        rules = self.__parse(lines)
        if len(rules) == 0: raise Exception('Empty grammar!')
        self.axiom = rules[0][0]
        self.rules = self.__mk_grammar(rules)
        self.__counter = 0
        self.__rm_unheaded()

    def __repr__(self):
        return '\n'.join(['Axiom: %s' % self.axiom] + [str(r) for r in self.rules])

    def __parse(self, lines):
        rules = []
        for line in lines:
            lexemes = line.split()
            if [] == lexemes: continue
            rules.append((lexemes[0], lexemes[2:]))
        return rules

    # def __get_nonterms(self): return list(set([r[0] for r in rules]))

    def __mk_grammar(self, rules):
        return map(lambda r: Rule(r[0], r[1]), rules)

    def __gen_name(self, a):
        self.__counter += 1
        return a + str(self.__counter)

    def __gen_name(self):
        self.__counter += 1
        return 'T' + str(self.__counter)

    def __distinct(self):
        self.rules = list(set(self.rules))

    def __rm_unheaded(self):
        heads = map(lambda r: r.head, self.rules)
        self.rules = [r for r in self.rules if all([is_term(a) or a in heads for a in r.body])]

    def __rm_self_prod(self):
        self.rules = [r for r in self.rules if not r.is_self_prod()]

    def __filter_bad_rules(self):
        self.__rm_self_prod()
        self.__rm_unheaded()
        self.__distinct()

    def __rm_eps(self, eps_rule):
        rest_rules = (r for r in self.rules if r != eps_rule)
        old_name = eps_rule.head
        new_name = self.__gen_name(eps_rule.head)
        if old_name == self.axiom: self.axiom = new_name
        new_rules = []
        for r in rest_rules:
            if r.has_in_body(old_name):
                for i in xrange(len(r.body)):
                    if r.body[i] == old_name:
                        left = r.body[:i]
                        right = r.body[(i+1):]
                        new_rules.append(Rule(r.head, left + [new_name] + right))
                        new_rules.append(Rule(r.head, left + right))
            else:
                new_rules.append(r)
        for r in new_rules: r.rename(old_name, new_name)
        self.rules = new_rules
        self.__filter_bad_rules()

    def rm_epses(self):
        changed = True
        while changed:
            changed = False
            for r in self.rules:
                if r.is_eps():
                    changed = True
                    self.__rm_eps(r)
                    break

    def __rm_chain(self, chain_rule):
        rest_rules = [r for r in self.rules if r != chain_rule]
        head = chain_rule.head
        mid_nonterm = chain_rule.body[0]
        new_rules = rest_rules[:]
        for r in rest_rules:
            if r.head == mid_nonterm:
                new_rules.append(Rule(head, r.body))

        self.rules = new_rules
        self.__filter_bad_rules()

    def rm_chains(self):
        changed = True
        while changed:
            changed = False
            for r in self.rules:
                if r.is_chain():
                    changed = True
                    self.__rm_chain(r)
                    break

    # def rm_nonprod(self):

    def __find_reachable(self, a):
        reachable = []
        for r in self.rules:
            if a == r.head:
                reachable += r.reachable()
        return list(set(reachable))

    def rm_unreach(self):
        reachable = []
        new = [self.axiom]
        while len(new) > 0:
            found = []
            for a in new:
                found += self.__find_reachable(a)
            reachable += new
            new = [a for a in found if not a in reachable]
        self.rules = [r for r in self.rules if r.head in reachable]

    def __mk_fake_nonterms(self):
        new_rules = []
        for r in self.rules:
            if not r.is_finite():
                new_body = []
                for a in r.body:
                    if is_term(a):
                        new_nonterm = self.__gen_name()
                        new_rules.append(Rule(new_nonterm, a))
                        new_body.append(new_nonterm)
                    else:
                        new_body.append(a)
                new_rules.append(Rule(r.head, new_body))
            else:
                new_rules.append(r)
        self.rules = new_rules

    def __reduce_body_len(self):
        new_rules = []
        for r in self.rules:
            if len(r.body) > 2:
                one = r.body[0]
                for two in r.body[1:-1]:
                    new_nonterm = self.__gen_name()
                    new_rules.append(Rule(new_nonterm, [one, two]))
                    one = new_nonterm
                new_rules.append(Rule(r.head, [one, r.body[-1]]))
            else:
                new_rules.append(r)
        self.rules = new_rules

    def clear_up(self):
        self.__mk_fake_nonterms()
        self.__reduce_body_len()


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
S -> A x B  y C z D
A -> a
B -> b
C -> c
D -> d
    """.split('\n')

def main():
    import sys
    if len(sys.argv) > 1:
        gfile = sys.argv[1]
        # translate_to_CNF(read_input(gfile))        
    else: 
        # translate_to_CNF(default_grammar())
        g = Grammar(default_grammar())
        # print(g)
        g.rm_epses()
        print(g)
        g.rm_chains()
        print(g)
        g.rm_unreach()
        print(g)
        g.clear_up()
        print('Final grammar:')
        print(g)

if __name__ == '__main__':
    main()
