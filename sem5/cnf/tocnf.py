
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

    def __cmp__(self, other):
        if isinstance(other, Rule):
            self_key = self.head + ''.join(self.body)
            other_key = other.head + ''.join(other.body)
            return cmp(self_key, other_key)
        return NotImplemented

    def __ne__(self, other):
        result = self.__eq__(other)
        if result is NotImplemented:
            return result
        return not result

    def __repr__(self): return '{0} -> {1}'.format(self.head, ' '.join(self.body))

    def __hash__(self):
        return hash(str([self.head] + self.body))

    def is_eps(self): return len(self.body) == 0
    def is_chain(self): return len(self.body) == 1 and is_nonterm(self.body[0]) and self.head != self.body[0]
    def is_self_prod(self): return len(self.body) == 1 and self.head == self.body[0]
    def is_finite(self): return len(self.body) == 1 and is_term(self.body[0])
    def has_in_body(self, a): return a in self.body

    def reachable(self): return list(set([a for a in self.body if is_nonterm(a) and a != self.head]))

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
        self.__filter_bad_rules()
        self.eps = False
        self.sort_rules()

    def __repr__(self):
        e = []
        if self.eps:
            e = [str(Rule(self.axiom, []))]
        elif self.rules == []: return 'FAIL'
        return '\n'.join(['Axiom: %s' % self.axiom] + e + [str(r) for r in self.rules])

    def __parse(self, lines):
        rules = []
        for line in lines:
            lexemes = line.split()
            if [] == lexemes: continue
            rules.append((lexemes[0], lexemes[2:]))
        return rules

    def __mk_grammar(self, rules):
        return map(lambda r: Rule(r[0], r[1]), rules)

    def __gen_name(self, a):
        self.__counter += 1
        return a + str(self.__counter)

    def __gen_def_name(self):
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
        new_name = self.__gen_name(old_name)
        if old_name == self.axiom: self.axiom = new_name
        new_rules = []
        for r in rest_rules:
            if r.has_in_body(old_name):
                from_one = [r]
                changed = True
                while changed:
                    changed = False
                    tmp = []
                    for rr in from_one:
                        if rr.has_in_body(old_name):
                            changed = True
                            for i in xrange(len(rr.body)):
                                if rr.body[i] == old_name:
                                    left = rr.body[:i]
                                    right = rr.body[(i+1):]
                                    tmp.append(Rule(r.head, left + [new_name] + right))
                                    tmp.append(Rule(r.head, left + right))
                                    break
                        else: tmp.append(rr)
                    from_one = tmp
                new_rules += from_one
            else:
                new_rules.append(r)
        for r in new_rules: r.rename(old_name, new_name)
        self.rules = new_rules
        self.__filter_bad_rules()

    def rm_epses(self):
        changed = True
        while changed:
            # print('RM epses iter')
            # print(self)
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
                        new_nonterm = self.__gen_def_name()
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
                    new_nonterm = self.__gen_def_name()
                    new_rules.append(Rule(new_nonterm, [one, two]))
                    one = new_nonterm
                new_rules.append(Rule(r.head, [one, r.body[-1]]))
            else:
                new_rules.append(r)
        self.rules = new_rules

    def clear_up(self):
        self.__mk_fake_nonterms()
        self.__reduce_body_len()

    def rm_nonprod(self):
        prods = []
        found = True
        while found:
            found = False
            for r in self.rules:
                if not r.head in prods:
                    if all(is_term(a) or a in prods for a in r.body):
                        found = True
                        prods.append(r.head)
        self.rules = [r for r in self.rules if r.head in prods]
        self.__filter_bad_rules()

    def __is_gen_eps(self):
        epses = []
        found = True
        while found:
            found = False
            for r in self.rules:
                if not r.head in epses:
                    if all(is_nonterm(a) and a in epses for a in r.body):
                        found = True
                        epses.append(r.head)
        return self.axiom in epses

    def sort_rules(self):
        axiom_rules = [r for r in self.rules if r.head == self.axiom]
        other_rules = [r for r in self.rules if r.head != self.axiom]
        axiom_rules.sort()
        other_rules.sort()
        self.rules = axiom_rules + other_rules

    def translate_to_CNF(self):
        #print('Source grammar')
        #print(self)
        self.eps = self.__is_gen_eps()
        self.rm_epses()
        #print('Without eps')
        #print(self)
        self.rm_chains()
        #print('Without chains')
        #print(self)
        self.rm_nonprod()
        #print('Without nonproductive')
        self.rm_unreach()
        #print(self)
        #print('Without unreachable')
        #print(self)
        self.clear_up()
        #print('After cleaning')
        #print(self)
        self.sort_rules()

def read_input(gfile):
    with open(gfile, 'r') as f:
        return f.readlines()

def default_grammar():
    return """
S -> A A A
A -> 0 A
A -> 

    """.split('\n')

def main():
    import sys
    if len(sys.argv) > 1: 
        g = Grammar(read_input(sys.argv[1]))
    else: 
        g = Grammar(default_grammar())
    print('Source grammar:')
    print(g)
    g.translate_to_CNF()
    print('---' * 10)
    print('CNF form:')
    print(g)

if __name__ == '__main__':
    main()
