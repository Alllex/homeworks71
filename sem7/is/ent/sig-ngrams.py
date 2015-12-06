#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import division

import time
import codecs

ENCODING = 'utf-8'

def dt2str(dt):
    mins = int(dt / 60)
    secs = int(dt) % 60
    return '{:>2} mins {:>2} secs'.format(mins, secs)

last_progress = -1
start_time = time.time()
def print_progress(cur, all):
    global last_progress, start_time
    progress = int(cur * 100 / all)
    if progress != last_progress and progress % 20 == 0:
        last_progress = progress
        dt = time.time() - start_time
        print '{:>3}% processed... [{}]'.format(progress, dt2str(dt))

def read_content(content_file_path, lines_to_read=-1):
    ru_lines = []
    line_count = 0
    line_count_mod = 1024

    print 'Content file:', content_file_path
    print 'Reading content...'
    with codecs.open(content_file_path, encoding=ENCODING) as content_file:
        for line in content_file:
            ru_lines.append(line)
            line_count += 1
            if line_count % line_count_mod == 0:
                print '{:<7} lines processed...'.format(line_count)
                line_count_mod *= 2

            if line_count == lines_to_read:
                break
    
    print line_count, 'lines processed in total.\n'

    return ru_lines

def group_words(lines):
    abc = set(list(u"абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"))
    ws = set([' ', '\t', '-'])

    groups = []
    group = []

    line_count = 0
    all_line_count = len(lines)

    print 'Grouping words...'
    for line in lines:
        is_ws, is_sep = False, False
        st, i = -1, 0

        for c in line:
            if c in abc:
                if st < 0:
                    st = i
                is_ws, is_sep = False, False
            elif c in ws:
                if st >= 0:
                    group.append(line[st:i].lower())
                is_ws, is_sep, st = True, False, -1
                st = -1
            else: # got separator
                if st >= 0:
                    group.append(line[st:i].lower())
                if len(group) > 0:
                    groups.append(group)
                    group = []
                is_ws, is_sep, st = False, True, -1

            i += 1

        if len(group) > 0:
            groups.append(group)

        line_count += 1
        print_progress(line_count, all_line_count)

    print all_line_count, 'lines processed in total.\n'
    return groups

def stem_words(groups):
    import Stemmer
    stemmer = Stemmer.Stemmer(u'russian')

    word_dict = {}
    word_counter = 0
    new_groups = []
    i, count = 0, len(groups)

    print 'Stemming and counting words in groups...'
    for group in groups:
        if len(group) > 1:
            new_group = []
            for word in group:
                stemmed = stemmer.stemWord(word)
                index = 0
                if stemmed in word_dict:
                    index = word_dict[stemmed]
                else:
                    word_counter += 1
                    index = word_counter
                    word_dict[stemmed] = index
                    word_dict[index] = stemmed
                new_group.append(index)
            new_groups.append(new_group)
        i += 1
        print_progress(i, count)

    print count, 'groups processed in total.\n'
    unique_words_count = int(len(word_dict) / 2)
    print 'Unique words count - {}\n'.format(unique_words_count)
    return word_dict, new_groups

def count_ngrams(groups, n=2):
    ns = range(2, n+1)
    ngram_dict = dict()
    i, count = 0, len(groups)

    print 'Counting ngrams...'
    for group in groups:
        size = len(group)
        for j in xrange(size):
            for k in ns:
                if j+k <= size:
                    key = tuple(group[j:j+k])
                    if key not in ngram_dict:
                        ngram_dict[key] = 1
                    else:
                        ngram_dict[key] += 1
        i += 1
        print_progress(i, count)

    print count, 'groups processed in total.\n'
    return ngram_dict

def count_ngram_words(ngram_dict, n=2):
    ns = range(2, n+1)
    word_count_dict = dict()
    i, count = 0, len(ngram_dict)

    print 'Counting ngram words...'
    for wis, cc in ngram_dict.iteritems():
        k = len(wis)
        for j in xrange(len(wis)):
            key = wis[j], j, k
            if key in word_count_dict:
                word_count_dict[key] += cc
            else:
                word_count_dict[key] = cc
        i += 1
        print_progress(i, count)

    print count, 'ngrams processed in total.\n'
    return word_count_dict

def rank_ngrams(ngram_dict, word_count, n=2, topn=10000):
    import math
    ns = range(2, n+1)
    ranked_lists = { k: [] for k in ns }
    i, count = 0, len(ngram_dict)

    print 'Ranking ngrams...'
    for wis, cc in ngram_dict.iteritems():
        k = len(wis)
        rk = math.log(cc)
        for c in [word_count[(wis[j], j, k)] for j in range(k)]:
            rk *= cc / c
        ranked_lists[k].append((rk, wis))
        i += 1
        print_progress(i, count)

    print count, 'ngrams processed in total.\n'

    for k in ns:
        print '{}-grams collected: {}'.format(k, len(ranked_lists[k]))

    print ''
    print 'Sorting by rank...'
    for k in ns:
        c = len(ranked_lists[k])
        print ''
        print '{}-grams collected: {}\nSorting...'.format(k, c)
        ranked_lists[k].sort(key=lambda x: x[0], reverse=True)
        print 'Keep top-{}'.format(topn)
        ranked_lists[k] = ranked_lists[k][:topn]

    print 'Finished sorting.'
    return ranked_lists

def print_results(sorted_ranked_lists, word_dict, output_file_postfix, n, topn=0):
    ns = range(2, n+1)
    for k in ns:
        out_file_name = '%s-grams-%s.txt' % (k, output_file_postfix)
        i, count = 0, len(sorted_ranked_lists[k])
        top = []
        print 'Printing results...'
        with codecs.open(out_file_name, 'w+', encoding=ENCODING) as out:
            for rk, wis in sorted_ranked_lists[k]:
                w = u' '.join([word_dict[wi] for wi in wis])
                s = u'{:<40} {}\n'.format(w, rk)
                out.write(s)
                if i < topn:
                    top.append(s.encode(ENCODING))
                i += 1
                print_progress(i, count)

        print count, 'ngrams processed in total.\n'
        if topn > 0:
            print 'Top-%s %s-grams:' % (topn, k)
            for s in top:
                print s[:-1]
            print '-----------------------------------'

def find_significant_ngrams(content_file_path, output_file_postfix, n=2):
    print 'Algorithm for finding significant ngrams in text'
    ru_lines = read_content(content_file_path)
    groups = group_words(ru_lines)
    del ru_lines
    word_dict, stemmed = stem_words(groups)
    del groups
    ngram_dict = count_ngrams(stemmed, n)
    del stemmed
    word_count_dict = count_ngram_words(ngram_dict, n)
    sorted_ranked_lists = rank_ngrams(ngram_dict, word_count_dict, n)
    del ngram_dict
    del word_count_dict
    print_results(sorted_ranked_lists, word_dict, output_file_postfix, n, topn=10)
    print 'Successfully finished!'

if __name__ == "__main__":
    import sys, time
    if len(sys.argv) != 4:
        print 'Usage %s "content_file" "output_file_postfix" N' % sys.argv[0]
        exit(1)
    else:
        find_significant_ngrams(sys.argv[1], sys.argv[2], int(sys.argv[3]))
