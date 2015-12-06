#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import division

import codecs

ENCODING = 'utf-8'

last_progress = -1
def print_progress(cur, all):
    global last_progress
    progress = int(cur * 100 / all)
    if progress != last_progress and progress % 20 == 0:
        last_progress = progress
        print '%s%% processed...' % progress

def read_content(content_file_path, lines_to_read=-1):
    ru_lines = list()
    line_count = 0
    line_count_mod = 1

    print 'Content file:', content_file_path
    print 'Reading content...'
    with codecs.open(content_file_path, encoding=ENCODING) as content_file:
        for line in content_file:
            ru_lines.append(line)
            line_count += 1
            if line_count % line_count_mod == 0:
                print line_count, 'lines processed...'
                line_count_mod *= 2

            if line_count == lines_to_read:
                break
    
    print line_count, 'lines processed in total.'

    return ru_lines

def group_words(lines):
    abc = set(list(u"абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"))
    ws = set([' ', '\t', '-'])

    groups = list()
    group = list()

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
                    group = list()
                is_ws, is_sep, st = False, True, -1

            i += 1

        if len(group) > 0:
            groups.append(group)

        line_count += 1
        print_progress(line_count, all_line_count)

    print all_line_count, 'lines processed in total.'
    return groups

def stem_and_stop_words(groups, stopwords_list_path):
    import Stemmer
    stemmer = Stemmer.Stemmer(u'russian')
    stopwords = set()

    print 'Stopword-list file:', stopwords_list_path
    print 'Building stopword set...'
    with codecs.open(stopwords_list_path, encoding=ENCODING) as swlf:
        for stopword in swlf:
            stopwords.add(stemmer.stemWord(stopword[:-1]))

    filtered_groups = list()
    i, count = 0, len(groups)

    print 'Stemming and filtering words in groups...'
    for group in groups:
        new_group = list()

        for word in group:
            w = stemmer.stemWord(word)
            if w not in stopwords:
                new_group.append(w) 
            else:
                if len(new_group) > 1:
                    filtered_groups.append(new_group)
                    new_group = list()
                elif len(new_group) > 0:
                    new_group = list()

        if len(new_group) > 1:
            filtered_groups.append(new_group)

        i += 1
        print_progress(i, count)

    print count, 'groups processed in total.'
    return filtered_groups

def count_bigrams(groups):
    bigram_dict = dict()
    i, count = 0, len(groups)

    print 'Counting bigrams...'
    for group in groups:
        for wi in xrange(len(group)-1):
            key = group[wi], group[wi+1]
            if key not in bigram_dict:
                bigram_dict[key] = 1
            else:
                bigram_dict[key] += 1
        i += 1
        print_progress(i, count)

    print count, 'groups processed in total.'
    return bigram_dict

def count_bigram_words(bigram_dict):
    fst_count = dict()
    snd_count = dict()
    i, count = 0, len(bigram_dict)

    print 'Counting bigram words...'
    for (w1, w2), cc in bigram_dict.iteritems():
        fst_count[w1] = cc if w1 not in fst_count else fst_count[w1]+cc
        snd_count[w2] = cc if w2 not in snd_count else snd_count[w2]+cc
        i += 1
        print_progress(i, count)

    print count, 'bigrams processed in total.'
    return fst_count, snd_count

def rank_bigrams(bigram_dict, fst_count, snd_count):
    import math
    ranked_list = list()    
    i, count = 0, len(bigram_dict)

    print 'Ranking bigrams...'
    for (w1, w2), cc in bigram_dict.iteritems():
        c1, c2 = fst_count[w1], snd_count[w2]
        rk = cc * cc * math.log(cc) / c1 / c2
        ranked_list.append((rk, (w1, w2), (c1, cc, c2)))
        i += 1
        print_progress(i, count)

    print count, 'bigrams processed in total.'

    print 'Sorting by rank...'
    sorted_ranked_list = sorted(ranked_list, key=lambda x: x[0], reverse=True)

    print 'Finished sorting.'
    return sorted_ranked_list

def print_results(sorted_ranked_list, output_file_path, topn=0):
    i, count = 0, len(sorted_ranked_list)

    top = list()

    print 'Printing results...'
    with codecs.open(output_file_path, 'w+', encoding=ENCODING) as out:
        for rk, (w1, w2), (c1, cc, c2) in sorted_ranked_list:
            s = u'{:<25} {} [{}-{}-{}]\n'.format(w1+' '+w2, rk, c1, cc, c2)
            out.write(s)
            if i < topn:
                top.append(s.encode(ENCODING))
            i += 1
            print_progress(i, count)

    print count, 'bigrams processed in total.'
    print 'Top %s bigrams:' % topn
    for s in top:
        print s[:-1]

def find_significant_bigrams(content_file_path, stopwords_list_path, output_file_path):
    print 'Algorithm for finding significant bigrams in text'
    ru_lines = read_content(content_file_path)
    groups = group_words(ru_lines)
    filtered_groups = stem_and_stop_words(groups, stopwords_list_path)
    bigram_dict = count_bigrams(filtered_groups)
    fst_count, snd_count = count_bigram_words(bigram_dict)
    sorted_ranked_list = rank_bigrams(bigram_dict, fst_count, snd_count)
    print_results(sorted_ranked_list, output_file_path, 10)
    print '>>>>>>>>>>>> <<<<<<<<<'
    print 'Successfully finished!'

if __name__ == "__main__":
    import sys
    if len(sys.argv) != 4:
        print 'Usage %s "content_file" "stopwords_file" "output_file"' % sys.argv[0]
        exit(1)
    else:
        find_significant_bigrams(sys.argv[1], sys.argv[2], sys.argv[3])
