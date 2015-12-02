# -*- coding: utf-8 -*-

from __future__ import division

import codecs
import Stemmer

enc = 'utf-8'

ru_filtered_words = "ru_filtered_word_content.txt"
ru_dict_words = "ru_dict_paired_count.txt"

stem_dict = dict()

def upd_dict(w1, w2):
    global stem_dict
    w = ' '.join(sorted([w1, w2]))
    if w not in stem_dict:
        stem_dict[w] = 1
    else:
        stem_dict[w] += 1

line_count = 0

with codecs.open(ru_filtered_words, encoding=enc) as fwf:
    for line in fwf:
        line_count += 1
        words = line[:-1].split(' ')

        for i in xrange(1, len(words)):
            upd_dict(words[i-1], words[i])

        if line_count % 100000 == 0:
            print line_count, 'lines processed...'

print line_count, 'lines processed in total'

dict_size = len(stem_dict)

with codecs.open(ru_dict_words, 'w+', enc) as dwc:
    for (w, c) in sorted(stem_dict.items(), key=lambda x: (x[1],x[0]), reverse=True):
        dwc.write(w)
        dwc.write(' ')
        dwc.write(str(c / dict_size))
        dwc.write('\n')

print 'Successfully finished'
print 'Stem-dictionary size:', len(stem_dict)
