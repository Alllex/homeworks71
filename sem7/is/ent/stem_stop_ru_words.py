# -*- coding: utf-8 -*-

from __future__ import division

import codecs
import Stemmer

enc = 'utf-8'

stemmer = Stemmer.Stemmer(u'russian')
stopwords = set()

ru_stopword_list = 'ru_stopword_list.txt'
with codecs.open(ru_stopword_list, encoding=enc) as swf:
    for stopword in swf:
        stopwords.add(stemmer.stemWord(stopword[:-1]))


ru_word_content = "ru_word_content.txt"
ru_filtered_words = "ru_filtered_word_content.txt"
ru_dict_words = "ru_dict_word_count.txt"

line_count = 0
stem_dict = dict()

def upd_dict(w):
    global stem_dict
    if w not in stem_dict:
        stem_dict[w] = 1
    else:
        stem_dict[w] += 1

output_file = codecs.open(ru_filtered_words, 'w+', enc)

with codecs.open(ru_word_content, encoding=enc) as wcf:
    for line in wcf:

        line_count += 1

        filtered_words = list()
        for word in line[:-1].split(' '):
            w = stemmer.stemWord(word)

            if w in stopwords:
                if len(filtered_words) > 1:
                    newline = ' '.join(filtered_words)
                    output_file.write(newline)
                    output_file.write('\n')
                if len(filtered_words) > 0:
                    filtered_words = list()
            else:
                upd_dict(w)
                filtered_words.append(w)

        if line_count % 500000 == 0:
            print line_count, 'lines processed...'

output_file.close()

dict_size = len(stem_dict)

with codecs.open(ru_dict_words, 'w+', enc) as dwc:
    for (w, c) in sorted(stem_dict.items(), key=lambda x: (x[1],x[0]), reverse=True):
        dwc.write(w)
        dwc.write(' ')
        dwc.write(str(c / dict_size))
        dwc.write('\n')

print 'Successfully finished'
print line_count, 'lines processed in total'
print 'Stem-dictionary size:', len(stem_dict)


