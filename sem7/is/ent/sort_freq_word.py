# -*- coding: utf-8 -*-

from __future__ import division

import codecs
import numpy as np
import matplotlib.pyplot as plt

enc = 'utf-8'

def ru(ru):
    return ru.encode(enc)

ru_dict_words = "ru_dict_word_count.txt"
ru_dict_pairs = "ru_dict_paired_count.txt"
ru_sort_pairs = "ru_sorted_pairs.txt"

line_count = 0

word_freq_dict = dict()
word_freq_list = list()
freq_set = set()
min_freq = 2 
max_freq = -1

with codecs.open(ru_dict_words, encoding=enc) as dwf:
    for line in dwf:
        ws = line[:-1].split(' ')
        w = ws[0]
        p = float(ws[1].encode(enc))
        word_freq_dict[w] = p
        freq_set.add(p)
        # word_freq_list.append((ru(w), p))
        # min_freq = min(min_freq, p)
        # max_freq = max(max_freq, p)

print 'dictionary is built: size =', len(word_freq_dict)

# freq_list = sorted(freq_set, reverse=True)
# freq_count = len(freq_list)
# shift = int(freq_count * 1 / 100)
# low_bias = freq_list[-shift]
# high_bias = freq_list[shift]

# def passes(p):
#     return low_bias <= p <= high_bias

pair_list = list()

with codecs.open(ru_dict_pairs, encoding=enc) as dpf:
    for line in dpf:
        ws = line[:-1].split(' ')
        w1, w2 = ws[0], ws[1]
        # pw1, pw2 = word_freq_dict[w1], word_freq_dict[w2]
        p = float(ws[2].encode(enc))
        pair_list.append((w1, w2, p))

pair_count = len(pair_list)

print 'pair list is loaded: size =', pair_count

rate_list = list()
max_df = 0
max_kf = -1
min_kf = 100

for (w1, w2, p) in pair_list: 
    pw1, pw2 = word_freq_dict[w1], word_freq_dict[w2]
    df = abs(pw1 - pw2)
    max_df = max(max_df, df)
    kf = 1.0 / (4 * (0.1 + df))
    min_kf = min(min_kf, kf)
    max_kf = max(max_kf, kf)
    rate = p * pair_count 
    rate_list.append((w1, w2, p, rate, df, rate * kf))

print 'rate list assembled: max df =', max_df
print 'min kf =', min_kf, 'max kf =', max_kf

# exit(0)

# for w1, w2, p, rate, df, newrate in rate_list[:11]:
#     print ru(w1), ru(w2), 'rate =', rate, 'df =', df, ' new rate =', newrate

def words_key((w1, w2, p, r, df, nr)):
    return nr

with codecs.open(ru_sort_pairs, 'w+', encoding=enc) as spf:
    for w1, w2, p, rate, df, newrate in sorted(rate_list, key = words_key, reverse=True):
        w = w1 + ' ' + w2
        s = '\t'.join((w, str(rate), str(newrate)))
        spf.write(s)
        spf.write('\n')


# frame_count = 100
# rmax_freq = 1.0 / max_freq
# win_size = (1.0 / min_freq - rmax_freq) / frame_count

# frame_index = 0
# count_occur = np.zeros((frame_count+1,), dtype=np.int)

# for (w, p) in word_freq_list:
#     rp = 1.0 / p
#     if rp > rmax_freq + frame_index * win_size:
#         frame_index += 1

#     count_occur[frame_index] += 1

# print 'reversed max freq =', rmax_freq
# print 'frame count =', frame_count
# print 'window size =', win_size
# print count_occur

# ps = np.array([p for w, p in word_freq_list])
# avg = ps.mean()
# mx = ps.max()
# mn = ps.min()
# low = (mn + avg) / 2.0
# high = (avg + mx) / 2.0
# bounds = map(lambda x: str(x), [mn, low, avg, high, mx])
# print ' < '.join(bounds)

# count = len(ps)
# lower = (ps < low).sum() * 100.0 / count
# higher = (ps > high).sum() * 100.0 / count
# passed = ((low < ps) & (ps < high)).sum() * 100.0 / count

# print lower, passed, higher
# print (lower + passed + higher)

# print ((ps > mn).sum() * 100.0 / count)

# print mn, '<', avg, '<', mx
# print mn, '<', my_avg, '<', mx

# print 'len =', len(ps)
# print 'avg <', (ps > avg).sum()

# plt.plot(np.linspace(0.0, len(rps), num=len(rps)), rps)
# plt.show()



print 'Successfully finished'
