# -*- coding: utf-8 -*-

import codecs

enc = 'utf-8'

alphabet = set(list(u"абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"))
whitespace = set([' ', '\t', '-'])

ru_content = "ru_content.txt"
ru_word_content = "ru_word_content.txt"
ru_content_file = codecs.open(ru_content, encoding=enc)
ru_word_content_file = codecs.open(ru_word_content, 'w+', enc)

def print_word(is_fst, word):
    if not is_fst:
        ru_word_content_file.write(' ')
    lowered = word.lower()
    ru_word_content_file.write(lowered)

def print_sep():
    ru_word_content_file.write('\n')

line_count = 0

for line in ru_content_file:
    line_count += 1

    if line_count % 1000 == 0:
        print line_count, 'lines processed...'

    printed = False

    st = -1
    is_ws = False
    is_sep = False
    i = 0

    def have_word():
        return st >= 0

    def on_word():
        global printed
        print_word(not printed, line[st:i])
        printed = True

    def on_sep():
        if printed and not is_sep:
            print_sep()

    for c in line:

        if c in alphabet:

            if not have_word():
                st = i

            is_ws = False
            is_sep = False

        elif c in whitespace:

            if have_word():
                on_word()

            is_ws = True
            is_sep = False
            st = -1

        else: # got separator

            if have_word():
                on_word()

            on_sep()

            is_sep = True
            is_ws = False
            st = -1
            printed = False

        i += 1

    on_sep()


ru_content_file.close()
ru_word_content_file.close()

print 'Succesfully finished'
print line_count, 'lines processed in total'