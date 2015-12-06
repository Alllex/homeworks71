#!/usr/bin/env python
# -*- coding: utf-8 -*-

import codecs

ENCODING = 'utf-8'

def filter_russian(content_file_path, output_file_path):
    from langdetect import detect, lang_detect_exception
    line_count = 0
    line_count_mod = 1
    lang = None

    print 'Content file:', content_file_path
    print 'Filtering Russian language...'
    out = codecs.open(output_file_path, 'w+', encoding=ENCODING)
    with codecs.open(content_file_path, encoding=ENCODING) as content_file:
        for line in content_file:
            try:
                lang = detect(line)
            except lang_detect_exception.LangDetectException as e: 
                pass

            if lang == 'ru':
                out.write(line)

            line_count += 1
            if line_count % line_count_mod == 0:
                print line_count, 'lines processed...'
                line_count_mod *= 2
    
    print line_count, 'lines processed in total.'
    out.close()

if __name__ == "__main__":
    import sys
    if len(sys.argv) != 3:
        print 'Usage %s "content_file" "output_file"' % sys.argv[0]
        exit(1)
    else:
        filter_russian(sys.argv[1], sys.argv[2])
