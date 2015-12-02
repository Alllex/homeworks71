# -*- coding: utf-8 -*-

from langdetect import detect, lang_detect_exception
import codecs


input_all = "train_content.csv"
output_ru = "ru_content.txt"
output_others = "other_content.txt"
output_err = "error_content.txt"

output_ru_file = codecs.open(output_ru, 'w+', 'utf-8')
output_others_file = codecs.open(output_others, 'w+', 'utf-8')
output_err_file = codecs.open(output_err, 'w+', 'utf-8')

def process(lang, text, err=''):
    if lang == 'ru':
        output_ru_file.write(text)
    elif lang == 'err':
        output_err_file.write(err)
        output_err_file.write('\n')
        output_err_file.write(text)
    else:
        output_others_file.write(lang + '\n')
        output_others_file.write(text)

input_all_file = codecs.open(input_all, encoding='utf-8')
line_count = 0
for line in input_all_file:
    line_count += 1
    if line_count % 1000 == 0:
        print line_count, 'lines processed...'
    try:
        lang = detect(line)
        process(lang, line)
    except lang_detect_exception.LangDetectException as e: 
        process('err', line, repr(e))

input_all_file.close()
output_ru_file.close()
output_others_file.close()
output_err_file.close()

print 'Succesfully finished'
print line_count, 'lines processed in total'