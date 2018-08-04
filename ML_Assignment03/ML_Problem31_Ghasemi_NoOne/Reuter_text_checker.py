import re

tst = open('training.data', 'r')
i = 1


def check_text_line(line):
    if re.match('^(?!\n).*\n$', line):
        return True
    return False


def read_line(file):
    global i
    try:
        line = file.readline()
        i += 1
    except Exception as e:
        print(e)
        print('line number: ', i + 1)
        return

    return line


def check_doc(doc):
    if not check_text_line(doc['title']):
        print('Error in title')
        print(doc)
        print('i*************', i)
        return False
    if not check_text_line(doc['subject']):
        print('Error in subject')
        print(doc)
        print('i*************', i)
        return False
    if not check_text_line(doc['date']):
        print('Error in date')
        print(doc)
        print('i*************', i)
        return False
    return True

while True:
    print('iiiiiiiiiiiiiiiiii', i)
    doc = {}

    doc['title'] = read_line(tst)
    read_line(tst)
    doc['subject'] = read_line(tst)
    read_line(tst)
    doc['date'] = read_line(tst)
    read_line(tst)

    doc['body'] = ''
    cur_line = read_line(tst)
    while check_text_line(cur_line):
        doc['body'] += cur_line
        cur_line = read_line(tst)
    if not check_doc(doc):
        tst.close()
        break

    cur_line = read_line(tst)
    if cur_line != '\n':
        print('errrrrrrrrrrrrrrrrrrr', i)
        print(doc)
        tst.close()
        break


tst.close()
