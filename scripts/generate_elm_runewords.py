import json
import re

with open('runeword-data.json', 'r') as f:
    data = json.load(f)

runeword_list = data['collection1']

strings = []
for rw in runeword_list:
    name = rw['runeword-name']['text']
    runes = re.findall('[A-Z][^A-Z]*', rw['runeword-runes'])
    properties = rw['runeword-properties'].split('\n')
    is_one_eleven = rw['runeword-1.11'] == 'Yes'
    is_one_ten = rw['runeword-1.10'] == 'Yes'
    is_one_nine = rw['runeword-1.09'] == 'Yes'
    if is_one_nine:
        patch = 'OneNine'
    elif is_one_ten:
        patch = 'OneTen'
    elif is_one_eleven:
        patch = 'OneEleven'
    else:
        raise Exception('wrong patch')

    sockets = int(re.search('[^{]*\{(\d)\}', rw['runeword-info']).group(1))
    _type = re.search('([^{]+) \{\d\}', rw['runeword-info'].split('\n')[2]).group(1)
    clvl = int(re.search('[^\d]+[ ]?(\d\d)', rw['runeword-info'].split('\n')[3]).group(1))

    runeword_string = '''
    {{
        name = "{}"
        ,runes = {}
        ,sockets = {}
        ,properties = {}
        ,ladderOnly = False
        ,patch = {}
        ,clvl={}
    }}
    '''.format(name, '{}'.format(runes).replace("'", ''), sockets, '{}'.format(properties).replace("'", '"'), patch, clvl)

    strings.append(runeword_string)


output = '''
runewords : List Runeword
runewords = [
    {}
]
'''.format(','.join(strings))

with open('runewords.elm', 'w') as f:
    f.write(output)
