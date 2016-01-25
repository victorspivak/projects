import collections

extensions = {'java': '.java', 'python': '.py', 'c++': '.cpp'}
print(extensions['python'])

square = dict(zip([1, 2, 3], [1, 4, 9]))
print(square)

dictionary = dict(name='Vic', company='Box')
print(dictionary)

dictionary = dict([('name', 'Vic'), ('company', 'Box')])
print(dictionary)

keys = ['name', 'company']
values = ['Vic', 'Box']
dictionary = dict(zip(keys, values))
print(dictionary)
print('keys:', dictionary.keys())
print('values:', dictionary.values())
print('items:', dictionary.items())
dictionary.clear()
print('clear:', dictionary)

print('=' * 80)
person = {'name': {'first': 'Vic', 'last': 'Spivak'}, 'jobs': ['dev', 'architect'], 'age': 33}
print(person)

if 'bogus' in person:
    print(person['bogus'])
else:
    print('Bogus key')

print('get with default:', person.get('bogus', 'EMPTY'))

print('=' * 80)
dictionary = dict.fromkeys([1, 2, 3, 4, 5], 0)
print('fromKeys:', dictionary)

print('=' * 80)
dictionary = {x: x ** 2 for x in range(1, 10)}
print(dictionary)

codea = ord('a')
codeA = ord('A')
dictionary = {chr(codea + i): chr(codeA + i) for i in range(0, 26)}
print(dictionary)
keys = list(dictionary.keys())
keys.sort()
for k in keys:
    print(k, ' --> ', dictionary[k])

print('=' * 80)
map1 = collections.defaultdict(lambda: 'No Value')
map1['k1'] = 'v1'
print('Values %s %s' % (map1['k1'], map1['k2']))

# map with 0 as default value
map2 = collections.defaultdict(int)
map2['k1'] += 1
print('Ints %d %d' % (map2['k1'], map2['k2']))

print('=' * 80)
counter = collections.Counter('abrakadabra')
print('chars frequency: ' + str(counter))
print('3 most common char: ' + str(counter.most_common(3)))

counter = collections.Counter()
counter['a'] += 5
counter['b'] += 15
counter['c'] += 25
counter['x'] += 125
counter['y'] += 2
counter['z'] += 3
print('chars frequency: ' + str(counter))
print('3 most common char: ' + str(counter.most_common(3)))

