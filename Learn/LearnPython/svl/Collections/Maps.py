extensions = {'java': '.java', 'python':'.py', 'c++':'.cpp'}
print(extensions['python'])

square = dict(zip([1,2,3], [1,4,9]))
print(square)

map = dict(name='Vic', company = 'Box')
print(map)

map = dict([('name', 'Vic'), ('company', 'Box')])
print(map)

keys = ['name', 'company']
values = ['Vic', 'Box']
map = dict(zip(keys, values))
print(map)
print('keys:', map.keys())
print('values:', map.values())
print('items:', map.items())
map.clear()
print('clear:', map)

print('=' * 80)
person = {'name':{'first':'Vic', 'last':'Spivak'}, 'jobs':['dev', 'architect'], 'age':33}
print(person)

if 'bogus' in person:
    print(person['bogus'])
else:
    print('Bogus key')

print('get with default:', person.get('bogus', 'EMPTY'))

print('=' * 80)
map = dict.fromkeys([1,2,3,4,5], 0)
print('fromKeys:', map)

print('=' * 80)
map = {x: x** 2 for x in range(1, 10)}
print(map)

codea = ord('a')
codeA = ord('A')
map = {chr(codea + i): chr(codeA + i) for i in range(0, 26)}
print(map)
keys = list(map.keys())
keys.sort()
for k in keys:
    print(k, ' --> ', map[k])

