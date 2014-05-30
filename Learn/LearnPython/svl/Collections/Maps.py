extensions = {'java': '.java', 'python':'.py', 'c++':'.cpp'}
print(extensions['python'])

square = dict(zip([1,2,3], [1,4,9]))
print(square)

map = dict(name='Vic', company = 'Box')
print(map)

person = {'name':{'first':'Vic', 'last':'Spivak'}, 'jobs':['dev', 'architect'], 'age':33}
print(person)
if 'bogus' in person:
    print(person['bogus'])
else:
    print('Bogus key')