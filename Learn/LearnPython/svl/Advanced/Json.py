import json

person = {'name':{'first':'Vic', 'last':'Spivak'}, 'jobs':['dev', 'architect'], 'age':33}

json1 = json.dumps(person)
print('json:', json1)

person1 = json.loads(json1)
print(person1)

print('=' * 80)
print(json.dumps(person, indent=4))

