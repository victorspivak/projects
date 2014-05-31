str = 'hello world'

chars = set(str)

sorted = list(chars)
sorted.sort()
for c in sorted:
    print(c)

l1 = [1,3,5,7,9]
l2 = [9,3,5,7,1]

print('Match' if l1 == l2 else 'Failed')
print('Match' if set(l1) == set(l2) else 'Failed')

engineers = {'vic', 'bob', 'sue', 'ann'}
managers = {'tom', 'sue'}

print('vic found' if 'vic' in engineers else 'vic not found')
print('jeff found' if 'jeff' in managers else 'jeff not found')

print('Intersection:', engineers & managers)
print('Union:', engineers | managers)
print('Engineers but not managers:', engineers - managers)
print('Managers but not engineers:', managers - engineers)
print('Are all managers engineers:', engineers > managers)
print('Are both engineers:', {'bob', 'sue'} < engineers)
print('IN one but not in both sets:', engineers ^ managers)

