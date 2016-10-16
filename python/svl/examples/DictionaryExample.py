from collections import defaultdict

d = dict()

d['a'] = 1
d['b'] = 2
d['c'] = 3

k = list(d.keys())

print(k)


def factory_value(value):
    return lambda: value

d1 = defaultdict(factory_value('missing'))

d1['a'] = ['a', 'A']

print(d1['a'])
print(d1['b'])


d2 = defaultdict(list)
d2[1] = [1, 2, 3, 4, 5]

ll = d2[2]
ll.append(111)

print(d2)
