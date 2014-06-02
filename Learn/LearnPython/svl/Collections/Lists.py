l = [1,2,3,4,5]

print(len(l))
print(l)
s = str(l)
print(s)
l.append(6)
print(len(l))
l.extend([10,11,12])
print(len(l))
print(l[2])

print('=' * 80)
l = [0,1,2,3,4,5,6,7,8,9]
l[1] = 11
l[2:4] = [12,13]
print('modified array:', l)
l[0:4] = [99]
print('modified array:', l)

print('=' * 80)
letters = list('abcdefgh')
for c in letters:
    print(c, end=' ')
print()
print('is \'d\' in the list? ', 'd' in letters)
print('is \'z\' in the list? ', 'z' in letters)
print('\'d\' index in the list? ', letters.index('d'))
letters.reverse()
print('reversed:', letters)
letters.reverse()

print('=' * 80)
def transform(c):
    return chr(ord(c) + 10)
letters1 = list(map(transform, letters))
print('Transformed letters:', letters1)

print('=' * 80)
print('Array before pop: ', l)
print('pop third element: ', l.pop(2))
print('Array after pop: ', l)

print('=' * 80)
letters.remove('d')
print('after remove \'d\':', letters)
del(letters[1])
print('after remove second element:', letters)
del(letters[1:])
print('after remove all but first element:', letters)

print('=' * 80)
print('slice: ', l[5:9])

l = [1, 2, 3]
l = l * 10

print('=' * 80)
print('array before sorting:', l)
l.sort()
print('array after sorting:', l)
print('get last element:', l[-1])

l = list('Hello World')
l.sort()
print('Sorted: ', l)
l.sort(key = str.lower)
print('Sorted by lower: ', l)
l.sort(key = str.lower, reverse=True)
print('Sorted by lower, reversed: ', l)

print('=' * 80)
m= [[1,2,3], [4,5,6], [7,8,9]]
print('matrix:', m)
print(m[1][1])

col2 = [row[1] for row in m]
print(col2)

print('=' * 80)
l1 = [1,2,3]
l2 = [4,5,6]
l = l1 + l2


