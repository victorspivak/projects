l = [1,2,3,4,5]

print(len(l))
l.append(6)
print(len(l))
l.extend([10,11,12])
print(len(l))

print(l[2])
print(l.pop(2))

print(l[5:9])

l = [1, 2, 3]
l = l * 10
print(l)
l.sort()
print(l)

print(l[-1])

m= [[1,2,3], [4,5,6], [7,8,9]]
print(m[1][1])

col2 = [row[1] for row in m]
print(col2)


