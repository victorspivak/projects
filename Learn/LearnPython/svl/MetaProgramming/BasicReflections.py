a = 'Hello'

print(type(a))
print(type(True))

print(isinstance(a, int))
print(isinstance('1', int))
print(isinstance(1, int))

l1 = [1,2,3]
l2 = l1
l3 = l1

import sys
print(sys.getrefcount(l1))
print(sys.getrefcount(1))
a = 1
print(sys.getrefcount(1))
