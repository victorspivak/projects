l1 = [1,2,3,4]
l2 = [1,2,3,4]
l3 = l1

print('Equality test: l1 == l2', l1 == l2)
print('Identity test: l1 is l2', l1 is l2)
print('Identity test: l1 is l3', l1 is l3)

def func1(cond, a, b):
    return a if cond else b

print(func1(True, 1, 2))
print(func1(False, 1, 2))

def func2(a,b,c):
    if (a < b < c):
        print('Match Success', a,b,c)
    else:
        print('Match Failed', a,b,c)

def func3(a,b,c):
    if (a < b and b < c):
        print('Match Success', a,b,c)
    else:
        print('Match Failed ', a,b,c)


func2(3,4,5)
func3(3,4,5)
func2(3,4,3)
func3(3,4,3)