#multiple per line
a=1;b=2;c=3
print(a,b,c)

if a > 0: print('I ma here')

if a > 0:
    if b > 1:
        print('Inside 2')
    else:
        print('Outside 2')

if a > 1:
    if b > 2:
        print('Inside 2')
else:
    print('Outside 1')

print('=' * 80)
a=b=c='spam'
print(a,b,c)

#Sequence assignments
print('=' * 80)
[a1,b1,c1] = (1,2,3)
print(a1,b1,c1)

s = 'abcd'
a,b,c,d = s
print(a,b,c,d)

zero, one, two = range(3)
print(zero, one, two)

print('=' * 80)
l = [1,2,3,4,5]
print('list:', l)
head, *rest = l
print('head:', head)
print('rest:', rest)

*top, last = l
print('top:', top)
print('last:', last)

head, *middle, last = l
print('head:', head)
print('middle:', middle)
print('last:', last)



