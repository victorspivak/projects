s = 'spam'
while s:
    print(s, end=' ')
    s = s[1:]

print()
print('', '=' * 80)
a = 0
b = 10
while a < b:
    print(a, end=' ')
    a += 1

print()
print('=' * 80)


def is_prime(y):
    x = y // 2
    while x > 1:
        if y % x == 0:
            print(y, 'has factor', x)
            break
        x -= 1
    else:
        print(y, 'is prime')


is_prime(61)
is_prime(63)
is_prime(65)

print('=' * 80)
s = 0
for i in range(11):
    s += i

print('sum:', s)
