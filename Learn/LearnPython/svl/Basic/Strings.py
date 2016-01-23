s1 = "Spam"
print(len(s1))
s8 = s1 * 8
print(len(s8))

print(s1[0])
for c in s1:
    print(c, ' ', end='')

print(s1[-1])
print(s1[-1] == s1[len(s1) - 1])

print('=' * 80)
s = 'Hello World'
print(s[:4])
print(s[6:])
print(s[:-1])
print('Slice from 2 to 6 chars: ', s[2:6])
print('Slice from 2 to 6 chars with step 1: ', s[2:6:1])
print('Slice from 2 to 6 chars with step 2: ', s[2:6:2])
print('Slice every 3-rd character: ', s[::3])

print(s.find('ll'))
print(s.find('ll', 3))
print('ends with orld: ', s.endswith('orld'))
print(s.replace('ll', 'LL'))
print(s.upper())
print(s.lower())

print('=' * 80)
pack = '1,2,3,4,5'
elements = pack.split(',')
print(elements)
print('join: ', '|'.join(elements))

print('=' * 80)
print(pack.isalnum(), pack.isalpha(), pack.isalnum())
print(elements[2].isalnum(), elements[2].isalpha(), elements[2].isalnum())

multiline = """
It is a first line
It is a second line
\tand
It is the last one.
"""

print(multiline)
