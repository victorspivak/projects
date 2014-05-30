s1 = "Spam"
print(len(s1))
s8 = s1 * 8
print(len(s8))

print(s1[0])
for c in s1:
    print(c, ' ', end='')

print(s1[-1])
print(s1[-1] == s1[len(s1) - 1])

s = 'Hello World'
print(s[:4])
print(s[6:])
print(s[2:4])

print(s.find('ll'))
print(s.find('ll', 3))

print(s.replace('ll', 'LL'))
print(s.upper())
print(s.lower())

pack = '1,2,3,4,5'
elements = pack.split(',')
print(elements)

print(pack.isalnum(), pack.isalpha(), pack.isalnum())
print(elements[2].isalnum(), elements[2].isalpha(), elements[2].isalnum())

multiline = """
It is a first line
It is a second line
\tand
It is the last one.
"""

print(multiline)