firstName = 'Victor'
lastName = 'Spivak'
print('Hello %s %s' % (firstName, lastName))
print('Hello {} {}'.format (firstName, lastName))
print('Hello {0} {1}'.format (firstName, lastName))
print('Hello {1} {0}'.format (firstName, lastName))
print('Hello {0} {1}. Mr. {1} ...'.format (firstName, lastName))
print('Hello {first} {last}. Mr. {last} ...'.format (first = firstName, last = lastName))

print('==============================================')
table = [('A name', 100, 1.12345, 1), ('A long name', 10000, 111.999912345, -1), ('A very long name', 10, 1.12345, 99)]

for r in table:
    print('{0:>15.15} {1:6d} {1:6x} {2:6.2f} {3:^+4d}'.format(r[0], r[1], r[2], r[3]))

