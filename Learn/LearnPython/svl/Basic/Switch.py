def sqr():
    print('perfect sqr')
def even():
    print('perfect even')
def prime():
    print('perfect prime')
def unknown():
    print('unknown')

options = {
    1: sqr,
    2: even,
    3: prime,
    4: sqr,
    5: prime,
    6: even,
}

print('4:', end = '')
options.get(4, unknown)()
print('15:', end = '')
options.get(15, unknown)()
