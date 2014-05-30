s= 'Hello World'

methods = dir(s)
for m in methods:
    print(m)

print(help(s.zfill))
print(help(s.__add__))