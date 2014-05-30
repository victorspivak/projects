import os
import pickle

def dumpFile(fn):
    input = open(fn)
    for line in input:
        print(line, end='')
    input.close()

print(os.getcwd())
filename1 = 'python-test-file.txt'
out = open(filename1, mode='w')
out.write('Hello Python\n')
for i in range(0, 10):
    out.writelines("line: {}\n".format(i))
out.write('Bye Python\n')
out.close()

input = open(filename1)
for line in input:
    print(line, end='')
input.close()

out = open(filename1, mode='wb')
l = [1,2,'a', 'b']
pickle.dump(l, out)
out.close()

input = open(filename1, 'rb')
l1 = pickle.load(input)
print(l1)
input.close()

print('=' * 80)
out = open(filename1, mode='w')
for i in range(5):
    print(i, file=out)
out.close()
dumpFile(filename1)

os.remove(filename1)



