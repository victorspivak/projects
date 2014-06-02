import sys
import os

sys.stdout.write('hello world')
stdout = sys.stdout

filename = 'std-out.txt'
sys.stdout = open(filename, 'w')
print('header')
print('hello world')
print('footer')
sys.stdout.close()
sys.stdout = stdout

print('restored')
os.remove(filename)

print('=' * 80)
class Printer:
    def __init__(self):
        self.stdout = sys.stdout

    def write(self, s):
        self.stdout.write('|' + s + '|')

    def flush(self):
        self.stdout.flush()

sys.stdout = Printer()
print('Hello Printer')
print([1,2,3,4,5])





