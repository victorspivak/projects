def foo(*args):
    for arg in args:
        print(arg)

foo (1, 'hello', 2, 'world')

l = [1,2,3]
foo(*l)