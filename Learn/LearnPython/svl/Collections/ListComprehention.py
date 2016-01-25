a = ["aa", "foo", "Hello World"]
b = [x.upper() for x in a]
print(b)

ff = list(filter(lambda xx: len(xx) > 2, a))
print(ff)


def string_length(s):
    return len(s)


c = list(map(string_length, a))
print(c)
