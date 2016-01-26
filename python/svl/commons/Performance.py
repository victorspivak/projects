import time


def timing(msg, f, formatter=lambda m, t, r: m % t):
    start_time = time.time()
    res = f()
    line = formatter(msg, (time.time() - start_time), res)
    print(line)
    return res

