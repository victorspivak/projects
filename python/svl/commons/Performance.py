import time


def timing(msg, f, formatter=lambda m, t, r: m % t):
    start_time = time.time()
    res = f()
    running_time = time.time() - start_time
    line = formatter(msg, running_time, res)

    print(line)

    return res, running_time

