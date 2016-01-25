import time


def timing(msg, f):
    start_time = time.time()
    f()
    print(msg % (time.time() - start_time))
