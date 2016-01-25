from collections import *

c = Counter('abracadabra')
c = list(k for k, v in c.most_common(3))
print(c)

