import random

print(random.randint(1, 10))

dict = [] open("/usr/share/dict/words").readlines()
words_count = len(dict)

def random_text(max_length):
    length = random.randint(1, max_length)
    words = []
    for i in range(length):
        #we have to strip the trailing CR
        words.append(dict[random.randint(0, words_count)][0:-1])
    return " ".join(words)

for i in range(20):
    print(random_text(25))


