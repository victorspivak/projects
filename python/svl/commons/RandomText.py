import random
from string import digits, ascii_letters
from collections import Counter


def random_text(min_length, max_length):
    length = random.randint(min_length, max_length)
    text = []
    for index in range(length):
        word = random.choice(words)
        used_words[word] += 1
        text.append(word)
    return " ".join(text)


def random_id(prefix, length):
    return prefix + ''.join(random.choice(id_symbols) for i in range(length - len(prefix)))


def reset_used_words():
    used_words.clear()


def get_used_words():
    return used_words


# we have to strip the trailing CR
words = list(line.rstrip('\n').rstrip('\r') for line in open('/usr/share/dict/words', 'r').readlines())
id_symbols = ascii_letters + digits
used_words = Counter()
