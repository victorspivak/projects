strings = ['hello', 'world', 'it', 'is', 'a', 'text']
# do not forget to wrap with list - map returns iterator
lengths1 = list(map(lambda word: len(word), strings))
print(lengths1)

# another way to transform list
lengths2 = list(len(word) for word in strings)
print(lengths2)




