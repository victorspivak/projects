class Pet:
    def __init__(self, kind, name):
        self.kind = kind
        self.name = name


class Dog(Pet):
    count = 0

    def __init__(self, name):
        super().__init__('Dog', name)
        Dog.count += 1


class Cat(Pet):
    count = 0

    def __init__(self, name):
        super().__init__('Cat', name)
        Cat.count += 1

cats = [Cat('Tommy'), Cat('Franky')]
dogs = [Dog('Wisky'), Dog('Rommy'), Dog('Levie')]
print(type(cats[0]).count, type(dogs[0]).count)

