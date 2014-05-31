class Person:
    def __init__(self, firstName, lastName, age, ssn):
        self.firstName = firstName
        self.lastName = lastName
        self.age = age
        self.ssn = ssn

    def firstName(self):
        return self.firstName

    def lastName(self):
        return self.lastName

    def name(self):
        return '{} {}'.format(self.firstName, self.lastName)

p1 = Person("Vic", "Spivak", 33, "111-11-1111")

print(p1.name())


