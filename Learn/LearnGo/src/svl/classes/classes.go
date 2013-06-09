package classes

type Person struct {
	firstName string
	lastName string
}

func NewPerson(first, last string) Person {
	p := new(Person)
	p.firstName = first
	p.lastName = last

	return *p
}

func (p Person) GetName() string {
	return p.firstName + " " + p.lastName
}
