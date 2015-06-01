package svl.learn.java.apt.client;

public class PersonBuilder{
	private java.lang.String firstname;
	private java.lang.String lastname;
	private int age;

	public PersonBuilder setFirstname(java.lang.String firstname){
		this.firstname = firstname;
		return this;
	}

	public PersonBuilder setLastname(java.lang.String lastname){
		this.lastname = lastname;
		return this;
	}

	public PersonBuilder setAge(int age){
		this.age = age;
		return this;
	}

	public Person build(){
		return new Person(firstname, lastname, age);
	}
}
