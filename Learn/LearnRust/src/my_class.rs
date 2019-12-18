mod person {
    use std::fmt;

    #[derive(Clone,Debug,PartialEq)]
    pub(crate) struct Person {
        name: String,
        age: u8,
    }

    impl fmt::Display for Person {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{} : {}", self.name, self.age)
        }
    }

    impl Person {
        pub fn create(name:&str, age:u8) -> Person {
            Person{name:name.to_owned(), age}
        }

        pub fn bump_age(&mut self) {
            self.age += 1;
        }

        pub fn age(&self) -> u8 {
            self.age
        }
    }
}

use my_class::person::Person;

pub fn test_class() {
    let mut p1 = Person::create("Vic", 22);
    println!("{}", p1);
    p1.bump_age();

    println!("{}", p1.age());

}