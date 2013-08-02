package main

import (
	"fmt"
	"svl/basic"
	"svl/classes"
)

func main() {
	fmt.Print("Hello Basic!!!...\n")
	fmt.Printf("Abs -5 is %d\n", basic.Abs(-5))
	fmt.Printf("Sum 1 to 10 is %d\n", basic.Sum(1, 11))
	stats := basic.CountChars("Hello World")
	for ch, count := range stats {
		fmt.Printf("%c -> %d\n", ch, count)
	}

	p := classes.NewPerson("Vic", "Spivak")
	fmt.Println(p)
	fmt.Println(p.GetName())

}


