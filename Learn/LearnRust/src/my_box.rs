pub fn test_memory() {
    let s = "Hello World";
    let n = 111;

    println!("Stack: {:p} --> {:?}    {:p} --> {:?}", &s, s, &n, n);

    let mut a1 = Box::new(10i32);
    let a2 = Box::new(10i64);
    let a3 = Box::new(10i32);

    println!("Heap: {} {} {}", a1, a2, a3);
    println!("Heap: {:p} {:p} {:p}", &a1, &a2, &a3);

    *a1 = 20;

    println!("Heap: {} {} {}", a1, a2, a3);
    println!("Heap: {:p} {:p} {:p}", &a1, &a2, &a3);
}