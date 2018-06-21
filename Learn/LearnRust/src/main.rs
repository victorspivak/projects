mod simple;
mod my_utils;
mod structures;
mod my_box;

use simple::*;
use structures::*;
use my_box::*;

fn main() {
    simple_match();
    primitives();
    literals();
    tuples();
    slices();
    simple_structures();
    test_memory();
}

