use std::fs::File;
use std::io::prelude::Read;
use std::string::String;

fn opposite(c1: char, c2: char) -> bool {
    c1 != c2 && c1.to_ascii_uppercase() == c2.to_ascii_uppercase()
}

fn count_cancel(data: &str, ignore: Option<char>) -> i32 {
    let mut stack: Vec<char> = Vec::with_capacity(data.len());
    for c in data.chars() {
        if let Some(bad) = ignore {
            if c.to_ascii_lowercase() == bad {
                continue;
            }
        }
        if let Some(top) = stack.last().cloned() {
            if opposite(top, c) {
                stack.pop();
                continue;
            }
        }
        stack.push(c);
    }
    stack.len() as i32
}

fn main() {
    let mut file = File::open("data/5.txt").expect("Couldn't open input file");
    let mut data = String::new();
    file.read_to_string(&mut data).expect("Couldn't read input file");

    println!("Solution 1: {}", count_cancel(&data, None));

    let mut min = -1;
    for c in "abcdefghijklmnopqrstuvwxyz".chars() {
        let count = count_cancel(&data, Some(c));
        if min < 0 || count < min {
            min = count;
        }
    }
    println!("Solution 2: {}", min);
}