#![allow(unused)]

use std::fs::File;
use std::io::{self, BufRead, Error};
use std::path::Path;

pub fn read_lines<T>(file_path: &Path, parse_line: fn(String) -> T) -> Result<Vec<T>, Error> {
    return File::open(file_path).map(|f| {
        io::BufReader::new(f)
            .lines()
            .map(|l| parse_line(l.unwrap()))
            .collect()
    });
}

#[cfg(test)]
mod common_test {
    use super::*;
    #[test]
    fn read_works() {
        assert_eq!(
            read_lines(Path::new("resources/test-input.txt"), |l| l
                .parse::<i32>()
                .unwrap())
            .unwrap(),
            Vec::from([5535, 2202])
        )
    }
}
