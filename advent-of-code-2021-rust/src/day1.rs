#![allow(unused)]

use std::path::Path;

fn parse(line: String) -> i32 {
    line.parse::<i32>().unwrap()
}

fn count_increase(input: &Vec<i32>) -> i32 {
    let mut count = 0;
    for v in [&input].windows(2) {
        if (v[1] > v[0]) {
            count = count + 1
        }
    }
    return count;
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::common::read_lines;
    #[test]
    fn count_increase_gets_correct_count_for_real_input() {
        let input = read_lines(Path::new("resources/day-1-input-william.txt"), parse);
        assert_eq!(true, true)
    }
}
