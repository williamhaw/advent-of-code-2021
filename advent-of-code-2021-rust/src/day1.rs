#![allow(unused)]

use std::path::Path;

fn parse(line: String) -> i32 {
    line.parse::<i32>().unwrap()
}

fn count_increase(input: Vec<i32>) -> i32 {
    let mut count = 0;
    for v in input.windows(2) {
        if (v[1] > v[0]) {
            count = count + 1
        }
    }
    return count;
}

fn count_increase_three_window(input: Vec<i32>) -> i32 {
    return count_increase(input.windows(3).map(|v| v.iter().sum()).collect::<Vec<i32>>());
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::common::read_lines;
    #[test]
    fn count_increase_gets_correct_count_for_real_input() {
        let input = read_lines(Path::new("resources/day-1-input-william.txt"), parse).unwrap();
        assert_eq!(count_increase(input), 1195)
    }

    #[test]
    fn count_increase_three_window_gets_correct_count_for_real_input() {
        let input = read_lines(Path::new("resources/day-1-input-william.txt"), parse).unwrap();
        assert_eq!(count_increase_three_window(input), 1235)
    }
}
