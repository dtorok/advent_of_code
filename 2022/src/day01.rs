pub fn task1(input: String) -> u32 {
    top_n_elf_sum_calories(input, 1)
}

pub fn task2(input: String) -> u32 {
    top_n_elf_sum_calories(input, 3)
}

fn top_n_elf_sum_calories(input: String, n: usize) -> u32 {
    let mut calories = input
            .split("\n").into_iter()
            .map(str::trim).collect::<Vec<&str>>().as_slice()
            .split(|&x| x == "")
            .map(|x| x.into_iter()
                .map(|l| l.parse::<u32>().unwrap()))
            .map(Iterator::sum).collect::<Vec<_>>();
    calories.sort();
    calories.reverse();
    calories[..n].into_iter().sum()
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::helpers::test_helpers::load;
    use crate::helpers::test_helpers::InputType::{Sample, Input};

    #[test]
    fn test_01_sample() {
        assert_eq!(24000, task1(load(1, 1, Sample)));
    }
    
    #[test]
    fn test_01_input() {
        assert_eq!(71506, task1(load(1, 1, Input)));
    }

    #[test]
    fn test_02_sample() {
        assert_eq!(45000, task2(load(1, 1, Sample)));
    }

    #[test]
    fn test_02_input() {
        assert_eq!(209603, task2(load(1, 1, Input)));
    }
}
