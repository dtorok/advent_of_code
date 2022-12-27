use std::{collections::VecDeque, vec};

type Item = usize;

struct Throw {
    to_monkey: usize,
    item: Item
}

struct MonkeyNum(usize);

type MonkeyAction = Box<dyn Fn(Item) -> MonkeyNum>;

enum MonkeyOperation {
    Add(usize),
    Mul(usize),
    Pow(usize),
}

impl MonkeyOperation {
    fn parse(value: &str) -> MonkeyOperation {
        let parts = value.split(" ").collect::<Vec<&str>>();
        let p = |v: &str| v.parse::<usize>().expect("op_value should be a number");

        match (parts[0], parts[1]) {
            ("+", "old") => MonkeyOperation::Mul(2),
            ("*", "old") => MonkeyOperation::Pow(2),
            ("+", value) => MonkeyOperation::Add(p(value)),
            ("*", value) => MonkeyOperation::Mul(p(value)),
            _ => panic!("Unexpected MonkeyOperation")
        }
    }

    fn execute(&self, input: usize) -> usize {
        match self {
            MonkeyOperation::Add(value) => input + value,
            MonkeyOperation::Mul(value) => input * value,
            MonkeyOperation::Pow(value) => input.pow(*value as u32),
        }
    }
}

struct Monkey
{
    items: VecDeque<Item>,
    operation: MonkeyOperation,
    action: MonkeyAction,
    num_obs: usize
}

impl Monkey
{
    fn test_start_with<'a>(line: &'a str, prefix: &str) -> Option<&'a str> {
        if line.starts_with(prefix) {
            Some(&line[prefix.len()..])
        } else {
            None
        }
    }
    fn parse_next<'a, I>(lines: &mut I) -> Option<Monkey>
    where
        I: Iterator<Item=&'a str>
    {
        let mut starting_items: Vec<Item> = vec![];
        let mut divisible_by = 0;
        let mut monkey_operation = MonkeyOperation::Add(0);
        let mut if_true_monkey = 0;
        let mut if_false_monkey = 0;

        while let Some(line) = lines.next() {
            let line = line.trim();

            if line == "" {
                break;
            } else if let Some(value) = Self::test_start_with(line, "Starting items: ") {
                starting_items = value
                    .split(", ")
                    .map(&str::parse::<Item>)
                    .map(Result::unwrap)
                    .collect();
            } else if let Some(value) = Self::test_start_with(line, "Operation: new = old ") {
                monkey_operation = MonkeyOperation::parse(value);
            } else if let Some(value) = Self::test_start_with(line, "Test: divisible by ") {
                divisible_by = value.parse::<usize>().expect("divisible_by should be a number");
            } else if let Some(value) = Self::test_start_with(line, "If true: throw to monkey ") {
                if_true_monkey = value.parse::<usize>().expect("if_true_monkey should be a number");
            } else if let Some(value) = Self::test_start_with(line, "If false: throw to monkey ") {
                if_false_monkey = value.parse::<usize>().expect("if_false_monkey should be a number");
            }
        }

        if divisible_by == 0 {
            // assuming this means we couldn't read any monkey record
            return None
        }

        let action = move |item: Item| {
            if item % divisible_by == 0 {
                MonkeyNum(if_true_monkey)
            } else {
                MonkeyNum(if_false_monkey)
            }
        };

        Some(Monkey {
            items: VecDeque::from(starting_items),
            operation: monkey_operation,
            action: Box::new(action),
            num_obs: 0
        })
    }

    fn round(&mut self) -> Vec<Throw> {
        let mut throws = vec![];

        while let Some(item) = self.items.pop_front() {
            // observation
            let mut item = self.operation.execute(item);
            self.num_obs += 1;

            // bored
            item = item / 3;

            // test
            let to_monkey = (self.action)(item);

            throws.push(Throw{
                to_monkey: to_monkey.0,
                item: item,
            });
        }
        throws
    }

    fn add_item(&mut self, item: Item) {
        self.items.push_back(item);
    }

}


struct Tribe
{
    monkeys: Vec<Monkey>,
}

impl Tribe
{
    fn parse(input: String) -> Tribe {
        let mut lines = input.lines();

        let mut monkeys = vec![];
        while let Some(monkey) = Monkey::parse_next(&mut lines) {
            monkeys.push(monkey);
        }

        Tribe {
            monkeys
        }
    }

    fn round(&mut self) {
        // for monkey in self.monkeys.iter_mut() {
        for i in 0..self.monkeys.len() {
            let monkey = &mut self.monkeys[i];
            let throws = monkey.round();

            for throw in throws {
                self.monkeys[throw.to_monkey].add_item(throw.item);
            }
        }
    }
}

pub fn task1(input: String) -> usize {
    let mut tribe = Tribe::parse(input);

    for _ in 0..20 {
        tribe.round();
    }

    let mut num_obs: Vec<usize> = tribe.monkeys.iter().map(|m| m.num_obs).collect();
    num_obs.sort();
    num_obs.reverse();

    num_obs[0] * num_obs[1]
}

pub fn task2(_: String) -> usize {
    todo!()
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::helpers::test_helpers::load;
    use crate::helpers::test_helpers::InputType::{Sample, Input};

    #[test]
    fn test_01_sample() {
        assert_eq!(10605, task1(load(11, 1, Sample)));
    }

    #[test]
    fn test_01_input() {
        assert_eq!(67830, task1(load(11, 1, Input)));
    }

    #[test]
    fn test_02_sample() {
        assert_eq!(0, task2(load(11, 1, Sample)));
    }

    #[test]
    fn test_02_input() {
        assert_eq!(0, task2(load(11, 1, Input)));
    }

}