use std::collections::HashSet;

#[derive(PartialEq, Eq, Hash, Debug)]
struct Item(char);

impl Item {
    fn new(c: char) -> Item {
        Item(c)
    }

    fn priority(&self) -> u32 {
        let c = self.0;

        if c.is_lowercase() {
            (c as u32) - ('a' as u32) + 1
        } else if c.is_uppercase() {
            (c as u32) - ('A' as u32) + 27
        } else {
            panic!("Invalid item: {}", c)
        }
    }
}

struct Compartment {
    items: HashSet<Item>
}

impl Compartment {
    fn from(data: &str) -> Compartment {
        Compartment {
            items: HashSet::from_iter(data.chars().map(Item::new))
        }
    }
}

struct Rucksack {
    a: Compartment,
    b: Compartment
}

impl Rucksack {
    fn from(data: &str) -> Rucksack {
        let l = data.len();
        let half = l / 2;

        Rucksack {
            a: Compartment::from(&data[..half]),
            b: Compartment::from(&data[half..]),
        }
    }

    fn wrong_item(&self) -> &Item {
        self.a.items.intersection(&self.b.items).next().expect("No common item!")
    }

    fn items(&self) -> HashSet<&Item> {
        self.a.items.union(&self.b.items).collect::<HashSet<&Item>>()
    }
}

trait First<T> {
    fn first(&self) -> &T;
}

impl<T> First<T> for HashSet<T> {
    fn first(&self) -> &T {
        self.iter().next().unwrap()
    }
}

pub fn task1(input: String) -> u32 {
    let rucksacks: Vec<Rucksack> = input
        .split('\n')
        .map(Rucksack::from)
        .collect();

    let wrong_items: Vec<&Item> = rucksacks
        .iter()
        .map(Rucksack::wrong_item)
        .collect();
    
    wrong_items.into_iter().map(Item::priority).sum()
}

fn find_badge(rucksacks: &[Rucksack]) -> &Item {
    rucksacks
        .iter()
        .map(Rucksack::items)
        .reduce(|r1, r2| {
            let a = r1.intersection(&r2)
                .collect::<HashSet<&&Item>>()
                .into_iter()
                .map(|item| { *item} )
                .collect::<HashSet<&Item>>();
            a
        }).unwrap()
        .first()
}

pub fn task2(input: String) -> u32 {
    let rucksacks: Vec<Rucksack> = input
        .split('\n')
        .map(Rucksack::from)
        .collect();

    let groups = rucksacks
        .chunks(3);

    groups
        .map(|rucksacks| {
            find_badge(rucksacks).priority()
        })
        .sum()
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::helpers::test_helpers::load;
    use crate::helpers::test_helpers::InputType::{Sample, Input};

    #[test]
    fn test_01_sample() {
        assert_eq!(157, task1(load(3, 1, Sample)));
    }

    #[test]
    fn test_01_input() {
        assert_eq!(7446, task1(load(3, 1, Input)));
    }

    #[test]
    fn test_02_sample() {
        assert_eq!(70, task2(load(3, 1, Sample)));
    }

    #[test]
    fn test_02_input() {
        assert_eq!(2646, task2(load(3, 1, Input)));
    }

}