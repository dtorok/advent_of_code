use crate::helpers::v2t;

#[derive(Clone, Copy)]
struct Range(u32, u32);

impl Range {
    fn from(data: &str) -> Range {
        let x = v2t(data
            .split("-")
            .map(&str::parse)
            .map(Result::unwrap)
            .collect()
        );

        Range(x.0, x.1)
    }

    fn contains(&self, other: &Range) -> bool {
        self.0 <= other.0 && self.1 >= other.1
    }

    fn overlaps(&self, other: &Range) -> bool {
        !(
            (self.0 > other.1) || (self.1 < other.0)
        )
    }
}

fn parse_range_tuple(data: &str) -> (Range, Range) {
    v2t(data
        .split(",")
        .map(Range::from)
        .collect()
    )   
}

fn num_range_pairs_if<F>(input: String, f: F) -> u32 
    where F: Fn((Range, Range)) -> bool
{
    input
        .split("\n")
        .map(parse_range_tuple)
        .map(f)
        .filter(|b| { *b })
        .collect::<Vec<_>>()
        .len() as u32
}

pub fn task1(input: String) -> u32 {
    num_range_pairs_if(input, 
        |(r1, r2)| {
            r1.contains(&r2) || r2.contains(&r1)
        }
    )
}

pub fn task2(input: String) -> u32 {
    num_range_pairs_if(input, 
        |(r1, r2)| {
            r1.overlaps(&r2)
        }
    )
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::helpers::test_helpers::load;
    use crate::helpers::test_helpers::InputType::{Sample, Input};

    #[test]
    fn test_01_sample() {
        assert_eq!(2, task1(load(4, 1, Sample)));
    }

    #[test]
    fn test_01_input() {
        assert_eq!(651, task1(load(4, 1, Input)));
    }

    #[test]
    fn test_02_sample() {
        assert_eq!(4, task2(load(4, 1, Sample)));
    }

    #[test]
    fn test_02_input() {
        assert_eq!(956, task2(load(4, 1, Input)));
    }

}
