enum Result {
    Wins,
    Loses,
    Draw
}

impl Result {
    fn from(input: &str) -> Result {
        match input {
            "X" => Result::Loses,
            "Y" => Result::Draw,
            "Z" => Result::Wins,
            x   => panic!("Invalid result: {}", x),
        }
    }
    fn score(&self) -> u32 {
        match self {
            Result::Loses => 0,
            Result::Draw  => 3,
            Result::Wins  => 6,
        }
    }
    fn opposite(&self) -> Result {
        match *self {
            Result::Loses => Result::Wins,
            Result::Draw  => Result::Draw,
            Result::Wins  => Result::Loses,
        }
    }
}
#[derive(Clone, Copy, PartialEq)]
#[repr(u8)]
enum Choice {
    Rock = 0,
    Paper,
    Scissors
}

impl Choice {
    fn wins_against(&self) -> Choice {
        match *self {
            Choice::Rock => Choice::Scissors,
            Choice::Scissors => Choice::Paper,
            Choice::Paper => Choice::Rock,
        }
    }

    fn loses_against(&self) -> Choice {
        match *self {
            Choice::Scissors => Choice::Rock,
            Choice::Paper => Choice::Scissors,
            Choice::Rock => Choice::Paper,
        }
    }

    fn draw_against(&self) -> Choice {
        *self
    }

    fn from(c: &str) -> Choice {
        match c {
            "A" | "X" => Choice::Rock,
            "B" | "Y" => Choice::Paper,
            "C" | "Z" => Choice::Scissors,
            x => panic!("Invalid choice: {}", x),
        }
    }
    fn against(self, other: Choice) -> Result {
        if self.loses_against() == other {
            Result::Loses
        } else if self.wins_against() == other {
            Result::Wins
        } else {
            Result::Draw
        }
        // let s = self as u8;
        // let o = other as u8;

        // if s == o {
        //     Result::Draw
        // } else if (s + 1) % 3 == o {
        //     Result::Loses
        // } else {
        //     Result::Wins
        // }
    }

    // 
    fn other_if_my_result(self, result: &Result) -> Choice {
        match result {
            Result::Loses => self.loses_against(),
            Result::Draw => self.draw_against(),
            Result::Wins => self.wins_against()
        }
    }

    fn score(self) -> u32 {
        match self {
            Choice::Rock => 1,
            Choice::Paper => 2,
            Choice::Scissors => 3,
        }
    }
}

pub fn task1(input: String) -> u32 {
    input
        .split('\n')
        .map(|l| {
            let (op, me) = v2t(l
                .split(" ")
                .map(Choice::from)
                .collect());
                
            me.score() + me.against(op).score()
        })
        .sum()
}

pub fn task2(input: String) -> u32 {
    input
        .split('\n')
        .map(|l| {
            let (op, res) = v2t(l.split(' ').collect());
            let (op, res) = (Choice::from(op), Result::from(res));
            
            op.other_if_my_result(&(res.opposite())).score() + res.score()
        }).sum()
}


fn v2t<T: Copy>(from: Vec<T>) -> (T, T) {
    (from[0], from[1])
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::helpers::test_helpers::load;
    use crate::helpers::test_helpers::InputType::{Sample, Input};

    #[test]
    fn test_01_sample() {
        assert_eq!(15, task1(load(2, 1, Sample)));
    }

    #[test]
    fn test_01_input() {
        assert_eq!(10595, task1(load(2, 1, Input)));
    }

    #[test]
    fn test_02_sample() {
        assert_eq!(12, task2(load(2, 1, Sample)));
    }

    #[test]
    fn test_02_input() {
        assert_eq!(9541, task2(load(2, 1, Input)));
    }

}