use std::collections::HashSet;

// ===
// Coord
// ===
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct Coord(isize, isize);

impl Coord {
    fn is_too_far_from(&self, other: &Coord) -> bool {
        (self.0 - other.0).abs() > 1 || (self.1 - other.1).abs() > 1
    }

    fn move_towards(&mut self, to: &Coord) {
        if self.is_too_far_from(to) {
            // far enough to move
            if self.0 != to.0 {
                if self.0 > to.0 {
                    self.0 -= 1
                } else {
                    self.0 += 1
                }
            }

            if self.1 != to.1 {
                if self.1 > to.1 {
                    self.1 -= 1
                } else {
                    self.1 += 1
                }
            }
        }
    }

    fn move_by(&mut self, dir: &Direction) {
        match dir {
            Direction::Right => self.1 += 1,
            Direction::Up => self.0 -= 1,
            Direction::Left => self.1 -= 1,
            Direction::Down => self.0 += 1,
        }
    }
}

// ===
// Direction
// ===
enum Direction {
    Right,
    Up,
    Left,
    Down,
}

// ===
// Movement
// ===
struct Movement(Direction, usize);

impl Movement {
    fn parse(line: &str) -> Movement {
        let parts: Vec<&str> = line.split(" ").collect();

        let amount: usize = parts[1].parse().expect("Expected to be a number");
        let dir = match parts[0] {
            "L" => Direction::Left,
            "U" => Direction::Up,
            "R" => Direction::Right,
            "D" => Direction::Down,
            _ => panic!("Not a command!"),
        };

        Movement(dir, amount)
    }
}

// ===
// Rope
// ===
struct Rope {
    knots: Vec<Coord>,
    tail_visited: HashSet<Coord>
}

impl Rope {
    fn new(num_knots: usize) -> Rope {
        let start = Coord(0, 0);

        let mut tail_visited = HashSet::new();
        tail_visited.insert(start.clone());

        Rope {
            knots: vec![start; num_knots],
            tail_visited: tail_visited,
        }
    }

    fn move_head_by(&mut self, movement: Movement) {
        let l = self.knots.len();

        for _ in 0..movement.1 {
            self.knots[0].move_by(&movement.0);
            for i in 1..l {
                let last_knot = self.knots[i - 1].clone();
                let curr_knot = &mut self.knots[i];
                curr_knot.move_towards(&last_knot);
            }

            self.tail_visited.insert(self.knots[l - 1].clone());
        }
    }

    fn move_head(&mut self, movements: impl Iterator<Item=Movement>) {
        for movement in movements {
            self.move_head_by(movement);
        }
    }
}

// ===
// Tasks
// ===
pub fn task1(input: String) -> usize {
    let mut rope = Rope::new(2);
    rope.move_head(
        input.lines().map(Movement::parse)
    );

    rope.tail_visited.len()
}

pub fn task2(input: String) -> usize {
    let mut rope = Rope::new(10);
    rope.move_head(
        input.lines().map(Movement::parse)
    );

    rope.tail_visited.len()
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::helpers::test_helpers::load;
    use crate::helpers::test_helpers::InputType::{Sample, Input};

    #[test]
    fn test_01_sample() {
        assert_eq!(13, task1(load(9, 1, Sample)));
    }

    #[test]
    fn test_01_input() {
        assert_eq!(6209, task1(load(9, 1, Input)));
    }

    #[test]
    fn test_02_sample() {
        assert_eq!(1, task2(load(9, 1, Sample)));
        assert_eq!(36, task2(load(9, 2, Sample)));
    }

    #[test]
    fn test_02_input() {
        assert_eq!(2460, task2(load(9, 1, Input)));
    }

}