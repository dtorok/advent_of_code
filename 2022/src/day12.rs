use std::collections::{HashMap, HashSet, VecDeque};

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
struct Coord {
    row: i32,
    col: i32
}

#[derive(Debug, Hash)]
struct Position {
    coord: Coord,
    height: usize,
    start: bool,
    end: bool,
}

impl Position {
    fn possible_next_coords(&self, size: &Coord) -> Vec<Coord>
    {
        let c = &self.coord;
        let mut coords: Vec<Coord> = vec![];

        if self.coord.row > 0 {
            coords.push(Coord{ row: c.row - 1, col: c.col });
        }

        if self.coord.row < size.row - 1 {
            coords.push(Coord{ row: c.row + 1, col: c.col });
        }

        if self.coord.col > 0 {
            coords.push(Coord{ row: c.row, col: c.col - 1 });
        }

        if self.coord.col < size.col - 1 {
            coords.push(Coord{ row: c.row, col: c.col + 1 });
        }

        coords
    }
}

impl Position {
    fn parse_at(input: char, coord: Coord) -> Position {
        let (input, start, end) = match input {
            'S' => ('a', true, false),
            'E' => ('z', false, true),
            _ => (input, false, false),
        };

        Position {
            coord,
            height: (input as u8 - 'a' as u8) as usize,
            start,
            end
        }
    }
}

struct Path<'a> {
    current_position: &'a Position,
    number_of_steps: usize,
}

impl<'a> Path<'a> {
    fn new(current_position: &'a Position) -> Path<'a> {
        Path {
            current_position,
            number_of_steps: 0
        }
    }
}

#[derive(Debug)]
struct Map {
    positions: HashMap<Coord, Position>,
    size: Coord,
}

impl Map {
    fn parse(input: String) -> Map {
        let mut positions: HashMap<Coord, Position> = HashMap::new();
        let mut coord = Coord{ row: 0, col: 0 };

        for line in input.trim().split("\n") {
            coord.col = 0;
            for ch in line.chars() {
                positions.insert(
                    coord.clone(),
                    Position::parse_at(ch, coord.clone())
                );

                coord.col += 1;
            }
            coord.row += 1;
        }
        Map { positions, size: coord }
    }

    fn get_position(&self, cond: impl Fn(&&Position) -> bool) -> &Position {
        self.positions
            .values()
            .filter(cond)
            .next()
            .expect("Start not found")
    }

    fn get_start(&self) -> &Position {
        self.get_position(|p| p.start)
    }

    fn get_end(&self) -> &Position {
        self.get_position(|p| p.end)
    }

    fn find_shortest_path<'a>(
        &'a self,
        start: &'a Position,
        fn_step_condition: impl Fn(&Position, &Position) -> bool,
        fn_end_condition: impl Fn(&Path) -> bool,
    ) -> Option<Path<'a>> {
        let mut visited_positions: HashSet<&Coord> = HashSet::new();
        let mut next_paths: VecDeque<Path> = VecDeque::new();

        next_paths.push_back(Path::new(start));

        while let Some(path) = next_paths.pop_front() {
            if visited_positions.contains(&path.current_position.coord) {
                continue;
            }
            visited_positions.insert(&path.current_position.coord);

            if fn_end_condition(&path) {
                return Some(path);
            }

            for coord in path.current_position.possible_next_coords(&self.size) {
                let pos = self.positions.get(&coord).unwrap();
                if fn_step_condition(path.current_position, pos) {
                    next_paths.push_back(Path {
                        current_position: pos,
                        number_of_steps: path.number_of_steps + 1,
                    });
                }
            }
        }

        None
    }
}

pub fn task1(input: String) -> usize {
    let map = Map::parse(input);
    let path = map.find_shortest_path(
        map.get_start(),
        |curr, next| { next.height <= curr.height + 1 },
        |path| { path.current_position.end },
    );
    path.expect("No path found").number_of_steps
}

pub fn task2(input: String) -> usize {
    let map = Map::parse(input);
    let path = map.find_shortest_path(
        map.get_end(),
        |curr, next| { next.height >= curr.height - 1 },
        |path| { path.current_position.height == 0 },
    );
    path.expect("No path found").number_of_steps
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::helpers::test_helpers::load;
    use crate::helpers::test_helpers::InputType::{Sample, Input};

    #[test]
    fn test_01_sample() {
        assert_eq!(31, task1(load(12, 1, Sample)));
    }

    #[test]
    fn test_01_input() {
        assert_eq!(391, task1(load(12, 1, Input)));
    }

    #[test]
    fn test_02_sample() {
        assert_eq!(29, task2(load(12, 1, Sample)));
    }

    #[test]
    fn test_02_input() {
        assert_eq!(386, task2(load(12, 1, Input)));
    }

}