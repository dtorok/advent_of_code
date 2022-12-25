use std::{collections::HashSet, ops::Range, iter::Rev};

#[derive(Debug)]
struct Grid {
    trees: Vec<Vec<usize>>,
}

#[derive(Debug, Hash, PartialEq, Eq)]
struct Coord(usize, usize);

impl Grid {
    fn parse(input: String) -> Grid {
        let mut trees: Vec<Vec<usize>> = vec![];
        for line in input.lines() {
            trees.push(
                line.chars()
                    .map(|c| (c as u8) - ('0' as u8))
                    .map(|h| h as usize)
                    .collect::<Vec<usize>>()
            );
        }
        Grid {
            trees: trees
        }
    }

    fn height_at(&self, coord: &Coord) -> usize {
        self.trees[coord.0][coord.1]
    }

    fn visible_trees_from<I: Iterator<Item=usize>, J: Iterator<Item=usize>>(
        &self, 
        visible_trees: &mut HashSet<Coord>, 
        f_it_outer: impl Fn() -> I,
        f_it_inner: impl Fn() -> J,
        f_coord: impl Fn(usize, usize) -> Coord,
    )
    {
        let mut max_height: i32;

        // grid is rectangular
        for row in f_it_outer() {
            max_height = -1;

            for col in f_it_inner() {
                let coord = f_coord(row, col);
                let height = self.height_at(&coord) as i32;

                if height > max_height {
                    visible_trees.insert(coord);
                    max_height = height;
                }
            }
        }
    }

    fn coord_row_column(row: usize, column: usize) -> Coord {
        Coord(row, column)
    }

    fn coord_column_row(column: usize, row: usize) -> Coord {
        Coord(row, column)
    }

    fn visible_trees(&self) -> HashSet<Coord> {
        let mut visible_trees = HashSet::<Coord>::new();
        let l = self.trees.len();

        // from left
        self.visible_trees_from(
            &mut visible_trees,
            || { 0..l },
            || { 0..l },
            Self::coord_row_column
        );

        // from right
        self.visible_trees_from(
            &mut visible_trees,
            || { 0..l },
            || { (0..l).rev() },
            Self::coord_row_column
        );

        // from top
        self.visible_trees_from(
            &mut visible_trees,
            || { 0..l },
            || { 0..l },
            Self::coord_column_row
        );

        // from bottom
        self.visible_trees_from(
            &mut visible_trees,
            || { 0..l },
            || { (0..l).rev() },
            Self::coord_column_row
        );

        visible_trees
    }
}

pub fn task1(input: String) -> usize {
    let grid = Grid::parse(input);
    grid.visible_trees().len()
}

pub fn task2(_: String) -> u32 {
    todo!()
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::helpers::test_helpers::load;
    use crate::helpers::test_helpers::InputType::{Sample, Input};

    #[test]
    fn test_01_sample() {
        assert_eq!(21, task1(load(8, 1, Sample)));
    }

    #[test]
    fn test_01_input() {
        assert_eq!(1789, task1(load(8, 1, Input)));
    }

    #[test]
    fn test_02_sample() {
        assert_eq!(0, task2(load(8, 1, Sample)));
    }

    #[test]
    fn test_02_input() {
        assert_eq!(0, task2(load(8, 1, Input)));
    }
}
