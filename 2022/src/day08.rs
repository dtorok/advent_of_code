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

const LEFT: usize = 0;
const TOP: usize = 1;
const RIGHT: usize = 2;
const BOTTOM: usize = 3;

#[derive(Debug, Clone)]
struct View(Vec<usize>);

impl View {
    fn default() -> View {
        View(vec![0; 4])
    }

    fn set_max_view(&mut self, size: usize, row: usize, col: usize) {
        self.0[LEFT] = col;
        self.0[TOP] = row;
        self.0[RIGHT] = size - col - 1;
        self.0[BOTTOM] = size - row - 1;
    }

    fn score(&self) -> usize {
        self.0.iter().product()
    }
}

fn build_default_shadow_map(size: usize) -> Vec<Vec<View>> {
    let mut shadow_map: Vec<Vec<View>> = vec![vec![View::default(); size]; size];

    for row in 0..size {
        for col in 0..size {
            shadow_map[row][col].set_max_view(size, row, col);
        }
    }

    shadow_map
}

fn update_shadow_map<I: Iterator<Item=usize>>(
    grid: &Grid, 
    mut shadow_map: Vec<Vec<View>>,
    f_inner_iter: impl Fn() -> I,
    f_coord_gen: impl Fn(usize, usize) -> Coord,
    f_step_inner: impl Fn(i32) -> i32,
    direction: usize,
) -> Vec<Vec<View>> {
    let l = grid.trees.len() as i32;

    for row in 0..(l as usize) {
        for col in f_inner_iter() {
            let mut coord = f_coord_gen(row, col);
            let cur_height = grid.height_at(&coord);
            let mut col_shadow = f_step_inner(col as i32);
            let mut col_distance = 1;

            coord = f_coord_gen(row, col_shadow as usize);
            while col_shadow < l && col_shadow >= 0 && grid.height_at(&coord) <= cur_height {
                if shadow_map[coord.0][coord.1].0[direction] > col_distance {
                    shadow_map[coord.0][coord.1].0[direction] = col_distance;
                }

                col_distance += 1;
                col_shadow = f_step_inner(col_shadow);
                coord = f_coord_gen(row, col_shadow as usize);
            }
        }
    }
    shadow_map
}

fn find_highest_scenic_score(grid: &Grid) -> usize {
    let l = grid.trees.len();

    let mut shadow_map: Vec<Vec<View>> = build_default_shadow_map(l);

    shadow_map = update_shadow_map(
        grid,
        shadow_map,
        || 0..l,
        Grid::coord_row_column,
        |x| x + 1,
        LEFT,
    );

    shadow_map = update_shadow_map(
        grid,
        shadow_map,
        || 0..l,
        Grid::coord_column_row,
        |x| x + 1,
        TOP,
    );

    shadow_map = update_shadow_map(
        grid,
        shadow_map,
        || (0..l).rev(),
        Grid::coord_row_column,
        |x| x - 1,
        RIGHT,
    );

    shadow_map = update_shadow_map(
        grid,
        shadow_map,
        || (0..l).rev(),
        Grid::coord_column_row,
        |x| x - 1,
        BOTTOM,
    );

    let mut view_scores = shadow_map
        .iter()
        .flatten()
        .collect::<Vec<&View>>()
        .into_iter()
        .map(View::score)
        .collect::<Vec<usize>>();

    view_scores.sort();
    view_scores.reverse();

    view_scores[0]
}

pub fn task1(input: String) -> usize {
    let grid = Grid::parse(input);
    grid.visible_trees().len()
}

pub fn task2(input: String) -> usize {
    let grid = Grid::parse(input);
    find_highest_scenic_score(&grid)
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
        assert_eq!(8, task2(load(8, 1, Sample)));
    }

    #[test]
    fn test_02_input() {
        assert_eq!(314820, task2(load(8, 1, Input)));
    }
}
