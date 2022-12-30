use std::{cmp::{max, min}, fmt::Display};

use num::abs;

#[derive(Debug, Clone)]
struct Coord {
    row: i32,
    col: i32
}

impl Coord {
    fn new(row: i32, col: i32) -> Coord {
        Coord { row, col }
    }

    fn parse(input: &str) -> Coord {
        let parts: Vec<&str> = input.split(",").collect();
        Coord::new(
            parts[1].parse().unwrap(),
            parts[0].parse().unwrap(),
        )
    }
}

#[derive(Debug)]
struct Path {
    coords: Vec<Coord>,
    #[allow(dead_code)]
    bb_min: Coord,
    #[allow(dead_code)]
    bb_max: Coord,
}

impl Path {
    fn parse(input: &str) -> Path {
        let mut coords: Vec<Coord> = vec![];
        let mut bb_min = Coord::new(0, 500);
        let mut bb_max = Coord::new(0, 500);

        for coord in input.split(" -> ").map(Coord::parse) {
            bb_min.row = min(bb_min.row, coord.row);
            bb_min.col = min(bb_min.col, coord.col);
            bb_max.row = max(bb_max.row, coord.row);
            bb_max.col = max(bb_max.col, coord.col);

            coords.push(coord);
        }

        Path { coords, bb_min, bb_max }
    }
}

#[derive(Clone)]
enum MapPoint {
    Air,
    Rock,
    Sand
}

impl Display for MapPoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}",
            match &self {
                Self::Air => " ",
                Self::Rock => "x",
                Self::Sand => "."
            }
        )
    }
}

impl MapPoint {
    fn is_air(&self) -> bool {
        match &self {
            MapPoint::Air => true,
            _ => false
        }
    }

    fn is_sand(&self) -> bool {
        match &self {
            MapPoint::Sand => true,
            _ => false
        }
    }
}

#[derive(Debug)]
enum CoordError {
    OutOfBounds(Coord),
    HitSolid,
}

struct Map {
    grid: Vec<Vec<MapPoint>>,
    bb_min: Coord,
    bb_max: Coord,
}

impl Map {
    fn from(paths: Vec<Path>) -> Result<Map, CoordError> {
        let (bb_min, bb_max) = Self::get_bb(&paths);
        let mut map = Map {
            grid: Self::create_grid(&bb_min, &bb_max),
            bb_min,
            bb_max,
        };

        let mut last_coord: Option<Coord> = None;

        for path in paths {
            last_coord = None;

            for coord in path.coords {
                if let Some(lc) = last_coord {
                    map.draw_line(&lc, &coord)?;
                }

                last_coord = Some(coord);
            }
        }

        Ok(map)
    }

    fn draw_line(&mut self, from: &Coord, to: &Coord) -> Result<(), CoordError> {
        let d_row = to.row - from.row;
        let d_col = to.col - from.col;
        let d = max(abs(d_row), abs(d_col));

        let dt_row: f32 = (d_row as f32) / (d as f32);
        let dt_col: f32 = (d_col as f32) / (d as f32);

        self.set(from, MapPoint::Rock)?;

        for i in 0..d {
            let row = ((from.row as f32) + dt_row * (i as f32)) as i32;
            let col = ((from.col as f32) + dt_col * (i as f32)) as i32;
            self.set(&Coord::new(row, col), MapPoint::Rock)?;
        }

        self.set(to, MapPoint::Rock)?;

        Ok(())
    }

    fn coord2indices(&self, coord: &Coord) -> Result<(usize, usize), CoordError> {
        if coord.row < self.bb_min.row
            || coord.col < self.bb_min.col
            || coord.row > self.bb_max.row
            || coord.col > self.bb_max.col
        {
            Err(CoordError::OutOfBounds(coord.clone()))
        } else {
            Ok((
                (coord.row - self.bb_min.row) as usize,
                (coord.col - self.bb_min.col) as usize
            ))
        }
    }

    fn set(&mut self, coord: &Coord, mp: MapPoint) -> Result<(), CoordError>{
        let (row, col) = self.coord2indices(coord)?;
        self.grid[row][col] = mp;
        Ok(())
    }

    fn at(&self, coord: &Coord) -> Result<&MapPoint, CoordError> {
        let (row, col) = self.coord2indices(coord)?;
        Ok(&self.grid[row][col])
    }

    fn create_grid(bb_min: &Coord, bb_max: &Coord) -> Vec<Vec<MapPoint>> {
        let rows = (bb_max.row - bb_min.row + 1) as usize;
        let cols = (bb_max.col - bb_min.col + 1) as usize;

        vec![vec![MapPoint::Air; cols]; rows]
    }

    fn get_bb(paths: &Vec<Path>) -> (Coord, Coord) {
        let mut bb_min = Coord::new(0, 500);
        let mut bb_max = Coord::new(0, 500);

        for path in paths {
            for coord in path.coords.iter() {
                bb_min.row = min(bb_min.row, coord.row);
                bb_min.col = min(bb_min.col, coord.col);
                bb_max.row = max(bb_max.row, coord.row);
                bb_max.col = max(bb_max.col, coord.col);
            }
        }

        (bb_min, bb_max)
    }

    /// Adds and moves a unit of sand
    fn move_sand(&mut self, coord: &Coord) -> Result<(), CoordError> {
        self.set(coord, MapPoint::Sand)?;

        let mut coord = Ok(coord.clone());

        while let Ok(c) = coord {
            coord = self.step_sand(&c);
        }

        match coord {
            Ok(_) => Ok(()),
            Err(CoordError::HitSolid) => Ok(()),
            Err(err @ CoordError::OutOfBounds(_)) => Err(err),
        }
    }

    fn step_sand(&mut self, c: &Coord) -> Result<Coord, CoordError> {
        let mp = self.at(c)?;

        if !mp.is_sand() {
            panic!("It's not sand at {:?}", c)
        }

        match self.step_sand_if_air(c, Coord::new(c.row + 1, c.col)) {
            new_coord @ Ok(_) => return new_coord,
            err@ Err(CoordError::OutOfBounds(_)) => return err,
            Err(CoordError::HitSolid) => (), // try further
        }

        match self.step_sand_if_air(c, Coord::new(c.row + 1, c.col - 1)) {
            new_coord @ Ok(_) => return new_coord,
            err@ Err(CoordError::OutOfBounds(_)) => return err,
            Err(CoordError::HitSolid) => (), // try further
        }

        match self.step_sand_if_air(c, Coord::new(c.row + 1, c.col + 1)) {
            new_coord @ Ok(_) => return new_coord,
            err@ Err(CoordError::OutOfBounds(_)) => return err,
            err @ Err(CoordError::HitSolid) => return err,
        }

    }

    fn step_sand_if_air(&mut self, coord: &Coord, new_coord: Coord) -> Result<Coord, CoordError> {
        if self.at(&new_coord)?.is_air() {
            self.set(coord, MapPoint::Air)?;
            self.set(&new_coord, MapPoint::Sand)?;

            Ok(new_coord)
        } else {
            Err(CoordError::HitSolid)
        }
    }
}

impl Display for Map {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for row in self.grid.iter() {
            for mp in row {
                write!(f, "{}", mp)?
            }

            write!(f, "\n")?
        }

        Ok(())
    }
}

pub fn task1(input: String) -> usize {
    let mut map = Map::from(input.lines().map(Path::parse).collect::<Vec<Path>>())
        .expect("creation failed");

    println!("Bdefore:\n{}", &map);

    let mut cnt = 0;
    let mut res: Result<(), CoordError> = Ok(());
    while res.is_ok() {
        cnt += 1;
        res = map.move_sand(&Coord::new(0, 500));
    }

    println!("After:\n{}", &map);

    cnt - 1
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
        assert_eq!(24, task1(load(14, 1, Sample)));
    }

    #[test]
    fn test_01_input() {
        assert_eq!(0, task1(load(14, 1, Input)));
    }

    #[test]
    fn test_02_sample() {
        assert_eq!(0, task2(load(14, 1, Sample)));
    }

    #[test]
    fn test_02_input() {
        assert_eq!(0, task2(load(14, 1, Input)));
    }

}