use std::collections::HashSet;
use std::convert::TryInto;
use std::fs::read_to_string;

#[derive(PartialEq, Eq, std::hash::Hash)]
struct Grid {
    w: usize, // width
    h: usize, // height
    cells: Vec<bool>,
}

impl std::fmt::Display for Grid {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        for (i, c) in self.cells.iter().enumerate() {
            if i != 0 && i % self.w == 0 {
                write!(fmt, "\n")?
            }
            write!(fmt, "{}", if *c { '#' } else { '.' })?
        }
        Ok(())
    }
}

impl Grid {
    fn get(&self, x: isize, y: isize) -> bool {
        if x < 0 || y < 0 {
            return false;
        }
        let y: usize = y.try_into().unwrap();
        let x: usize = x.try_into().unwrap();
        if x >= self.w || y >= self.h {
            false
        } else {
            self.cells[y * self.w + x]
        }
    }

    fn step(&self) -> Grid {
        let mut new_cells = Vec::with_capacity(self.cells.len());
        for y in 0..self.h {
            for x in 0..self.w {
                let y: isize = y.try_into().unwrap();
                let x: isize = x.try_into().unwrap();
                let ns = [
                    self.get(x - 1, y),
                    self.get(x + 1, y),
                    self.get(x, y - 1),
                    self.get(x, y + 1),
                ];
                let c = self.get(x, y);
                let neighs = ns.into_iter().filter(|&b| *b).count();
                let c_new = if c {
                    neighs == 1
                } else {
                    neighs == 1 || neighs == 2
                };
                new_cells.push(c_new);
            }
        }
        Grid {
            w: self.w,
            h: self.h,
            cells: new_cells,
        }
    }

    fn biodiversity(&self) -> usize {
        let mut b = 0;
        let mut s = 1;
        for c in &self.cells {
            if *c {
                b += s
            };
            s *= 2;
        }
        b
    }
}

fn read_grid(s: &str) -> Grid {
    let ls = s.lines();
    let mut cells = Vec::new();
    let mut h = 0;
    let mut w = None;
    for l in ls {
        h += 1;
        let mut this_w = 0;
        for c in l.chars() {
            this_w += 1;
            match c {
                '#' => cells.push(true),
                '.' => cells.push(false),
                _ => panic!("unexpected char: {}", c),
            }
        }
        match w {
            None => w = Some(this_w),
            Some(ww) => {
                if ww != this_w {
                    panic!("non-square grid")
                }
            }
        }
    }
    match w {
        None => panic!("no lines"),
        Some(ww) => Grid { w: ww, h, cells },
    }
}

fn parta(mut grid: Grid) -> Grid {
    let mut seen = HashSet::new();
    loop {
        if seen.contains(&grid) {
            return grid;
        }
        let grid_new = grid.step();
        seen.insert(grid);
        grid = grid_new;
    }
}

fn main() {
    let data = read_to_string("../data/day24").unwrap();
    let grid = read_grid(&data);
    let parta = parta(grid).biodiversity();
    println!("part a: {}", parta);
}
