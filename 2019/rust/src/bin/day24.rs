use std::collections::HashSet;
use std::convert::{TryFrom, TryInto};
use std::fs::read_to_string;

#[derive(PartialEq, Eq, std::hash::Hash, Clone)]
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
                new_cells.push(rule(c, neighs));
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

struct RecGrid {
    top: isize,       // layer number of the first grid (grows negative, which is bigger grid)
    grids: Vec<Grid>, // we ignore the centre squares
}

impl RecGrid {
    fn grid(&self, l: isize) -> Option<&Grid> {
        if l < self.top {
            None
        } else {
            let ix: usize = (l - self.top).try_into().unwrap();
            self.grids.get(ix)
        }
    }
    fn grid_dims(&self, l: isize) -> (usize, usize) {
        // if this layer does not yet exist, duplicate the size of the nearest extant one
        let ll = if l < self.top {
            self.top
        } else if l - self.top >= self.grids.len().try_into().unwrap() {
            self.top + isize::try_from(self.grids.len()).unwrap() - 1
        } else {
            l
        };
        match self.grid(ll) {
            Some(g) => (g.w, g.h),
            None => panic!("huh?"),
        }
    }
    fn get(&self, l: isize, x: usize, y: usize) -> bool {
        match self.grid(l) {
            None => false,
            Some(g) => g.get(x.try_into().unwrap(), y.try_into().unwrap()),
        }
    }
    fn is_centre(&self, l: isize, x: usize, y: usize) -> bool {
        let (w, h) = self.grid_dims(l);
        2 * x + 1 == w && 2 * y + 1 == h
    }
    fn neighbours(&self, l: isize, x: usize, y: usize) -> usize {
        let (w, h) = self.grid_dims(l);
        let (wp, hp) = self.grid_dims(l - 1);
        assert!(wp == 0 || wp % 2 == 1);
        assert!(hp == 0 || hp % 2 == 1);
        let cxp = if wp > 2 {
            //middle column in bigger layer
            wp / 2
        } else {
            1 // a hack so cxp-1 is valid as a usize
        };
        let cyp = if hp > 2 {
            //middle row in bigger layer
            hp / 2
        } else {
            1
        };
        let (wn, hn) = self.grid_dims(l + 1); // dimensions of inner layer
        let mut neighs = 0;
        // up
        if y == 0 {
            // middle 1-up-from-centre of previous layer
            if self.get(l - 1, cxp, cyp - 1) {
                neighs += 1;
            }
        } else if self.is_centre(l, x, y - 1) {
            // last row of next layer
            for x in 0..wn {
                if self.get(l + 1, x, hn - 1) {
                    neighs += 1;
                }
            }
        } else {
            if self.get(l, x, y - 1) {
                neighs += 1;
            }
        }

        // down
        if y == h - 1 {
            // middle 1-down-from-centre of previous layer
            if self.get(l - 1, cxp, cyp + 1) {
                neighs += 1;
            }
        } else if self.is_centre(l, x, y + 1) {
            // first row of next layer
            for x in 0..wn {
                if self.get(l + 1, x, 0) {
                    neighs += 1;
                }
            }
        } else {
            if self.get(l, x, y + 1) {
                neighs += 1;
            }
        }

        // left
        if x == 0 {
            // middle 1-left-of-centre of previous layer
            if self.get(l - 1, cxp - 1, cyp) {
                neighs += 1;
            }
        } else if self.is_centre(l, x - 1, y) {
            // rightmost column of next layer
            for y in 0..hn {
                if self.get(l + 1, wn - 1, y) {
                    neighs += 1;
                }
            }
        } else {
            if self.get(l, x - 1, y) {
                neighs += 1;
            }
        }

        // right
        if x == w - 1 {
            // middle 1-right-of-centre of previous layer
            if self.get(l - 1, cxp + 1, cyp) {
                neighs += 1;
            }
        } else if self.is_centre(l, x + 1, y) {
            // leftmost column of next layer
            for y in 0..hn {
                if self.get(l + 1, 0, y) {
                    neighs += 1;
                }
            }
        } else {
            if self.get(l, x + 1, y) {
                neighs += 1;
            }
        }
        neighs
    }
    fn next_rec_grid(
        &self,
        l: isize, // which layer
    ) -> Grid // new version of that layer
    {
        let (w, h) = self.grid_dims(l);
        let mut new_grid = Vec::with_capacity(h * w);
        for y in 0..h {
            for x in 0..w {
                if self.is_centre(l, x, y) {
                    new_grid.push(false); // centre square
                } else {
                    let c = self.get(l, x, y);
                    let ns = self.neighbours(l, x, y);
                    new_grid.push(rule(c, ns));
                }
            }
        }
        Grid {
            h,
            w,
            cells: new_grid,
        }
    }
    fn step(&self) -> RecGrid {
        let prev = self.top - 1;
        let new_prev = self.next_rec_grid(prev); // we will only record this if it is non-empty
        let new_top;
        let mut new_grids = Vec::new();
        if new_prev.cells.iter().any(|x| *x) {
            new_grids.push(new_prev);
            new_top = prev;
        } else {
            new_top = self.top;
        }
        let next = self.top + isize::try_from(self.grids.len()).unwrap();
        for l in self.top..next {
            new_grids.push(self.next_rec_grid(l));
        }
        let new_next = self.next_rec_grid(next); // we will only record this if it is non-empty
        if new_next.cells.iter().any(|x| *x) {
            new_grids.push(new_next);
        }
        RecGrid {
            top: new_top,
            grids: new_grids,
        }
    }
}

impl std::fmt::Display for RecGrid {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        for (i, g) in self.grids.iter().enumerate() {
            write!(fmt, "layer {}:\n{}\n\n", self.top + i as isize, g)?
        }
        Ok(())
    }
}

// the evolution rule
// c: is the current square alive
// neighs: number of alive neighbours
fn rule(c: bool, neighs: usize) -> bool {
    if c {
        neighs == 1
    } else {
        neighs == 1 || neighs == 2
    }
}

fn main() {
    let data = read_to_string("../data/day24").unwrap();
    let grid = read_grid(&data);
    let parta = parta(grid.clone()).biodiversity();
    println!("part a: {}", parta);

    let mut partb = RecGrid {
        top: 0,
        grids: vec![grid],
    };
    for _ in 0..200 {
        partb = partb.step();
    }
    let partb_bugs = partb
        .grids
        .into_iter()
        .flat_map(|g| g.cells.into_iter())
        .filter(|x| *x)
        .count();
    println!("part b: {}", partb_bugs);
}
