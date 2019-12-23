use std::convert::TryInto;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct V2 {
    pub x: isize,
    pub y: isize,
}

impl std::ops::Add for V2 {
    type Output = V2;
    fn add(self, v: V2) -> V2 {
        V2 {
            x: self.x + v.x,
            y: self.y + v.y,
        }
    }
}

impl std::ops::Sub for V2 {
    type Output = V2;
    fn sub(self, v: V2) -> V2 {
        V2 {
            x: self.x - v.x,
            y: self.y - v.y,
        }
    }
}

pub fn neighbours(V2 { x, y }: V2) -> Vec<V2> {
    vec![
        V2 { x: x - 1, y },
        V2 { x: x + 1, y },
        V2 { x, y: y - 1 },
        V2 { x, y: y + 1 },
    ]
}

pub fn char_array_iter(grid: &String) -> impl '_ + Iterator<Item = (V2, char)> {
    grid.lines().enumerate().flat_map(move |(y, l)| {
        l.chars().enumerate().map(move |(x, c)| {
            (
                V2 {
                    x: x.try_into().unwrap(),
                    y: y.try_into().unwrap(),
                },
                c,
            )
        })
    })
}
