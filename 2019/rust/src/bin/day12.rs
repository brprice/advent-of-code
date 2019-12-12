use std::cmp::Ordering;
use std::fmt::Display;
use std::fs;
use std::str::FromStr;

use num::integer::lcm;

#[derive(Clone, Eq, PartialEq, Hash)]
struct V3 {
    x: i64,
    y: i64,
    z: i64,
}

fn l1(v: &V3) -> i64 {
    v.x.abs() + v.y.abs() + v.z.abs()
}

#[derive(Debug)]
enum V3ParseErr {
    NotEnoughCoords(String, String),
    BadCoord(String, String),
    NoParse(std::num::ParseIntError, String),
}

impl FromStr for V3 {
    type Err = V3ParseErr;
    // input is <x= a, y=b, z= c> where the spacing after the equals may vary
    fn from_str(s: &str) -> Result<V3, V3ParseErr> {
        let mut parts = s.trim_start_matches('<').trim_end_matches('>').split(',');
        let xs = parts
            .next()
            .ok_or(V3ParseErr::NotEnoughCoords("x".to_string(), s.to_string()))?;
        let x = {
            let mut xss = xs.split('=');
            xss.next();
            let x = xss
                .next()
                .ok_or(V3ParseErr::BadCoord("x".to_string(), xs.to_string()))?;
            x.parse()
                .map_err(|e| V3ParseErr::NoParse(e, x.to_string()))?
        };
        let ys = parts
            .next()
            .ok_or(V3ParseErr::NotEnoughCoords("y".to_string(), s.to_string()))?;
        let y = {
            let mut yss = ys.split('=');
            yss.next();
            let y = yss
                .next()
                .ok_or(V3ParseErr::BadCoord("y".to_string(), ys.to_string()))?;
            y.parse()
                .map_err(|e| V3ParseErr::NoParse(e, y.to_string()))?
        };
        let zs = parts
            .next()
            .ok_or(V3ParseErr::NotEnoughCoords("z".to_string(), s.to_string()))?;
        let z = {
            let mut zss = zs.split('=');
            zss.next();
            let z = zss
                .next()
                .ok_or(V3ParseErr::BadCoord("z".to_string(), zs.to_string()))?;
            z.parse()
                .map_err(|e| V3ParseErr::NoParse(e, z.to_string()))?
        };
        Ok(V3 { x, y, z })
    }
}

impl Display for V3 {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "<x= {},y= {},z= {}>", self.x, self.y, self.z)
    }
}

fn step(planets: &mut Vec<(V3, V3)>) {
    for i in 0..planets.len() {
        for j in 0..planets.len() {
            match planets[i].0.x.cmp(&planets[j].0.x) {
                Ordering::Less => planets[i].1.x += 1,
                Ordering::Equal => {}
                Ordering::Greater => planets[i].1.x -= 1,
            }
            match planets[i].0.y.cmp(&planets[j].0.y) {
                Ordering::Less => planets[i].1.y += 1,
                Ordering::Equal => {}
                Ordering::Greater => planets[i].1.y -= 1,
            }
            match planets[i].0.z.cmp(&planets[j].0.z) {
                Ordering::Less => planets[i].1.z += 1,
                Ordering::Equal => {}
                Ordering::Greater => planets[i].1.z -= 1,
            }
        }
    }
    for (p, v) in planets {
        p.x += v.x;
        p.y += v.y;
        p.z += v.z;
    }
}

fn total_energy(planets: &Vec<(V3, V3)>) -> i64 {
    let mut tot = 0;
    for (p, v) in planets {
        tot += l1(p) * l1(v);
    }
    tot
}

fn main() {
    let input = fs::read_to_string("../data/day12").unwrap();
    let input = input.lines().map(|s| s.parse().unwrap());
    let mut planets: Vec<_> = input.map(|p: V3| (p, V3 { x: 0, y: 0, z: 0 })).collect();
    let start = planets.clone(); // for part b
    for _ in 0..1000 {
        step(&mut planets);
    }
    println!("part a: total energy at end: {}", total_energy(&planets));

    // Part b needs a bit of thought. There are two insights required here:
    // Firstly: the process is reversable, so the first repeated state is the start state;
    // Secondly: the three dimensions are independent.
    // Thus we iterate until we have seed a repitition in each three dimensions individually,
    // and then take the lcm of those times.
    let mut planets = start.clone();
    let mut xrep: Option<u64> = None;
    let mut yrep: Option<u64> = None;
    let mut zrep: Option<u64> = None;
    let l = planets.len();
    for i in 1.. {
        step(&mut planets);
        if xrep.is_none()
            && (0..l).all(|i| planets[i].0.x == start[i].0.x && planets[i].1.x == start[i].1.x)
        {
            println!("part b: found a repeated x state after {} iterations", i);
            xrep = Some(i);
        }
        if yrep.is_none()
            && (0..l).all(|i| planets[i].0.y == start[i].0.y && planets[i].1.y == start[i].1.y)
        {
            println!("part b: found a repeated y state after {} iterations", i);
            yrep = Some(i);
        }
        if zrep.is_none()
            && (0..l).all(|i| planets[i].0.z == start[i].0.z && planets[i].1.z == start[i].1.z)
        {
            println!("part b: found a repeated z state after {} iterations", i);
            zrep = Some(i);
        }
        if xrep.is_some() && yrep.is_some() && zrep.is_some() {
            let res = lcm(lcm(xrep.unwrap(), yrep.unwrap()), zrep.unwrap());
            println!("part b: thus first rep is at iteration {}", res);
            break;
        }
    }
}
