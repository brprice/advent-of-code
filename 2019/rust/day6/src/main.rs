use std::collections::hash_map::HashMap;
use std::fs;
use std::iter::{empty, once, Iterator};

fn main() {
    let orbs = fs::read_to_string("../../data/day6").unwrap();
    let orbs = orbs.lines().map(|s| {
        let mut x = s.splitn(2, ')');
        (x.next().unwrap(), x.next().unwrap())
    });
    let mut tree: HashMap<&str, Vec<&str>> = HashMap::new();
    for (sun, planet) in orbs {
        let ps = tree.entry(sun).or_insert(Vec::new());
        ps.push(planet);
    }

    let mut total_orbits = 0;
    let mut level = 0;
    let mut cur: Box<dyn Iterator<Item = &str>> = Box::new(once("COM"));
    loop {
        let mut cur_count = 0;
        let mut next: Box<dyn Iterator<Item = &str>> = Box::new(empty());
        for p in cur {
            cur_count += 1;
            match tree.remove(p) {
                None => {}
                Some(children) => next = Box::new(next.chain(children.into_iter())),
            }
        }
        if cur_count == 0 {
            break;
        }
        total_orbits += level * cur_count;
        level += 1;
        cur = Box::new(next);
    }
    println!("part a: total orbits: {}", total_orbits);
}
