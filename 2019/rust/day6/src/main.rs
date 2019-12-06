use std::collections::hash_map::HashMap;
use std::collections::hash_set::HashSet;
use std::fs;
use std::hash::Hash;
use std::iter::{empty, once, Iterator};

fn main() {
    let orbs = fs::read_to_string("../../data/day6").unwrap();
    let orbs = orbs.lines().map(|s| {
        let mut x = s.splitn(2, ')');
        (x.next().unwrap(), x.next().unwrap())
    });
    let mut tree: HashMap<&str, Vec<&str>> = HashMap::new();
    // the tree, but with edges pointing towards the root (for part b)
    let mut eert: HashMap<&str, &str> = HashMap::new();
    for (sun, planet) in orbs {
        let ps = tree.entry(sun).or_insert(Vec::new());
        ps.push(planet);

        eert.insert(planet, sun);
    }
    let eert = eert; // set non-mutable

    // part a
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

    // part b
    let com = {
        let mut tmp = HashSet::new();
        tmp.insert("COM");
        tmp
    };
    let santa_path = path_to_set(&eert, &com, "SAN").unwrap();
    let you_path = path_to_set(&eert, &santa_path.iter().map(|x| *x).collect(), "YOU").unwrap();
    let common_ancestor = you_path.last().unwrap();
    let santa_path = santa_path.split(|p| p == common_ancestor).next().unwrap(); // NB: doesn't contain common_ancestor
    let transfers_needed = you_path.len() - 2 + santa_path.len() - 1;
    println!("part b: transfers needed: {}", transfers_needed);
}

fn path_to_set<'a, T: Eq + Hash + ?Sized>(
    tree: &HashMap<&'a T, &'a T>, // upward edges
    set: &HashSet<&'a T>,
    node: &'a T,
) -> Option<Vec<&'a T>> {
    let mut cur = node;
    let mut path = vec![cur];
    while !set.contains(cur) {
        cur = tree[cur];
        path.push(cur);
    }
    Some(path)
}
