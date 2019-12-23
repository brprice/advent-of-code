use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::fs;
use std::hash::Hash;

use aoc2019::parse_array::{char_array_iter, neighbours, V2};

// for edge weights all 1
fn dijkstra<T: Copy + Hash + Ord>(neighbours: HashMap<T, Vec<T>>, start: T) -> HashMap<T, usize> {
    let mut dists = HashMap::new();
    let mut seen = HashSet::new();
    let mut todo = BinaryHeap::new();
    let mut push = |d, x| todo.push((Reverse(d), x));
    push(0, start);

    while let Some((Reverse(d), n)) = todo.pop() {
        if seen.contains(&n) {
            continue;
        }

        dists.insert(n, d);
        seen.insert(n);
        for &m in neighbours[&n].iter() {
            todo.push((Reverse(d + 1), m));
        }
    }

    dists
}

fn parse_maze(maze: String) -> (HashMap<V2, Vec<V2>>, V2, V2) {
    let mut corridor = HashSet::new();
    let mut portal_chars = HashMap::new(); // map (x,y) -> portal letter

    for (p, c) in char_array_iter(&maze) {
        match c {
            '.' => {
                corridor.insert(p);
            }
            '#' => {}
            ' ' => {}
            c => {
                portal_chars.insert(p, c);
            }
        }
    }

    let mut neighs = HashMap::new();
    for &c in corridor.iter() {
        let ns = neighbours(c)
            .into_iter()
            .filter(|n| corridor.contains(n))
            .collect();
        neighs.insert(c, ns);
    }

    let mut portals = HashMap::new(); // map portal name -> vector of locations
    for (&p, c) in &portal_chars {
        // first letter of label
        for dp in vec![V2 { x: 1, y: 0 }, V2 { x: 0, y: 1 }] {
            // second letter of label (right or down)
            match portal_chars.get(&(p + dp)) {
                None => {}
                Some(&d) => {
                    let mut l = String::with_capacity(2);
                    l.push(*c);
                    l.push(d);
                    let ents = [(p + dp + dp), (p - dp)];
                    for entrance in &ents {
                        if corridor.contains(&entrance) {
                            let port = portals.entry(l.clone()).or_insert(Vec::new());
                            port.push(entrance.clone());
                        }
                    }
                }
            }
        }
    }
    let start = portals["AA"][0];
    let end = portals["ZZ"][0];

    for (p_name, pls) in &portals {
        // sanity check
        if p_name == "AA" || p_name == "ZZ" {
            assert_eq!(pls.len(), 1);
            continue;
        }
        assert_eq!(pls.len(), 2);

        // add portals to neighbours
        neighs.entry(pls[0]).or_insert(Vec::new()).push(pls[1]);
        neighs.entry(pls[1]).or_insert(Vec::new()).push(pls[0]);
    }

    (neighs, start, end)
}

fn main() {
    let maze = fs::read_to_string("../data/day20").unwrap();
    let (neighbours, entrance, exit) = parse_maze(maze);
    let dists = dijkstra(neighbours, entrance);
    println!("part a: {}", dists[&exit]);
}
