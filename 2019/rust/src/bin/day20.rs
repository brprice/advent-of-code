use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::fs;
use std::hash::Hash;

use aoc2019::parse_array::{char_array_iter, neighbours, V2};

// for edge weights all 1
fn dijkstra<'a, T: 'a + Copy + Hash + Ord, S: Iterator<Item = T>>(
    neighbours: impl Fn(&T) -> S,
    start: T,
    end: T,
) -> usize {
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
        if n == end {
            break;
        }
        seen.insert(n);
        //for &m in neighbours(&n).iter() {
        for m in neighbours(&n) {
            todo.push((Reverse(d + 1), m));
        }
    }

    dists[&end]
}

enum Neighbour {
    SameLevel(V2), // a normal step
    Inner(V2),     // into smaller recursive maze, at this point
    Outer(V2),     // exit into outer recursive maze, at this point
}

fn parse_maze(maze: String) -> (HashMap<V2, Vec<Neighbour>>, V2, V2) {
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

    let max_x = corridor.iter().map(|p| p.x).max().unwrap();
    let max_y = corridor.iter().map(|p| p.y).max().unwrap();

    let mut neighs = HashMap::new();
    for &c in corridor.iter() {
        let ns = neighbours(c)
            .into_iter()
            .filter(|n| corridor.contains(n))
            .map(Neighbour::SameLevel)
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
        // say we are on the outside edge of the maze iff we are within 5 tiles of the border
        // NB: this is saying where the target is, not the source, so the condition is negated
        // to what you may expect
        let mk_port = |p: V2| {
            if p.x < 5 || p.x > max_x - 5 || p.y < 5 || p.y > max_y - 5 {
                Neighbour::Inner(p)
            } else {
                Neighbour::Outer(p)
            }
        };
        neighs
            .entry(pls[0])
            .or_insert(Vec::new())
            .push(mk_port(pls[1]));
        neighs
            .entry(pls[1])
            .or_insert(Vec::new())
            .push(mk_port(pls[0]));
    }

    (neighs, start, end)
}

fn main() {
    let maze = fs::read_to_string("../data/day20").unwrap();
    let (neighbours, entrance, exit) = parse_maze(maze);
    fn to_same_level(n: &Neighbour) -> V2 {
        match n {
            Neighbour::SameLevel(p) => *p,
            Neighbour::Inner(p) => *p,
            Neighbour::Outer(p) => *p,
        }
    }
    let dist = dijkstra(|n| neighbours[n].iter().map(to_same_level), entrance, exit);
    println!("part a: {}", dist);

    let entb = (entrance, 0);
    let exib = (exit, 0);
    fn to_rec_level(n: &Neighbour, cur: usize) -> Option<(V2, usize)> {
        match n {
            Neighbour::SameLevel(p) => Some((*p, cur)),
            Neighbour::Inner(p) => Some((*p, cur + 1)),
            Neighbour::Outer(p) => {
                if cur == 0 {
                    None
                } else {
                    Some((*p, cur - 1))
                }
            }
        }
    }
    let neib = |&(n, l): &(V2, usize)| neighbours[&n].iter().flat_map(move |p| to_rec_level(p, l));
    let distb = dijkstra(neib, entb, exib);
    println!("part b: {}", distb);
}
