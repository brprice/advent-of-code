use std::collections::{BTreeSet, HashMap};
use std::fs::read_to_string;

type V2 = (usize, usize);

// The entrance is treated as a key '@'
// The 4 part b entrances will be treated as a keys '1','2','3','4',
// but this is done in main, not read_maze
enum MazeElem {
    Space,
    Key(char),
    Door(char),
}

// extract the spaces, entrance, keys and doors of the maze
fn read_maze(maze: String) -> (V2, HashMap<V2, MazeElem>) {
    let mut map = HashMap::new();
    let maze = maze.lines();
    let mut ent = None;
    for (y, l) in maze.enumerate() {
        for (x, c) in l.chars().enumerate() {
            print!("{}", c);
            match c {
                '.' => {
                    map.insert((x, y), MazeElem::Space);
                }
                '@' => {
                    ent = Some((x, y));
                    map.insert((x, y), MazeElem::Key('@'));
                }
                d => {
                    if d.is_ascii_lowercase() {
                        map.insert((x, y), MazeElem::Key(d));
                    } else if d.is_ascii_uppercase() {
                        map.insert((x, y), MazeElem::Door(d.to_ascii_lowercase()));
                    }
                }
            }
        }
        println!("");
    }
    (ent.unwrap(), map)
}

fn neighbours((x, y): V2) -> Vec<V2> {
    vec![(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
}

/* Given a map of node-locations |-> MazeElem, and a bunch of roots,
 * extract the forest rooted at the given roots (respecting the order),
 * and also a map Key |-> (location,blocking-doors).
 * Trees are represented as a map of nodes pointing towards the root.
 * NB: we use BTreeSets as we will eventually want to compute the shortest path
 * hitting a set of nodes, and we wish to memoise this, so need a set in the key of
 * a map. Unfortunately HashSet is not Hash-able, but BTreeSet is!
 */
fn extract_forest(
    maze: HashMap<V2, MazeElem>,
    roots: Vec<V2>,
) -> (Vec<HashMap<V2, V2>>, HashMap<char, (V2, BTreeSet<char>)>) {
    fn children(maze: &HashMap<V2, MazeElem>, parent: V2, node: V2) -> Vec<V2> {
        neighbours(node)
            .into_iter()
            .filter(|c| *c != parent && maze.contains_key(c))
            .collect()
    }

    fn extract_tree(
        maze: &HashMap<V2, MazeElem>,
        unseen: &mut BTreeSet<V2>,
        keys: &mut HashMap<char, (V2, BTreeSet<char>)>,
        root: V2,
    ) -> HashMap<V2, V2> {
        unseen.remove(&root);
        let mut tree = HashMap::new();
        let mut todo = Vec::new();
        let mut doors = Vec::new();
        match maze[&root] {
            MazeElem::Space => {}
            MazeElem::Key(k) => {
                keys.insert(k, (root, doors.iter().copied().collect()));
            }
            MazeElem::Door(k) => {
                doors.push(k);
            }
        }
        for n in neighbours(root) {
            if maze.contains_key(&n) {
                todo.push((root, n, doors.clone()));
            }
        }
        while let Some((p, c, mut doors)) = todo.pop() {
            if !unseen.contains(&c) {
                panic!("extractForest: not a forest!");
            }
            unseen.remove(&c);
            tree.insert(c, p);
            match maze[&c] {
                MazeElem::Space => {}
                MazeElem::Key(k) => {
                    keys.insert(k, (c, doors.iter().copied().collect()));
                }
                MazeElem::Door(k) => {
                    doors.push(k);
                }
            }
            for ch in children(maze, p, c) {
                todo.push((c, ch, doors.clone()));
            }
        }
        tree
    }

    let mut forest = Vec::new();
    let mut keys = HashMap::new();
    let mut unseen = maze.keys().copied().collect();
    for r in roots {
        forest.push(extract_tree(&maze, &mut unseen, &mut keys, r));
    }
    if !unseen.is_empty() {
        panic!("extractForest: we have leftovers: {:?}", unseen);
    }
    (forest, keys)
}

// assume the roots are connected together, with l1-weighted edges
fn get_dist(forest: &Vec<HashMap<V2, V2>>, x: &V2, y: &V2) -> usize {
    fn l1(x: V2, y: V2) -> usize {
        let d0 = if x.0 > y.0 { x.0 - y.0 } else { y.0 - x.0 };
        let d1 = if x.1 > y.1 { x.1 - y.1 } else { y.1 - x.1 };
        d0 + d1
    }
    fn to_root(forest: &Vec<HashMap<V2, V2>>, mut x: V2) -> Vec<V2> {
        match forest.iter().filter(|t| t.contains_key(&x)).next() {
            None => vec![x], // it was a root
            Some(tree) => {
                let mut path = Vec::new();
                path.push(x);
                while let Some(&p) = tree.get(&x) {
                    path.push(p);
                    x = p
                }
                path
            }
        }
    }
    if x == y {
        return 0;
    };
    let px = to_root(forest, *x);
    let py = to_root(forest, *y);
    let x_root = px[px.len() - 1];
    let y_root = py[py.len() - 1];
    if x_root == y_root {
        // same tree
        let mut xi = px.len() - 1;
        let mut yi = py.len() - 1;
        while px[xi] == py[yi] {
            if px[xi] == *y {
                return xi;
            };
            if py[yi] == *x {
                return yi;
            };
            xi -= 1;
            yi -= 1;
        }
        xi + yi + 2
    } else {
        px.len() - 1 + py.len() - 1 + l1(x_root, y_root)
    }
}

fn shortest_path_hitting(
    dist: &Vec<usize>,
    keys: &HashMap<char, (usize, V2, BTreeSet<char>)>,
    ent: char,
    to_hit: BTreeSet<char>,
) -> usize {
    shortest_multi_path_hitting(dist, keys, vec![(ent, to_hit)])
}

fn shortest_multi_path_hitting(
    dist: &Vec<usize>,
    keys: &HashMap<char, (usize, V2, BTreeSet<char>)>,
    vaults: Vec<(char, BTreeSet<char>)>,
) -> usize {
    fn go(
        dist: &Vec<usize>,
        keys: &HashMap<char, (usize, V2, BTreeSet<char>)>,
        memo: &mut HashMap<Vec<(char, BTreeSet<char>)>, usize>,
        vaults: Vec<(char, BTreeSet<char>)>,
    ) -> usize {
        let mut to_hit_all = BTreeSet::new();
        for th in vaults.iter().map(|(_, to_hit)| to_hit) {
            let mut thc = th.clone();
            to_hit_all.append(&mut thc);
        }
        if to_hit_all.is_empty() {
            return 0;
        }
        if memo.contains_key(&vaults) {
            return memo[&vaults];
        }
        // grab 'next' which are not blocked by any of the nodes we wish to visit
        let nexts = vaults
            .iter()
            .map(|(_, to_hit)| to_hit.iter().filter(|n| keys[n].2.is_disjoint(&to_hit_all)));

        let mut best = None;
        let num_keys = keys.len();
        for (v, nv) in nexts.enumerate() {
            let id_start_v = keys[&vaults[v].0].0;
            for n in nv {
                let mut new_vaults = vaults.clone();
                new_vaults[v].0 = *n;
                new_vaults[v].1.remove(n);
                let id_n = keys[n].0;
                let d = dist[id_start_v * num_keys + id_n] + go(dist, keys, memo, new_vaults);
                if best.map_or(true, |b| d < b) {
                    best = Some(d);
                }
            }
        }
        let best = match best {
            None => panic!("could not find any path in vaults {:?}", vaults),
            Some(b) => b,
        };
        memo.insert(vaults, best);
        best
    }
    let mut memo = HashMap::new();
    go(dist, keys, &mut memo, vaults)
}

fn main() {
    let (ent, mut maze) = read_maze(read_to_string("../data/day18").unwrap());
    /* For part a, we note that our input is essentially 4 trees connected to a 4-cycle.
     * We also rely on the fact that it has a solid border, so coordinates are >=1, and we can
     * thus subtract one without overflowing usize (when finding neighbours)
     */
    for n in neighbours(ent) {
        maze.remove(&n);
    }
    let (x, y) = ent;

    // preperation for part b: the 4 vault entrances
    maze.insert((x - 1, y - 1), MazeElem::Key('1'));
    maze.insert((x + 1, y - 1), MazeElem::Key('2'));
    maze.insert((x + 1, y + 1), MazeElem::Key('3'));
    maze.insert((x - 1, y + 1), MazeElem::Key('4'));

    let (forest, keys) = extract_forest(
        maze,
        vec![
            ent,
            (x - 1, y - 1),
            (x + 1, y - 1),
            (x + 1, y + 1),
            (x - 1, y + 1),
        ],
    );
    // assign sequential ids starting from 0 to keys
    let keys: HashMap<char, (usize, V2, BTreeSet<char>)> = keys
        .into_iter()
        .enumerate()
        .map(|(id, (k, (pos, doors)))| (k, (id, pos, doors)))
        .collect();

    // shortest paths between all pairs of keys, ignoring doors
    let num_keys = keys.len();
    let mut dist = vec![0; num_keys * num_keys];
    for (_, (id1, p1, _)) in &keys {
        for (_, (id2, p2, _)) in &keys {
            dist[id1 * num_keys + id2] = get_dist(&forest, p1, p2);
        }
    }
    let dist = dist;

    fn not_root(&k: &char) -> bool {
        k != '@' && k != '1' && k != '2' && k != '3' && k != '4'
    };
    let parta = shortest_path_hitting(
        &dist,
        &keys,
        '@',
        keys.keys().copied().filter(not_root).collect(),
    );
    println!("part a: {}", parta);

    // part b: partition into the 4 vaults
    let v1 = keys[&'1'].0;
    let v2 = keys[&'2'].0;
    let v3 = keys[&'3'].0;
    let v4 = keys[&'4'].0;
    let in_vault = |k, v| {
        let dkv = dist[k * num_keys + v];
        let dkv1 = dist[k * num_keys + v1];
        let dkv2 = dist[k * num_keys + v2];
        let dkv3 = dist[k * num_keys + v3];
        let dkv4 = dist[k * num_keys + v4];
        dkv <= dkv1 && dkv <= dkv2 && dkv <= dkv3 && dkv <= dkv4
    };
    let vaults = ['1', '2', '3', '4'].into_iter().map(|v| {
        let vid = keys[v].0;
        (
            *v,
            keys.iter()
                .filter_map(|(k, (id, _, _))| {
                    if not_root(k) && in_vault(id, vid) {
                        Some(*k)
                    } else {
                        None
                    }
                })
                .collect(),
        )
    });
    let partb = shortest_multi_path_hitting(&dist, &keys, vaults.collect());
    println!("part b: {}", partb);
}
