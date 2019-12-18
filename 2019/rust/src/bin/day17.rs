use aoc2019::intcode::*;
use aoc2019::sparse::*;
use std::collections::HashSet;
use std::convert::TryFrom;
use std::iter::empty;

#[derive(Debug, PartialEq)]
// We only convert a bunch of F to a number when we output code,
// this lets our compressor split in the middle of a straight
enum PathElt {
    L,
    R,
    F,
}

fn to_code(mut path: &[PathElt]) -> String {
    let mut code = "".to_string();
    while !path.is_empty() {
        match path[0] {
            PathElt::L => {
                code.push_str("L,");
                path = &path[1..]
            }
            PathElt::R => {
                code.push_str("R,");
                path = &path[1..]
            }
            PathElt::F => {
                let mut n = 0;
                while n < path.len() && path[n] == PathElt::F {
                    n += 1;
                }
                let (_, rest) = path.split_at(n);
                code.push_str(&n.to_string());
                code.push(',');
                path = rest;
            }
        }
    }
    code.pop(); // remove trailing comma
    return code;
}

/* Compression, for part b
 * This is a depth-first search, where the children of a state are
 *   - match a prefix of the string against an entry in the dictionary
 *   - add a new dictionary entry for some prefix of the string
 *     (if the dictionary has room to grow)
 * In both cases, we consume that prefix.
 * We also enforce bounds on
 *   - max_size: the size of each dictionary item (sizes given by a parameter)
 *   - max_parse_len: the length of the compressed string (i.e. number of references into the
 *                    dictionary, nothing to do with the size parameter)
 */
struct CompressState<'a, T> {
    dict: Vec<&'a [T]>,
    dict_spare: usize,
    parse_cur: Vec<usize>, // indices into dict
    string_rem: &'a [T],
}

// Find children of a state
fn compress_worker<'a, T: PartialEq>(
    size: impl Fn(&[T]) -> usize,
    max_size: usize,
    st: CompressState<'a, T>,
) -> Vec<CompressState<'a, T>> {
    fn parse1<'a, T: PartialEq>(p: &'a [T], xs: &'a [T]) -> Option<&'a [T]> {
        if xs.starts_with(p) {
            Some(&xs[p.len()..])
        } else {
            None
        }
    }
    let mut res = Vec::new();
    // new dict item
    if st.dict_spare > 0 {
        let mut i = 0;
        let mut it = &st.string_rem[0..0];
        while size(it) <= max_size && i < st.string_rem.len() {
            i += 1;
            it = &st.string_rem[0..i];
            let mut new_dict = st.dict.clone();
            new_dict.push(it);
            let new_spare = st.dict_spare - 1;
            let mut new_parse = st.parse_cur.clone();
            new_parse.push(new_dict.len() - 1);
            let new_rem = &st.string_rem[i..];
            let new_state = CompressState {
                dict: new_dict,
                dict_spare: new_spare,
                parse_cur: new_parse,
                string_rem: new_rem,
            };
            res.push(new_state);
        }
    }
    // a prefix is a current dict item
    for i in 0..st.dict.len() {
        match parse1(st.dict[i], st.string_rem) {
            None => {}
            Some(t) => {
                let mut new_parse = st.parse_cur.clone();
                new_parse.push(i);
                let new_state = CompressState {
                    dict: st.dict.clone(),
                    dict_spare: st.dict_spare,
                    parse_cur: new_parse,
                    string_rem: t,
                };
                res.push(new_state);
            }
        }
    }
    res
}
// Do the DFS to compress a string.
// Returns the dictionary and a compression of the input in terms of indices into that dict.
// The dictionary size is bounded by dict_size, each entry in the dict is bounded in size (given by
// the first parameter) by max_size, and the compressed string is bounded in length by max_parse_len.
fn compress<'a, T: PartialEq>(
    size: &impl Fn(&[T]) -> usize,
    max_size: usize,
    max_parse_len: usize,
    dict_size: usize,
    s: &'a [T],
) -> (Vec<&'a [T]>, Vec<usize>) {
    let init_state = CompressState {
        dict: Vec::new(),
        dict_spare: dict_size,
        parse_cur: Vec::new(),
        string_rem: s,
    };
    let mut state_stack = Vec::new();
    state_stack.push(init_state);
    while let Some(st) = state_stack.pop() {
        if st.parse_cur.len() > max_parse_len {
            continue;
        }
        if st.string_rem.is_empty() {
            return (st.dict, st.parse_cur);
        } else {
            let mut new_states = compress_worker(size, max_size, st);
            state_stack.append(&mut new_states);
        }
    }
    panic!("compress: search failed");
}

fn main() {
    let mem: Sparse<usize, i64> = read_intcode_sparse("../data/day17");
    let mut mach = IC::new(0, mem.clone());
    let mut no_in = empty();
    let scaffold = mach.run(&mut no_in);

    println!("part a, the scaffold looks like:");

    let mut x = -1;
    let mut y = 0;
    let mut scaf = HashSet::new();
    let mut robot = None;
    for c in scaffold {
        x += 1;
        match u8::try_from(c).map(char::from) {
            Ok(d) => {
                print!("{}", d);
                match d {
                    '\n' => {
                        x = -1;
                        y += 1;
                    }
                    '.' => {}
                    'X' => {}
                    '#' => {
                        scaf.insert((x, y));
                    }
                    '^' => {
                        scaf.insert((x, y));
                        robot = Some(((x, y), (0, -1)));
                    }
                    '>' => {
                        scaf.insert((x, y));
                        robot = Some(((x, y), (1, 0)));
                    }
                    'v' => {
                        scaf.insert((x, y));
                        robot = Some(((x, y), (0, 1)));
                    }
                    '<' => {
                        scaf.insert((x, y));
                        robot = Some(((x, y), (-1, 0)));
                    }
                    _ => panic!("unexpected character: {}", d),
                }
            }
            Err(e) => panic!("could not convert {} to a char: {}", c, e),
        }
    }

    let mut align_param_sum = 0;
    for (x, y) in &scaf {
        let up = scaf.contains(&(x + 1, *y));
        let down = scaf.contains(&(x - 1, *y));
        let left = scaf.contains(&(*x, y - 1));
        let right = scaf.contains(&(*x, y + 1));
        if up && down && left && right {
            // this is an intersection
            align_param_sum += x * y;
        }
    }
    println!("part a: sum of alignment parameters: {}", align_param_sum);

    // part b: we assume that the path only crosses "on a straight", meaning that
    // always going straight over an intersection will trace out the whole path
    // first, find a path. We don't RLE straights, so our compression can split them
    // up if that is more efficient.
    let ((mut rx, mut ry), (mut rdx, mut rdy)) = robot.unwrap();
    scaf.remove(&(rx, ry));
    let mut path: Vec<PathElt> = Vec::new();
    while !scaf.is_empty() {
        let rx_next = rx + rdx;
        let ry_next = ry + rdy;
        let (rdx_r, rdy_r) = (-rdy, rdx); // if turn right
        let (rdx_l, rdy_l) = (rdy, -rdx); // if turn left
        if scaf.contains(&(rx_next, ry_next)) {
            path.push(PathElt::F);
            rx = rx_next;
            ry = ry_next;
            // Only remove if not an intersection, i.e. if we will not need to traverse this point
            // again.
            if !(scaf.contains(&(rx + rdx_l, ry + rdy_l))
                && scaf.contains(&(rx + rdx_r, ry + rdy_r)))
            {
                scaf.remove(&(rx, ry));
            }
        } else {
            if scaf.contains(&(rx + rdx_r, ry + rdy_r)) {
                path.push(PathElt::R);
                rdx = rdx_r;
                rdy = rdy_r;
            } else {
                if scaf.contains(&(rx + rdx_l, ry + rdy_l)) {
                    path.push(PathElt::L);
                    rdx = rdx_l;
                    rdy = rdy_l;
                } else {
                    panic!("part b: I got stuck!\n{:?}\n{:?}", path, scaf);
                }
            }
        }
    }

    // now compress the path
    println!("part b: path is {}", to_code(&path));
    let max_len = 20;
    let max_main_len = 10; // 10 calls to functions, interspersed with commas is length 19; 11 would be 21, which is too long
    let max_funcs = 3;
    let (mut fns, main) = compress(
        &|x| to_code(x).len(),
        max_len,
        max_main_len,
        max_funcs,
        &path,
    );
    let fnc = fns.pop().unwrap_or(&[]);
    let fnc = to_code(fnc);
    let fnb = fns.pop().unwrap_or(&[]);
    let fnb = to_code(fnb);
    let fna = fns.pop().unwrap_or(&[]);
    let fna = to_code(fna);
    let main: String = main
        .iter()
        .flat_map(|&x| &[['A', ','], ['B', ','], ['C', ',']][x])
        .collect();
    let main = &main[0..main.len() - 1];
    println!(
        "part b, compressed: fna: {:?}, fnb: {:?}, fnc: {:?}, main: {:?}",
        fna, fnb, fnc, main
    );

    // now run the robot
    let mut mem = mem;
    mem[0] = 2;
    let mut robot = IC::new(0, mem);
    let mut input = String::new();
    input.push_str(main);
    input.push_str("\n");
    input.push_str(&fna);
    input.push_str("\n");
    input.push_str(&fnb);
    input.push_str("\n");
    input.push_str(&fnc);
    input.push_str("\n");
    input.push_str("n\n"); // no video feed
    let mut input = input.bytes().map(i64::from);
    let out = robot.run(&mut input);
    let dust = out.last().unwrap();
    println!("part b: dust collected: {}", dust);
}
