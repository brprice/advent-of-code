use std::fs::read_to_string;

use aoc2019::ext_gcd::ext_gcd;

/* A basic shuffle of p cards (and thus a sequence of basic shuffles) can be represented as a map
 * initial-index |-> final-index, in Z/pZ. Equivalently, as two numbers a, b, where the map is
 * x |-> a*x+b. We will always assume p is prime.
 * For example, cut 2 is (1,-2), as the card in position 0 ends up in position p-2, and the card in
 * position 2 ends up in 0 etc.
 * In this representation concatenation of shuffles is function composition, we can exponentiate
 * quickly by repeated squaring and inversion can be done via Bezout's identity.
 */

#[derive(Clone, Copy)]
struct Shuffle {
    // c1*x+c0
    c1: i64,
    c0: i64,
}

fn split_prefix<'a, 'b>(pre: &'a str, s: &'b str) -> Option<&'b str> {
    if s.starts_with(pre) {
        return Some(s.split_at(pre.len()).1);
    }
    None
}

fn parse(shuf: &str) -> Shuffle {
    if shuf == "deal into new stack" {
        return Shuffle { c1: -1, c0: -1 };
    }
    match split_prefix("cut ", shuf) {
        Some(n) => {
            return Shuffle {
                c1: 1,
                c0: -n.parse::<i64>().unwrap(),
            }
        }
        None => match split_prefix("deal with increment ", shuf) {
            Some(n) => {
                return Shuffle {
                    c1: n.parse().unwrap(),
                    c0: 0,
                }
            }
            None => panic!("no parse"),
        },
    }
}

// does the first elt of the array as the first shuffle
fn concat_shuffles_mod(p: i64, shuffles: impl IntoIterator<Item = Shuffle>) -> Shuffle {
    let mut sh = Shuffle { c1: 1, c0: 0 };
    for s in shuffles {
        let c1 = (s.c1 as i128 * sh.c1 as i128) % (p as i128);
        let c1 = c1 as i64;
        let c0 = (s.c1 as i128 * sh.c0 as i128) % (p as i128);
        let c0 = (c0 + s.c0 as i128) % (p as i128);
        let c0 = c0 as i64;
        sh = Shuffle { c1, c0 };
    }
    sh
}

fn do_shuffle(p: i64, shuffle: Shuffle, card: i64) -> i64 {
    (card * shuffle.c1 + shuffle.c0).rem_euclid(p)
}

fn inv_shuffle(p: i64, Shuffle { c1, c0 }: Shuffle) -> Shuffle {
    // inverse is x |-> (x-c0)/c1
    let (d, _, c1_inv) = ext_gcd(p, c1);
    assert_eq!(d, 1);
    let c0_new = (-c0 as i128) * (c1_inv as i128);
    let c0_new = (c0_new % (p as i128)) as i64;
    Shuffle {
        c1: c1_inv,
        c0: c0_new,
    }
}

fn exp_shuffle_mod(p: i64, s: Shuffle, mut e: u64) -> Shuffle {
    let mut base = s;
    let mut acc = Shuffle { c1: 1, c0: 0 };
    while e > 0 {
        if e % 2 == 1 {
            acc = concat_shuffles_mod(p, vec![acc, base]);
        }
        e /= 2;
        base = concat_shuffles_mod(p, vec![base, base]);
    }
    return acc;
}

fn main() {
    let shufs = read_to_string("../data/day22").unwrap();
    let shufs = shufs.lines().map(parse);

    let pa = 10007;
    let s = concat_shuffles_mod(pa, shufs.clone());
    let part_a = do_shuffle(pa, s, 2019);
    println!("part a: {}", part_a);

    let pb: i64 = 119315717514047;
    let iters: u64 = 101741582076661;
    println!("b: pb {}", pb);
    let s = concat_shuffles_mod(pb, shufs);
    println!("b: cat {} {}", s.c1, s.c0);
    let s = exp_shuffle_mod(pb, s, iters);
    println!("b: exp {} {}", s.c1, s.c0);
    let s = inv_shuffle(pb, s);
    println!("b: inv {} {}", s.c1, s.c0);
    let part_b = do_shuffle(pb, s, 2020);
    println!("part b: {}", part_b);
}
