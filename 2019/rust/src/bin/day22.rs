use std::fs::read_to_string;

enum Shuffle {
    Stack,
    Cut(isize),
    Incr(usize),
}

fn split_prefix<'a, 'b>(pre: &'a str, s: &'b str) -> Option<&'b str> {
    if s.starts_with(pre) {
        return Some(s.split_at(pre.len()).1);
    }
    None
}

fn parse(shuf: &str) -> Shuffle {
    if shuf == "deal into new stack" {
        return Shuffle::Stack;
    }
    match split_prefix("cut ", shuf) {
        Some(n) => return Shuffle::Cut(n.parse().unwrap()),
        None => match split_prefix("deal with increment ", shuf) {
            Some(n) => return Shuffle::Incr(n.parse().unwrap()),
            None => panic!("no parse"),
        },
    }
}

fn do_shuffle(mut deck: Vec<usize>, shuffle: Shuffle) -> Vec<usize> {
    match shuffle {
        Shuffle::Stack => deck.reverse(),
        Shuffle::Cut(n) => {
            let mut t = deck.split_off(if n >= 0 {
                n as usize
            } else {
                deck.len() - (-n) as usize
            });
            t.append(&mut deck);
            deck = t;
        }
        Shuffle::Incr(n) => {
            let p = deck.len();
            let mut new = vec![0; p];
            let mut i = 0;
            for c in deck {
                new[i] = c;
                i += n;
                i %= p;
            }
            deck = new;
        }
    }
    deck
}

fn main() {
    let shufs = read_to_string("../data/day22").unwrap();
    let s = shufs.lines().map(parse);
    let mut deck = (0..10007).collect();
    for x in s {
        deck = do_shuffle(deck, x);
    }
    println!(
        "part a: {}",
        deck.into_iter().position(|c| c == 2019).unwrap()
    );
}
