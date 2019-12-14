use std::collections::HashMap;
use std::fs;

fn parse_num_chem(nc: &str) -> (u64, &str) {
    let mut tmp = nc.trim().split(" ");
    let n = tmp.next().unwrap().parse().unwrap();
    let c = tmp.next().unwrap();
    (n, c)
}

fn parse_recipe(recipe: &str) -> (Vec<(u64, &str)>, (u64, &str)) {
    let mut tmp = recipe.split("=>");
    let inputs = tmp.next().unwrap().trim();
    let output = tmp.next().unwrap().trim();
    let output = parse_num_chem(output);
    let inputs = inputs.split(",").map(parse_num_chem).collect();
    (inputs, output)
}

fn fuel_to_ore(recipes: &HashMap<&str, (u64, Vec<(u64, &str)>)>, fuel: u64) -> u64 {
    let mut ore_needed = 0;
    let mut want = vec![(fuel, "FUEL")];
    let mut leftovers = HashMap::new();
    while let Some((n1, c)) = want.pop() {
        if c == "ORE" {
            ore_needed += n1;
        } else {
            let lo = leftovers.entry(c).or_insert(0);
            if *lo >= n1 {
                leftovers.entry(c).and_modify(|x| *x -= n1);
            } else {
                let n = n1 - *lo;
                leftovers.insert(c, 0);
                let (make, from) = &recipes[c];
                // need to run recipe 'mult' times
                let mult = n / make + if n % make == 0 { 0 } else { 1 };
                leftovers
                    .entry(c)
                    .and_modify(|l| *l += mult * make - n)
                    .or_insert(mult * make - n);
                for (kf, cf) in from {
                    want.push((kf * mult, cf));
                }
            }
        }
    }
    ore_needed
}

// find the first input that makes the predicate true,
// assuming that it is monotone.
fn binary_search_monotone_change(lo: u64, hi: u64, pred: impl Fn(u64) -> bool) -> u64 {
    if lo + 1 == hi {
        return hi;
    }
    let mid = (hi + lo) / 2;
    if pred(mid) {
        return binary_search_monotone_change(lo, mid, pred);
    } else {
        return binary_search_monotone_change(mid, hi, pred);
    }
}

fn main() {
    let dat = fs::read_to_string("../data/day14").unwrap();
    let mut recipes = HashMap::new(); // map from output to inputs
    for l in dat.lines() {
        let (inputs, (o_n, output)) = parse_recipe(l);
        recipes.insert(output, (o_n, inputs));
    }

    // part a: want to make 1 fuel
    let ore_needed = fuel_to_ore(&recipes, 1);
    println!("part a: total ore needed: {}", ore_needed);

    // part b: let's not try anything clever, as simplemindedness is fast enough
    // do a brute-force exponential search
    let ore = 1_000_000_000_000;
    let mut n = 1;
    while fuel_to_ore(&recipes, n) <= ore {
        n *= 2;
    }
    let min_bad = binary_search_monotone_change(n / 2, n, |k| fuel_to_ore(&recipes, k) > ore);
    println!("part b: maximum fuel produceable {}", min_bad - 1);
}
