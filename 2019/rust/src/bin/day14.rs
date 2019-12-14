use std::collections::HashMap;
use std::fs;

fn parse_num_chem(nc: &str) -> (u32, &str) {
    let mut tmp = nc.trim().split(" ");
    let n = tmp.next().unwrap().parse().unwrap();
    let c = tmp.next().unwrap();
    (n, c)
}

fn parse_recipe(recipe: &str) -> (Vec<(u32, &str)>, (u32, &str)) {
    let mut tmp = recipe.split("=>");
    let inputs = tmp.next().unwrap().trim();
    let output = tmp.next().unwrap().trim();
    let output = parse_num_chem(output);
    let inputs = inputs.split(",").map(parse_num_chem).collect();
    (inputs, output)
}

fn main() {
    let dat = fs::read_to_string("../data/day14").unwrap();
    let mut recipes = HashMap::new(); // map from output to inputs
    for l in dat.lines() {
        let (inputs, (o_n, output)) = parse_recipe(l);
        recipes.insert(output, (o_n, inputs));
    }

    // part a: want to make 1 fuel
    let mut ore_needed = 0;
    let mut want = vec![(1, "FUEL")];
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

    println!("part a: total ore needed: {}", ore_needed);
}
