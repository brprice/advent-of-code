use std::cmp::Ordering;
use std::collections::HashSet;
use std::convert::TryInto;
use std::fmt::{Debug, Display};
use std::mem;

use num_traits::{Num, ToPrimitive};

use aoc2019::intcode::*;
use aoc2019::sparse::*;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Door {
    North,
    East,
    South,
    West,
}
fn rev(s: &Door) -> Door {
    match s {
        Door::North => Door::South,
        Door::East => Door::West,
        Door::South => Door::North,
        Door::West => Door::East,
    }
}

impl Display for Door {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Door::North => write!(fmt, "north"),
            Door::East => write!(fmt, "east"),
            Door::South => write!(fmt, "south"),
            Door::West => write!(fmt, "west"),
        }
    }
}

impl std::str::FromStr for Door {
    type Err = String;
    fn from_str(s: &str) -> Result<Door, String> {
        match s {
            "north" => Ok(Door::North),
            "east" => Ok(Door::East),
            "south" => Ok(Door::South),
            "west" => Ok(Door::West),
            _ => Err(s.to_string()),
        }
    }
}

#[derive(Debug)]
struct Room {
    name: String,
    desc: String,
    doors: Vec<Door>,
    items: Vec<String>,
}

#[must_use]
fn ensure(b: bool) -> Option<()> {
    if b {
        Some(())
    } else {
        None
    }
}

enum ParsedRoom {
    Room(Room),
    EjectedFromCheckpoint(Room), // we found the checkpoint, but were ejected back to this room
}
fn parse_room(s: &str) -> Option<ParsedRoom> {
    let mut ls = s.lines();
    ensure("" == ls.next()?)?;
    ensure("" == ls.next()?)?;
    ensure("" == ls.next()?)?;
    let title = ls.next().unwrap();
    let name = title.trim_matches('=').trim().to_string();
    // The desc may be overwritten when we get through the security door,
    // as the desc there is "Analysing...", and the real description comes later
    let mut desc = ls.next().unwrap().to_string();
    assert_eq!(Some(""), ls.next());
    assert_eq!(Some("Doors here lead:"), ls.next());
    let mut doors = Vec::new();
    while let Some(l) = ls.next() {
        if l.starts_with("- ") {
            doors.push(l.split_at(2).1.parse().unwrap());
        } else {
            assert_eq!("", l);
            break;
        }
    }
    let mut items = Vec::new();
    match ls.next() {
        Some("Items here:") => {
            while let Some(l) = ls.next() {
                if l.starts_with("- ") {
                    items.push(l.split_at(2).1.to_string());
                } else {
                    assert_eq!("", l);
                    assert_eq!(Some("Command?"), ls.next());
                    break;
                }
            }
        }
        Some("Command?") => {}
        Some(l) => {
            if l.starts_with("A loud, robotic voice says \"Alert! Droids on this ship are") {
                // found security door
                let ejected : String = ls.flat_map(|e| vec![e,"\n"]).collect();
                match parse_room(&ejected)? {
                    ParsedRoom::Room(room) => return Some(ParsedRoom::EjectedFromCheckpoint(room)),
                    ParsedRoom::EjectedFromCheckpoint(_) => panic!("Huh, two checkpoints?"),
                }
            } else if l.starts_with("A loud, robotic voice says \"Analysis complete! You may proceed.\" and you enter the cockpit.") {
            // Got through the part a security door
                desc = l.to_string();
                while let Some(l) = ls.next() {
                    desc.push_str("\n");
                    desc.push_str(l);
                }
            } else {
                panic!("Unexpected line in room: {}",l)
            }
        }
        None => panic!("Unexpected end of room"),
    }
    assert_eq!(None, ls.next());
    Some(ParsedRoom::Room(Room {
        name,
        desc,
        doors,
        items,
    }))
}
struct Game<T, C: Vector<T>> {
    mach: Option<Box<dyn FnOnce(&str) -> IC<T, C>>>, // None: has halted
    room: Room,
    inv: Vec<String>,
    last_pickup: Option<String>, // some items are traps, remember what we last picked up in case it was a bad idea
}
#[derive(Debug)]
enum Cmd {
    Move(Door),
    Take(String),
    Drop(String),
}

impl<T, C: Vector<T>> Game<T, C> {
    // Will return last item picked up if we realise it was a trap
    // in that case, self may be in an inconsistent state.
    // In the case we move into the checkpoint, and get ejected, we return Ok(true)
    fn do_action(&mut self, cmd: Cmd) -> Result<bool, String>
    where
        C: 'static,
        T: 'static,
        T: Clone,
        T: TryInto<usize>,
        <T as TryInto<usize>>::Error: Debug,
        T: ToPrimitive,
        T: From<u8>,
        T: Num,
        T: PartialOrd,
        T: Display,
    {
        fn untrap<X>(item: Option<String>, x: Option<X>) -> Result<X, String> {
            match x {
                None => return Err(item.expect("nothing picked up, but found trap?")),
                Some(y) => return Ok(y),
            }
        };
        let mach = mem::replace(&mut self.mach, None); //This is a hack to avoid rust failing to move self.mach, even though we replace it later.
        let mach = mach.expect("The game is over");
        let new_mach;
        let mut ejected = false;
        match cmd {
            Cmd::Move(d) => {
                let ds = d.to_string();
                let (rm, m) = run_to_in(mach(&ds));
                new_mach = m;
                let room = untrap(self.last_pickup.clone(), parse_room(&rm))?;
                match room {
                    ParsedRoom::Room(r) => self.room = r,
                    ParsedRoom::EjectedFromCheckpoint(r) => {
                        ejected = true;
                        self.room = r
                    }
                }
            }
            Cmd::Take(i) => {
                let cmd = format!("take {}", i);
                let expected = format!("\nYou take the {}.\n\nCommand?\n", i);
                self.last_pickup = Some(i.clone());
                let (_, m) = untrap(
                    self.last_pickup.clone(),
                    run_to_in_expect(&expected, mach(&cmd)),
                )?;
                new_mach = m;
                let j = self
                    .room
                    .items
                    .iter()
                    .position(|item| *item == i)
                    .expect("bad item");
                self.inv.push(i);
                self.room.items.remove(j);
            }
            Cmd::Drop(i) => {
                let cmd = format!("drop {}", i);
                let expected = format!("\nYou drop the {}.\n\nCommand?\n", i);
                let (_, m) = untrap(
                    self.last_pickup.clone(),
                    run_to_in_expect(&expected, mach(&cmd)),
                )?;
                new_mach = m;
                let j = self
                    .inv
                    .iter()
                    .position(|item| *item == i)
                    .expect("bad item");
                self.room.items.push(i);
                self.inv.remove(j);
            }
        }
        self.mach = new_mach;
        Ok(ejected)
    }
}

fn run_to_in_expect<T: 'static, C: 'static>(
    expect: &str,
    mach: IC<T, C>,
) -> Option<(String, Option<Box<dyn FnOnce(&str) -> IC<T, C>>>)>
// TODO: I really should abstract this list of requirements!
where
    C: Vector<T>,
    T: Clone,
    T: TryInto<usize>,
    <T as TryInto<usize>>::Error: Debug,
    T: ToPrimitive,
    T: From<u8>,
    T: Num,
    T: PartialOrd,
    T: Display,
{
    run_to_in_worker(Some(expect), mach)
}

fn run_to_in<T: 'static, C: 'static>(
    mach: IC<T, C>,
) -> (String, Option<Box<dyn FnOnce(&str) -> IC<T, C>>>)
where
    C: Vector<T>,
    T: Clone,
    T: TryInto<usize>,
    <T as TryInto<usize>>::Error: Debug,
    T: ToPrimitive,
    T: From<u8>,
    T: Num,
    T: PartialOrd,
    T: Display,
{
    run_to_in_worker(None, mach).unwrap()
}

// Runs the machine just enough to check if the output is as we expect (if anything),
// then either returns None, or continues running until we want input or halt.
// Thus, if expect = None, the return will always be Some(...)
fn run_to_in_worker<T: 'static, C: 'static>(
    expect: Option<&str>,
    mut mach: IC<T, C>,
) -> Option<(String, Option<Box<dyn FnOnce(&str) -> IC<T, C>>>)>
where
    C: Vector<T>,
    T: Clone,
    T: TryInto<usize>,
    <T as TryInto<usize>>::Error: Debug,
    T: ToPrimitive,
    T: From<u8>,
    T: Num,
    T: PartialOrd,
    T: Display,
{
    let e_len = expect.map(str::len);
    let mut got_expected = expect.is_none();
    let mut out = String::new();
    loop {
        match mach.run_to_io() {
            IO::Out(m, c) => {
                let ch: char = c.to_u8().unwrap().into();
                out.push(ch);
                match e_len.map(|l| l.cmp(&out.len())) {
                    Some(Ordering::Equal) => {
                        ensure(expect.unwrap() == out)?;
                        got_expected = true;
                    }
                    Some(Ordering::Less) => return None,
                    _ => {}
                }
                mach = m;
            }
            IO::In(f) => {
                ensure(got_expected)?;
                return Some((out, Some(Box::new(|s| give_string(f, &s)))));
            }
            IO::Halt => {
                ensure(got_expected)?;
                return Some((out, None));
            }
        }
    }
}

// This function adds a newline
fn give_string<T: 'static, C: 'static>(
    mut mach: Box<dyn FnOnce(T) -> IC<T, C>>,
    s: &str,
) -> IC<T, C>
where
    C: Vector<T>,
    T: Clone,
    T: TryInto<usize>,
    <T as TryInto<usize>>::Error: Debug,
    T: ToPrimitive,
    T: From<u8>,
    T: Num,
    T: PartialOrd,
    T: Display,
{
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        let t = c as u32;
        let t: u8 = t.try_into().unwrap();
        let t: T = t.into();
        match mach(t).run_to_io() {
            IO::In(f) => mach = f,
            _ => panic!("expected machine to want input"),
        }
    }
    let newline = '\n' as u32;
    let newline: u8 = newline.try_into().unwrap();
    let newline: T = newline.into();
    mach(newline)
}

// PART A, General Strategy
// It appears the ship is a tree (with back-edges), so:
// do a pre-order traversal, picking up all items and finding the security door,
// then go back to the door, and work out which items we need to hold to let us through.
// Some items are traps: when we realise we have fallen into a trap, restart and remember
// not to pick up that item next time.

// Either find a trap, or end up where we started with all non-trap items,
// and (hopefully) knowing where the security door is (a stack of moves)
fn parta_traverse<T: 'static, C: 'static>(
    traps: &HashSet<String>,
    ret: Option<Door>,
    mut game: Game<T, C>,
) -> Result<(Game<T, C>, Option<Vec<Door>>), String>
where
    C: Vector<T>,
    T: Clone,
    T: TryInto<usize>,
    <T as TryInto<usize>>::Error: Debug,
    T: ToPrimitive,
    T: From<u8>,
    T: Num,
    T: PartialOrd,
    T: Display,
{
    let mut sec_door = None;
    for i in game.room.items.clone() {
        if !traps.contains(&i) {
            game.do_action(Cmd::Take(i))?;
        }
    }
    for d in game.room.doors.clone() {
        if ret.map_or(false, |r| r == d) {
            continue;
        }
        if game.do_action(Cmd::Move(d.clone()))? {
            // found security door
            sec_door = Some(vec![d]);
        } else {
            let tmp = parta_traverse(traps, Some(rev(&d)), game)?;
            game = tmp.0;
            let sec_door_tmp = tmp.1;
            match sec_door_tmp {
                None => {}
                Some(ms) => {
                    let mut tmp = ms;
                    tmp.push(d);
                    sec_door = Some(tmp)
                }
            }
        }
    }
    match ret {
        None => {}
        Some(r) => {
            game.do_action(Cmd::Move(r))?;
        }
    }
    Ok((game, sec_door))
}

#[derive(Clone, Copy, Debug)]
enum Gray<T> {
    Up(T),
    Down(T),
}
impl<T> Gray<T> {
    fn flip(self) -> Self {
        match self {
            Gray::Up(x) => Gray::Down(x),
            Gray::Down(x) => Gray::Up(x),
        }
    }
}
// A Gray code on items.len() bits, expressed as transitions of named bits
// assuming we start from all bits 0
fn gray<T: Clone>(items: &[T]) -> impl Iterator<Item = Gray<&T>> {
    fn gray1<T: Clone>(items: &[T]) -> Vec<Gray<&T>> {
        if items.len() == 0 {
            return Vec::new();
        }
        let rest = gray1(&items[1..items.len()]);
        let mut ret: Vec<Gray<&T>> = Vec::with_capacity(rest.len() * 2 + 1);
        ret.extend(rest.clone());
        ret.push(Gray::Up(&items[0]));
        ret.extend(rest.into_iter().rev().map(Gray::flip));
        ret
    }
    gray1(items).into_iter()
}

fn parta<T: 'static, C: 'static>(mach: IC<T, C>) -> String
where
    C: Vector<T>,
    T: Clone,
    C: Clone,
    T: TryInto<usize>,
    <T as TryInto<usize>>::Error: Debug,
    T: ToPrimitive,
    T: From<u8>,
    T: Num,
    T: PartialOrd,
    T: Display,
{
    fn mk_game<T: 'static, C: 'static>(mach: IC<T, C>) -> Game<T, C>
    where
        C: Vector<T>,
        T: Clone,
        T: TryInto<usize>,
        <T as TryInto<usize>>::Error: Debug,
        T: ToPrimitive,
        T: From<u8>,
        T: Num,
        T: PartialOrd,
        T: Display,
    {
        let (room1, kont) = run_to_in(mach);
        let room = parse_room(&room1).unwrap();
        match room {
            ParsedRoom::Room(room) => Game {
                mach: kont,
                room,
                inv: Vec::new(),
                last_pickup: None,
            },
            ParsedRoom::EjectedFromCheckpoint(_) => panic!("Start should not be in the checkpoint"),
        }
    }

    let mut traps = HashSet::new();

    let (mut game, mut sec_door) = loop {
        match parta_traverse(&traps, None, mk_game(mach.clone())) {
            Err(trap) => {
                println!("Found trap: {}", trap);
                traps.insert(trap);
                continue;
            }
            Ok((_, None)) => panic!("Traversed and not found security checkpoint"),
            Ok((game, Some(sec_door))) => {
                break (game, sec_door);
            }
        }
    };
    println!(
        "Got all items, now seaching for correct set to pass security checkpoint: {:?}",
        game.inv
    );
    println!("The security door is at {:?}", sec_door);
    while sec_door.len() > 1 {
        let d = sec_door.pop().unwrap();
        game.do_action(Cmd::Move(d)).unwrap();
    }
    let security = sec_door.pop().unwrap();

    // Find the correct set of items to get through the checkpoint
    // We could be more efficient, because we get told "heavier" or "lighter", but brute force
    // is fast enough. Using Gray code means we only pick up/drop one item between each test.
    let all_items = game.inv.clone();
    let mut gray_items = gray(&all_items);
    game.do_action(Cmd::Move(security)).unwrap();
    while game.room.name != "Pressure-Sensitive Floor" {
        match gray_items.next() {
            Some(Gray::Up(i)) => game.do_action(Cmd::Drop(i.to_string())).unwrap(),
            Some(Gray::Down(i)) => game.do_action(Cmd::Take(i.to_string())).unwrap(),
            None => panic!("Could not get through door using items: {:?}!", all_items),
        };
        game.do_action(Cmd::Move(security)).unwrap();
    }
    game.room.desc.lines().nth(2).unwrap().to_string()
}

fn main() {
    let mem: Sparse<usize, i64> = read_intcode_sparse("../data/day25");
    let mach = IC::new(0, mem.clone());

    println!("part a: {}", parta(mach));
}
