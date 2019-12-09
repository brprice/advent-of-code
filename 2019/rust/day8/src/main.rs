use std::fs;

#[derive(Clone)]
struct Chunked<T> {
    size: usize,
    source: T,
}

// the last item may not be full-sized
impl<I: Iterator> Iterator for Chunked<I> {
    type Item = Vec<I::Item>;
    fn next(&mut self) -> Option<Self::Item> {
        let mut buf = Vec::with_capacity(self.size);
        for _ in 0..self.size {
            match self.source.next() {
                Some(i) => buf.push(i),
                None => break,
            }
        }
        if buf.len() > 0 {
            Some(buf)
        } else {
            None
        }
    }
}

fn chunks<I: Iterator>(source: I, size: usize) -> Chunked<I> {
    Chunked { size, source }
}

#[derive(Clone, Copy)]
enum Colour {
    Black,
    White,
    Alpha,
}

fn main() {
    let dat = fs::read_to_string("../../data/day8").unwrap();
    let pix = dat.trim().chars().map(|x| x.to_digit(10).unwrap());
    let w = 25;
    let h = 6;
    let layers = chunks(pix, w * h);

    let best_layer = layers.min_by_key(|l| l.iter().filter(|d| **d == 0).count());
    let (ones, twos) = best_layer
        .unwrap()
        .iter()
        .fold((0, 0), |(os, ts), d| match d {
            1 => (os + 1, ts),
            2 => (os, ts + 1),
            _ => (os, ts),
        });
    println!("part a: {}", ones * twos);

    let dat = fs::read_to_string("../../data/day8").unwrap();
    let layers = chunks(
        dat.trim().chars().map(|x| match x {
            '0' => Colour::Black,
            '1' => Colour::White,
            '2' => Colour::Alpha,
            _ => panic!("unknown colour {}", x),
        }),
        w * h,
    );
    let mut final_img = Vec::with_capacity(w * h);
    for _ in 0..w * h {
        final_img.push(Colour::Alpha)
    }
    for l in layers {
        for i in 0..w * h {
            match final_img[i] {
                Colour::Alpha => final_img[i] = l[i],
                _ => {}
            }
        }
    }
    println!("part b:");
    for y in 0..h {
        for x in 0..w {
            match final_img[y * w + x] {
                Colour::Black => print!(" "),
                Colour::White => print!("#"),
                Colour::Alpha => print!("-"),
            }
        }
        print!("\n");
    }
}
