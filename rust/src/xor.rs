use std::error::Error;
use std::fmt::{Display, Formatter, Result as FmtResult};

use self::XORError::*;

pub trait XOR<T: ?Sized> {
    fn xor(&mut self, other: &T) -> Result<(), XORError>;
    fn xor_cycle(&mut self, other: &T);
}

impl<T, O> XOR<O> for T
    where T: AsMut<[u8]>,
          O: AsRef<[u8]> {
    fn xor(self: &mut T, other: &O) -> Result<(), XORError> {
        if self.as_mut().len() == other.as_ref().len() {
            for (b1, &b2) in self.as_mut().iter_mut().zip(other.as_ref().iter()) {
                *b1 ^= b2;
            }
            Ok(())
        }
        else { Err(BadLength) }
    }

    fn xor_cycle(&mut self, other: &O) {
        for (b1, &b2) in self.as_mut().iter_mut().zip(other.as_ref().iter().cycle()) {
            *b1 ^= b2;
        }
    }
}

#[derive(Debug)]
pub enum XORError {
    BadLength,
}

impl Display for XORError {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "XORing error: {}", self.description())
    }
}

impl Error for XORError {
    fn description(&self) -> &str {
        match *self {
            BadLength => "data length differ",
        }
    }
}

pub trait XORDecrypt<T>: XOR<T> {
    fn xor_decrypt_with_keys(&self, keys: &[T], frequent: &[u8]) -> Option<(usize, i32)>;
}

impl<T, K> XORDecrypt<K> for T
    where T: AsRef<[u8]> + AsMut<[u8]>,
          K: AsRef<[u8]> {
    fn xor_decrypt_with_keys(&self, keys: &[K], frequent: &[u8]) -> Option<(usize, i32)> {
        let mut solutions = Vec::new();
        'key: for (i, key) in keys.iter().enumerate() {
            let mut cipher = self.as_ref().to_owned();
            let mut stats = vec![0; 256];
            cipher.xor_cycle(key);
            for &b in &cipher {
                if bad_ascii(b as char) { continue 'key }
                stats[b as usize] += 1;
            }
            let mut histogram = (0..256).map(|b| (b as u8, stats[b])).collect::<Vec<_>>();
            histogram.sort_by(|&(_, ref count1), &(_, count2)| count2.cmp(count1));
            histogram.truncate(frequent.len());
            if 2 <= histogram.iter().filter(|&&(ref b, _)| frequent.contains(b)).count() {
                let grade = grade(&histogram, frequent);
                solutions.push((i, grade));
            }
        }
        solutions.sort_by(|&(_, ref grade1), &(_, grade2)| grade2.cmp(grade1));
        if solutions.len() != 0 { Some(solutions[0]) }
        else { None }
    }
}

fn bad_ascii(b: char) -> bool {
    b > '~' || (b < ' ' && (b != '\t' && b != '\n' && b != '\r' && b != 0x1b as char))
}

fn grade(histogram: &[(u8, usize)], frequent: &[u8]) -> i32 {
    let mut grade = 0;
    for (i, &(b, _)) in histogram.iter().enumerate() {
        if let Some(j) = index(b, frequent) {
            grade += frequent.len() as i32 - (i as i32 - j as i32).abs();
        }
        else { grade -= frequent.len() as i32; }
    }
    grade
}

fn index(b: u8, frequent: &[u8]) -> Option<usize> {
    frequent.iter().enumerate().filter_map(|(i, &c)| if b == c { Some(i) } else { None }).next()
}
