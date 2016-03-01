pub trait Hamming<T: ?Sized> {
    fn hamming(&self, other: &T) -> Option<usize>;
}

impl<T, O> Hamming<O> for T
    where T: AsRef<[u8]>,
          O: AsRef<[u8]> {
    fn hamming(&self, other: &O) -> Option<usize> {
        let (self_, other_) = (self.as_ref(), other.as_ref());
        if self_.len() == other_.len() {
            let result = self_.iter().zip(other_.iter())
                                     .fold(0, |acc, (&b0, &b1)| acc + hamming_bytes(b0, b1));
            Some(result)
        }
        else { None }
    }
}

pub fn hamming_bytes(b0: u8, b1: u8) -> usize {
    let mut bham = b0 ^ b1;
    let mut distance = 0;
    while bham != 0 {
        while bham & 0x1 == 0 { bham >>= 1; }
        distance += 1;
        bham >>= 1;
    }
    distance
}

