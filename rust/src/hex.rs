use b64::B64Vec;
use std::cmp::{Eq, PartialEq};
use std::borrow::{Borrow, BorrowMut};
use std::convert::From;
use std::error::Error;
use std::fmt::{Display, Formatter, LowerHex, UpperHex, Result as FmtResult};
use std::ops::{Deref, DerefMut};
use std::str::FromStr;

use self::ParseHexVecError::*;

#[derive(Clone, Debug)]
pub struct HexVec(pub Vec<u8>);

impl AsRef<[u8]> for HexVec {
    fn as_ref(&self) -> &[u8] {
        self.0.as_slice()
    }
}

impl AsMut<[u8]> for HexVec {
    fn as_mut(&mut self) -> &mut [u8] {
        self.0.as_mut_slice()
    }
}

impl AsRef<Vec<u8>> for HexVec {
    fn as_ref(&self) -> &Vec<u8> {
        &self.0
    }
}

impl AsMut<Vec<u8>> for HexVec {
    fn as_mut(&mut self) -> &mut Vec<u8> {
        &mut self.0
    }
}

impl Borrow<[u8]> for HexVec {
    fn borrow(&self) -> &[u8] {
        self.0.as_slice()
    }
}

impl BorrowMut<[u8]> for HexVec {
    fn borrow_mut(&mut self) -> &mut [u8] {
        self.0.as_mut_slice()
    }
}

impl Deref for HexVec {
    type Target = Vec<u8>;

    fn deref(&self) -> &Vec<u8> {
        &self.0
    }
}

impl DerefMut for HexVec {
    fn deref_mut(&mut self) -> &mut Vec<u8> {
        &mut self.0
    }
}

impl Eq for HexVec { }

impl PartialEq for HexVec {
    fn eq(&self, other: &HexVec) -> bool {
        self.0.eq(&other.0)
    }
}

impl Display for HexVec {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        let asciistr = self.iter().map(|&b| {
            if ' ' as u8 <= b && b <= '~' as u8 { b as char }
            else { '?' }
        }).collect::<String>();
        f.write_str(&asciistr)
    }
}

impl LowerHex for HexVec {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.write_str(&self.iter().map(|n| format!("{:02x}", n)).collect::<String>())
    }
}

impl UpperHex for HexVec {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.write_str(&self.iter().map(|n| format!("{:02X}", n)).collect::<String>())
    }
}

impl<T> From<T> for HexVec
    where T: AsRef<str> {
    fn from(s: T) -> HexVec {
        HexVec(s.as_ref().as_bytes().to_owned())
    }
}

impl From<B64Vec> for HexVec {
    fn from(b64: B64Vec) -> HexVec {
        HexVec(b64.into())
    }
}

impl<'a> From<&'a B64Vec> for HexVec {
    fn from(b64: &'a B64Vec) -> HexVec {
        HexVec(b64.into())
    }
}

impl FromStr for HexVec {
    type Err = ParseHexVecError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() % 2 == 0 {
            s.as_bytes().chunks(2).map(|chunk| {
                let d1 = try!((chunk[0] as char).to_digit(16).ok_or(NotAHexDigit));
                let d0 = try!((chunk[1] as char).to_digit(16).ok_or(NotAHexDigit));
                Ok((d1 * 16 + d0) as u8)
            }).collect::<Result<Vec<u8>, Self::Err>>().map(|hs| HexVec(hs))
        } else { Err(ParseHexVecError::OddLength) }
    }
}

#[derive(Clone, Debug)]
pub enum ParseHexVecError {
    OddLength,
    NotAHexDigit,
    
}

impl Display for ParseHexVecError {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "failed to parse HexVec: {}", self.description())
    }
}

impl Error for ParseHexVecError {
    fn description(&self) -> &str {
        match *self {
            OddLength => "string has odd length",
            NotAHexDigit => "character is not a hexadecimal digit",
        }
    }
}
