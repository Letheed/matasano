use std::convert::{From, Into};
use std::error::Error;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::str::FromStr;

use self::B64VecParseError::*;

const B64_CHAR_TABLE: [char; 65] = [
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
    'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
    'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
    'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
    'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
    'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
    'w', 'x', 'y', 'z', '0', '1', '2', '3',
    '4', '5', '6', '7', '8', '9', '+', '/',
    '=',
];

pub fn is_b64(c: char) -> bool {
    if 'A' <= c && c <= 'Z'
        || 'a' <= c && c <= 'z'
        || '/' <= c && c <= '9'
        || c == '+' { true }
    else { false }
}

#[derive(Clone, Debug)]
pub struct B64Vec(Vec<u8>);

impl Display for B64Vec {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        let b64str = self.0.iter().map(|&c| B64_CHAR_TABLE[c as usize]).collect::<String>();
        f.write_str(&b64str)
    }
}

impl<T> From<T> for B64Vec
    where T: AsRef<[u8]> {
    fn from(bytes: T) -> B64Vec {
        let bytes = bytes.as_ref();
        let triplets = bytes.len() / 3;
        let remaining_bytes = bytes.len() - triplets * 3;
        let b64len = if remaining_bytes != 0 { triplets * 4 + 1 } else { triplets * 4 };
        let mut b64 = Vec::with_capacity(b64len);
        let mut chunks = bytes.chunks(3);
        for _ in 0..triplets {
            let chunk = chunks.next().unwrap();
            let b0 = chunk[0] >> 2;
            let b1 = ((chunk[0] & 0x3) << 4) | (chunk[1] >> 4);
            let b2 = ((chunk[1] & 0xf) << 2) | (chunk[2] >> 6);
            let b3 = chunk[2] & 0x3f;
            b64.extend_from_slice(&[b0, b1, b2, b3]);
        }
        if remaining_bytes == 2 {
            let chunk = chunks.next().unwrap();
            let b0 = chunk[0] >> 2;
            let b1 = ((chunk[0] & 0x3) << 4) | (chunk[1] >> 4);
            let b2 = (chunk[1] & 0xf) << 2;
            b64.extend_from_slice(&[b0, b1, b2, 64]);
        }
        else if remaining_bytes == 1 {
            let chunk = chunks.next().unwrap();
            let b0 = chunk[0] >> 2;
            let b1 = (chunk[0] & 0x3) << 4;
            b64.extend_from_slice(&[b0, b1, 64, 64]);
        }
        B64Vec(b64)
    }
}

impl Into<Vec<u8>> for B64Vec {
    fn into(self) -> Vec<u8> {
        let mut bytes = self.0;
        let fours = bytes.len() / 4;
        let trunc_len = if *bytes.last().unwrap_or(&0) == 0x40 {
            if bytes[bytes.len() - 2] == 0x40 { fours * 3 - 2 }
            else { fours * 3 - 1 }
        }
        else { fours * 3 };
        for i in 0..fours {
            let decoded_bytes = {
                let chunk = &bytes[4*i..4*i + 4];
                let b0 = chunk[0] << 2 | chunk[1] >> 4;
                let b1 = chunk[1] << 4 | chunk[2] >> 2;
                let b2 = chunk[2] << 6 | chunk[3];
                &[b0, b1, b2]
            };
            for (b, db) in bytes[3*i..].iter_mut().zip(decoded_bytes.iter()) { *b = *db; }
        }
        bytes.truncate(trunc_len);
        bytes
    }
}

impl<'a> Into<Vec<u8>> for &'a B64Vec {
    fn into(self) -> Vec<u8> {
        let ref enc = self.0;
        let fours = enc.len() / 4;
        let start = if *enc.last().unwrap_or(&0) == 0x40 { 1 } else { 0 };
        let mut bytes = Vec::with_capacity(fours * 3);
        let mut chunks = enc.chunks(4);
        for _ in start..fours {
            let chunk = chunks.next().unwrap();
            let b0 = chunk[0] << 2 | chunk[1] >> 4;
            let b1 = chunk[1] << 4 | chunk[2] >> 2;
            let b2 = chunk[2] << 6 | chunk[3];
            bytes.extend_from_slice(&[b0, b1, b2]);
        }
        if start == 1 {
            let last_chunk = chunks.next().unwrap();
            if last_chunk[2] == 0x40 {
                let b0 = last_chunk[0] << 2 | last_chunk[1] >> 4;
                bytes.push(b0);
            }
            else {
                let b0 = last_chunk[0] << 2 | last_chunk[1] >> 4;
                let b1 = last_chunk[1] << 4 | last_chunk[2] >> 2;
                bytes.extend_from_slice(&[b0, b1]);
            }
        }
        bytes
    }
}

impl FromStr for B64Vec {
    type Err = B64VecParseError;

    fn from_str(s: &str) -> Result<B64Vec, B64VecParseError> {
        let mut pad_count = 0;
        let result = s.as_bytes().iter().map(|&c| {
            if 'A' as u8 <= c && c <= 'Z' as u8 { Ok(c - 65) }
            else if 'a' as u8 <= c && c <= 'z' as u8 { Ok(c - 71) }
            else if '0' as u8 <= c && c <= '9' as u8 { Ok(c + 4) }
            else if c == '+' as u8 { Ok(62) }
            else if c == '/' as u8 { Ok(63) }
            else if c == '=' as u8 { pad_count += 1; Ok(64) }
            else { Err(InvalidChar) }
        }).collect::<Result<Vec<u8>, Self::Err>>().map(|b64| B64Vec(b64));
        if let Ok(B64Vec(ref b64)) = result {
            if pad_count > 2 { return Err(PaddingError) }
            if pad_count == 2 && b64[b64.len() - 2] != 64 { return Err(PaddingError) }
            if pad_count >= 1 && b64[b64.len() - 1] != 64 { return Err(PaddingError) }
        } 
        result
    }
}

#[derive(Clone, Debug)]
pub enum B64VecParseError {
    InvalidChar,
    PaddingError,
}

impl Display for B64VecParseError {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "failed to parse base64: {}", self.description())
    }
}

impl Error for B64VecParseError {
    fn description(&self) -> &str {
        match *self {
            InvalidChar  => "invalid character",
            PaddingError => "padding error",
        }
    }
}
