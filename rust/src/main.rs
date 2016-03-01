#![feature(iter_arith)]

pub mod b64;
pub mod hamming;
pub mod hex;
pub mod xor;

macro_rules! read_file {
    ($filename: expr) => {{
        use std::fs::File;
        use std::io::Read;

        let mut buffer = String::new();
        File::open(&$filename).unwrap().read_to_string(&mut buffer).unwrap();
        buffer
    }}
}

macro_rules! data {
    ($set: expr, $challenge: expr, $filename: expr) => {
        read_file!(format!("../set{:02}/{:02}/{}", $set, $challenge, $filename));
    }
}

macro_rules! lines {
    ($data: expr) => { $data.lines().collect::<Vec<_>>() }
}

macro_rules! test {
    ($test_name: ident, $fn_tested: ident) => {
        #[test]
        fn $test_name() { $fn_tested(); }
    }
}

fn main() {
    set_1::challenge_6();
}

mod set_1 {
    use b64::B64Vec;
    use hamming::Hamming;
    use hex::HexVec;
    use xor::{XOR, XORDecrypt};

    test!(test_1, challenge_1);
    test!(test_2, challenge_2);
    test!(test_3, challenge_3);
    test!(test_4, challenge_4);
    test!(test_5, challenge_5);
    test!(test_6, challenge_6);

    #[allow(dead_code)]
    pub fn challenge_1() {
        let input = data!(1, 1, "hexadecimal");
        let result = data!(1, 1, "base64");
        let hexvec = input.parse::<HexVec>().unwrap();
        let b64str = B64Vec::from(hexvec).to_string();
        println!("{}", b64str);
        assert_eq!(b64str, result);
    }

    #[allow(dead_code)]
    pub fn challenge_2() {
        let data = data!(1, 2, "data");
        let mut hex_data = data.lines().map(|line| line.parse::<HexVec>().unwrap());
        let mut hex1 = hex_data.next().unwrap();
        let hex2 = hex_data.next().unwrap();
        let result = hex_data.next().unwrap();
        hex1.xor(&hex2).unwrap();
        println!("{:x}", hex1);
        assert_eq!(hex1, result);
    }

    #[allow(dead_code)]
    pub fn challenge_3() {
        let mut ciphertext = data!(1, 3, "ciphertext").parse::<HexVec>().unwrap();
        let plaintext = data!(1, 3, "plaintext");
        let keys = (0..256).map(|k| vec![k as u8]).collect::<Vec<_>>();
        let key_refs = keys.iter().map(|k| k.as_slice()).collect::<Vec<_>>();
        let key_id = ciphertext.xor_decrypt_with_keys(&key_refs, " etaoin".as_bytes()).unwrap().0;
        ciphertext.xor_cycle(&keys[key_id]);
        println!("{}", ciphertext);
        assert_eq!(*ciphertext, plaintext.as_bytes());
    }

    #[allow(dead_code)]
    pub fn challenge_4() {
        let data = data!(1, 4, "ciphertexts");
        let plaintext = data!(1, 4, "plaintext");
        let ciphers = data.lines().map(|line| line.parse::<HexVec>().unwrap()).collect::<Vec<_>>();
        let keys = (0..256).map(|k| vec![k as u8]).collect::<Vec<_>>();
        let key_refs = keys.iter().map(|k| k.as_slice()).collect::<Vec<_>>();
        let mut solutions = ciphers.iter().enumerate()
                                   .filter_map(|(i, cipher)| {
                                       if let Some(solution) = cipher.xor_decrypt_with_keys(&key_refs, " etaoin".as_bytes()) {
                                           Some((i, solution))
                                       }
                                       else { None } })
                                   .collect::<Vec<_>>();
        solutions.sort_by(|&(_, (_, ref grade1)), &(_, (_, grade2))| grade2.cmp(grade1));
        let (cipher_id, (key_id, _)) = solutions[0];
        let mut cipher = HexVec::from(ciphers[cipher_id].to_owned());
        cipher.xor_cycle(&keys[key_id]);
        println!("{}", cipher);
        assert_eq!(*cipher, plaintext.as_bytes());
    }

    #[allow(dead_code)]
    pub fn challenge_5() {
        let key = data!(1, 5, "key");
        let cipher = data!(1, 5, "ciphertext");
        let plaintext = data!(1, 5, "plaintext");
        let (key, result) = (key.as_bytes(), cipher.parse::<HexVec>().unwrap());
        let mut cipher = HexVec::from(plaintext);
        cipher.xor_cycle(&key);
        println!("{:x}", cipher);
        assert_eq!(cipher, result);
    }

    #[allow(dead_code)]
    pub fn challenge_6() {
        use std::cmp::{min, Ordering};
        use std::str;

        let base64 = data!(1, 6, "cipher_base64");
        let plaintext = data!(1, 6, "plaintext");
        let file: HexVec = base64.lines()
                                 .fold(String::new(), |file, line| file + line)
                                 .parse::<B64Vec>().unwrap()
                                 .into();
        let max_keysize = min(40, file.len() / 2);
        let mut weighted_keysizes = (2..max_keysize + 1).map(|keysize| {
            let n = file.len() / (keysize * 2);
            let nhd_sum = file.chunks(keysize * 2).take(n).map(|chunk| {
                let chunk1 = &chunk[0..keysize];
                let chunk2 = &chunk[keysize..2 * keysize];
                chunk1.hamming(&chunk2).unwrap() as f64
            }).sum::<f64>();
            (keysize, nhd_sum / (keysize * n) as f64)
        }).collect::<Vec<_>>();
        weighted_keysizes.sort_by(|&(_, nhd1), &(_, nhd2)| nhd1.partial_cmp(&nhd2).unwrap_or(Ordering::Greater));
        let keysize = weighted_keysizes[0].0;
        let char_keys = (0..256).map(|k| vec![k as u8]).collect::<Vec<_>>();
        let char_keys_refs = char_keys.iter().map(|k| k.as_slice()).collect::<Vec<_>>();
        let chunks = file.chunks(keysize).collect::<Vec<_>>();
        let mut blocks = Vec::new();
        for i in 0..keysize {
            let block = chunks.iter()
                              .take_while(|chunk| i < chunk.len())
                              .map(|chunk| chunk[i]).collect::<Vec<u8>>();
            blocks.push(block);
        }
        let key = blocks.into_iter()
                        .map(|block| block.xor_decrypt_with_keys(&char_keys_refs, " etaoin".as_bytes()))
        //              .map(|solution| solution.unwrap_or((0, 0)).0 as u8)
                        .map(|solution| solution.unwrap().0 as u8)
                        .collect::<Vec<u8>>();
        let mut file = file.clone();
        file.xor_cycle(&key);
        let text = str::from_utf8(&file).unwrap();
        println!("{}", text);
        assert_eq!(text, plaintext);
    }
}
