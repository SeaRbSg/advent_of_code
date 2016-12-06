use std::char;

use crypto::digest::Digest;
use crypto::md5::Md5;

use errors::*;

pub fn solve(input: &str) -> Result<String> {
  let pw = Password::new(input.trim());
  let mut solution: [Option<char>; 8] = [None, None, None, None, None, None, None, None];
  while solution.iter().any(Option::is_none) {
  }
  solution.iter().map(|&x| x).collect::<Option<String>>().ok_or("".into())
}

struct Password {
  id: String,
  index: usize,
  md5: Md5,
}

impl Password {
  fn new(id: &str) -> Self {
    Password{id: id.into(), index: 0, md5: Md5::new()}
  }
}

impl Iterator for Password {
  // we will be counting with usize
  type Item = (usize, char);

  // next() is the only required method
  fn next(&mut self) -> Option<(usize, char)> {
    let mut result = None;
    let mut hash = [0; 16];

    while result.is_none() {
      let input = format!("{}{}", self.id, self.index);
      self.index += 1;

      self.md5.input_str(&input);
      self.md5.result(&mut hash);
      self.md5.reset();

      if hash[0] as u16 + hash[1] as u16 + (hash[2] >> 4) as u16 == 0 {
        let pos = (hash[2] & 0b00001111) as usize;
        let c = char::from_digit((hash[3] >> 4) as u32, 16).unwrap();
        result = Some((pos, c));
        break
      }
    }
    result
  }
}

#[test]
fn test_password() {
  let mut pw = Password{id: "abc".into(), index: 3231928, md5: Md5::new()};
  assert_eq!(pw.next(), Some((1, '5'.into())));
  assert_eq!(pw.index, 3231930);

  let mut pw = Password{id: "abc".into(), index: 5017308, md5: Md5::new()};
  assert_eq!(pw.next(), Some((8, 'f'.into())));
  // assert_eq!(pw.next(), Some((4, 'e'.into())));
}
