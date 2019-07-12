use std::collections::VecDeque;

pub fn lexer(s: String, sep: &str) -> VecDeque<String> {
  let sep: Vec<char> = sep.chars().collect();

  let mut tmp: Vec<char> = vec![];

  let mut ret: VecDeque<String> = VecDeque::new();

  for c in s.chars() {
    if sep.contains(&c) {
      if !tmp.is_empty() {
        ret.push_back(tmp.into_iter().collect());
        tmp = vec![];
      }
      if c != ' ' {
        ret.push_back(c.to_string());
      }
    } else if c == '̃' {
      let mut last = ret.pop_back().expect("panic");
      last.push(c);
      ret.push_back(last);
    } else {
      tmp.push(c);
    }
  }
  if !tmp.is_empty() {
    ret.push_back(tmp.into_iter().collect());
  }

  ret
}
