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
    } else {
      tmp.push(c);
    }
  }
  if !tmp.is_empty() {
    ret.push_back(tmp.into_iter().collect());
  }

  ret
}

fn remove_head_whitespace(s: &String) -> String {
  let mut vd: VecDeque<char> = s.chars().collect();

  loop {
    let c = vd.get(0).expect("empty string.");
    if *c == ' ' {
      vd.pop_front();
    } else {
      break;
    }
  }
  vd.into_iter().collect::<String>()
}
