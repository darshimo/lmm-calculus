use std::char;

static mut TCNT: u32 = 0;

pub fn generate_tvariable() -> String {
  let mut a: u32;
  unsafe {
    TCNT += 1;
    a = TCNT;
  }

  let mut s = String::new();

  while a > 0 {
    let b = a % 10;
    a /= 10;
    s = format!("{}{}", char::from_u32(8320 + b).expect("panic"), s);
  }

  format!("x{}", s)
}

static mut CCNT: u32 = 0;

pub fn generate_cvariable() -> String {
  let mut a: u32;
  unsafe {
    CCNT += 1;
    a = CCNT;
  }

  let mut s = String::new();

  while a > 0 {
    let b = a % 10;
    a /= 10;
    s = format!("{}{}", char::from_u32(8320 + b).expect("panic"), s);
  }

  format!("α{}", s)
}

static mut KCNT: u32 = 0;

pub fn generate_kvariable() -> String {
  let mut a: u32;
  unsafe {
    KCNT += 1;
    a = KCNT;
  }

  let mut s = String::new();

  while a > 0 {
    let b = a % 10;
    a /= 10;
    s = format!("{}{}", char::from_u32(8320 + b).expect("panic"), s);
  }

  format!("k{}", s)
}
