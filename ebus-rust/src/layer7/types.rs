use crate::model;
use std::convert::TryFrom;
use std::ops::Neg;

#[derive(PartialEq, Debug)]
pub enum Value {
    Bool(bool),
    I8(i8),
    U8(u8),
    U16(u16),
    F32(f32),
    String(String),
}

impl Value {
    pub fn convert(&self) -> String {
        match self {
            Value::Bool(v) => format!("{}", v),
            Value::I8(v) => format!("{}", v),
            Value::U8(v) => format!("{}", v),
            Value::U16(v) => format!("{}", v),
            Value::F32(v) => format!("{}", v),
            Value::String(v) => v.clone(),
        }
    }
}

#[inline]
fn low_nibble(v: u8) -> u8 {
    0x0f & v
}

#[inline]
fn high_nibble(v: u8) -> u8 {
    v >> 4
}

pub fn bcd(data: &[u8]) -> Option<Value> {
    data.get(0).and_then(|value| {
        if *value == 0xff {
            None // Ersatzwert
        } else {
            Some(Value::U8(((*value & 0xf0) >> 4) * 10 + (*value & 0x0f)))
        }
    })
}

pub fn word(data: &[u8]) -> Option<Value> {
    data.get(0..2)
        .and_then(|data| <[u8; 2]>::try_from(data).ok())
        .and_then(|[low, high]| {
            if low == 0xff && high == 0xff {
                None // Ersatzwert
            } else {
                Some(Value::U16(low as u16 | (high as u16) << 8))
            }
        })
}

pub fn byteenum(data: &[u8], options: &model::ByteEnumList) -> Option<Value> {
    data.get(0).and_then(|v| options.get(*v)).map(Value::String)
}

pub fn data2c(data: &[u8]) -> Option<Value> {
    data.get(0..2)
        .and_then(|data| <[u8; 2]>::try_from(data).ok())
        .and_then(|[low, high]| {
            if high == 0x80 && low == 0x00 {
                None
            } else if high & 0x80 == 0x80 {
                Some(Value::F32(
                    ((((!high as u16) * 16) + (high_nibble(!low) as u16)) as f32
                        + ((low_nibble(!low) + 1) as f32 / 16.0))
                        .neg(),
                ))
            } else {
                Some(Value::F32(
                    ((high as u16) * 16 + high_nibble(low) as u16) as f32
                        + low_nibble(low) as f32 / 16.0,
                ))
            }
        })
}

pub fn data2b(data: &[u8]) -> Option<Value> {
    data.get(0..2)
        .and_then(|data| <[u8; 2]>::try_from(data).ok())
        .and_then(|[low, high]| {
            if high == 0x80 && low == 0x00 {
                None
            } else if high & 0x80 == 0x80 {
                Some(Value::F32(
                    ((!high as f32) + ((!low as f32 + 1.0) / 256.0_f32)).neg(),
                ))
            } else {
                Some(Value::F32(high as f32 + (low as f32 / 256.0)))
            }
        })
}

pub fn bit(data: &[u8]) -> Option<Value> {
    data.get(0).map(|v| *v == 1).map(Value::Bool)
}

pub fn byte(data: &[u8]) -> Option<Value> {
    data.get(0).and_then(|value| {
        if *value == 0xff {
            None
        } else {
            Some(Value::U8(*value))
        }
    })
}

pub fn data1b(data: &[u8]) -> Option<Value> {
    data.get(0).and_then(|value| {
        if *value == 0x80 {
            None
        } else if *value >> 7 == 1 {
            Some(Value::I8(((1 + !(*value)) as i8).neg()))
        } else {
            Some(Value::I8(*value as i8))
        }
    })
}

pub fn data1c(data: &[u8]) -> Option<Value> {
    data.get(0).and_then(|value| {
        if *value == 0xff {
            None
        } else {
            Some(Value::F32(*value as f32 / 2.0))
        }
    })
}

pub fn string(data: &[u8], length: usize) -> Option<Value> {
    data.get(0..length)
        .map(|data| {
            data.iter()
                .map(|p| *p as char)
                .filter(|p| {
                    *p >= 'a' && *p <= 'z' || *p >= 'A' && *p <= 'Z' || *p >= ' ' && *p <= '9'
                })
                .map(String::from)
                .collect::<Vec<String>>()
                .join("")
        })
        .map(Value::String)
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_f32_eq_approx {
        ($x:expr, $y:expr, $Δ:expr) => {
            match $y {
                Some(Value::F32(v)) => {
                    if !($x - v < $Δ && v - $x < $Δ) {
                        panic!("Left({}) != Right({}) Δ={}", $x, v, $Δ);
                    }
                }
                _ => panic!("Left({}) != Right(None)", $x),
            }
        };
    }

    #[test]
    fn test_decoders() {
        // Bei allen 16-Bit Typen (2 Byte), wird das Low-Byte immer zuerst übertragen.
        assert_eq!(None, bcd(&[0xff]));
        assert_eq!(Some(Value::U8(0)), bcd(&[0x00]));
        assert_eq!(Some(Value::U8(1)), bcd(&[0x01]));
        assert_eq!(Some(Value::U8(2)), bcd(&[0x02]));
        assert_eq!(Some(Value::U8(10)), bcd(&[0x10]));
        assert_eq!(Some(Value::U8(11)), bcd(&[0x11]));
        assert_eq!(Some(Value::U8(99)), bcd(&[0x99]));

        assert_eq!(Some(Value::I8(0)), data1b(&[0x00]));
        assert_eq!(Some(Value::I8(1)), data1b(&[0x01]));
        assert_eq!(Some(Value::I8(127)), data1b(&[0x7f]));
        assert_eq!(Some(Value::I8(-127)), data1b(&[0x81]));
        assert_eq!(None, data1b(&[0x80]));

        assert_eq!(Some(Value::F32(0.0)), data1c(&[0]));
        assert_eq!(Some(Value::F32(50.0)), data1c(&[0x64]));
        assert_eq!(Some(Value::F32(100.0)), data1c(&[0xc8]));

        assert_eq!(Some(Value::F32(0.0)), data2b(&[0x00, 0x00]));
        assert_eq!(Some(Value::F32(1.0 / 256.0)), data2b(&[0x01, 0x00,]));
        assert_eq!(Some(Value::F32(-1.0 / 256.0)), data2b(&[0xff, 0xff]));
        assert_eq!(Some(Value::F32(-1.0)), data2b(&[0x00, 0xff]));
        assert_eq!(None, data2b(&[0x00, 0x80]));
        assert_f32_eq_approx!(-127.996, data2b(&[0x01, 0x80]), 0.001);
        assert_f32_eq_approx!(127.99609, data2b(&[0xff, 0x7f]), 0.001);
        assert_f32_eq_approx!(52.7, data2b(&[0xb3, 0x34]), 0.1);

        assert_f32_eq_approx!(0.0, data2c(&[0x00, 0x00]), f32::EPSILON);
        assert_f32_eq_approx!((1.0 / 16.0), data2c(&[0x01, 0x00]), f32::EPSILON);
        assert_f32_eq_approx!((-1.0 / 16.0), data2c(&[0xff, 0xff]), f32::EPSILON);
        assert_f32_eq_approx!(-1.0, data2c(&[0xf0, 0xff]), f32::EPSILON);
        assert_eq!(None, data2c(&[0x00, 0x80]));
        assert_f32_eq_approx!(-2047.9, data2c(&[0x01, 0x80]), 0.1);
        assert_f32_eq_approx!(2047.9, data2c(&[0xff, 0x7f]), 0.1);

        assert_eq!(None, word(&[0xff, 0xff]));
        assert_eq!(Some(Value::U16(65279)), word(&[0xff, 0xfe]));
        assert_eq!(Some(Value::U16(256)), word(&[0x00, 0x01]));
        assert_eq!(Some(Value::U16(1)), word(&[0x01, 0x00]));
        assert_eq!(Some(Value::U16(0)), word(&[0x00, 0x00]));
    }
}
