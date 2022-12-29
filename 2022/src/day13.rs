use std::cmp::{max, Ordering};
use serde::Deserialize;


#[derive(Deserialize, Debug)]
#[serde(untagged)]
enum Packet {
    Integer(usize),
    List(Vec<Packet>),
}

impl Packet {
    fn parse<'a>(input: &mut impl Iterator<Item=&'a str>) -> Option<Packet> {
        serde_json::from_str(input.filter(|&s| s != "").next()?).ok()
    }
}

#[derive(Debug)]
struct Signal {
    left: Packet,
    right: Packet,
}

impl Signal {
    fn parse<'a>(input: &mut impl Iterator<Item=&'a str>) -> Option<Signal> {
        Some(Signal { 
            left: Packet::parse(input)?,
            right: Packet::parse(input)?,
        })
    }

    fn compare_order(left: &Packet, right: &Packet) -> Ordering {
        use Packet::{Integer, List};

        match (left, right) {
            (Integer(v_left), Integer(v_right)) if v_left < v_right => {
                Ordering::Less
            }
            (Integer(v_left), Integer(v_right)) if v_left > v_right => {
                Ordering::Greater
            }
            (Integer(_), Integer(_)) => {
                Ordering::Equal
            }
            (List(l_left), List(l_right)) => {
                let len = max(l_left.len(), l_right.len());
                for i in 0..len {
                    if l_left.len() <= i {
                        return Ordering::Less;
                    } else if l_right.len() <= i {
                        return Ordering::Greater;
                    } else {
                        let c = Signal::compare_order(&l_left[i], &l_right[i]);
                        if !c.is_eq() {
                            return c;
                        }
                    }
                }

                Ordering::Equal
            }
            (List(_), Integer(v_right)) => {
                Signal::compare_order(left, &Packet::List(vec![Packet::Integer(*v_right)]))
            }
            (Integer(v_left), List(_)) => {
                Signal::compare_order(&Packet::List(vec![Packet::Integer(*v_left)]), right)
            }
        }
    }

    fn check_order(&self) -> Ordering {
        let left = &self.left;
        let right = &self.right;

        Signal::compare_order(left, right)
    }
}

pub fn task1(input: String) -> usize {
    let mut lines = input.lines();

    let mut cnt = 0;
    let mut i = 1;
    while let Some(signal) = Signal::parse(&mut lines) {
        if signal.check_order().is_le() {
            cnt += i;
        }
        i += 1;
    }

    cnt
}

pub fn task2(_: String) -> usize {
    todo!()
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::helpers::test_helpers::load;
    use crate::helpers::test_helpers::InputType::{Sample, Input};

    #[test]
    fn test_01_sample() {
        assert_eq!(13, task1(load(13, 1, Sample)));
    }

    #[test]
    fn test_01_input() {
        assert_eq!(5605, task1(load(13, 1, Input)));
    }

    #[test]
    fn test_02_sample() {
        assert_eq!(0, task2(load(13, 1, Sample)));
    }

    #[test]
    fn test_02_input() {
        assert_eq!(0, task2(load(13, 1, Input)));
    }

}