use std::cmp::{max, Ordering};
use serde::Deserialize;


#[derive(Deserialize, Debug, Eq)]
#[serde(untagged)]
enum Packet {
    Integer(usize),
    List(Vec<Packet>),
}

struct PacketParserIterator<'a, I: Iterator<Item=&'a str>>(&'a mut I);

impl<'a, I: Iterator<Item=&'a str>> Iterator for PacketParserIterator<'a, I> {
    type Item = Packet;

    fn next(&mut self) -> Option<Self::Item> {
        serde_json::from_str(self.0.filter(|&s| s != "").next()?).ok()
    }
}

impl Ord for Packet {
    fn cmp(&self, other: &Self) -> Ordering {
        use Packet::{Integer, List};

        match (self, other) {
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
                        let c = l_left[i].cmp(&l_right[i]);
                        if !c.is_eq() {
                            return c;
                        }
                    }
                }

                Ordering::Equal
            }
            (List(_), Integer(v_right)) => {
                self.cmp(&Packet::List(vec![Packet::Integer(*v_right)]))
            }
            (Integer(v_left), List(_)) => {
                let p = Packet::List(vec![Packet::Integer(*v_left)]);
                p.cmp(other)
            }
        }
    }
}

impl PartialOrd for Packet {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Packet {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other).is_eq()
    }
}

#[derive(Debug)]
struct Signal {
    left: Packet,
    right: Packet,
}

impl Signal {
    fn is_in_order(&self) -> bool {
        self.left < self.right
    }
}

struct SignalParserIterator<'a, I: Iterator<Item=&'a str>>(PacketParserIterator<'a, I>);

impl<'a, I: Iterator<Item=&'a str>> SignalParserIterator<'a, I> {
    fn from(input: &'a mut I) -> SignalParserIterator<'a, I> {
        SignalParserIterator(PacketParserIterator(input))
    }
}

impl<'a, I: Iterator<Item=&'a str>> Iterator for SignalParserIterator<'a, I> {
    type Item = Signal;

    fn next(&mut self) -> Option<Self::Item> {
        Some(Signal {
            left: self.0.next()?,
            right: self.0.next()?,
        })
    }
}

pub fn task1(input: String) -> usize {
    let mut lines = input.lines();

    let mut cnt = 0;
    let mut i = 1;
    for signal in SignalParserIterator::from(&mut lines) {
        if signal.is_in_order() {
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