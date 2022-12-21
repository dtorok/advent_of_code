use std::collections::HashMap;
use std::collections::VecDeque;


struct Parser<T:Iterator<Item=u8>>(T);

impl<T:Iterator<Item=u8>> Parser<T>
{
    fn find_marker(&mut self, marker_len: usize) -> usize {
        // keep stats of the items
        let mut marker: HashMap<u8, usize> = HashMap::new();

        // to know what to remove
        let mut marker_queue: VecDeque<u8> = VecDeque::new();

        let mut index = 0;

        while let Some(ch) = self.0.next() {
            if marker.len() >= marker_len {
                break;
            }

            if marker_queue.len() >= marker_len {
                let ch_right = marker_queue.pop_front().unwrap();
                let entry = marker.get_mut(&ch_right);
                if let Some(cnt) = entry {
                    if *cnt == 1 {
                        marker.remove(&ch_right);
                    } else {
                        *cnt -= 1;
                    }
                }

            }

            let cnt = marker.entry(ch).or_insert_with(|| 0);
            *cnt += 1;

            marker_queue.push_back(ch);

            index += 1;
        }

        index
    }
}

pub fn task1(input: String) -> usize {
    Parser(input.bytes()).find_marker(4)
}

pub fn task2(input: String) -> usize {
    Parser(input.bytes()).find_marker(14)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::helpers::test_helpers::load;
    use crate::helpers::test_helpers::InputType::{Input};

    #[test]
    fn test_01_sample() {
        assert_eq!(5, task1("bvwbjplbgvbhsrlpgdmjqwftvncz".to_string()));
        assert_eq!(6, task1("nppdvjthqldpwncqszvftbrmjlhg".to_string()));
        assert_eq!(10, task1("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg".to_string()));
        assert_eq!(11, task1("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw".to_string()));
    }

    #[test]
    fn test_01_input() {
        assert_eq!(1080, task1(load(6, 1, Input)));
    }

    #[test]
    fn test_02_sample() {
        assert_eq!(19, task2("mjqjpqmgbljsphdztnvjfqwrcgsmlb".to_string()));
        assert_eq!(23, task2("bvwbjplbgvbhsrlpgdmjqwftvncz".to_string()));
        assert_eq!(23, task2("nppdvjthqldpwncqszvftbrmjlhg".to_string()));
        assert_eq!(29, task2("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg".to_string()));
        assert_eq!(26, task2("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw".to_string()));
    }

    #[test]
    fn test_02_input() {
        assert_eq!(3645, task2(load(6, 1, Input)));
    }
}
