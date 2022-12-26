#[derive(Debug)]
struct Mem {
    x: i32,
    cycle_count: i32,
}

struct CPU<F>
where
    F: FnMut(&Mem) -> ()
{
    mem: Mem,
    cycle_listener: F,
}

impl<F> CPU<F>
where
    F: FnMut(&Mem) -> ()
{
    fn new(cycle_listener: F) -> CPU<F> {
        CPU {
            mem: Mem {
                x: 1,
                cycle_count: 0,
            },
            cycle_listener: cycle_listener
        }
    }

    fn cycle(&mut self, num: usize) {
        for _ in 0..num {
            self.mem.cycle_count += 1;
            (self.cycle_listener)(&self.mem)
        }
    }
}

enum Cmd {
    NoOp,
    AddX(i32),
}

impl Cmd {
    fn parse(line: &str) -> Cmd {
        let parts: Vec<&str> = line.split(" ").collect();

        match parts[0] {
            "noop" => Cmd::NoOp,
            "addx" => {
                let value: i32 = parts[1].parse().expect("Expected number after addx");
                Cmd::AddX(value)
            },
            _ => panic!("Unknown command")
        }
    }

    fn execute<F>(&self, cpu: &mut CPU<F>)
    where
        F: FnMut(&Mem) -> ()
    {
        match self {
            Cmd::NoOp => {
                cpu.cycle(1);
            },
            Cmd::AddX(value) => {
                cpu.cycle(2);
                cpu.mem.x += value;
            }
        }
    }
}

pub fn task1(input: String) -> i32 {
    let mut score: i32 = 0;
    let mut cpu = CPU::new(
        |mem| {
            if mem.cycle_count >= 20 && (mem.cycle_count - 20) % 40 == 0 {
                score += mem.cycle_count * mem.x;
            }
        }
    );
    let cmds = input.split("\n").map(Cmd::parse);
    for cmd in cmds {
        cmd.execute(&mut cpu);
    }

    score
}

pub fn task2(input: String) -> String {
    let mut output = String::from("");

    let mut cpu = CPU::new(
        |mem| {
            let cc = mem.cycle_count - 1;

            let pos = cc % 40;
            if pos == 0 {
                output.push('\n');
            }
            if mem.x >= pos - 1 && mem.x <= pos + 1 {
                output.push('#');
            } else {
                output.push('.');
            }
        }
    );
    let cmds = input.split("\n").map(Cmd::parse);
    for cmd in cmds {
        cmd.execute(&mut cpu);
    }

    output
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::helpers::test_helpers::load;
    use crate::helpers::test_helpers::InputType::{Sample, Input};

    #[test]
    fn test_01_sample() {
        assert_eq!(13140, task1(load(10, 1, Sample)));
    }

    #[test]
    fn test_01_input() {
        assert_eq!(13920, task1(load(10, 1, Input)));
    }

    #[test]
    fn test_02_sample() {
        let expected_output = "\n\
        ##..##..##..##..##..##..##..##..##..##..\n\
        ###...###...###...###...###...###...###.\n\
        ####....####....####....####....####....\n\
        #####.....#####.....#####.....#####.....\n\
        ######......######......######......####\n\
        #######.......#######.......#######.....";
        assert_eq!(expected_output.to_string(), task2(load(10, 1, Sample)));
    }

    #[test]
    fn test_02_input() {
        let expected_output = "\n\
        ####..##..#....#..#.###..#....####...##.\n\
        #....#..#.#....#..#.#..#.#....#.......#.\n\
        ###..#....#....####.###..#....###.....#.\n\
        #....#.##.#....#..#.#..#.#....#.......#.\n\
        #....#..#.#....#..#.#..#.#....#....#..#.\n\
        ####..###.####.#..#.###..####.#.....##..";
        assert_eq!(expected_output.to_string(), task2(load(10, 1, Input)));
    }

}