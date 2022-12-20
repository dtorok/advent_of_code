// use core::slice::SlicePattern;
use scanf::sscanf;


// ===
// COMMAND
// ===
#[derive(Debug)]
struct Command {
    from: usize,
    to: usize,
    amount: u8
}

impl Command {
    fn new(from: usize, to: usize, amount: u8) -> Command {
        Command {
            from: from,
            to: to,
            amount: amount
        }
    }
}

// ===
// CRATE
// ===
#[derive(Debug, Clone)]
struct Crate(char);

// ===
// STACK
// ===
#[derive(Debug, Clone)]
struct Stack(Vec<Crate>);

impl Stack {
    fn new() -> Stack {
        Stack(vec![])
    }

    fn push(&mut self, cr: Crate) {
        self.0.push(cr);
    }

    fn reverse(&mut self) {
        self.0.reverse();
    }

    fn pop(&mut self) -> Crate {
        self.0.pop().unwrap()
    }

    fn peep(&self) -> Option<&Crate> {
        self.0.last()
    }
}

// ===
// CARGO
// ===
#[derive(Debug)]
struct Cargo {
    stacks: Vec<Stack>
}

impl Cargo {
    fn new() -> Cargo {
        Cargo {
            stacks: vec![Stack::new(); 1]
        }
    }

    fn reverse_all_stacks(&mut self) {
        self.stacks
            .iter_mut()
            .for_each(|st| {
                st.reverse();
            });
    }

    fn run_command(&mut self, cmd: &Command) {
        for _ in 0..cmd.amount {
            let cr = self.stacks[cmd.from].pop();
            self.stacks[cmd.to].push(cr);
        }
    }

    fn get_top_crates(&self) -> Vec<&Crate> {
        self.stacks
            .iter()
            .map(Stack::peep)
            .filter(Option::is_some)
            .map(Option::unwrap)
            .collect()
    }

    fn get_or_create_stack(&mut self, index: usize) -> &mut Stack {
        let l = self.stacks.len();
        if l <= index {
            let ex = vec![Stack::new(); index - l + 1];
            self.stacks.extend_from_slice(ex.as_slice());
        }

        &mut self.stacks[index]
    }
}

struct Parser{}

impl Parser {
    fn parse_cargo_line(line: &str, cargo: &mut Cargo) {
        let line = line.as_bytes();
        let l = line.len();
        let mut n = 0;
        let mut i = 0;
        while n + 1 < l {
            let left = line[n];
            let ch = line[n + 1];

            if left == '[' as u8 && ch != ' ' as u8 {
                // cargo.stacks[i].push(Crate(ch as char))
                let stack: &mut Stack = cargo.get_or_create_stack(i);
                stack.push(Crate(ch as char));
            }

            n += 4;
            i += 1;
        }
    }

    fn parse_cargo<'a, I>(input: &mut I) -> Cargo 
    where
        I: Iterator<Item=&'a str>
    {
        let mut cargo = Cargo::new();

        while let Some(l) = input.next() {
            if l.trim() == "" {
                break;
            }

            Parser::parse_cargo_line(l, &mut cargo);
        }

        cargo.reverse_all_stacks();

        cargo
    }

    fn parse_command_line(line: &str) -> Command {
        let mut from: usize = 0;
        let mut to: usize = 0;
        let mut amount: u8 = 0;

        _ = sscanf!(line, "move {} from {} to {}", amount, from, to);

        // indexing by 0
        Command::new(from - 1, to - 1, amount)
    }

    fn parse_commands<'a, I>(input: &mut I) -> Vec<Command>
    where
        I: Iterator<Item=&'a str>
    {
        let mut commands: Vec<Command> = vec![];

        while let Some(line) = input.next() {
            commands.push(Parser::parse_command_line(line));
        }

        commands
    }
}

pub fn task1(input: String) -> String {
    let mut iter = input.split("\n");

    let mut cargo = Parser::parse_cargo(&mut iter);
    let cmds = Parser::parse_commands(&mut iter);

    for cmd in cmds.iter() {
        cargo.run_command(cmd);
    }

    cargo.get_top_crates().iter().map(|Crate(ch)| ch).collect()
}

pub fn task2(input: String) -> String {
    "".to_string()
}


#[cfg(test)]
mod test {
    use super::*;
    use crate::helpers::test_helpers::load;
    use crate::helpers::test_helpers::InputType::{Sample, Input};

    #[test]
    fn test_01_sample() {
        assert_eq!("CMZ".to_string(), task1(load(5, 1, Sample)));
    }

    #[test]
    fn test_01_input() {
        assert_eq!("TLNGFGMFN".to_string(), task1(load(5, 1, Input)));
    }

    #[test]
    fn test_02_sample() {
        assert_eq!("".to_string(), task2(load(5, 1, Sample)));
    }

    #[test]
    fn test_02_input() {
        assert_eq!("".to_string(), task2(load(5, 1, Input)));
    }

}
