use std::iter::Peekable;


#[derive(Debug)]
struct File<'a> {
    // parent: Option<Box<FileEntry<'a>>>,
    name: &'a str,
    filesize: usize,
}

impl<'a> File<'a> {
    fn new(name: &'a str, filesize: usize) -> File<'a> {
        File {
            // parent: parent,
            name: name,
            filesize: filesize
        }
    }
}

#[derive(Debug)]
struct Dir<'a> {
    // parent: Option<Box<FileEntry<'a>>>,
    name: &'a str,
    comm_size: usize,
    file_entries: Vec<FileEntry<'a>>
}

impl<'a> Dir<'a> {
    fn new(name: &'a str, comm_size: usize, file_entries: Vec<FileEntry<'a>>) -> Dir<'a> {
        Dir {
            // parent: parent,
            name: name,
            comm_size: comm_size,
            file_entries: file_entries,
        }
    }

    fn get_dir_entries(&self) -> Vec<&Dir<'a>> {
        self.file_entries.iter().filter_map(|e| {
            match e {
                FileEntry::File(_) => None,
                FileEntry::Dir(dir) => Some(dir)
            }
        }).collect()
    }
}

#[derive(Debug)]
enum FileEntry<'a> {
    File(File<'a>),
    Dir(Dir<'a>)
}

impl<'a> FileEntry<'a> {
    // // fn new_file(parent: Option<Box<FileEntry<'a>>>, name: &'a str, size: usize) -> FileEntry<'a> {
    fn new_file(name: &'a str, filesize: usize) -> FileEntry<'a> {
        FileEntry::File(File::new(name, filesize))
    }

    // // fn new_dir(parent: Option<Box<FileEntry<'a>>>, name: &'a str, comm_size: usize, file_entries: &'a [FileEntry<'a>]) -> FileEntry<'a> {
    fn new_dir(name: &'a str, comm_size: usize, file_entries: Vec<FileEntry<'a>>) -> FileEntry<'a> {
        FileEntry::Dir(Dir::new(name, comm_size, file_entries))
    }

    fn size(&self) -> usize {
        match self {
            FileEntry::File(file) => file.filesize,
            FileEntry::Dir(dir) => dir.comm_size
        }
    }
}

enum Cmd<'a> {
    Cd(&'a str),
    Ls,
}

fn parse_cmd<'a>(line: &'a str) -> Cmd<'a> {
    let parts: &[&str] = &line.split(" ").collect::<Vec<&str>>();
    match parts[0] {
        "$" => 
            match parts[1] {
                "cd" => Cmd::Cd(parts[2]),
                "ls" => Cmd::Ls,
                _ => panic!("Unknown command: {} of \"{}\"", parts[1], line),
            }
        _ => panic!("Not a command: {}", line),
    }
}

fn next_ls_output_line<'a, I>(input: &mut Peekable<I>) -> Option<&'a str>
where 
    I: Iterator<Item=&'a str>
{
    match input.peek() {
        None => Option::None,
        Some(&s) => {
            dbg!(s);
            if !s.starts_with("$") {
                dbg!("$");
                input.next()
            } else {
                None
            }
        }
    }
}

fn parse_ls_output<'a, I>(input: &mut Peekable<I>) -> Vec<FileEntry<'a>>
where
    I: Iterator<Item=&'a str>
{
    let mut file_entries: Vec<FileEntry<'a>> = vec![];

    while let Some(next) = next_ls_output_line(input) {
        let parts: Vec<&'a str> = next.split(" ").collect();
        if parts[0] == "dir" {
            continue;
        }

        let (filesize, name) = (parts[0].parse::<usize>().expect(parts[1]), parts[1]);
        file_entries.push(FileEntry::new_file(name, filesize));
    }

    file_entries
}

fn parse_input<'a, I>(input: &mut Peekable<I>, name: &'a str) -> Dir<'a> 
where
    I: Iterator<Item=&'a str>
{
    dbg!("Parsing", name);

    let mut file_entries: Vec<FileEntry<'a>> = vec![];
    let mut comm_size = 0;

    while let Some(line) = input.next() {
    // while let l = input.next() {
    //     dbg!(&l);
    //     let line = l.unwrap();
        dbg!(line);
        match parse_cmd(line) {
            Cmd::Cd("..") => {
                break;
            }
            Cmd::Cd("/") => {
                continue;
            }
            Cmd::Cd(dirname) => {
                let dir = parse_input(input, dirname);
                let dir_entry = FileEntry::Dir(dir);
                
                comm_size += dir_entry.size();
                file_entries.push(dir_entry);
            }
            Cmd::Ls => {
                let mut files = parse_ls_output(input);
                comm_size += files.iter().map(FileEntry::size).sum::<usize>();
                file_entries.append(&mut files);
            }
        }
    };

    Dir::new(name, comm_size, file_entries)
}

fn dir_sizes_smaller_than(root: &Dir, limit: usize) -> usize {
    let mut num = 0;

    if root.comm_size <= limit {
        num += root.comm_size;
    }

    for dir in root.get_dir_entries() {
        num += dir_sizes_smaller_than(dir, limit)
    }

    num
}

pub fn task1(input: String) -> usize {
    // dbg!(input.lines().collect::<Vec<&str>>());
    let root = parse_input(&mut input.lines().peekable(), "/");
    dir_sizes_smaller_than(&root, 100000)
}

pub fn task2(input: String) -> usize {
    0
}


#[cfg(test)]
mod test {
    use super::*;
    use crate::helpers::test_helpers::load;
    use crate::helpers::test_helpers::InputType::{Sample, Input};

    #[test]
    fn test_01_sample() {
        assert_eq!(95437, task1(load(7, 1, Sample)));
    }

    #[test]
    fn test_01_input() {
        assert_eq!(2104783, task1(load(7, 1, Input)));
    }

    #[test]
    fn test_02_sample() {
        assert_eq!(0, task2(load(7, 1, Sample)));
    }

    #[test]
    fn test_02_input() {
        assert_eq!(0, task2(load(7, 1, Input)));
    }

}
