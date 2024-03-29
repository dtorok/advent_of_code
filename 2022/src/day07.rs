use std::iter::Peekable;

const TASK1_SIZE_LIMIT: usize = 100000;
const TASK2_DISK_SIZE: usize = 70000000;
const TASK2_SPACE_REQUIRED: usize = 30000000;

// ===
// File
// ===
#[derive(Debug)]
struct File<'a> {
    // name not used, but a file without a name?!?
    #[allow(dead_code)]
    name: &'a str,
    filesize: usize,
}

impl<'a> File<'a> {
    fn new(name: &'a str, filesize: usize) -> File<'a> {
        File {
            name: name,
            filesize: filesize
        }
    }
}

// ===
// Dir
// ===
#[derive(Debug)]
struct Dir<'a> {
    // name not used, but a dir without a name?!?
    #[allow(dead_code)]
    name: &'a str,
    comm_size: usize,
    file_entries: Vec<FileEntry<'a>>
}

impl<'a> Dir<'a> {
    fn new(name: &'a str, comm_size: usize, file_entries: Vec<FileEntry<'a>>) -> Dir<'a> {
        Dir {
            name: name,
            comm_size: comm_size,
            file_entries: file_entries,
        }
    }

    fn iter<'d>(&'a self) -> DirIterator<'a, 'd> {
        DirIterator::new(self)
    }
}

// ===
// DirIterator
// ===
struct DirIterator<'a, 'd>
{
    iterators: Vec<std::slice::Iter<'d, FileEntry<'a>>>,
}

impl<'a, 'd> DirIterator<'a, 'd>
{
    fn new(root: &'d Dir<'a>) -> DirIterator<'a, 'd> {
        let i = root.file_entries.iter();
        DirIterator {
            iterators: vec![i]
        }
    }
}

impl<'a, 'd> Iterator for DirIterator<'a, 'd>
{
    type Item = &'d FileEntry<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(last) = self.iterators.last_mut() {
            let item = last.next();
            match item {
                None => {
                    self.iterators.pop();
                },
                Some(FileEntry::File(_)) => {
                    return item;
                },
                Some(FileEntry::Dir(dir)) => {
                    self.iterators.push(dir.file_entries.iter());
                    return item;
                }

            }
        }

        None
    }

}

// ===
// FileEntry
// ===
#[derive(Debug)]
enum FileEntry<'a> {
    File(File<'a>),
    Dir(Dir<'a>)
}

impl<'a> FileEntry<'a> {
    fn size(&self) -> usize {
        match self {
            FileEntry::File(file) => file.filesize,
            FileEntry::Dir(dir) => dir.comm_size
        }
    }

    fn as_dir(&self) -> Option<&Dir<'a>> {
        match &self {
            FileEntry::File(_) => None,
            FileEntry::Dir(dir) => Some(dir)
        }
    }
}

// ===
// Cmd
// ===
enum Cmd<'a> {
    Cd(&'a str),
    Ls,
}

impl<'a> Cmd<'a> {
    fn parse(line: &'a str) -> Cmd<'a> {
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
}

// ===
// LsOutputIterator
// ===
struct LsOutputIterator<'a, 'b, I>(&'b mut Peekable<I>)
where
    I: Iterator<Item=&'a str>;

impl<'a, 'b, I> Iterator for LsOutputIterator<'a, 'b, I>
where
   I: Iterator<Item=&'a str>
{
   type Item = &'a str;

   fn next(&mut self) -> Option<&'a str> {
       match self.0.peek() {
           None => Option::None,
           Some(&s) => {
               if !s.starts_with("$") {
                   self.0.next()
               } else {
                   None
               }
           }
       }
   }
}

// ===
// Parser
// ===
struct Parser();

impl Parser {
    fn parse_ls_files<'a, I>(input: &mut Peekable<I>) -> Vec<FileEntry<'a>>
    where
        I: Iterator<Item=&'a str>
    {
        let mut file_entries: Vec<FileEntry<'a>> = vec![];

        for next in LsOutputIterator(input) {
            let parts: Vec<&'a str> = next.split(" ").collect();
            if parts[0] == "dir" {
                continue;
            }

            let (filesize, name) = (parts[0].parse::<usize>().expect(parts[1]), parts[1]);
            file_entries.push(
                FileEntry::File(File::new(name, filesize))
            )
        }

        file_entries
    }

    fn parse<'a, I>(input: &mut Peekable<I>, name: &'a str) -> Dir<'a> 
    where
        I: Iterator<Item=&'a str>
    {
        let mut file_entries: Vec<FileEntry<'a>> = vec![];
        let mut comm_size = 0;

        while let Some(line) = input.next() {
            match Cmd::parse(line) {
                Cmd::Cd("..") => {
                    break;
                }
                Cmd::Cd("/") => {
                    continue;
                }
                Cmd::Cd(dirname) => {
                    let dir = Self::parse(input, dirname);
                    let dir_entry = FileEntry::Dir(dir);

                    comm_size += dir_entry.size();
                    file_entries.push(dir_entry);
                }
                Cmd::Ls => {
                    let mut files = Self::parse_ls_files(input);
                    comm_size += files.iter().map(FileEntry::size).sum::<usize>();
                    file_entries.append(&mut files);
                }
            }
        };

        Dir::new(name, comm_size, file_entries)
    }
}

// ===
// TASKS
// ===
pub fn task1(input: String) -> usize {
    let root = Parser::parse(&mut input.lines().peekable(), "/");

    root
        .iter()
        .filter_map(FileEntry::as_dir)
        .filter(|d| d.comm_size <= TASK1_SIZE_LIMIT)
        .map(|d| d.comm_size)
        .sum()
}

pub fn task2(input: String) -> usize {
    let root = Parser::parse(&mut input.lines().peekable(), "/");
    let space_required = TASK2_SPACE_REQUIRED - (TASK2_DISK_SIZE - root.comm_size);

    let mut dir_sizes = root
        .iter()
        .filter_map(FileEntry::as_dir)
        .filter(|d| d.comm_size >= space_required)
        .map(|d| d.comm_size)
        .collect::<Vec<usize>>();

    dir_sizes.sort();

    dir_sizes[0]
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
        assert_eq!(24933642, task2(load(7, 1, Sample)));
    }

    #[test]
    fn test_02_input() {
        assert_eq!(5883165, task2(load(7, 1, Input)));
    }

}
