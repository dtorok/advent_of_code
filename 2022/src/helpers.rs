trait IntoTuple2<T> {
    fn into_tuple_2(&self) -> (T, T);
}

pub fn v2t<T: Copy>(from: Vec<T>) -> (T, T) {
    (from[0], from[1])
}

pub mod test_helpers {
    use std::fs;

    #[allow(dead_code)]
    pub enum InputType {
        Sample,
        Input
    }

    #[allow(dead_code)]
    pub fn load(day: u8, task: u8, typ: InputType) -> String {
        let filename = format!(
            "input/day{:02}_{:02}.{}",
            day,
            task,
            match typ {
                InputType::Sample => "sample",
                InputType::Input => "input"
            }
        );

        fs::read_to_string(filename).expect("Couldn't open file")
    }
}