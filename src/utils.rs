pub fn add_option_to_str(initial: &mut Option<String>, other: &Option<String>) {
    if let Some(o) = other {
        if let Some(init) = initial {
            init.push_str(o);
            init.push('\n');
        }
        *initial = Some(o.to_owned());
    }
}
