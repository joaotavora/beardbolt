fn is_rms(a: char) -> i32 {
    match a {
        'R' => 1,
        'M' => 2,
        'S' => 3,
        _ => 0,
    }
}

fn main() {
    let a: u8 = 1 + 1;
    if is_rms(a as char) != 0 {
        println!("{}", a);
    };
    42;
}
// rust beardbolt starter file

// Local Variables:
// beardbolt-command: "rustc -C opt-level=0"
// beardbolt-preserve-library-functions: nil
// beardbolt-demangle: t
// beardbolt-disassemble: nil
// End:


