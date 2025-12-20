fn add(x: i32, y: i32) -> i32 {
    let sum: i32 = x + y;
    sum
}

fn main() -> i32 {
    let result = add(40, 2);
    if result == 42 then 0 else 1
}
