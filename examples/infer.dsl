fn infer_sum(a: i32, b: i32) -> i32 {
    let total = a + b;
    let doubled = {
        let inner = total + 1;
        inner * 2
    };
    doubled
}

fn demo() -> i32 {
    infer_sum(5, 7)
}
