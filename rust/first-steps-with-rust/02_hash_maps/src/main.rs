use std::collections::HashMap;

fn fruit_basket() -> HashMap<String, u32> {
    let mut basket = HashMap::<String, u32>::new();

    // Two bananas are already given for you :)
    basket.insert(String::from("banana"), 2);
    basket.insert(String::from("apple"), 2);
    basket.insert(String::from("mango"), 1);

    basket
}

fn main() {
    let basket = fruit_basket();
    assert!(
        basket.len() >= 3,
        "basket must have at least three types of fruits"
    );
    assert!(
        basket.values().sum::<u32>() >= 5,
        "basket must have at least five fruits"
    );
}
