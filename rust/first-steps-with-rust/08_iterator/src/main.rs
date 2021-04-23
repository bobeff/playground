struct Groups<T> {
    inner: Vec<T>,
    current_index: usize,
}

impl<T> Groups<T> {
    fn new(inner: Vec<T>) -> Self {
        Groups {
            inner,
            current_index: 0,
        }
    }
}

impl<T: PartialEq + Copy> Iterator for Groups<T> {
    type Item = Vec<T>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_index == self.inner.len() {
            return None;
        }

        let mut result = Vec::<T>::new();
        let mut current_value = self.inner[self.current_index];
        result.push(current_value);

        while self.current_index < self.inner.len() - 1
            && current_value == self.inner[self.current_index + 1]
        {
            self.current_index += 1;
            current_value = self.inner[self.current_index];
            result.push(current_value);
        }

        self.current_index += 1;
        Some(result)
    }
}

fn main() {
    let data = vec![4, 1, 1, 2, 1, 3, 3, -2, -2, -2, 5, 5];
    // groups:     |->|---->|->|->|--->|----------->|--->|
    assert_eq!(
        Groups::new(data).into_iter().collect::<Vec<Vec<_>>>(),
        vec![
            vec![4],
            vec![1, 1],
            vec![2],
            vec![1],
            vec![3, 3],
            vec![-2, -2, -2],
            vec![5, 5],
        ]
    );

    let data2 = vec![1, 2, 2, 1, 1, 2, 2, 3, 4, 4, 3];
    // groups:      |->|---->|---->|----|->|----->|->|
    assert_eq!(
        Groups::new(data2).into_iter().collect::<Vec<Vec<_>>>(),
        vec![
            vec![1],
            vec![2, 2],
            vec![1, 1],
            vec![2, 2],
            vec![3],
            vec![4, 4],
            vec![3],
        ]
    )
}
