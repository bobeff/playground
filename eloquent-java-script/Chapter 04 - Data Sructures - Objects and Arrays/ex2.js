/*
Arrays have a reverse method that changes the array by inverting the order in
which its elements appear. For this exercise, write two functions, reverseArray
and reverseArrayInPlace. The first, reverseArray, should take an array as
argument and produce a new array that has the same elements in the inverse order.
The second, reverseArrayInPlace, should do what the reverse method does: modify
the array given as argument by reversing its elements. Neither may use the
standard reverse method.

Thinking back to the notes about side effects and pure functions in the previous
chapter, which variant do you expect to be useful in more situations? Which one
runs faster?
*/

function reverseArray(arr) {
  let result = [];
  for (let i = arr.length - 1; i >= 0; --i) {
    result.push(arr[i]);
  }
  return result;
}

function reverseArrayInPlace(arr) {
  for (let i = 0, j = arr.length - 1; i < j; ++i, --j) {
    [arr[i], arr[j]] = [arr[j], arr[i]];
  }
}

let myArray = ["A", "B", "C"];
console.log(reverseArray(myArray));

let arrayValue = [1, 2, 3, 4, 5];
reverseArrayInPlace(arrayValue);
console.log(arrayValue);
