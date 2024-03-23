/*
Exercise 1: Flattening
----------------------

Use the reduce method in combination with the concat method to “flatten” an
array of arrays into a single array that has all the elements of the original
arrays.
*/

function flatten(arrays) {
  return arrays.reduce((c, a) => c.concat(a), []);
}

console.log(flatten([]));
console.log(flatten([1]));
console.log(flatten([1, 2, 3, [4, 5], 6]));
console.log(flatten([[1, 2, 3], [4, 5], [6]]));

function flattenRec(x) {
  if (!Array.isArray(x)) return x;
  return x.reduce((c, a) => c.concat(flattenRec(a)), []);
}

console.log(flattenRec(42));
console.log(flattenRec([[[],[[]]]]));
console.log(flattenRec([[[1]]]));
console.log(flattenRec([[[],[[]]]]));
console.log(flattenRec([1, 2, [3, [4, 5], 6, [7, 8, [9, 10], 11], 12], 13]));
