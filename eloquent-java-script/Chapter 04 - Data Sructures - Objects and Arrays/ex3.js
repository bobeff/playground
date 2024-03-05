/*
A list

As generic blobs of values, objects can be used to build all sorts of data
structures. A common data structure is the list (not to be confused with arrays).
A list is a nested set of objects, with the first object holding a reference to
the second, the second to the third, and so on:

let list = {
  value: 1,
  rest: {
    value: 2,
    rest: {
      value: 3,
      rest: null
    }
  }
};

The resulting objects form a chain, as shown in the following diagram:

--------------
| value: 1   |       --------------
| rest ------|------>| value: 2   |       --------------
--------------       | rest ------|------>| value: 3   |
                     --------------       | rest: null |
                                          --------------

A diagram showing the memory structure of a linked list. There are 3 cells, each
with a value field holding a number, and a 'rest' field with an arrow to the rest
of the list. The first cell's arrow points at the second cell, the second cell's
arrow at the last cell, and the last cell's 'rest' field holds null.

A nice thing about lists is that they can share parts of their structure. For
example, if I create two new values {value: 0, rest: list} and {value: -1, rest: list}
(with list referring to the binding defined earlier), they are both independent
lists, but they share the structure that makes up their last three elements. The
original list is also still a valid three-element list.

Write a function arrayToList that builds up a list structure like the one shown
when given [1, 2, 3] as argument. Also write a listToArray function that produces
an array from a list. Add the helper functions prepend, which takes an element
and a list and creates a new list that adds the element to the front of the input
list, and nth, which takes a list and a number and returns the element at the
given position in the list (with zero referring to the first element) or undefined
when there is no such element.

If you haven’t already, also write a recursive version of nth.
*/

function prepend(value, rest) {
  rest = rest || null;
  return { value, rest };
}

function arrayToList(arr) {
  let list = null;
  for (let i = arr.length - 1; i >= 0; --i) {
    list = prepend(arr[i], list);
  }
  return list;
}

function listToArray(list) {
  if (!list) return [];
  let result = [];
  for (let current = list; current; current = current.rest) {
    result.push(current.value);
  }
  return result;
}

function nth(list, n) {
  if (!list) return undefined;
  if (n == 0) return list.value;
  else return nth(list.rest, n - 1);
}

console.log(arrayToList([10, 20]));
console.log(listToArray(arrayToList([10, 20, 30])));
console.log(prepend(10, prepend(20, null)));

let list = arrayToList([10, 20, 30]);
console.log(nth(list, 0));
console.log(nth(list, 1));
console.log(nth(list, 2));
console.log(nth(list, 3));
