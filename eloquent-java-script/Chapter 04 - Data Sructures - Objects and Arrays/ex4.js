/*
Exercise 4: Deep comparison
---------------------------

The == operator compares objects by identity, but sometimes you’d prefer to
compare the values of their actual properties.

Write a function deepEqual that takes two values and returns true only if they
are the same value or are objects with the same properties, where the values of
the properties are equal when compared with a recursive call to deepEqual.

To find out whether values should be compared directly (using the === operator
for that) or have their properties compared, you can use the typeof operator.
If it produces "object" for both values, you should do a deep comparison. But
you have to take one silly exception into account: because of a historical
accident, typeof null also produces "object".

The Object.keys function will be useful when you need to go over the properties
of objects to compare them.
*/

function deepEqual(lhs, rhs) {
  if (lhs === null && rhs === null) return true;
  if (typeof lhs === "object" && typeof rhs === "object") {
    const lhsKeys = Object.keys(lhs);
    const rhsKeys = new Set(Object.keys(rhs));;
    if (lhsKeys.length !== rhsKeys.size) return false;
    for (let key of lhsKeys) {
      if (!rhsKeys.has(key) || !deepEqual(lhs[key], rhs[key])) {
        return false;
      }
    }
    return true;
  }
  else {
    return lhs === rhs;
  }
}

console.log(deepEqual(null, null));
console.log(deepEqual(null, undefined));

let obj = {here: {is: "an"}, object: 2};
console.log(deepEqual(obj, obj));
console.log(deepEqual(obj, {here: 1, object: 2}));
console.log(deepEqual(obj, {object: 2, here: {is: "an"}}));
console.log(deepEqual({"null": null, second: "second"},
                      {"null": null, third: "third"}));
