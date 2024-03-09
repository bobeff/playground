/*
Borrowing a method
-------------------

Earlier in this chapter I mentioned that an object's hasOwnProperty ca be used
as a more robust alternative to the in operator when you want to ignore the
prototype's properties. But what if your map needs to include the word
"hasOwnProperty"? You won't be able to call that method anymore because the
object's own property hides the method value.

Can you think if a way to call hasOwnProperty on an object that has its own
property by that name?
*/

let obj = {
  prop1: "prop1",
};

console.log(obj.hasOwnProperty("prop1"));
console.log(obj.hasOwnProperty("prop2"));

obj.hasOwnProperty = (x) => "foo: " + String(x);

console.log(obj.hasOwnProperty("prop1"));
console.log(obj.hasOwnProperty("prop2"));

console.log(Object.prototype.hasOwnProperty.call(obj, "prop1"));
console.log(Object.prototype.hasOwnProperty.call(obj, "prop2"));
