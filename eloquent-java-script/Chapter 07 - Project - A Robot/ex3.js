/*
Exercise 3: Persistent group
----------------------------

Most data structures provided in a standard JavaScript environment aren’t very
well suited for persistent use. Arrays have slice and concat methods, which
allow us to easily create new arrays without damaging the old one. But Set, for
example, has no methods for creating a new set with an item added or removed.

Write a new class PGroup, similar to the Group class from Chapter 6, which
stores a set of values. Like Group, it has add, delete, and has methods. Its add
method, however, returns a new PGroup instance with the given member added and
leaves the old one unchanged. Similarly, delete creates a new instance without a
given member.

The class should work for values of any type, not just strings. It does not have
to be efficient when used with large numbers of values.

The constructor shouldn’t be part of the class’s interface (though you’ll
definitely want to use it internally). Instead, there is an empty instance,
PGroup.empty, that can be used as a starting value.

Why do you need only one PGroup.empty value, rather than having a function that
creates a new, empty map every time?
*/

class PGroup {
  #items = [];

  add(x) {
    let group = PGroup.#fromArray(this.#items);
    if (!group.#items.includes(x)) {
      group.#items.push(x);
    }
    return group;
  }

  delete(x) {
    return PGroup.#fromArray(this.#items.filter(item => x !== item));
  }

  has(x) {
    return this.#items.includes(x);
  }

  static #fromArray(items) {
    let group = new PGroup;
    for (let item of items) {
      group.#items.push(item);
    }
    return group;
  }

  static empty = PGroup.#fromArray([]);
}

let a = PGroup.empty.add("a");
let ab = a.add("b");
let b = ab.delete("a");

console.log(b.has("b"));
console.log(a.has("b"));
console.log(b.has("a"));
