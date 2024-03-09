/*
Exercise 2: Groups
-------------------

The standard JavaScript environment provides another data structure called Set.
Like an instance of Map, a set holds a collection of values. Unlike Map, it does
not associate other values with those—it just tracks which values are part of
the set. A value can be part of a set only once—adding it again doesn’t have any
effect.

Write a class called Group (since Set is already taken). Like Set, it has add,
delete, and has methods. Its constructor creates an empty group, add adds a
value to the group (but only if it isn’t already a member), delete removes its
argument from the group (if it was a member), and has returns a Boolean value
indicating whether its argument is a member of the group.

Use the === operator, or something equivalent such as indexOf, to determine
whether two values are the same.

Give the class a static from method that takes an iterable object as argument.

Exercise 3: Iterable groups
----------------------------

Make the Group class from the previous exercise iterable. Refer to the section
about the iterator interface earlier in the chapter if you aren’t clear on the
exact form of the interface anymore.

If you used an array to represent the group’s members, don’t just return the
iterator created by calling the Symbol.iterator method on the array. That would
work, but it defeats the purpose of this exercise.

It is okay if your iterator behaves strangely when the group is modified during
iteration.
*/

class Group {
  #items = [];

  add(x) {
    if (this.#items.includes(x)) return;
    this.#items.push(x);
  }

  delete(x) {
    this.#items = this.#items.filter(item => x !== item);
  }

  has(x) {
    return this.#items.includes(x);
  }

  [Symbol.iterator]() {
    class GroupIterator {
      constructor(group) {
        this.group = group;
        this.current = 0;
      }

      next() {
        if (this.current < this.group.#items.length) {
          let value = this.group.#items[this.current++];
          return {value, done: false};
        }
        return {done: true};
      }
    }

    return new GroupIterator(this);
  }

  static from(array) {
    let group = new Group();
    for (let x of array) {
      group.add(x);
    }
    return group;
  }
}

let group = Group.from([10, 20]);
console.log(group.has(10));
console.log(group.has(30))
group.add(10);
group.delete(10);
console.log(group.has(10));

for (let value of Group.from(["a", "b", "c"])) {
  console.log(value);
}
