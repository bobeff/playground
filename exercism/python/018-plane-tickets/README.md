# Plane Tickets

Welcome to Plane Tickets on Exercism's Python Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

A `generator` is a function or expression that returns a special type of [iterator][iterator] called [generator iterator][generator-iterator].
`Generator-iterators` are [lazy][lazy iterator]: they do not store their `values` in memory, but _generate_ their values when needed.

A generator function looks like any other function, but contains one or more [yield expressions][yield expression].
Each `yield` will suspend code execution, saving the current execution state (_including all local variables and try-statements_).
When the generator resumes, it picks up state from the suspension - unlike regular functions which reset with every call.


## Constructing a generator

Generators are constructed much like other looping or recursive functions, but require a [`yield` expression](#the-yield-expression), which we will explore in depth a bit later.

An example is a function that returns the _squares_ from a given list of numbers.
As currently written, all input must be processed before any values can be returned:

```python
>>> def squares(list_of_numbers):
...     squares = []
...     for number in list_of_numbers:
...         squares.append(number ** 2)
...     return squares
```

You can convert that function into a generator like this:

```python
>>> def squares_generator(list_of_numbers):
...     for number in list_of_numbers:
...         yield number ** 2
```

The rationale behind this is that you use a generator when you do not need to produce all the values _at once_.
This saves memory and processing power, since only the value you are _currently working on_ is calculated.


## Using a generator

Generators may be used in place of most `iterables` in Python.
This includes _functions_ or _objects_ that require an `iterable`/`iterator` as an argument.

To use the `squares_generator()` generator:

```python
>>> squared_numbers = squares_generator([1, 2, 3, 4])

>>> for square in squared_numbers:
...     print(square)
...
1
4
9
16
```

Values within a `generator` can also be produced/accessed via the `next()` function.
`next()` calls the `__next__()` method of a generator-iterator object, "advancing" or evaluating the code up to its `yield` expression, which then "yields" or returns a value:

```python
>>> squared_numbers = squares_generator([1, 2])

>>> next(squared_numbers)
1
>>> next(squared_numbers)
4
```

When a `generator-iterator` is fully consumed and has no more values to return, it throws a `StopIteration` error.

```python
>>> next(squared_numbers)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
StopIteration
```


~~~~exercism/note

Generator-iterators are a special sub-set of [iterators][iterator].
`Iterators` are the mechanism/protocol that enables looping over _iterables_.
Generator-iterators and the iterators returned by common Python [`iterables`][iterables] act very similarly, but there are some important differences to note:

- They are _[lazily evaluated][lazy evaluation]_; iteration is _one-way_ and there is no "backing up" to a previous value.
- They are _consumed_ by iterating over the returned values; there is no resetting or saving in memory.
- They are not sortable and cannot be reversed.
- They are not sequence types, and _do not_ have `indexes`. 
  You cannot reference a previous or future value using addition or subtraction and you cannot use bracket (`[]`) notation or slicing.
- They cannot be used with the `len()` function, as they have no length.
- They can be _finite_ or _infinite_ - be careful when collecting all values from an _infinite_ `generator-iterator`!

[iterator]: https://docs.python.org/3.11/glossary.html#term-iterator
[iterables]: https://wiki.python.org/moin/Iterator
[lazy evaluation]: https://en.wikipedia.org/wiki/Lazy_evaluation
~~~~


## The yield expression

The [yield expression][yield expression] is very similar to the `return` expression.
_Unlike_ the `return` expression, `yield` gives up values to the caller at a _specific point_, suspending evaluation/return of any additional values until they are requested.
When `yield` is evaluated, it pauses the execution of the enclosing function and returns any values of the function _at that point in time_.
The function then _stays in scope_, and when `__next__()` is called, execution resumes until `yield` is encountered again.


~~~~exercism/note
Using `yield` expressions is prohibited outside of functions.
~~~~

```python
>>> def infinite_sequence():
...     current_number = 0
...     while True:
...         yield current_number
...         current_number += 1

>>> lets_try = infinite_sequence()
>>> lets_try.__next__()
0
>>> lets_try.__next__()
1
```


## Why Create a Generator?

Generators are useful in a lot of applications.

When working with a potentially large collection of values, you might not want to put all of them into memory.
A generator can be used to work on larger data piece-by-piece, saving memory and improving performance.

Generators are also very helpful when a process or calculation is _complex_, _expensive_, or _infinite_:

```python
>>> def infinite_sequence():
...     current_number = 0
...     while True:
...         yield current_number
...         current_number += 1
```

Now whenever `__next__()` is called on the `infinite_sequence` object, it will return the _previous number_ + 1.


[generator-iterator]: https://docs.python.org/3.11/glossary.html#term-generator-iterator
[iterables]: https://wiki.python.org/moin/Iterator
[iterator]: https://docs.python.org/3.11/glossary.html#term-iterator
[lazy evaluation]: https://en.wikipedia.org/wiki/Lazy_evaluation
[lazy iterator]: https://en.wikipedia.org/wiki/Lazy_evaluation
[yield expression]: https://docs.python.org/3.11/reference/expressions.html#yield-expressions

## Instructions

Conda Airlines is the programming-world's biggest airline, with over 10,000 flights a day!

They are currently assigning all seats to passengers by hand; this will need to be automated.

They have asked _you_ to create software to automate passenger seat assignments.
They require your software to be memory efficient and performant.

## 1. Generate seat letters

Conda wants to generate seat letters for their airplanes.
An airplane is made of rows of seats.
Each row has _4 seats_.
The seats in each row are always named `A`, `B`, `C`, and `D`.
The first seat in the row is `A`, the second seat in the row is `B`, and so on.
After reaching `D`, it should start again with `A`.

Implement a function `generate_seat_letters(<number>)` that accepts an `int` that holds how many seat letters to be generated.
The function should then return an _iterable_ of seat letters.

```python
>>> letters = generate_seat_letters(4)
>>> next(letters)
"A"
>>> next(letters)
"B"
```

## 2. Generate seats

Conda wants a system that can generate a given number of seats for their airplanes.
Each airplane has _4 seats_ in each row.
The rows are defined using numbers, starting from `1` and going up.
The seats should be ordered, like: `1A`, `1B`, `1C`, `1D`, `2A`, `2B`, `2C`, `2D`, `3A`, `3B`, `3C`, `3D`, ...

Here is an example:

|      x      |  1  |  2  |
| :---------: | :-: | :-: |
|     Row     |  5  | 21  |
| Seat letter |  A  |  D  |
|   Result    | 5A  | 21D |

Many airlines do not have _row_ number 13 on their flights, due to superstition amongst passengers.
Conda Airlines also follows this convention, so make sure you _don't_ generate seats for _row_ number 13.

Implement a function `generate_seats(<number>)` that accepts an `int` that holds how many seats to be generated.
The function should then return an _iterable_ of seats given.

```python
>>> seats = generate_seats(10)
>>> next(seats)
"1A"
>>> next(seats)
"1B"
```

## 3. Assign seats to passengers

Now that you have a function that generates seats, you can use it to assign seats to passengers.

Implement a function `assign_seats(<passengers>)` that accepts a `list` of passenger names.
The function should then return a _dictionary_ of `passenger` as _key_, and `seat_number` as _value_.

```python
>>> passengers = ['Jerimiah', 'Eric', 'Bethany', 'Byte', 'SqueekyBoots', 'Bob']

>>> assign_seats(passengers)
{'Jerimiah': '1A', 'Eric': '1B', 'Bethany': '1C', 'Byte': '1D', 'SqueekyBoots': '2A', 'Bob': '2B'}
```

## 4. Ticket codes

Conda Airlines would like to have a unique code for each ticket.
Since they are a big airline, they have a lot of flights.
This means that there are multiple flights with the same seat number.
They want you to create a system that creates a unique ticket that is _12_ characters long string code for identification.

This code begins with the `assigned_seat` followed by the `flight_id`.
The rest of the code is appended by `0s`.

Implement a function `generate_codes(<seat_numbers>, <flight_id>)` that accepts a `list` of `seat_numbers` and a `string` with the flight number.
The function should then return a `generator` that yields a `ticket_number`.

```python
>>> seat_numbers = ['1A', '17D']
>>> flight_id = 'CO1234'
>>> ticket_ids = generate_codes(seat_numbers, flight_id)

>>> next(ticket_ids)
'1ACO12340000'
>>> next(ticket_ids)
'17DCO1234000'
```

## Source

### Created by

- @J08K

### Contributed to by

- @BethanyG
- @kytrinyx
- @meatball133