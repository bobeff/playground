# Tisbury Treasure Hunt

Welcome to Tisbury Treasure Hunt on Exercism's Python Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

In Python, a [tuple][tuple] is an _immutable_ collection of items in _sequence_.
Like most collections, `tuples` can hold any (or multiple) data type(s) -- including other `tuples`.
Tuples support all [common sequence operations][common sequence operations], but **do not** support [mutable sequence operations][mutable sequence operations].

The elements of a tuple can be iterated over using the `for item in <tuple>` construct.
If both element index and value are needed, `for index, item in enumerate(<tuple>)` can be used.
Like any sequence, elements within `tuples` can be accessed via _bracket notation_ using a `0-based index` number from the left or a `-1-based index` number from the right.
Tuples can also be copied in whole or in part using slice notation (_`<tuple>[<start>:<stop>:<step>]`_).


## Tuple Construction

Tuples can be formed in multiple ways, using either the `tuple(<iterable>)` class constructor or the `tuple` literal declaration.

### Using the `tuple()` constructor empty or with an _iterable_:

```python
>>> no_elements = tuple()
()

# The constructor *requires* an iterable, so single elements must be passed in a list or another tuple.
>>> one_element = tuple([16])
(16,)
```

Strings are iterable, so using a single `str` as an argument to the `tuple()` constructor can have surprising results:

```python
# String elements (characters) are iterated through and added to the tuple
>>> multiple_elements_string = tuple("Timbuktu")
('T', 'i', 'm', 'b', 'u', 'k', 't', 'u')
```

Single iterables have their elements added one by one:

```python
>>> multiple_elements_list = tuple(["Parrot", "Bird", 334782])
("Parrot", "Bird", 334782)

>>> multiple_elements_set = tuple({2, 3, 5, 7, 11})
(2,3,5,7,11)
```

#### Declaring a tuple as a _literal_ :

Because the `tuple(<iterable>)` constructor only takes _iterables_ (or nothing) as arguments, it is much easier to create
 a one-tuple via the literal method.

```python
>>> no_elements = ()
()

>>> one_element = ("Guava",)
("Guava",)
```

Nested data structures can be included as `tuple` elements, including other `tuples`:

```python
>>> nested_data_structures = ({"fish": "gold", "monkey": "brown", "parrot" : "grey"}, ("fish", "mammal", "bird"))
({"fish": "gold", "monkey": "brown", "parrot" : "grey"}, ("fish", "mammal", "bird"))

>>> nested_data_structures_1 = (["fish", "gold", "monkey", "brown", "parrot", "grey"], ("fish", "mammal", "bird"))
(["fish", "gold", "monkey", "brown", "parrot", "grey"], ("fish", "mammal", "bird"))
```

## Tuple Concatenation

Tuples can be concatenated using plus `+` operator, which unpacks each `tuple` creating a new, combined `tuple`.

```python
>>> new_via_concatenate = ("George", 5) + ("cat", "Tabby")
("George", 5, "cat", "Tabby")

#likewise, using the multiplication operator * is the equivalent of using + n times
>>> first_group = ("cat", "dog", "elephant")

>>> multiplied_group = first_group * 3
('cat', 'dog', 'elephant', 'cat', 'dog', 'elephant', 'cat', 'dog', 'elephant')
```

## Accessing Elements Inside a Tuple

Elements within a `tuple` can be accessed via _bracket notation_ using a `0-based index` number from the left or a `-1-based index` number from the right.

```python
student_info = ("Alyssa", "grade 3", "female", 8 )

#gender is at index 2 or index -2
>>> student_gender = student_info[2]
'female'

>>> student_gender = student_info[-2]
'female'

#name is at index 0 or index -4
>>> student_name = student_info[0]
Alyssa

>>> student_name = student_info[-4]
Alyssa
```

## Iterating Over a Tuples Elements

Elements inside a `tuple` can be _iterated over_ in a loop using `for item in <tuple>` syntax.
If both indexes and values are needed, `for index, item in enumerate(<tuple>)` can be used.

```python
>>> student_info = ("Alyssa", "grade 3", "female", 8 )
>>> for item in student_info:
...   print(item)

...
Alyssa
grade 3
female
8

>>> for index, item in enumerate(student_info):
...  print("Index is: " + str(index) + ", value is: " + str(item) +".")

...
Index is: 0, value is: Alyssa.
Index is: 1, value is: grade 3.
Index is: 2, value is: female.
Index is: 3, value is: 8.
```

## Checking Membership in a Tuple

The `in` operator can be used to check membership in a `tuple`.

```python
>>> multiple_elements_list = tuple(["Parrot", "Bird", 334782])
("Parrot", "Bird", 334782)

>>> "Parrot" in multiple_elements_list
True
```

[common sequence operations]: https://docs.python.org/3/library/stdtypes.html#common-sequence-operations
[mutable sequence operations]: https://docs.python.org/3/library/stdtypes.html#mutable-sequence-types
[tuple]: https://docs.python.org/3/library/stdtypes.html#tuple

## Instructions

Azara and Rui are teammates competing in a pirate-themed treasure hunt.
One has a list of treasures with map coordinates, the other a list of location names with map coordinates.
They've also been given blank maps with a starting place marked YOU ARE HERE.

<br>
<table>
<tr><th>Azara's List</th><th></th><th>Rui's List</th></tr>
<tr><td>

| Treasure                    | Coordinates |
| --------------------------- | ----------- |
| Amethyst Octopus            | 1F          |
| Angry Monkey Figurine       | 5B          |
| Antique Glass Fishnet Float | 3D          |
| Brass Spyglass              | 4B          |
| Carved Wooden Elephant      | 8C          |
| Crystal Crab                | 6A          |
| Glass Starfish              | 6D          |
| Model Ship in Large Bottle  | 8A          |
| Pirate Flag                 | 7F          |
| Robot Parrot                | 1C          |
| Scrimshawed Whale Tooth     | 2A          |
| Silver Seahorse             | 4E          |
| Vintage Pirate Hat          | 7E          |

</td><td></td><td>

| Location Name                         | Coordinates | Quadrant  |
| ------------------------------------- | ----------- | --------- |
| Seaside Cottages                      | ("1", "C")  | Blue      |
| Aqua Lagoon (Island of Mystery)       | ("1", "F")  | Yellow    |
| Deserted Docks                        | ("2", "A")  | Blue      |
| Spiky Rocks                           | ("3", "D")  | Yellow    |
| Abandoned Lighthouse                  | ("4", "B")  | Blue      |
| Hidden Spring (Island of Mystery)     | ("4", "E")  | Yellow    |
| Stormy Breakwater                     | ("5", "B")  | Purple    |
| Old Schooner                          | ("6", "A")  | Purple    |
| Tangled Seaweed Patch                 | ("6", "D")  | Orange    |
| Quiet Inlet (Island of Mystery)       | ("7", "E")  | Orange    |
| Windswept Hilltop (Island of Mystery) | ("7", "F")  | Orange    |
| Harbor Managers Office                | ("8", "A")  | Purple    |
| Foggy Seacave                         | ("8", "C")  | Purple    |

</td></tr>
</table>

<br>

But things are a bit disorganized: Azara's coordinates appear to be formatted and sorted differently from Rui's, and they have to keep looking from one list to the other to figure out which treasures go with which locations.
Being budding pythonistas, they have come to you for help in writing a small program (a set of functions, really) to better organize their hunt information.


## 1. Extract coordinates

Implement the `get_coordinate()` function that takes a `(treasure, coordinate)` pair from Azara's list and returns only the extracted map coordinate.


```python
>>> get_coordinate(('Scrimshawed Whale Tooth', '2A'))
2A
```

## 2. Format coordinates

Implement the `convert_coordinate()` function that takes a coordinate in the format "2A" and returns a tuple in the format `("2", "A")`.


```python
>>> convert_coordinate("2A")
("2", "A")
```

## 3. Match coordinates

Implement the `compare_records()` function that takes a `(treasure, coordinate)` pair and a `(location, coordinate, quadrant)` record and compares coordinates from each.
Return **`True`** if the coordinates "match", and return **`False`** if they do not.
Re-format coordinates as needed for accurate comparison.


```python
>>> compare_records(('Brass Spyglass', '4B'), ('Seaside Cottages', ('1', 'C'), 'blue'))
False

>>> compare_records(('Model Ship in Large Bottle', '8A'), ('Harbor Managers Office', ('8', 'A'), 'purple'))
True
```

## 4. Combine matched records

Implement the `create_record()` function that takes a `(treasure, coordinate)` pair from Azara's list and a `(location, coordinate, quadrant)` record from Rui's list and returns `(treasure, coordinate, location, coordinate, quadrant)` **if the coordinates match**.
If the coordinates _do not_ match, return the string **"not a match"**.
Re-format the coordinate as needed for accurate comparison.


```python
>>> create_record(('Brass Spyglass', '4B'), ('Abandoned Lighthouse', ('4', 'B'), 'Blue'))
('Brass Spyglass', '4B', 'Abandoned Lighthouse', ('4', 'B'), 'Blue')

>>> create_record(('Brass Spyglass', '4B'), ('Seaside Cottages', ('1', 'C'), 'blue'))
"not a match"
```

## 5. "Clean up" & make a report of all records

Clean up the combined records from Azara and Rui so that there's only one set of coordinates per record. Make a report so they can see one list of everything they need to put on their maps.
Implement the `clean_up()` function that takes a tuple of tuples (_everything from both lists_), looping through the _outer_ tuple, dropping the unwanted coordinates from each _inner_ tuple and adding each to a 'report'.
Format and return the 'report' so that there is one cleaned record on each line.


```python
>>> clean_up((('Brass Spyglass', '4B', 'Abandoned Lighthouse', ('4', 'B'), 'Blue'), ('Vintage Pirate Hat', '7E', 'Quiet Inlet (Island of Mystery)', ('7', 'E'), 'Orange'), ('Crystal Crab', '6A', 'Old Schooner', ('6', 'A'), 'Purple')))

"""
('Brass Spyglass', 'Abandoned Lighthouse', ('4', 'B'), 'Blue')\n
('Vintage Pirate Hat', 'Quiet Inlet (Island of Mystery)', ('7', 'E'), 'Orange')\n
('Crystal Crab', 'Old Schooner', ('6', 'A'), 'Purple')\n
"""
```

## Source

### Created by

- @BethanyG