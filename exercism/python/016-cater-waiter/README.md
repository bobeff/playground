# Cater Waiter

Welcome to Cater Waiter on Exercism's Python Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

A [`set`][type-set] is a _mutable_ and _unordered_ collection of [_hashable_][hashable] objects.
Set members must be distinct -- duplicate items are not allowed.
They can hold multiple different data types and even nested structures like a `tuple` of `tuples` -- as long as all elements can be _hashed_.
Sets also come in an immutable [`frozensets`][type-frozenset] flavor.

Sets are most commonly used to quickly remove duplicates from other data structures or item groupings.
They are also used for efficient comparisons when sequencing and duplicate tracking are not needed.

Like other collection types (_dictionaries, lists, tuples_), `sets` support:
- Iteration via `for item in <set>`
- Membership checking via `in` and `not in`,
- Length calculation through `len()`, and
- Shallow copies through `copy()`

`sets` do not support:
- Indexing of any kind
- Ordering via sorting or insertion
- Slicing
- Concatenation via `+`


Checking membership in a `set` has constant time complexity (on average) versus checking membership in a `list` or `string`, where the time complexity grows as the length of the data increases.
Methods such as `<set>.union()`, `<set>.intersection()`, or `<set>.difference()` also have constant time complexity (on average).


## Set Literals

A `set` can be directly entered as a _set literal_ with curly `{}` brackets and commas between elements.
Duplicates are silently omitted:

```python
>>> one_element = {'😀'}
{'😀'}

>>> multiple_elements = {'😀', '😃', '😄', '😁'}
{'😀', '😃', '😄', '😁'}

>>> multiple_duplicates =  {'Hello!', 'Hello!', 'Hello!', 
                            '¡Hola!','Привіт!', 'こんにちは！', 
                            '¡Hola!','Привіт!', 'こんにちは！'}
{'こんにちは！', '¡Hola!', 'Hello!', 'Привіт!'}
```

Set literals use the same curly braces as `dict` literals, which means you need to use `set()` to create an empty `set`.


## The Set Constructor

`set()` (_the constructor for the `set` class_) can be used with any `iterable` passed as an argument.
Elements of the `iterable` are cycled through and added to the `set` individually.
Element order is not preserved and duplicates are silently omitted:

```python
# To create an empty set, the constructor must be used.
>>> no_elements = set()
set()

# The tuple is unpacked & each element is added.  
# Duplicates are removed.
>>> elements_from_tuple = set(("Parrot", "Bird", 
                               334782, "Bird", "Parrot"))
{334782, 'Bird', 'Parrot'}

# The list is unpacked & each element is added.
# Duplicates are removed.
>>> elements_from_list = set([2, 3, 2, 3, 3, 3, 5, 
                              7, 11, 7, 11, 13, 13])
{2, 3, 5, 7, 11, 13}
```

### Gotchas when Creating Sets

Due to its "unpacking" behavior, using `set()` with a string might be surprising:

```python
# String elements (Unicode code points) are 
# iterated through and added *individually*.
>>> elements_string = set("Timbuktu")
{'T', 'b', 'i', 'k', 'm', 't', 'u'}

# Unicode separators and positioning code points 
# are also added *individually*.
>>> multiple_code_points_string = set('अभ्यास')
{'अ', 'भ', 'य', 'स', 'ा', '्'}
```

Sets can hold different datatypes and _nested_ datatypes, but all `set` elements must be _hashable_:

```python
# Attempting to use a list for a set member throws a TypeError
>>> lists_as_elements = {['😅','🤣'], 
                        ['😂','🙂','🙃'], 
                        ['😜', '🤪', '😝']}

Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: unhashable type: 'list'


# Standard sets are mutable, so they cannot be hashed.
>>> sets_as_elements = {{'😅','🤣'}, 
                        {'😂','🙂','🙃'}, 
                        {'😜', '🤪', '😝'}}

Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: unhashable type: 'set'
```


## Working with Sets

Sets have methods that generally mimic [mathematical set operations][mathematical-sets].
Most (_not all_) of these methods have an [operator][operator] equivalent.
Methods generally take any `iterable` as an argument, while operators require that both sides of the operation are `sets` or `frozensets`.


### Disjoint Sets

The `<set>.isdisjoint(<other_collection>)` method is used to test if a `sets` elements have any overlap with the elements of another `set`.
The method will accept any `iterable` or `set` as an argument.
It will return `True` if the two sets have **no elements in common**, `False` if elements are **shared**.
There is no operator equivalent:


```python
# Mammals and birds don't share any elements.
>>> birds.isdisjoint(mammals)
True

# There are also no shared elements between 
# additional_animals and birds.
>>> birds.isdisjoint(additional_animals)
True

# Animals and mammals have shared elements.
# **Note** The first object needs to be a set or converted to a set
# since .isdisjoint() is a set method.
>>> set(animals).isdisjoint(mammals)
False
```


### Subsets and Supersets

`<set>.issubset(<other_collection>)` is used to check if every element in `<set>` is also in `<other_collection>`.
The operator form is `<set> <= <other_set>`:


```python
# Both mammals and additional_animals are lists.
>>> mammals = ['squirrel','dog','cat','cow', 'tiger', 'elephant']
>>> additional_animals = ['pangolin', 'panda', 'parrot', 
                          'lemur', 'tiger', 'pangolin']

# Animals is a dict.
>>> animals = {'chicken': 'white',
               'sparrow': 'grey',
               'eagle': 'brown and white',
               'albatross': 'grey and white',
               'crow': 'black',
               'elephant': 'grey', 
               'dog': 'rust',
               'cow': 'black and white',
               'tiger': 'orange and black',
               'cat': 'grey',
               'squirrel': 'black'}

# Birds is a set.
>>> birds = {'crow','sparrow','eagle','chicken', 'albatross'}

# Set methods will take any iterable as an argument.
# All members of birds are also members of animals.
>>> birds.issubset(animals)
True

# All members of mammals also appear in animals.
# **Note** The first object needs to be a set or converted to a set
# since .issubset() is a set method.
>>> set(mammals).issubset(animals)
True

# Both objects need to be sets to use a set operator
>>> birds <= set(mammals)
False

# A set is always a loose subset of itself.
>>> set(additional_animals) <= set(additional_animals)
True
```

`<set>.issuperset(<other_collection>)` is the inverse of `.issubset()`.
It is used to check if every element in `<other_collection>` is also in `<set>`.
The operator form is `<set> >= <other_set>`:


```python
# All members of mammals also appear in animals.
# **Note** The first object needs to be a set or converted to a set
# since .issuperset() is a set method.
>>> set(animals).issuperset(mammals)
True

# All members of animals do not show up as members of birds.
>>> birds.issuperset(animals)
False

# Both objects need to be sets to use a set operator
>>> birds >= set(mammals)
False

# A set is always a loose superset of itself.
>>> set(animals) >= set(animals)
True
```


### Set Intersections

`<set>.intersection(*<other iterables>)` returns a new `set` with elements common to the original `set` and all `<others>` (_in other words, the `set` where everything [intersects][intersection]_).
The operator version of this method is `<set> & <other set> & <other set 2> & ... <other set n>`:


```python
>>> perennials = {'Annatto','Asafetida','Asparagus','Azalea',
                 'Winter Savory', 'Broccoli','Curry Leaf','Fennel', 
                 'Kaffir Lime','Kale','Lavender','Mint','Oranges',
                 'Oregano', 'Tarragon', 'Wild Bergamot'}

>>> annuals = {'Corn', 'Zucchini', 'Sweet Peas', 'Marjoram', 
              'Summer Squash', 'Okra','Shallots', 'Basil', 
              'Cilantro', 'Cumin', 'Sunflower', 'Chervil', 
              'Summer Savory'}

>>> herbs = ['Annatto','Asafetida','Basil','Chervil','Cilantro',
            'Curry Leaf','Fennel','Kaffir Lime','Lavender',
            'Marjoram','Mint','Oregano','Summer Savory' 
            'Tarragon','Wild Bergamot','Wild Celery',
            'Winter Savory']


# Methods will take any iterable as an argument.
>>> perennial_herbs = perennials.intersection(herbs)
{'Annatto', 'Asafetida', 'Curry Leaf', 'Fennel', 'Kaffir Lime',
 'Lavender', 'Mint', 'Oregano', 'Wild Bergamot','Winter Savory'}

# Operators require both groups be sets.
>>> annuals & set(herbs)
 {'Basil', 'Chervil', 'Marjoram', 'Cilantro'}
```


### Set Unions

`<set>.union(*<other iterables>)` returns a new `set` with elements from `<set>` and all `<other iterables>`.
The operator form of this method is `<set> | <other set 1> | <other set 2> | ... | <other set n>`:


```python
>>> perennials = {'Asparagus', 'Broccoli', 'Sweet Potato', 'Kale'}
>>> annuals = {'Corn', 'Zucchini', 'Sweet Peas', 'Summer Squash'}
>>> more_perennials = ['Radicchio', 'Rhubarb', 
                      'Spinach', 'Watercress']

# Methods will take any iterable as an argument.
>>> perennials.union(more_perennials)
{'Asparagus','Broccoli','Kale','Radicchio','Rhubarb',
'Spinach','Sweet Potato','Watercress'}

# Operators require sets.
>>> set(more_perennials) | perennials
{'Asparagus',
 'Broccoli',
 'Kale',
 'Radicchio',
 'Rhubarb',
 'Spinach',
 'Sweet Potato',
 'Watercress'}
```


### Set Differences

`<set>.difference(*<other iterables>)` returns a new `set` with elements from the original `<set>` that are not in `<others>`.
The operator version of this method is `<set> - <other set 1> - <other set 2> - ...<other set n>`.

```python
>>> berries_and_veggies = {'Asparagus', 
                          'Broccoli', 
                          'Watercress', 
                          'Goji Berries', 
                          'Goose Berries', 
                          'Ramps', 
                          'Walking Onions', 
                          'Blackberries', 
                          'Strawberries', 
                          'Rhubarb', 
                          'Kale', 
                          'Artichokes', 
                          'Currants'}

>>> veggies = ('Asparagus', 'Broccoli', 'Watercress', 'Ramps',
               'Walking Onions', 'Rhubarb', 'Kale', 'Artichokes')

# Methods will take any iterable as an argument.
>>> berries = berries_and_veggies.difference(veggies)
{'Blackberries','Currants','Goji Berries',
 'Goose Berries', 'Strawberries'}

# Operators require sets.
>>> berries_and_veggies - just_berries
{'Artichokes','Asparagus','Broccoli','Kale',
'Ramps','Rhubarb','Walking Onions','Watercress'}
```


`<set>.symmetric_difference(<other iterable>)` returns a new `set` that contains elements that are in `<set>` OR `<other>`, **but not in both**.
The operator version of this method is  `<set> ^ <other set>`:


```python
>>> plants_1 = {'🌲','🍈','🌵', '🥑','🌴', '🥭'}
>>> plants_2 = ('🌸','🌴', '🌺', '🌲', '🌻', '🌵')


# Methods will take any iterable as an argument.
>>> fruit_and_flowers = plants_1.symmetric_difference(plants_2)
>>> fruit_and_flowers
{'🌸', '🌺', '🍈', '🥑', '🥭','🌻' }


# Operators require both groups be sets.
>>> fruit_and_flowers ^ plants_1
{'🌲',  '🌸', '🌴', '🌵','🌺', '🌻'}

>>> fruit_and_flowers ^ plants_2
{ '🥑', '🌴','🌲', '🌵', '🍈', '🥭'}
```

~~~~exercism/note

A symmetric difference of more than two sets will result in a `set` that includes both the elements unique to each `set` AND elements shared between more than two sets in the series (_details in the Wikipedia article on [symmetric difference][symmetric_difference]_).  

To obtain only items unique to each `set` in the series, intersections between all 2-set combinations need to be aggregated in a separate step, and removed:  


```python
>>> one = {'black pepper','breadcrumbs','celeriac','chickpea flour',
           'flour','lemon','parsley','salt','soy sauce',
           'sunflower oil','water'}

>>> two = {'black pepper','cornstarch','garlic','ginger',
           'lemon juice','lemon zest','salt','soy sauce','sugar',
           'tofu','vegetable oil','vegetable stock','water'}

>>> three = {'black pepper','garlic','lemon juice','mixed herbs',
             'nutritional yeast', 'olive oil','salt','silken tofu',
             'smoked tofu','soy sauce','spaghetti','turmeric'}

>>> four = {'barley malt','bell pepper','cashews','flour',
            'fresh basil','garlic','garlic powder', 'honey',
            'mushrooms','nutritional yeast','olive oil','oregano',
            'red onion', 'red pepper flakes','rosemary','salt',
            'sugar','tomatoes','water','yeast'}

>>> intersections = (one & two | one & three | one & four | 
                     two & three | two & four | three & four)
...
{'black pepper','flour','garlic','lemon juice','nutritional yeast', 
'olive oil','salt','soy sauce', 'sugar','water'}

# The ^ operation will include some of the items in intersections, 
# which means it is not a "clean" symmetric difference - there
# are overlapping members.
>>> (one ^ two ^ three ^ four) & intersections
{'black pepper', 'garlic', 'soy sauce', 'water'}

# Overlapping members need to be removed in a separate step
# when there are more than two sets that need symmetric difference.
>>> (one ^ two ^ three ^ four) - intersections
...
{'barley malt','bell pepper','breadcrumbs', 'cashews','celeriac',
  'chickpea flour','cornstarch','fresh basil', 'garlic powder',
  'ginger','honey','lemon','lemon zest','mixed herbs','mushrooms',
  'oregano','parsley','red onion','red pepper flakes','rosemary',
  'silken tofu','smoked tofu','spaghetti','sunflower oil', 'tofu', 
  'tomatoes','turmeric','vegetable oil','vegetable stock','yeast'}
```

[symmetric_difference]: https://en.wikipedia.org/wiki/Symmetric_difference
~~~~

[hashable]: https://docs.python.org/3.7/glossary.html#term-hashable
[intersection]: https://www.mathgoodies.com/lessons/sets/intersection
[mathematical-sets]: https://en.wikipedia.org/wiki/Set_theory#Basic_concepts_and_notation
[operator]: https://www.computerhope.com/jargon/o/operator.htm
[type-frozenset]: https://docs.python.org/3/library/stdtypes.html#frozenset
[type-set]: https://docs.python.org/3/library/stdtypes.html#set

## Instructions

You and your business partners operate a small catering company. You've just agreed to run an event for a local cooking club that features "club favorite" dishes. The club is inexperienced in hosting large events, and needs help with organizing, shopping, prepping and serving. You've decided to write some small Python scripts to speed the whole planning process along.

## 1. Clean up Dish Ingredients

The event recipes were added from various sources and their ingredients appear to have duplicate (_or more_) entries -- you don't want to end up purchasing excess items!
 Before the shopping and cooking can commence, each dish's ingredient list needs to be "cleaned".

Implement the `clean_ingredients(<dish_name>, <dish_ingredients>)` function that takes the name of a dish and a `list` of ingredients.
 This function should return a `tuple` with the name of the dish as the first item, followed by the de-duped `set` of ingredients.


```python
>>> clean_ingredients('Punjabi-Style Chole', ['onions', 'tomatoes', 'ginger paste', 'garlic paste', 'ginger paste', 'vegetable oil', 'bay leaves', 'cloves', 'cardamom', 'cilantro', 'peppercorns', 'cumin powder', 'chickpeas', 'coriander powder', 'red chili powder', 'ground turmeric', 'garam masala', 'chickpeas', 'ginger', 'cilantro'])

>>> ('Punjabi-Style Chole', {'garam masala', 'bay leaves', 'ground turmeric', 'ginger', 'garlic paste', 'peppercorns', 'ginger paste', 'red chili powder', 'cardamom', 'chickpeas', 'cumin powder', 'vegetable oil', 'tomatoes', 'coriander powder', 'onions', 'cilantro', 'cloves'})
```

## 2. Cocktails and Mocktails

The event is going to include both cocktails and "mocktails" - mixed drinks _without_ the alcohol.
 You need to ensure that "mocktail" drinks are truly non-alcoholic and the cocktails do indeed _include_ alcohol.

Implement the `check_drinks(<drink_name>, <drink_ingredients>)` function that takes the name of a drink and a `list` of ingredients.
 The function should return the name of the drink followed by "Mocktail" if the drink has no alcoholic ingredients, and drink name followed by "Cocktail" if the drink includes alcohol.
  For the purposes of this exercise, cocktails will only include alcohols from the ALCOHOLS constant in `sets_categories_data.py`:

```python
>>> from sets_categories_data import ALCOHOLS 

>>> check_drinks('Honeydew Cucumber', ['honeydew', 'coconut water', 'mint leaves', 'lime juice', 'salt', 'english cucumber'])
...
'Honeydew Cucumber Mocktail'

>>> check_drinks('Shirley Tonic', ['cinnamon stick', 'scotch', 'whole cloves', 'ginger', 'pomegranate juice', 'sugar', 'club soda'])
...
'Shirley Tonic Cocktail'
```

## 3. Categorize Dishes

The guest list includes diners with different dietary needs, and your staff will need to separate the dishes into Vegan, Vegetarian, Paleo, Keto, and Omnivore.

Implement the `categorize_dish(<dish_name>, <dish_ingredients>)` function that takes a dish name and a `set` of that dish's' ingredients.
The function should return a string with the `dish name: <CATEGORY>` (_which meal category the dish belongs to_).
All dishes will "fit" into one of the categories imported from `sets_categories_data.py` (VEGAN, VEGETARIAN, PALEO, KETO, or OMNIVORE).

```python
>>> from sets_categories_data import VEGAN, VEGETARIAN, PALEO, KETO, OMNIVORE


>>> categorize_dish('Sticky Lemon Tofu', {'tofu', 'soy sauce', 'salt', 'black pepper', 'cornstarch', 'vegetable oil', 'garlic', 'ginger', 'water', 'vegetable stock', 'lemon juice', 'lemon zest', 'sugar'})
...
'Sticky Lemon Tofu: VEGAN'

>>> categorize_dish('Shrimp Bacon and Crispy Chickpea Tacos with Salsa de Guacamole', {'shrimp', 'bacon', 'avocado', 'chickpeas', 'fresh tortillas', 'sea salt', 'guajillo chile', 'slivered almonds', 'olive oil', 'butter', 'black pepper', 'garlic', 'onion'})
...
'Shrimp Bacon and Crispy Chickpea Tacos with Salsa de Guacamole: OMNIVORE'
```

## 4. Label Allergens and Restricted Foods

Some guests have allergies and additional dietary restrictions.
These ingredients need to be tagged/annotated for each dish so that they don't cause issues.

Implement the `tag_special_ingredients(<dish>)` function that takes a `tuple` with the dish name in the first position, and a `list` or `set` of ingredients for that dish in the second position.
Return the dish name followed by the `set` of ingredients that require a special note on the dish description.
Dish ingredients inside a `list` may or may not have duplicates.
 For the purposes of this exercise, all allergens or special ingredients that need to be labeled are in the SPECIAL_INGREDIENTS constant imported from `sets_categories_data.py`.

```python
>>> from sets_categories_data import SPECIAL_INGREDIENTS

>>> tag_special_ingredients(('Ginger Glazed Tofu Cutlets', ['tofu', 'soy sauce', 'ginger', 'corn starch', 'garlic', 'brown sugar', 'sesame seeds', 'lemon juice']))
...
('Ginger Glazed Tofu Cutlets', {'garlic','soy sauce','tofu'})

>>> tag_special_ingredients(('Arugula and Roasted Pork Salad', ['pork tenderloin', 'arugula', 'pears', 'blue cheese', 'pine nuts', 'balsamic vinegar', 'onions', 'black pepper']))
...
('Arugula and Roasted Pork Salad', {'pork tenderloin', 'blue cheese', 'pine nuts', 'onions'})
```

## 5. Compile a "Master List" of Ingredients

In preparation for ordering and shopping, you'll need to compile a "master list" of ingredients for everything on the menu (_quantities to be filled in later_).

Implement the `compile_ingredients(<dishes>)` function that takes a `list` of dishes and returns a set of all ingredients in all listed dishes.
Each individual dish is represented by its `set` of ingredients.

```python
dishes = [ {'tofu', 'soy sauce', 'ginger', 'corn starch', 'garlic', 'brown sugar', 'sesame seeds', 'lemon juice'},
           {'pork tenderloin', 'arugula', 'pears', 'blue cheese', 'pine nuts',
           'balsamic vinegar', 'onions', 'black pepper'},
           {'honeydew', 'coconut water', 'mint leaves', 'lime juice', 'salt', 'english cucumber'}]

>>> compile_ingredients(dishes)
...
{'arugula', 'brown sugar', 'honeydew', 'coconut water', 'english cucumber', 'balsamic vinegar', 'mint leaves', 'pears', 'pork tenderloin', 'ginger', 'blue cheese', 'soy sauce', 'sesame seeds', 'black pepper', 'garlic', 'lime juice', 'corn starch', 'pine nuts', 'lemon juice', 'onions', 'salt', 'tofu'}
```

## 6. Pull out Appetizers for Passing on Trays

The hosts have given you a list of dishes they'd like prepped as "bite-sized" appetizers to be served on trays.
 You need to pull these from the main list of dishes being prepared as larger servings.

Implement the `separate_appetizers(<dishes>, <appetizers>)` function that takes a `list` of dish names and a `list` of appetizer names.
The function should return the `list` of dish names with appetizer names removed.
Either the `<dishes>` or `<appetizers>` `list` could contain duplicates and may require de-duping.

```python
dishes =    ['Avocado Deviled Eggs','Flank Steak with Chimichurri and Asparagus', 'Kingfish Lettuce Cups',
             'Grilled Flank Steak with Caesar Salad','Vegetarian Khoresh Bademjan','Avocado Deviled Eggs',
             'Barley Risotto','Kingfish Lettuce Cups']
          
appetizers = ['Kingfish Lettuce Cups','Avocado Deviled Eggs','Satay Steak Skewers',
              'Dahi Puri with Black Chickpeas','Avocado Deviled Eggs','Asparagus Puffs',
              'Asparagus Puffs']
              
>>> separate_appetizers(dishes, appetizers)
...
['Vegetarian Khoresh Bademjan', 'Barley Risotto', 'Flank Steak with Chimichurri and Asparagus', 
 'Grilled Flank Steak with Caesar Salad']
```

## 7. Find Ingredients Used in Only One Recipe

Within in each category (_Vegan, Vegetarian, Paleo, Keto, Omnivore_), you're going to pull out ingredients that appear in only one dish.
These "singleton" ingredients will be assigned a special shopper to ensure they're not forgotten in the rush to get everything else done.

Implement the `singleton_ingredients(<dishes>, <INTERSECTIONS>)` function that takes a `list` of dishes and a `<CATEGORY>_INTERSECTIONS` constant for the same category.
Each dish is represented by a `set` of its ingredients.
Each `<CATEGORY>_INTERSECTIONS` is a `set` of ingredients that appear in more than one dish in the category.
Using set operations, your function should return a `set` of "singleton" ingredients (_ingredients appearing in only one dish in the category_).

```python
from sets_categories_data import example_dishes, EXAMPLE_INTERSECTION

>>> singleton_ingredients(example_dishes, EXAMPLE_INTERSECTION)
...
{'vegetable oil', 'vegetable stock', 'barley malt', 'tofu', 'fresh basil', 'lemon', 'ginger', 'honey', 'spaghetti', 'cornstarch', 'yeast', 'red onion', 'breadcrumbs', 'mixed herbs', 'garlic powder', 'celeriac', 'lemon zest', 'sunflower oil', 'mushrooms', 'silken tofu', 'smoked tofu', 'bell pepper', 'cashews', 'oregano', 'tomatoes', 'parsley', 'red pepper flakes', 'rosemary'}
```

## Source

### Created by

- @bethanyg

### Contributed to by

- @zepam