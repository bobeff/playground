import tables, sets, strutils, sequtils, algorithm

type
  AllergenToIngredients = tuple[allergen: string, ingredients: HashSet[string]]

var
  ingredients: seq[string]
  allergenToIngredient: Table[string, seq[HashSet[string]]]
  resultMapping: seq[AllergenToIngredients]

proc readInput(fileName: string) =
  for line in fileName.lines:
    let parts = line.split('(')
    let ingreds = parts[0].split(' ')[0 ..< ^1]
    ingredients.add ingreds
    let allergens = parts[1]["contains".len + 1 .. ^1].split(
      {',' , ')', ' '}).filterIt(it != "")
    for allergen in allergens:
      if not allergenToIngredient.hasKey(allergen):
        allergenToIngredient[allergen] = @[]
      allergenToIngredient[allergen].add ingreds.toHashSet

proc solvePartOne(): uint =
  for allergen, ingredientsSets in allergenToIngredient:
    var resultSet = ingredientsSets[0]
    for i in 1 ..< ingredientsSets.len:
      resultSet = resultSet * ingredientsSets[i]
    resultMapping.add((allergen, resultSet))

  resultMapping.sort do (lhs, rhs: AllergenToIngredients) -> int:
    lhs.ingredients.card - rhs.ingredients.card

  for i in 0 ..< resultMapping.len - 1:
    for j in i + 1 ..< resultMapping.len:
      for ingredient in resultMapping[i].ingredients:
        resultMapping[j].ingredients.excl ingredient
    resultMapping.sort do (lhs, rhs: AllergenToIngredients) -> int:
      lhs.ingredients.card - rhs.ingredients.card
  
  var ingredientsContainingAllergen: HashSet[string]
  for i in 0 ..< resultMapping.len:
    ingredientsContainingAllergen.incl resultMapping[i].ingredients.toSeq[0]

  for ingredient in ingredients:
    if ingredient notin ingredientsContainingAllergen:
      result.inc

proc solvePartTwo(): string =
  resultMapping.sort do (lhs, rhs: AllergenToIngredients) -> int:
    cmp(lhs.allergen, rhs.allergen)
  for i, x in resultMapping:
    result &= x.ingredients.toSeq[0]
    result &= ","
  result[0 ..< ^1]

readInput("input.txt")
echo solvePartOne()
echo solvePartTwo()
