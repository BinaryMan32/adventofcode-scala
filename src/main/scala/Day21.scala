package com.fivebytestudios.wildfreddy

object Day21 {
  case class Food(ingredients: Set[String], allergens: Set[String])
  val foodRegex = """(.*?)(?: \(contains (.+?)\))""".r
  def parseFood(line: String): Food = line match {
    case foodRegex(ingredients, allergens) => Food(
      ingredients = ingredients.split(" ").toSet,
      allergens = allergens.split(", ").toSet
    )
  }
  def parseFoods(input: List[String]): List[Food] =
    input.map(parseFood)
  def potentialAllergenIngredients(foods: List[Food]): Map[String, Set[String]] = {
    foods
      .flatMap(food => food.allergens.map(allergen => food.copy(allergens = Set(allergen))))
      .groupMapReduce(_.allergens.head)(_.ingredients)(_ intersect _)
  }
  def improveAllergenIngredients(input: Map[String, Set[String]]): Map[String, Set[String]] = {
    println(s"improveAllergenIngredients $input")
    val (single, multiple) = input.partition{case _ -> ingredients => ingredients.size == 1}
    val knownIngredients = single.values.reduceOption(_ ++ _).getOrElse(Set.empty)
    val multipleImproved = multiple.map{case allergen -> ingredients => allergen -> (ingredients -- knownIngredients)}
    if (multipleImproved == multiple)
      single ++ multiple
    else
      single ++ improveAllergenIngredients(multipleImproved)
  }
  def part1(input: List[String]): Long = {
    val foods = parseFoods(input)
    val allergenIngredients = improveAllergenIngredients(potentialAllergenIngredients(foods))
    println(s"allergenIngredients = $allergenIngredients")
    val mayContainAllergen = allergenIngredients.values.reduce(_ ++ _)
    println(s"mayContainAllergen = $mayContainAllergen")
    val allIngredients = foods.map(_.ingredients).reduce(_ ++ _)
    println(s"allIngredients = $allIngredients")
    val cannotContainAllergens = allIngredients -- mayContainAllergen
    val ingredientAppearances = foods
      .flatMap(_.ingredients intersect cannotContainAllergens)
      .groupMapReduce(identity)(_ => 1)(_ + _)
    println(s"ingredientAppearances = $ingredientAppearances")
    ingredientAppearances.values.sum
  }

  def part2(input: List[String]): String = {
    val foods = parseFoods(input)
    improveAllergenIngredients(potentialAllergenIngredients(foods))
      .toList
      .sortBy(_._1)
      .map(_._2.head)
      .mkString(",")
  }
}
