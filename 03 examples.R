# make some example meals and analyze.

mySteakBowl = c("Steak", "White Rice", "Pinto Beans", "Lettuce", "Guacamole", "Red Tomatillo Salsa")
mySteakBurrito = c("Steak", "White Rice", "Pinto Beans", "Lettuce", "Guacamole", "Red Tomatillo Salsa", "Soft Flour Tortilla")
myChickenBowl = c("Chicken", "White Rice", "Pinto Beans", "Lettuce", "Sour Cream", "Tomato Salsa", "Tomato Salsa")
mySofritasBowl = c("Sofritas", "Brown Rice", "Pinto Beans", "Sour Cream", "Corn Salsa", "Green Tomatillo Salsa")
myCarnitasBurrito = c("Carnitas", "Pinto Beans", "Guacamole", "Green Tomatillo Salsa","Soft Flour Tortilla")
mySteakTacos = c("Steak", "Lettuce", "Guacamole", "Red Tomatillo Salsa", "Soft Corn Tortillas")
myChickenSalad = c("Chicken", "Lettuce", "Vinaigrette", "Black Beans", "Corn Salsa", "Cheese")


myTots=function(meal){
  ings.nut=filter(nutrition, Name %in% meal)
  colSums(ings.nut[c(3:5,7:16)])
}

myTots2=function(meal){
  ings.nut=filter(nutrition2, Name %in% meal)
  colSums(ings.nut[c(3:5,7:16)])
}

mySplits = function(meal){
  raw=myTots(meal)
  fatCal=raw[2] * 9
  carbCal = raw[6] * 4
  proteinCal = raw[9] * 4
  calcCal = fatCal + carbCal + proteinCal
  fatPercent = fatCal / calcCal
  carbPercent = carbCal / calcCal
  proteinPercent = proteinCal / calcCal
  x=data.frame(fatPercent, carbPercent, proteinPercent)
  rownames(x) <- deparse(substitute(meal))
  x
}

x = myTots2(mySofritasBowl)
#myMealTots2 = rbind(myMealTots2, myTots2(mySteakBowl))
#row.names(myMealTots2)<-c("mySofritasBowl")
radarchart (data.frame(rbind(rep(4,12), rep(0,12), (x[2:13]/x[1]))), seg=4)