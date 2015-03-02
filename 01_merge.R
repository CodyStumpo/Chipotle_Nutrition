library("dplyr")

nutrition=read.csv(file = 'Nutrition.csv',header = TRUE, sep = ",", , stringsAsFactors=FALSE)
# nutrition %DV based on 2000 cal
DV=c('Daily Value', 'N/A', 2000, 65, 20, 2, 300, 2400, 300, 25, 90, 50, 100, 100, 100, 100)
nutrition=rbind(nutrition, DV)
for (i in 3:16){
nutrition[,i]=as.numeric(nutrition[,i])
}

#ought to just make some aliases for salsas and rice in nutrition (before making nutrition2)

addAlias = function(name, alias){
  x=filter(nutrition,Name==name)
  x$Name=alias
  rbind(nutrition,x)
}

nutrition=addAlias("White Rice", "Rice")
nutrition=addAlias("Corn Salsa", "Roasted Chili Corn Salsa (Medium)")
nutrition=addAlias("Corn Salsa", "Roasted Chili Corn Salsa")
nutrition=addAlias("Tomato Salsa", "Fresh Tomato Salsa (Mild)")
nutrition=addAlias("Tomato Salsa", "Fresh Tomato Salsa")
nutrition=addAlias("Red Tomatillo Salsa", "Tomatillo-Red Chili Salsa (Hot)")
nutrition=addAlias("Red Tomatillo Salsa", "Tomatillo Red Chili Salsa")
nutrition=addAlias("Green Tomatillo Salsa", "Tomatillo-Green Chili Salsa (Medium)")
nutrition=addAlias("Green Tomatillo Salsa", "Tomatillo-Green Chili Salsa")
nutrition=addAlias("Fajita Veggies", "Fajita Vegetables")
nutrition=addAlias("Soft Flour Tortilla", "Burrito")
nutrition=addAlias("Fajita Veggies", "Veggie")
nutrition=addAlias("Steak", "Adobo-Marinated and Grilled Steak")
nutrition=addAlias("Barbacoa", "Braised Barbacoa")
nutrition=addAlias("Carnitas", "Braised Carnitas")
nutrition=addAlias("Chicken", "Adobo-Marinated and Grilled Chicken")

#Need nutrition for canned sodas and Izze and Nantucket Nectar & 6 Pack Soft Drink
#Izze = Clementine, Grapefruit, Blackberry
#Nantucket = Apple, Pomegranate Cherry, Pineapple Orange Banana, Peach Orange
#Canned Soft Drink = Coke, Sprite, Dr. Pepper, Lemonade, Nestea, Diet Coke


#normalize all values in terms of DV
nutrition2=nutrition
for(i in 1:nrow(nutrition)){
  nutrition2[i,3:16]=nutrition[i,3:16]/nutrition[25,3:16]
}


#Move on to orders

orders=read.table(file="orders.tsv", header=TRUE, sep="\t", , stringsAsFactors=FALSE)

#Clean description
desc <- orders$choice_description
clean <- lapply(desc, function(x) gsub("[", "", x, fixed=TRUE))
clean <- lapply(clean, function(x) gsub("]", "", x, fixed=TRUE))
orders$choice_description <- clean
rm(clean, desc)

#function to turn choice_description into vector of ingredients
ingsplit = function(i){unlist(lapply(strsplit,X=orders[i,4],split=", "))}
#need one for item_name too
ingsplode = function(i){
  inglist=c()
  x=orders[i,3];
  words=strsplit(x," ")[[1]]
  #Can't test for words[2] if words[2] does not exist
  if (words[1]=="Salad"){inglist=append(inglist,c("Lettuce", "Vinaigrette"))}
  if (words[1]=="Chips" | words[1]=="Side"){inglist=append(inglist, "Chips")}
  if (words[1] %in% c('Steak', 'Chicken', 'Barbacoa', 'Carnitas', 'Veggie')){
    inglist=append(inglist, words[1])
    if (words[2]=="Burrito"){inglist = append(inglist, "Burrito")}
    if (words[2]=="Salad"){inglist = append(inglist, c("Lettuce", "Vinaigrette"))}
    if (words[2]=="Crispy"){inglist = append(inglist, "Crispy Corn Tortillas")}
    #NO way to tell if Soft tacos are flour or corn
    if (words[2]=="Soft"){inglist = append(inglist, "Soft Corn Tortillas")}
  }
  if (words[1]=="Chips" & length(words)>1 ){inglist = append(inglist, paste(words[3:length(words)], collapse=" "))}
  
 inglist
}

#Now we can add together the nutrition or nutrition2 for all the ingredients
tots=function(i){
  ings=append(ingsplode(i),ingsplit(i))
  ings.nut=filter(nutrition, Name %in% ings)
  colSums(ings.nut[3:16])
}

tots2=function(i){
  ings=append(ingsplode(i),ingsplit(i))
  ings.nut=filter(nutrition2, Name %in% ings)
  colSums(ings.nut[3:16])
}


#create empty data frame for orders
orders_nut=orders
namevector <- c(colnames(nutrition[3:16]),paste(colnames(nutrition[3:16]),"Share"))
orders_nut[,namevector] <- NA

#fill out nutrition info
for (i in 1:nrow(orders_nut)){
  orders_nut[i,6:19]=tots(i)
  orders_nut[i,20:33]=tots2(i)
}
#write.csv(data.frame(lapply(orders_nut, as.character), stringsAsFactors=FALSE), file="out/chipotle_nutrition.csv")
#Since choice_description is a list, need to cast as character first
