library(scatterplot3d)

getMain = function(i){strsplit(full_meals$item_name[i]," ")[[1]][1]}
getWrap = function(i){words=strsplit(full_meals$item_name[i]," ")[[1]]
                      if (length(words) >1) out=words[2] else out=words[1]
                      out}

full_meals$Main = full_meals$item_name
full_meals$Wrap = full_meals$item_name
for (i in 1:nrow(full_meals)){
  full_meals$Main[i] = getMain(i)
  full_meals$Wrap[i] = getWrap(i)
}

#Annoying to have these 10 misclassified (item_name = wrap type for these).  Just fix by hand.
full_meals$Main[274]="Carnitas"
full_meals$Main[322]="Chicken"
full_meals$Main[323]="Barbacoa"
full_meals$Main[327]="Barbacoa"
full_meals$Main[328]="Steak"
full_meals$Main[329]="Chicken"
full_meals$Main[425]="Steak"
full_meals$Main[426]="Chicken"
full_meals$Main[474]="Steak"
full_meals$Main[475]="Steak"
full_meals$Main[880]="Chicken"
full_meals$Main[881]="Chicken"

full_meals$Main=as.factor(full_meals$Main)
full_meals$Wrap=as.factor(full_meals$Wrap)


orders_nut$fatByCal <- (orders_nut$TotalFat.g. * 9) / orders_nut$Calories
orders_nut$proteinByCal <- (orders_nut$Protein.g. * 4) / orders_nut$Calories
orders_nut$carbByCal <- (orders_nut$Carbohydrates.g. * 4) / orders_nut$Calories

meals = filter(orders_nut, Calories > 0)
full_meals = filter(meals, !grepl("Chips", item_name))


#Start making plots
png("out/CalorieHist.png")
hist(full_meals$Calories)
dev.off()

#plot percent of calories that come from fat, protein, carbs
gmPal <- colorRampPalette(c('green','magenta')) #green = lo-cal, magenta = hi-cal
full_meals$CalColors <- gmPal(50)[as.numeric(cut(full_meals$Calories,breaks = 100))]

png("out/3dCalBreakdown.png")
scatterplot3d(full_meals$proteinByCal, full_meals$fatByCal,full_meals$carbByCal, color=full_meals$CalColors, angle = 20, cex.symbols = 0.25, xlab="% Cal from Protein", ylab="% Cal from Fat", zlab = "% Cal from Carb", sub="green = lo-cal; magenta=hi-cal")
dev.off()

png("out/ProteinVsFat.png")
plot(full_meals$proteinByCal, full_meals$fatByCal, col=full_meals$CalColors, cex= 0.25, xlab="% Cal from Protein", ylab="% Cal from Fat", main="Chipotle Meals", sub="green = lo-cal; magenta=hi-cal")
dev.off()

png("out/CarbVsFat.png")
plot(full_meals$carbByCal, full_meals$fatByCal, col=full_meals$CalColors, cex= 0.25, xlab="% Cal from Carb", ylab="% Cal from Fat", main="Chipotle Meals", sub="green = lo-cal; magenta=hi-cal")
dev.off()

png("out/ProteinVsProtein.png")
plot(full_meals$carbByCal, full_meals$proteinByCal, col=full_meals$CalColors, cex= 0.25, xlab="% Cal from Carb", ylab="% Cal from Protein", main="Chipotle Meals", sub="green = lo-cal; magenta=hi-cal")
dev.off()

#Plot Calorie per dollar
numPrice = as.numeric(sub("\\$","",full_meals$item_price))
calPerDollar = (full_meals$Calories * full_meals$quantity)/numPrice

png("out/CalperDollar.png")
hist(calPerDollar)
dev.off()

#Plot calories vs. nutrients, breakdown by main
rng=c(0,1.5)

palette(c("Orange","Grey","Brown", "Blue", "Green"))

png("out/CalVsProtein.png")
plot(full_meals$"Calories Share", full_meals$"Protein.g. Share",
     xlim=rng, ylim=rng, xlab = "%DV Calories", ylab="%DV Protein", 
     main = "Chipotle Meal Protein / Calorie Density", col=full_meals$Main)
abline(a = 0, b=1)
legend('topright', legend = levels(full_meals$Main), col = 1:5, cex = 0.8, pch = 1)
dev.off()

png("out/CalVsFiber.png")
plot(full_meals$"Calories Share", full_meals$"DietaryFiber.g. Share",
     xlim=rng, ylim=rng, xlab = "%DV Calories", ylab="%DV Fiber", 
     main = "Chipotle Meal Fiber / Calorie Density", col=full_meals$Main)
abline(a = 0, b=1)
legend('topright', legend = levels(full_meals$Main), col = 1:5, cex = 0.8, pch = 1)
dev.off()

png("out/CalVsVitA.png")
plot(full_meals$"Calories Share", full_meals$"VitaminA..DV. Share",
     xlim=rng, ylim=rng, xlab = "%DV Calories", ylab="%DV Vitamin A", 
     main = "Chipotle Meal Vitamin A / Calorie Density", col=full_meals$Main)
abline(a = 0, b=1)
legend('topright', legend = levels(full_meals$Main), col = 1:5, cex = 0.8, pch = 1)
dev.off()

png("out/CalVsVitC.png")
plot(full_meals$"Calories Share", full_meals$"VitaminC..DV. Share",
     xlim=rng, ylim=rng, xlab = "%DV Calories", ylab="%DV Vitamin C", 
     main = "Chipotle Meal Vitamin C / Calorie Density", col=full_meals$Main)
abline(a = 0, b=1)
legend('topright', legend = levels(full_meals$Main), col = 1:5, cex = 0.8, pch = 1)
dev.off()

png("out/CalVsIron.png")
plot(full_meals$"Calories Share", full_meals$"Iron..DV. Share",
     xlim=rng, ylim=rng, xlab = "%DV Calories", ylab="%DV Iron", 
     main = "Chipotle Meal Iron / Calorie Density", col=full_meals$Main)
abline(a = 0, b=1)
legend('topright', legend = levels(full_meals$Main), col = 1:5, cex = 0.8, pch = 1)
dev.off()

png("out/CalVsCalcium.png")
plot(full_meals$"Calories Share", full_meals$"Calcium...DV. Share",
     xlim=rng, ylim=rng, xlab = "%DV Calories", ylab="%DV Calcium", 
     main = "Chipotle Meal Calcium / Calorie Density", col=full_meals$Main)
abline(a = 0, b=1)
legend('topright', legend = levels(full_meals$Main), col = 1:5, cex = 0.8, pch = 1)
dev.off()

png("out/SugarShare.png")
hist(full_meals$"Sugar.g. Share")
dev.off()