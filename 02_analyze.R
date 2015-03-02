library(scatterplot3d)


orders_nut$fatByCal <- (orders_nut$TotalFat.g. * 9) / orders_nut$Calories
orders_nut$proteinByCal <- (orders_nut$Protein.g. * 4) / orders_nut$Calories
orders_nut$carbByCal <- (orders_nut$Carbohydrates.g. * 4) / orders_nut$Calories

meals = filter(orders_nut, Calories > 0)
full_meals = filter(meals, !grepl("Chips", item_name))


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

#save stuff out for R Markdown to load
save(full_meals,file = "out/full_meals.rda")


#Start making plots

savePLOTS = FALSE

if (savePLOTS == TRUE) png("out/CalorieHist.png")
hist(full_meals$Calories)
if (savePLOTS == TRUE) dev.off()

par(pty="s") #make plots square

#plot percent of calories that come from fat, protein, carbs
gmPal <- colorRampPalette(c('green','magenta')) #green = lo-cal, magenta = hi-cal
full_meals$CalColors <- gmPal(50)[as.numeric(cut(full_meals$Calories,breaks = 100))]

if (savePLOTS == TRUE) png("out/3dCalBreakdown.png")
scatterplot3d(full_meals$proteinByCal, full_meals$fatByCal,full_meals$carbByCal, color=full_meals$CalColors, angle = 20, cex.symbols = 0.25, xlab="% Cal from Protein", ylab="% Cal from Fat", zlab = "% Cal from Carb", sub="green = lo-cal; magenta=hi-cal")
if (savePLOTS == TRUE) dev.off()

if (savePLOTS == TRUE) png("out/ProteinVsFat.png")
plot(full_meals$proteinByCal, full_meals$fatByCal, col=full_meals$CalColors, cex= 0.25, xlab="% Cal from Protein", ylab="% Cal from Fat", main="Chipotle Meals", sub="green = lo-cal; magenta=hi-cal")
if (savePLOTS == TRUE) dev.off()

if (savePLOTS == TRUE) png("out/CarbVsFat.png")
plot(full_meals$carbByCal, full_meals$fatByCal, col=full_meals$CalColors, cex= 0.25, xlab="% Cal from Carb", ylab="% Cal from Fat", main="Chipotle Meals", sub="green = lo-cal; magenta=hi-cal")
if (savePLOTS == TRUE) dev.off()

if (savePLOTS == TRUE) png("out/CarbVsProtein.png")
plot(full_meals$carbByCal, full_meals$proteinByCal, col=full_meals$CalColors, cex= 0.25, xlab="% Cal from Carb", ylab="% Cal from Protein", main="Chipotle Meals", sub="green = lo-cal; magenta=hi-cal")
if (savePLOTS == TRUE) dev.off()

#try something like
rng=c(0,0.8)
pairs(~fatByCal+proteinByCal+carbByCal, data = full_meals, col=full_meals$CalColors, 
      cex= 0.1, xlim=rng, ylim=rng, main="Chipotle Meals", sub="green = lo-cal; magenta=hi-cal")

#Plot Calorie per dollar
numPrice = as.numeric(sub("\\$","",full_meals$item_price))
calPerDollar = (full_meals$Calories * full_meals$quantity)/numPrice

if (savePLOTS == TRUE) png("out/CalperDollar.png")
hist(calPerDollar)
if (savePLOTS == TRUE) dev.off()

#Plot calories vs. nutrients, breakdown by main
rng=c(0,1.5)

palette(c("Orange","Grey","Brown", "Blue", "Green"))

if (savePLOTS == TRUE) png("out/CalVsProtein.png")
plot(full_meals$"Calories Share", full_meals$"Protein.g. Share",
     xlim=rng, ylim=rng, xlab = "%DV Calories", ylab="%DV Protein", 
     main = "Chipotle Meal Protein / Calorie Density", col=full_meals$Main)
abline(a = 0, b=1)
legend('topright', legend = levels(full_meals$Main), col = 1:5, cex = 0.8, pch = 1)
if (savePLOTS == TRUE) dev.off()

if (savePLOTS == TRUE) png("out/CalVsFiber.png")
plot(full_meals$"Calories Share", full_meals$"DietaryFiber.g. Share",
     xlim=rng, ylim=rng, xlab = "%DV Calories", ylab="%DV Fiber", 
     main = "Chipotle Meal Fiber / Calorie Density", col=full_meals$Main)
abline(a = 0, b=1)
legend('topright', legend = levels(full_meals$Main), col = 1:5, cex = 0.8, pch = 1)
if (savePLOTS == TRUE) dev.off()

if (savePLOTS == TRUE) png("out/CalVsVitA.png")
plot(full_meals$"Calories Share", full_meals$"VitaminA..DV. Share",
     xlim=rng, ylim=rng, xlab = "%DV Calories", ylab="%DV Vitamin A", 
     main = "Chipotle Meal Vitamin A / Calorie Density", col=full_meals$Main)
abline(a = 0, b=1)
legend('topright', legend = levels(full_meals$Main), col = 1:5, cex = 0.8, pch = 1)
if (savePLOTS == TRUE) dev.off()

if (savePLOTS == TRUE) png("out/CalVsVitC.png")
plot(full_meals$"Calories Share", full_meals$"VitaminC..DV. Share",
     xlim=rng, ylim=rng, xlab = "%DV Calories", ylab="%DV Vitamin C", 
     main = "Chipotle Meal Vitamin C / Calorie Density", col=full_meals$Main)
abline(a = 0, b=1)
legend('topright', legend = levels(full_meals$Main), col = 1:5, cex = 0.8, pch = 1)
if (savePLOTS == TRUE) dev.off()

if (savePLOTS == TRUE) png("out/CalVsIron.png")
plot(full_meals$"Calories Share", full_meals$"Iron..DV. Share",
     xlim=rng, ylim=rng, xlab = "%DV Calories", ylab="%DV Iron", 
     main = "Chipotle Meal Iron / Calorie Density", col=full_meals$Main)
abline(a = 0, b=1)
legend('topright', legend = levels(full_meals$Main), col = 1:5, cex = 0.8, pch = 1)
if (savePLOTS == TRUE) dev.off()

if (savePLOTS == TRUE) png("out/CalVsCalcium.png")
plot(full_meals$"Calories Share", full_meals$"Calcium...DV. Share",
     xlim=rng, ylim=rng, xlab = "%DV Calories", ylab="%DV Calcium", 
     main = "Chipotle Meal Calcium / Calorie Density", col=full_meals$Main)
abline(a = 0, b=1)
legend('topright', legend = levels(full_meals$Main), col = 1:5, cex = 0.8, pch = 1)
if (savePLOTS == TRUE) dev.off()


par(pty="m")#don't need square plots anymore
if (savePLOTS == TRUE) png("out/SugarShare.png")
hist(full_meals$"Sugar.g. Share")
if (savePLOTS == TRUE) dev.off()

#how nutrient rich is each ingredient?  (%DV of nutrient compared to %DV of Cal)
nut_rich = nutrition2 
nut_rich[3:16] = nut_rich[3:16]/nutrition2$Calories
colnames(nut_rich)=lapply(1:ncol(nut_rich), function(i) {strsplit(colnames(nut_rich)[i],split = "\\.")[[1]][1]})
nut_rich$TransFat <- NULL
rownames(nut_rich) <- nut_rich$Name
#save stuff out for R Markdown to load
save(nut_rich,file = "out/nut_rich.rda")


#want to do like radar plots of this.
library(fmsb)
#par(mar=c(1, 1, 1, 1))
#layout(matrix(1:2, ncol=2))
lapply(1:24, function(i){
  dat=nut_rich[i,4:15]
  datC=min(ceiling(max(dat)),20)
  radarchart(rbind(rep(datC,12), rep(0,12), dat), seg=datC, title = nut_rich$Name[i])
})

meats=nut_rich[c(4,5,2,1,37),]
radarchart(rbind(rep(4,12), rep(0,12), meats[, 4:15]), seg=4, title="Meats")
legend('topright', legend = meats$Name[1:5], col = 1:5, cex = 0.8, pch = 1)

ricenbeans=nut_rich[c(7,8,9,10),]
radarchart(rbind(rep(4,12), rep(0,12), ricenbeans[, 4:15]), seg=4, title="Rice & Beans")
legend('topright', legend = ricenbeans$Name[1:4], col = 1:4, cex = 0.8, pch = 1)

tortillas=nut_rich[c(6,20,21,22,23),]
radarchart(rbind(rep(4,12), rep(0,12), tortillas[, 4:15]), seg=4, title="Tortillas")
legend('topright', legend = tortillas$Name[1:5], col = 1:5, cex = 0.8, pch = 1)

fats=nut_rich[c(16,17,18,24),]
radarchart(rbind(rep(4,12), rep(0,12), fats[, 4:15]), seg=4, title="Fats")
legend('topright', legend = fats$Name[1:4], col = 1:4, cex = 0.8, pch = 1)

salsas=nut_rich[c(12,13,14,15),]
radarchart(rbind(rep(4,12), rep(0,12), salsas[, 4:15]), seg=4, title="Salsas")
legend('topright', legend = salsas$Name[1:4], col = 1:4, cex = 0.8, pch = 1)

#maybe make veggies.  Bring in veggies here + lettuce
veg=nut_rich[c(11,12,13,14,15,19),]
radarchart(rbind(rep(4,12), rep(0,12), veg[, 4:15]), seg=4, title="Vegetables")
legend('topright', legend = veg$Name[1:6], col = 1:6, cex = 0.8, pch = 1)
#move legend out of the way & expand palette

#how about print 24 one by 1, but size them according to calories?  Lettuce would disappear.