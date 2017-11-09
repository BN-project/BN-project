dat <- read.csv(file = "Car_crash_data_project.csv", sep = ",", header = T)

names(dat)
# [1] "Month"                 "Year"                  "Week_day"              "Type_day"              "Casualties"           
# [6] "Num_vehicles"          "Lanes"                 "Road_width"            "Lane_width"            "Hard_shoulder"        
# [11] "Paved_hard_shoulder"   "Security_barriers"     "Aditional_panels"      "Edge_landmarks"        "Reflectors"           
# [16] "Surface"               "Luminosity"            "Atmosferic_factors"    "Limited_visibility"    "Danger_signs"         
# [21] "Crash_type"            "Traffic"               "Conc_distraction"      "Conc_alcohol_durgs"    "Conc_speed"           
# [26] "Conc_tyred_sleepy_ill" "Conc_weather"         
dat[,3] <- as.factor(dat[,3])
dat[,7] <- as.factor(dat[,7])



dat2015 <- dat[which(dat["Year"] == 2015 ),][,-2]
dat2006 <- dat[which(dat["Year"] == 2006 ),][,-2]

######################################################################################

nNaNCols2015 <- c()
for (i in 1:length(dat2015[1,])){
  nNaNCols2015 <- c(nNaNCols2015, sum(is.na(dat2015[,i])), i)
}
nNaNCols2015 <- matrix(nNaNCols2015, nrow = 2)
nNaNCols2015[,order(nNaNCols2015[1,], decreasing = T)]

# remove column 22 due to excessive NA density

nNaNRows2015 <- c()
for (i in 1:length(dat[,1])){
  nNaNRows2015 <- c(nNaNRows2015, sum(is.na(dat2015[i,])), i)
}
nNaNRows2015 <- matrix(nNaNRows2015, nrow = 2)
nNaNRows2015[,order(nNaNRows2015[1,], decreasing = T)[1:20]]

######################################################################################


nNaNCols2006 <- c()
for (i in 1:length(dat2006[1,])){
  nNaNCols2006 <- c(nNaNCols2006, sum(is.na(dat2006[,i])), i)
}
nNaNCols2006 <- matrix(nNaNCols2006, nrow = 2)
nNaNCols2006[,order(nNaNCols2006[1,], decreasing = T)]


nNaNRows2006 <- c()
for (i in 1:length(dat2006[,1])){
  nNaNRows2006 <- c(nNaNRows2006, sum(is.na(dat2006[i,])), i)
}
nNaNRows2006 <- matrix(nNaNRows2006, nrow = 2)
nNaNRows2006[,order(nNaNRows2006[1,], decreasing = T)[1:20]]

# Columns without NAs: 1, 2, 3, 4, 5, 6, 7, 16, 17, 19, 21, 23, 24, 25, 26, 27
# First column to analyze: 18, it has 13 NAs
library(class)

# assign values with knn


dat.test <- dat2006[which(is.na(dat2006[,17])),-17]
dat.train <- dat2006[which(!(is.na(dat2006[,17]))),]


require(tree)

names(dat.train)
d.tr <- tree(Atmosferic_factors~., data = dat.train)
predict(d.tr, dat.test)

require(bnlearn)

# Cualitative dataset reduction.
dat.test.cual = dat.test[,-5][,-4]
dat.train.cual = dat.train[,-5][,-4]

tan = tree.bayes(x=dat.train.cual, "Atmosferic_factors")

fitted = bn.fit(tan, dat.train.cual, method = "bayes")
predict(fitted, dat.test.cual)




