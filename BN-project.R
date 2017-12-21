dat <- read.csv(file = "Car_crash_data_project.csv", sep = ",", header = T)

names(dat)
# [1] "Month"                 "Year"                  "Week_day"              "Type_day"              "Casualties"           
# [6] "Num_vehicles"          "Lanes"                 "Road_width"            "Lane_width"            "Hard_shoulder"        
# [11] "Paved_hard_shoulder"   "Security_barriers"     "Aditional_panels"      "Edge_landmarks"        "Reflectors"           
# [16] "Surface"               "Luminosity"            "Atmosferic_factors"    "Limited_visibility"    "Danger_signs"         
# [21] "Crash_type"            "Traffic"               "Conc_distraction"      "Conc_alcohol_durgs"    "Conc_speed"           
# [26] "Conc_tyred_sleepy_ill" "Conc_weather"         
dat[,2] <- as.factor(dat[,2])
dat[,3] <- as.factor(dat[,3])
#dat[,5] <- as.factor(dat[,5])
#dat[,6] <- as.factor(dat[,6])
#dat[,7] <- as.factor(dat[,7])

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
library(rpart)

# decide values with a decission tree
# 2006 - 17

dat.test <- dat2006[which(is.na(dat2006[,17])),-17]
dat.train <- dat2006[which(!(is.na(dat2006[,17]))),]

names(dat.train)
d.tr <- rpart(Atmosferic_factors~., data = dat.train)
plot(d.tr, branch=0, margin=0.25, uniform=TRUE)
text(d.tr, use.n=TRUE, splits=TRUE, pretty=6)
predict(d.tr, dat.test, "prob")
predictions <- predict(d.tr, dat.test, "vector")
predictions



# 2015  - remove column 21

which(is.na(dat2015[,7]))


# Cleaning the data

cdat <- dat[,-22]
cdat <- cdat[-(which(is.na(cdat[,8]))),]


atmNAs <- which(is.na(cdat[,18]))

cdat$Atmosferic_factors[atmNAs] <- levels(cdat[,18])[predictions]


# Integer interpretation
cdat[which(cdat[,5]>5), 5] = "more_than_5"
cdat[,5] <- as.factor(cdat[,5])

cdat[which(cdat[,6]>2), 6] = "more_than_2"
cdat[,6] <- as.factor(cdat[,6])

cdat[,7] <- as.factor(cdat[,7])




cdat2015 <- cdat[which(cdat["Year"] == 2015 ),][,-2]
cdat2006 <- cdat[which(cdat["Year"] == 2006 ),][,-2]


for (i in 1: length(cdat[1,])){
  print(class(cdat[,i]))
}


chisqmat <- function(mat) {
  ncol = length(mat[1,]) 
  ret <- matrix(nrow = ncol, ncol = ncol)
  for (i in 1:ncol){
    for (j in 1:ncol) {
      ret[i, j]  <- chisq.test(mat[,i], mat[,j], simulate.p.value = T)$p.value
    }
  }
  return(ret)
}

mm <- chisqmat(cdat)

sum(mm[16,])
sum(mm[18,])
sum(mm[26,])




