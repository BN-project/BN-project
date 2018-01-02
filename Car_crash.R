
####    Probabilistic Modelling and Bayesian Networks    ####
####              The Project: Car Crashes               ####
#############################################################
# Reading the document:
crashes <-read.csv("Car_crash_data_project.csv",header=T,sep=",")

####    Preparation and exploration of data    ####

# Number of variables and observations:
dim(crashes) # 684 rows (observations) and 27 columns (variables)
anyNA(crashes) # True
# Name of the variables
names(crashes)
# [1] "Month"                 "Year"                  "Week_day"             
# [4] "Type_day"              "Casualties"            "Num_vehicles"         
# [7] "Lanes"                 "Road_width"            "Lane_width"           
# [10] "Hard_shoulder"         "Paved_hard_shoulder"   "Security_barriers"    
# [13] "Aditional_panels"      "Edge_landmarks"        "Reflectors"           
# [16] "Surface"               "Luminosity"            "Atmosferic_factors"   
# [19] "Limited_visibility"    "Danger_signs"          "Crash_type"           
# [22] "Traffic"               "Conc_distraction"      "Conc_alcohol_durgs"   
# [25] "Conc_speed"            "Conc_tyred_sleepy_ill" "Conc_weather"         

# 1.- Month
anyNA(crashes$Month) # FALSE
class(crashes$Month) # Factor
levels(crashes$Month) # Months
# length(levels(crashes$Month)) --> 12
summary(crashes$Month) # Frequency tables
# Apr Aug Dec Feb Jan Jul Jun Mar May Nov Oct Sep 
# 39  52  67  55  41  67  66  70  47  64  64  52 
plot(crashes$Month)
# Categorical, 12 categories, no NA, no 0 frequencies

# 2.- Year
anyNA(crashes$Year) # FALSE
class(crashes$Year) # INTEGER !!
crashes$Year <- as.factor(crashes$Year) # Factor transformation !!
levels(crashes$Year) # 2006, 2015
summary(crashes$Year) # Frequency tables
# 2006 2015 
#  431  253 
plot(crashes$Year)
# Categorical, 2 categories, no NA, no 0 frequencies

# 3.- Week day
anyNA(crashes$Week_day) # FALSE
class(crashes$Week_day) # INTEGER !!
crashes$Week_day <- as.factor(crashes$Week_day) # Factor transformation !!
levels(crashes$Week_day) # Days in numeric representation
summary(crashes$Week_day) # Frequency tables
#  1   2   3   4   5   6   7 
# 86 108  92  90 102 107  99 
plot(crashes$Week_day)
# Categorical, 7 categories, no NA, no 0 frequencies

# 4.- Type day
anyNA(crashes$Type_day) # FALSE
class(crashes$Type_day) # Factor
levels(crashes$Type_day) # Holiday, post-holiday, pre-holiday, work-day
summary(crashes$Type_day) # Frequency tables
#      Holiday Post_holiday  Pre_holiday     Work_day 
#          125           93          120          346 
plot(crashes$Type_day)
# Categorical, 4 categories, no NA, no 0 frequencies

# 5.- Casualties (Damnificados)
anyNA(crashes$Casualties) # FALSE
class(crashes$Casualties) # Integer, N
min(crashes$Casualties); max(crashes$Casualties) # Bounds: [1:33]
table(crashes$Casualties) # Values: 1-10,20,33 ; Frequencies.
#   1   2   3   4   5   6   7   8   9  10  20  33 
# 292 221  89  42  20   9   5   2   1   1   1   1 
summary(crashes$Casualties) # Summary of statisticals
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   1.000   1.000   2.000   2.114   2.250  33.000 
boxplot(crashes$Casualties)
# Integer ordinal, Values: 1-10,20,33, no NA

# 6.- Num_vehicles
anyNA(crashes$Num_vehicles) # FALSE
class(crashes$Num_vehicles) # Integer, N
min(crashes$Num_vehicles); max(crashes$Num_vehicles) # Bounds: [1:6]
table(crashes$Num_vehicles) # Values: 1-6 ; Frequencies.
#   1   2   3   4   5   6 
# 329 284  57   9   4   1 
summary(crashes$Num_vehicles) # Summary of statisticals
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   2.000   1.652   2.000   6.000 
boxplot(crashes$Num_vehicles)
# Integer ordinal, Values: 1-6, no NA

# 7.- Lanes
anyNA(crashes$Lanes) # FALSE
class(crashes$Lanes) # Integer
min(crashes$Lanes); max(crashes$Lanes) # Bounds: [0:5]
table(crashes$Lanes) # Values: 0-5 ; Frequencies.
#  0   1   2   3   4   5 
# 42  26 558  50   6   2 
summary(crashes$Lanes) # Summary of statisticals
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0.000   2.000   2.000   1.939   2.000   5.000 
boxplot(crashes$Lanes)
# Integer ordinal, Values: 0-5, no NA

# 8.- Road_width
anyNA(crashes$Road_width) # TRUE
which(is.na(crashes$Road_width) ==T) # NA values. All of them in 2015 data
#  [1] 436 437 448 454 455 467 468 477 480 483 486 489 495 502 518 527 528 537 540 541
# [21] 543 552 558 563 568 570 578 585 598 600 611 616 628 640 641 649 654 657 662 669
# [41] 675 676
length(which(is.na(crashes$Road_width) ==T)) # 42
class(crashes$Road_width) # Factor
levels(crashes$Road_width) # "6m_to_7m", "Less_6m", "More_7m" 
summary(crashes$Road_width) # Frequency tables
# 6m_to_7m  Less_6m  More_7m     NA's 
#      375       29      238       42 
plot(crashes$Road_width)
# Categorical, 3 categories, NA:42 (All in 2015), no 0 frequencies

# 9.- Lane_width
anyNA(crashes$Lane_width) # TRUE
which(is.na(crashes$Lane_width) ==T) # NA values. All of them in 2015 data.
#  [1] 436 437 448 454 455 467 468 477 480 483 486 489 495 502 518 527 528 537 540 541
# [21] 543 552 558 563 568 570 578 585 598 600 611 616 628 640 641 649 654 657 662 669
# [41] 675 676
which(is.na(crashes$Lane_width) ==T)==which(is.na(crashes$Road_width) ==T) # ALL TRUE
# length(which(is.na(crashes$Lane_width) ==T)) --> 42
class(crashes$Lane_width) # Factor
levels(crashes$Lane_width) # "325m_to_375m", "Less_325m", "More_375m"   
summary(crashes$Lane_width) # Frequency tables
# 325m_to_375m    Less_325m    More_375m         NA's 
#          509           88           45           42 
plot(crashes$Lane_width)
# Categorical, 3 categories, NA:42 (All in 2015)(same as Road_width), no 0 frequencies

# 10.- Hard_shoulder (Arcen)
anyNA(crashes$Hard_shoulder) # TRUE
which(is.na(crashes$Hard_shoulder) ==T) # NA values. All of them in 2015 data.
#  [1] 436 437 448 454 455 467 468 477 480 483 486 489 495 502 518 527 528 537 540 541
# [21] 543 552 558 563 568 570 578 585 598 600 611 616 628 640 641 649 654 657 662 669
# [41] 675 676
which(is.na(crashes$Hard_shoulder) ==T)==which(is.na(crashes$Road_width) ==T) # ALL TRUE
# length(which(is.na(crashes$Hard_shoulder) ==T)) --> 42
class(crashes$Hard_shoulder) # Factor
levels(crashes$Hard_shoulder) # "150m_to_250m", "Less_150m", "More_250m", "None"
summary(crashes$Hard_shoulder) # Frequency tables
# 150m_to_250m    Less_150m    More_250m         None         NA's 
#           47          266            5          324           42 
plot(crashes$Hard_shoulder)
# Categorical, 4 categories, NA:42 (All in 2015)(same as Road_width),  no 0 frequencies

# 11.- Paved_hard_shoulder
anyNA(crashes$Paved_hard_shoulder) # TRUE
which(is.na(crashes$Paved_hard_shoulder) ==T) # NA values. All of them in 2015 data.
#  [1] 436 437 448 454 455 467 468 477 480 483 486 489 495 502 518 527 528 537 540 541
# [21] 543 552 558 563 568 570 578 585 598 600 611 616 628 640 641 649 654 657 662 669
# [41] 675 676
which(is.na(crashes$Paved_hard_shoulder) ==T)==which(is.na(crashes$Road_width) ==T) # ALL TRUE
# length(which(is.na(crashes$Paved_hard_shoulder) ==T)) --> 42
class(crashes$Paved_hard_shoulder) # Factor
levels(crashes$Paved_hard_shoulder) # "N", "Y"
summary(crashes$Paved_hard_shoulder) # Frequency tables
#    N    Y NA's 
#  235  407   42 
plot(crashes$Paved_hard_shoulder)
# Categorical, 2 categories, NA:42 (All in 2015)(same as Road_width), no 0 frequencies

# 12.- Security_barriers
anyNA(crashes$Security_barriers) # TRUE
which(is.na(crashes$Security_barriers) ==T) # NA values. All of them in 2015 data.
#  [1] 436 437 448 454 455 467 468 477 480 483 486 489 495 502 518 527 528 537 540 541
# [21] 543 552 558 563 568 570 578 585 598 600 611 616 628 640 641 649 654 657 662 669
# [41] 675 676
which(is.na(crashes$Security_barriers) ==T)==which(is.na(crashes$Road_width) ==T) # ALL TRUE
# length(which(is.na(crashes$Security_barriers) ==T)) --> 42
class(crashes$Security_barriers) # Factor
levels(crashes$Security_barriers) # "N", "Y"
summary(crashes$Security_barriers) # Frequency tables
#    N    Y NA's 
#  391  251   42 
plot(crashes$Security_barriers)
# Categorical, 2 categories, NA:42 (All in 2015)(same as Road_width), no 0 frequencies

# 13.- Additional_panels
anyNA(crashes$Aditional_panels) # TRUE
which(is.na(crashes$Aditional_panels) ==T) # NA values. All of them in 2015 data.
#  [1] 436 437 448 454 455 467 468 477 480 483 486 489 495 502 518 527 528 537 540 541
# [21] 543 552 558 563 568 570 578 585 598 600 611 616 628 640 641 649 654 657 662 669
# [41] 675 676
which(is.na(crashes$Aditional_panels) ==T)==which(is.na(crashes$Road_width) ==T) # ALL TRUE
# length(which(is.na(crashes$Aditional_panels) ==T)) --> 42
class(crashes$Aditional_panels) # Factor
levels(crashes$Aditional_panels)  # "N", "Y"
summary(crashes$Aditional_panels) # Frequency tables
#    N    Y NA's 
#  452  190   42 
plot(crashes$Aditional_panels)
# Categorical, 2 categories, NA:42 (All in 2015)(same as Road_width), no 0 frequencies

# 14.- Edge_landmarks
anyNA(crashes$Edge_landmarks) # TRUE
which(is.na(crashes$Edge_landmarks) ==T) # NA values. All of them in 2015 data.
#  [1] 436 437 448 454 455 467 468 477 480 483 486 489 495 502 518 527 528 537 540 541
# [21] 543 552 558 563 568 570 578 585 598 600 611 616 628 640 641 649 654 657 662 669
# [41] 675 676
which(is.na(crashes$Edge_landmarks) ==T)==which(is.na(crashes$Road_width) ==T) # ALL TRUE
# length(which(is.na(crashes$Edge_landmarks) ==T)) --> 42
class(crashes$Edge_landmarks) # Factor
levels(crashes$Edge_landmarks) # "N", "Y"
summary(crashes$Edge_landmarks) # Frequency tables
#    N    Y NA's 
#  280  362   42 
plot(crashes$Edge_landmarks)
# Categorical, 2 categories, NA:42 (All in 2015)(same as Road_width), no 0 frequencies

# 15.- Reflectors
anyNA(crashes$Reflectors) # TRUE
which(is.na(crashes$Reflectors) ==T) # NA values. All of them in 2015 data.
#  [1] 436 437 448 454 455 467 468 477 480 483 486 489 495 502 518 527 528 537 540 541
# [21] 543 552 558 563 568 570 578 585 598 600 611 616 628 640 641 649 654 657 662 669
# [41] 675 676
which(is.na(crashes$Reflectors) ==T)==which(is.na(crashes$Road_width) ==T) # ALL TRUE
# length(which(is.na(crashes$Reflectors) ==T)) --> 42
class(crashes$Reflectors) # Factor
levels(crashes$Reflectors) # "N", "Y"
summary(crashes$Reflectors) # Frequency tables
#    N    Y NA's 
#  324  318   42 
plot(crashes$Reflectors)
# Categorical, 2 categories, NA:42 (All in 2015)(same as Road_width), no 0 frequencies

# 16.- Surface
anyNA(crashes$Surface) # FALSE
class(crashes$Surface) # Factor
levels(crashes$Surface) # "Dry", "Flooded", "Mud_or_gravel", "Oil",  "Other",  "Snow",  "Wet"         
# length(levels(crashes$Surface)) --> 7
summary(crashes$Surface) # Frequency tables
#  Dry   Flooded     Mud_or_gravel    Oil    Other     Snow     Wet
#  372         4                 3     10       17        2     276
plot(crashes$Surface)
# Categorical, 7 categories, no NA, ~ no 0 frequencies

# 17.- Luminosity
anyNA(crashes$Luminosity) # FALSE
class(crashes$Luminosity) # Factor
levels(crashes$Luminosity) # "Artificial_enough", "Artificial_not_enough", "Daylight", "Dusk",
#      "Dusk_artificial", "Dusk_no_artificial", "No_light"             
# length(levels(crashes$Luminosity)) --> 7
summary(crashes$Luminosity) # Frequency tables
#  Artificial_enough Artificial_not_enough  Daylight   Dusk  Dusk_artificial  Dusk_no_artificial  No_light 
#                 48                    30       468     29                6                  14        89 
plot(crashes$Luminosity)
# Categorical, 7 categories,  no NA, ~ no 0 frequencies

# 18.- Atmospheric_factors
anyNA(crashes$Atmosferic_factors) # TRUE
which(is.na(crashes$Atmosferic_factors) ==T) # NA values. All of them in 2006 data.
# 24  25  26  49  53  55 128 368 380 405 414 419 421
length(which(is.na(crashes$Atmosferic_factors) ==T)) # 13
class(crashes$Atmosferic_factors) # Factor
levels(crashes$Atmosferic_factors) # "Clear", "Cloudy", "Hail" , "Heavy_rain", "Light_fog", 
#      "Light_rain", "Snowing", "Strong_wind"
# length(levels(crashes$Atmosferic_factors)) --> 8
summary(crashes$Atmosferic_factors) # Frequency tables
# Clear   Cloudy    Hail  Heavy_rain  Light_fog  Light_rain  Snowing  Strong_wind   NA's 
#   395       37       1          59          7         167        2            3     13 
plot(crashes$Atmosferic_factors)
# Categorical, 8 categories, NA:13 (All in 2006),  no 0 frequencies

# 19.- Limited_visibility
anyNA(crashes$Limited_visibility) # FALSE
class(crashes$Limited_visibility) # Factor
levels(crashes$Limited_visibility) # "N" "Y"
summary(crashes$Limited_visibility) # Frequency tables
#   N   Y 
# 276 408 
plot(crashes$Limited_visibility)
# Categorical, 2 categories, no NA, no 0 frequencies

# 20.- Danger_signs
anyNA(crashes$Danger_signs) # TRUE
which(is.na(crashes$Danger_signs) ==T) # NA values. All of them in 2015 data.
#  [1] 436 437 448 454 455 467 468 477 480 483 486 489 495 502 518 527 528 537 540 541
# [21] 543 552 558 563 568 570 578 585 598 600 611 616 628 640 641 649 654 657 662 669
# [41] 675 676
which(is.na(crashes$Danger_signs) ==T)==which(is.na(crashes$Road_width) ==T) # ALL TRUE
# length(which(is.na(crashes$Danger_signs) ==T)) --> 42
class(crashes$Danger_signs) # Factor
levels(crashes$Danger_signs) # "No", "Not_needed", "Yes"     
# length(levels(crashes$Danger_signs)) --> 3
summary(crashes$Danger_signs) # Frequency tables
#     No Not_needed        Yes       NA's 
#    190        141        311         42 
plot(crashes$Danger_signs)
# Categorical, 3 categories, NA:42 (All in 2015)(same as Road_width), no 0 frequencies

# 21.- Crash_type
anyNA(crashes$Crash_type) # FALSE
class(crashes$Crash_type) # Factor
levels(crashes$Crash_type) #"Collision", "Crash", "Fall", "Off_the_road", "Other","Ped_run_over", "Rollover"
# length(levels(crashes$Crash_type)) --> 7
summary(crashes$Crash_type) # Frequency tables
#   Collision     Crash     Fall    Off_the_road     Other   Ped_run_over  Rollover 
#         361        28       23              91       156             14       11
plot(crashes$Crash_type)
# Categorical, 7 categories, no NA, ~ no 0 frequencies

# 22.- Traffic
anyNA(crashes$Traffic) # TRUE
which(is.na(crashes$Traffic) ==T) # NA values. All of them in 2015 data.
#   [1] 433 436 437 438 443 445 446 447 448 449 450 451 452 453 454 455 456 459 461 462 463
#  [22] 467 468 472 473 476 477 478 479 480 481 482 483 486 488 489 492 493 494 495 497 500
#  [43] 502 503 506 508 509 511 512 513 518 519 520 527 528 533 534 537 539 540 541 542 543
#  [64] 552 554 555 556 558 559 560 561 563 564 566 567 568 570 571 573 575 576 577 578 580
#  [85] 581 583 584 585 586 591 592 593 595 596 598 599 600 601 602 603 604 606 607 610 611
# [106] 612 613 614 616 620 622 625 626 628 629 630 631 632 633 635 636 637 639 640 641 643
# [127] 647 648 649 650 651 654 656 657 658 662 665 669 671 672 673 674 675 676 680 681 682
# [148] 683
intersect(which(is.na(crashes$Traffic) ==T),which(is.na(crashes$Road_width) ==T)
          )==which(is.na(crashes$Road_width) ==T) # ALL TRUE
length(which(is.na(crashes$Traffic) ==T)) # 148
class(crashes$Traffic) # Factor
levels(crashes$Traffic) # "Heavy", "Jam", "Light"
# length(levels(crashes$Traffic)) --> 3
summary(crashes$Traffic) # Frequency tables
#  Heavy   Jam Light  NA's 
#     33    13   490   148 
plot(crashes$Traffic)
# Categorical, 3 categories, NA:148 (All in 2015)(included Road_width), no 0 frequencies

# 23.- Conc_distraction
anyNA(crashes$Conc_distraction) # FALSE
class(crashes$Conc_distraction) # Factor
levels(crashes$Conc_distraction) # "N", "Y"
summary(crashes$Conc_distraction) # Frequency tables
#   N   Y 
# 481 203 
plot(crashes$Conc_distraction)
# Categorical, 2 categories, no NA, no 0 frequencies

# 24.- Conc_alcohol_durgs
anyNA(crashes$Conc_alcohol_durgs) # FALSE
class(crashes$Conc_alcohol_durgs) # Factor
levels(crashes$Conc_alcohol_durgs) # "N", "Y"
summary(crashes$Conc_alcohol_durgs) # Frequency tables
#   N   Y 
# 658  26 
plot(crashes$Conc_alcohol_durgs)
# Categorical, 2 categories, no NA, no 0 frequencies

# 25.- Conc_speed
anyNA(crashes$Conc_speed) # FALSE
class(crashes$Conc_speed) # Factor
levels(crashes$Conc_speed) # "N", "Y"
summary(crashes$Conc_speed) # Frequency tables
#   N   Y 
# 579 105 
plot(crashes$Conc_speed)
# Categorical, 2 categories, no NA, no 0 frequencies

# 26.- Conc_tyred_sleepy_ill
anyNA(crashes$Conc_tyred_sleepy_ill) # FALSE
class(crashes$Conc_tyred_sleepy_ill) # Factor
levels(crashes$Conc_tyred_sleepy_ill) # "N", "Y"
summary(crashes$Conc_tyred_sleepy_ill) # Frequency tables
#   N   Y 
# 588  96 
plot(crashes$Conc_tyred_sleepy_ill)
# Categorical, 2 categories, no NA, no 0 frequencies

# 27.- Conc_weather
anyNA(crashes$Conc_weather) # FALSE
class(crashes$Conc_weather) # Factor
levels(crashes$Conc_weather) # "N", "Y"
summary(crashes$Conc_weather) # Frequency tables
#   N   Y 
# 588  96 
plot(crashes$Conc_weather)
# Categorical, 2 categories, no NA, no 0 frequencies


#### Separate the samples of 2006 and 2015
crashes2015 <- crashes[which(crashes["Year"] == '2015' ),][,-2]
crashes2006 <- crashes[which(crashes["Year"] == '2006' ),][,-2]

#### Estimation of values for NA of 2006

# 1 Option. Decision tree and estimate values
library(rpart)

crashes2006.test <- crashes2006[which(is.na(crashes2006[,17])),-17]
crashes2006.train <- crashes2006[which(!(is.na(crashes2006[,17]))),]

names(crashes2006.train)
d.tr <- rpart(Atmosferic_factors~., data = crashes2006.train)
plot(d.tr, branch=0, margin=0.25, uniform=TRUE)
text(d.tr, use.n=TRUE, splits=TRUE, pretty=6)
predict(d.tr, crashes2006.test)
# We'll take the highest probability (Minimun max value > .7)
predictions <- predict(d.tr, crashes2006.test, "vector")
atmNAs2006 <- which(is.na(crashes2006[,17]))
crashes2006$Atmosferic_factors[atmNAs2006] <- levels(crashes2[,18])[predictions]


# We've tried estimate the values using all the data except from variable Traffic and "the 42 NA instances".
# Just one significant change. We've decided to use the 2006 prediction.

#### Estimation of values for NA of 2015

# 1 option: Remove Traffic + Remove NA instances

crashes2015 <- crashes2015[,-21]
#names(crashes2015)
crashes2015 <- crashes2015[-(which(is.na(crashes2015$Danger_signs) ==T)),]
# Verification: anyNA(crashes2015)


## Creation of crashes2 (combine the estimation values 1 of 2006 and 2015) to see
# the relation between variables.
crashes2 <-crashes[,-22]
crashes2 <- crashes2[-(which(is.na(crashes2$Danger_signs) ==T)),]

predictions <- predict(d.tr, crashes2006.test, "vector")
atmNAs <- which(is.na(crashes2[,18]))
crashes2$Atmosferic_factors[atmNAs] <- levels(crashes2[,18])[predictions]

#### Converting numeric attributes to categorical
# 1. More than 5 casualties -> new category
crashes2[which(crashes2[,5]>5),5]="more_than_5"
crashes2[,5] <-as.factor(crashes2[,5])

# 2. More than 2 vehicles -> new category
crashes2[which(crashes2[,6]>2),6]="more_than_2"
crashes2[,6] <-as.factor(crashes2[,6])
#table(crashes2[,6])

# 3. More than 2 lanes -> new category
crashes2[which(crashes2[,7]>2),7]="more_than_2"
crashes2[,7] <-as.factor(crashes2[,7])

# Modification of the data observing the first attempt of the model
# 1. Extreme category surface created -> new category
library(car)

crashes2$Surface <- recode(crashes2$Surface,"'Dry'='Dry';'Wet'='Wet';else='Extreme'")
# 2. Surface: grouping Dusk categories -> new category (diff between 2006 and 2015)
crashes2$Luminosity <- recode(crashes2$Luminosity,"'Artificial_enough'='Artificial_enough';'Artificial_not_enough'='Artificial_not_enough';'Daylight'='Daylight';'No_light'='No_light';else='Dusk'")


# Subsample of datas:
# crashes2015 <- crashes2[which(crashes2["Year"] == '2015' ),][,-2]
# crashes2006 <- crashes2[which(crashes2["Year"] == '2006' ),][,-2]

# Chi-square matrix p-value
chisqmatrix <- function(x) {
  names = colnames(x);  num = length(names)
  m = matrix(nrow=num,ncol=num,dimnames=list(names,names))
  for (i in 1:(num-1)) {
    for (j in (i+1):num) { 
      m[i,j] = chisq.test(x[,i],x[,j])$p.value
    }
  }
  return (m)
}

mat = chisqmatrix(crashes2)
mat

table(crashes2$Type_day,crashes2$Week_day)
table(crashes2$Surface,crashes2$Atmosferic_factors)
table(crashes2$Num_vehicles,crashes2$Casualties)
table(crashes2$Hard_shoulder,crashes2$Paved_hard_shoulder)
table(crashes2$Edge_landmarks,crashes2$Reflectors)
table(crashes2$Atmosferic_factors,crashes2$Conc_weather)

# Remove Week_day, Casualties, Paved_hard_shoulder, Edge landmarks, Atmosferic_factors,
#      Crash Type (because of low p-values)
crashes2 <- crashes2[,-c(3,5,11,14,18,21)]

## Finally, we separate data from 2006 and 2015
crashes2.2006 <- crashes2[which(crashes2["Year"] == '2006' ),][,-2]
crashes2.2015 <- crashes2[which(crashes2["Year"] == '2015' ),][,-2]

## Modelling the data
library(bnlearn)
library(Rgraphviz)
library(ggplot2)

## 2006 data
# Sampling data
set.seed(937) #937
rs <- sample(1:431, 300, replace=F)
# SAMPLE ERROR MADE
crashes2.2006.train <- crashes2.2006[rs,]
crashes2.2006.test <- crashes2.2006[-rs,]

rs2 <- sample(1:211, 150, replace=F)
crashes2.2015.train <- crashes2.2015[rs2,]
crashes2.2015.test <- crashes2.2015[-rs2,]


### Modelling (always optimized) 2006
# 1. gs
model2006.gs <- gs(crashes2.2006.train)

# 2. fast.iamb
model2006.iamb <-fast.iamb(crashes2.2006.train)
# 3. mmpc
model2006.mmpc <- mmpc(crashes2.2006.train)

# 4. hc 10/7
model2006.hc.bic <- hc(crashes2.2006.train,restart = 10, perturb = 7)
model2006.hc.k2 <- hc(crashes2.2006.train,restart = 10, perturb = 7,score = "k2")
model2006.hc.bde <- hc(crashes2.2006.train,restart = 10, perturb = 7,score = "bde")

bnlearn::score(x=model2006.hc.bic, data=crashes2.2006.train, type="bic") # -4519.796
bnlearn::score(x=model2006.hc.k2, data=crashes2.2006.train, type="bic") # -4698.222
bnlearn::score(x=model2006.hc.bde, data=crashes2.2006.train, type="bic") # -4519.244

# We will use model2006.hc.bde

# 5. tabu
model2006.tabu.bic <- tabu(crashes2.2006.train)
model2006.tabu.k2 <- tabu(crashes2.2006.train,score = "k2")
model2006.tabu.bde <- tabu(crashes2.2006.train,score = "bde")

bnlearn::score(x=model2006.tabu.bic, data=crashes2.2006.train, type="bic") # -4519.796
bnlearn::score(x=model2006.tabu.k2, data=crashes2.2006.train, type="bic") # -4899.863
bnlearn::score(x=model2006.tabu.bde, data=crashes2.2006.train, type="bic") # -4519.796

# We will use model2006.tabu.bic

# Plotting
graphviz.plot(x=model2006.gs, layout="dot", shape="ellipse")
graphviz.plot(x=model2006.iamb, layout="dot", shape="ellipse")
graphviz.plot(x=model2006.mmpc, layout="dot", shape="ellipse")
graphviz.plot(x=model2006.hc.bde, layout="dot", shape="ellipse")
graphviz.plot(x=model2006.tabu.bic, layout="dot", shape="ellipse")

# Our final model: HC.BDE  --> model2006.hc.bde

# Probability estimation
model2006.hc.bde.fitted <- bn.fit(model2006.hc.bde, data=crashes2.2006.train,method = "bayes")
model2006.hc.bde.fitted$Surface

### Modelling (always optimized) 2015
# 1. gs
model2015.gs <- gs(crashes2.2015.train)

# 2. fast.iamb
model2015.iamb <-fast.iamb(crashes2.2015.train)
# 3. mmpc
model2015.mmpc <- mmpc(crashes2.2015.train)

# 4. hc 10/7
model2015.hc.bic <- hc(crashes2.2015.train,restart = 10, perturb = 7)
model2015.hc.k2 <- hc(crashes2.2015.train,restart = 10, perturb = 7,score = "k2")
model2015.hc.bde <- hc(crashes2.2015.train,restart = 10, perturb = 7,score = "bde")

bnlearn::score(x=model2015.hc.bic, data=crashes2.2015.train, type="bic") # -2241.088
bnlearn::score(x=model2015.hc.k2, data=crashes2.2015.train, type="bic") # -7487.2192
bnlearn::score(x=model2015.hc.bde, data=crashes2.2015.train, type="bic") # -2241.442

# We will use model2015.hc.bde

# 5. tabu
model2015.tabu.bic <- tabu(crashes2.2015.train)
model2015.tabu.k2 <- tabu(crashes2.2015.train,score = "k2")
model2015.tabu.bde <- tabu(crashes2.2015.train,score = "bde")

bnlearn::score(x=model2015.tabu.bic, data=crashes2.2015.train, type="bic") #  -2241.088
bnlearn::score(x=model2015.tabu.k2, data=crashes2.2015.train, type="bic") # -7181.836
bnlearn::score(x=model2015.tabu.bde, data=crashes2.2015.train, type="bic") # -2241.442

# We will use model2015.tabu.bic

# Plotting
graphviz.plot(x=model2015.gs, layout="dot", shape="ellipse")
graphviz.plot(x=model2015.iamb, layout="dot", shape="ellipse")
graphviz.plot(x=model2015.mmpc, layout="dot", shape="ellipse")
graphviz.plot(x=model2015.hc.bde, layout="dot", shape="ellipse")
graphviz.plot(x=model2015.tabu.bic, layout="dot", shape="ellipse")

# Our final model: HC.BDE  --> model2015.hc.bde

# Probability estimation
model2015.hc.bde.fitted <- bn.fit(model2015.hc.bde, data=crashes2.2015.train,method = "bayes")
model2015.hc.bde.fitted$Surface

## 

eval_struct <- function(bn, train, test){
  gener <- stats::logLik(bn,test)/(dim(test)[2]*dim(test)[1])
  fit <- stats::logLik(bn,train)/(dim(test)[2]*dim(train)[1])
  return(c(gener, fit))
}

plotmodel1 <-eval_struct(model2006.hc.bde.fitted,crashes2.2006.train,crashes2.2006.test)
plotmodel1
