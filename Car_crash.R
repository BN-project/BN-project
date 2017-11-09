
####    Probabilistic Modelling and Bayesian Networks    ####
####              The Project: Car Crashes               ####
#############################################################
# Reading the document:
crashes <-read.csv("Car_crash_data_project.csv",header=T,sep=",")

####    Preparation and exploration of data    ####

# Number of variables and observations:
dim(crashes) # 684 rows (observations) and 27 columns (variables)
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
levels(crashes$Month) 
# length(levels(crashes$Month)) --> 12
summary(crashes$Month) # Frecuency tables
anyNA(crashes$Month) # FALSE
plot(crashes$Month)
# Categorical, 12 categories, no NA, no 0 frecuencies

# 2.- Year
anyNA(crashes$Year) # FALSE
class(crashes$Year) # INTEGER !!
crashes$Year <- as.factor(crashes$Year) # Factor transformation !!
levels(crashes$Year) 
summary(crashes$Year) # Frecuency tables
plot(crashes$Year)
# Categorical, 2 categories, no NA, no 0 frecuencies

# 3.- Week day
anyNA(crashes$Week_day) # FALSE
class(crashes$Week_day) # INTEGER !!
crashes$Week_day <- as.factor(crashes$Week_day) # Factor transformation !!
levels(crashes$Week_day) 
summary(crashes$Week_day) # Frecuency tables
plot(crashes$Week_day)
# Categorical, 7 categories, no NA, no 0 frecuencies

# 5.- Casualties (Damnificados)
anyNA(crashes$Casualties) # FALSE
class(crashes$Casualties) # Integer, N
min(crashes$Casualties); max(crashes$Casualties) # Bounds: [1:33]
table(crashes$Casualties) # Values: 1-10,20,33 ; Frecuencies.
summary(crashes$Casualties) # Summary of statisticals
boxplot(crashes$Casualties)
# Integer ordinal, Values: 1-10,20,33, no NA

# 6.- Num_vehicles
anyNA(crashes$Num_vehicles) # FALSE
class(crashes$Num_vehicles) # Integer, N
min(crashes$Num_vehicles); max(crashes$Num_vehicles) # Bounds: [1:6]
table(crashes$Num_vehicles) # Values: 1-6 ; Frecuencies.
summary(crashes$Num_vehicles) # Summary of statisticals
boxplot(crashes$Num_vehicles)
# Integer ordinal, Values: 1-6, no NA

# 7.- Lanes
anyNA(crashes$Lanes) # FALSE
class(crashes$Lanes) # Integer
min(crashes$Lanes); max(crashes$Lanes) # Bounds: [0:5]
table(crashes$Lanes) # Values: 0-5 ; Frecuencies.
summary(crashes$Lanes) # Summary of statisticals
boxplot(crashes$Lanes)
# Integer ordinal, Values: 0-5, no NA

# 8.- Road_width
anyNA(crashes$Road_width) # TRUE
which(is.na(crashes$Road_width) ==T) # NA values. All of them in 2015 data
# length(which(is.na(crashes$Road_width) ==T)) --> 42
class(crashes$Road_width) # Factor
levels(crashes$Road_width) 
summary(crashes$Road_width) # Frecuency tables
plot(crashes$Road_width)
# Categorical, 3 categories, NA:42 (All in 2015), no 0 frecuencies

# 9.- Lane_width
anyNA(crashes$Lane_width) # TRUE
which(is.na(crashes$Lane_width) ==T) # NA values. All of them in 2015 data.
which(is.na(crashes$Lane_width) ==T)==which(is.na(crashes$Road_width) ==T)
# length(which(is.na(crashes$Lane_width) ==T)) --> 42
class(crashes$Lane_width) # Factor
levels(crashes$Lane_width) 
summary(crashes$Lane_width) # Frecuency tables
plot(crashes$Lane_width)
# Categorical, 3 categories, NA:42 (All in 2015)(same as Road_width), no 0 frecuencies

# 10.- Hard_shoulder
anyNA(crashes$Hard_shoulder) # TRUE
which(is.na(crashes$Hard_shoulder) ==T) # NA values. All of them in 2015 data.
which(is.na(crashes$Hard_shoulder) ==T)==which(is.na(crashes$Road_width) ==T)
# length(which(is.na(crashes$Hard_shoulder) ==T)) --> 42
class(crashes$Hard_shoulder) # Factor
levels(crashes$Hard_shoulder) 
summary(crashes$Hard_shoulder) # Frecuency tables
plot(crashes$Hard_shoulder)
# Categorical, 4 categories, NA:42 (All in 2015)(same as Road_width), no 0 frecuencies

# 11.- Paved_hard_shoulder
anyNA(crashes$Paved_hard_shoulder) # TRUE
which(is.na(crashes$Paved_hard_shoulder) ==T) # NA values. All of them in 2015 data.
which(is.na(crashes$Paved_hard_shoulder) ==T)==which(is.na(crashes$Road_width) ==T)
# length(which(is.na(crashes$Paved_hard_shoulder) ==T)) --> 42
class(crashes$Paved_hard_shoulder) # Factor
levels(crashes$Paved_hard_shoulder) 
summary(crashes$Paved_hard_shoulder) # Frecuency tables
plot(crashes$Paved_hard_shoulder)
# Categorical, 2 categories, NA:42 (All in 2015)(same as Road_width), no 0 frecuencies

# 12.- Security_barriers
anyNA(crashes$Security_barriers) # TRUE
which(is.na(crashes$Security_barriers) ==T) # NA values. All of them in 2015 data.
which(is.na(crashes$Security_barriers) ==T)==which(is.na(crashes$Road_width) ==T)
# length(which(is.na(crashes$Security_barriers) ==T)) --> 42
class(crashes$Security_barriers) # Factor
levels(crashes$Security_barriers) 
summary(crashes$Security_barriers) # Frecuency tables
plot(crashes$Security_barriers)
# Categorical, 2 categories, NA:42 (All in 2015)(same as Road_width), no 0 frecuencies

# 13.- Aditional_panels
anyNA(crashes$Aditional_panels) # TRUE
which(is.na(crashes$Aditional_panels) ==T) # NA values. All of them in 2015 data.
which(is.na(crashes$Aditional_panels) ==T)==which(is.na(crashes$Road_width) ==T)
# length(which(is.na(crashes$Aditional_panels) ==T)) --> 42
class(crashes$Aditional_panels) # Factor
levels(crashes$Aditional_panels) 
summary(crashes$Aditional_panels) # Frecuency tables
plot(crashes$Aditional_panels)
# Categorical, 2 categories, NA:42 (All in 2015)(same as Road_width), no 0 frecuencies

# 14.- Edge_landmarks
anyNA(crashes$Edge_landmarks) # TRUE
which(is.na(crashes$Edge_landmarks) ==T) # NA values. All of them in 2015 data.
which(is.na(crashes$Edge_landmarks) ==T)==which(is.na(crashes$Road_width) ==T)
# length(which(is.na(crashes$Edge_landmarks) ==T)) --> 42
class(crashes$Edge_landmarks) # Factor
levels(crashes$Edge_landmarks) 
summary(crashes$Edge_landmarks) # Frecuency tables
plot(crashes$Edge_landmarks)
# Categorical, 2 categories, NA:42 (All in 2015)(same as Road_width), no 0 frecuencies

# 15.- Reflectors
anyNA(crashes$Reflectors) # TRUE
which(is.na(crashes$Reflectors) ==T) # NA values. All of them in 2015 data.
which(is.na(crashes$Reflectors) ==T)==which(is.na(crashes$Road_width) ==T)
# length(which(is.na(crashes$Reflectors) ==T)) --> 42
class(crashes$Reflectors) # Factor
levels(crashes$Reflectors) 
summary(crashes$Reflectors) # Frecuency tables
plot(crashes$Reflectors)
# Categorical, 2 categories, NA:42 (All in 2015)(same as Road_width), no 0 frecuencies

# 16.- Surface
anyNA(crashes$Surface) # FALSE
class(crashes$Surface) # Factor
levels(crashes$Surface) 
# length(levels(crashes$Surface)) --> 7
summary(crashes$Surface) # Frecuency tables
plot(crashes$Surface)
# Categorical, 7 categories, no NA, no 0 frecuencies

# 17.- Luminosity
anyNA(crashes$Luminosity) # FALSE
class(crashes$Luminosity) # Factor
levels(crashes$Luminosity) 
# length(levels(crashes$Luminosity)) --> 7
summary(crashes$Luminosity) # Frecuency tables
plot(crashes$Luminosity)
# Categorical, 7 categories,  no NA, no 0 frecuencies

# 18.- Atmosferic_factors
anyNA(crashes$Atmosferic_factors) # TRUE
which(is.na(crashes$Atmosferic_factors) ==T) # NA values. All of them in 2006 data.
# length(which(is.na(crashes$Atmosferic_factors) ==T)) --> 13
class(crashes$Atmosferic_factors) # Factor
levels(crashes$Atmosferic_factors) 
# length(levels(crashes$Atmosferic_factors)) --> 8
summary(crashes$Atmosferic_factors) # Frecuency tables
plot(crashes$Atmosferic_factors)
# Categorical, 8 categories, NA:13 (All in 2006), no 0 frecuencies

# 19.- Limited_visibility
anyNA(crashes$Limited_visibility) # FALSE
class(crashes$Limited_visibility) # Factor
levels(crashes$Limited_visibility) 
summary(crashes$Limited_visibility) # Frecuency tables
plot(crashes$Limited_visibility)
# Categorical, 2 categories, no NA, no 0 frecuencies

# 20.- Danger_signs
anyNA(crashes$Danger_signs) # TRUE
which(is.na(crashes$Danger_signs) ==T) # NA values. All of them in 2015 data.
which(is.na(crashes$Danger_signs) ==T)==which(is.na(crashes$Road_width) ==T)
# length(which(is.na(crashes$Danger_signs) ==T)) --> 42
class(crashes$Danger_signs) # Factor
levels(crashes$Danger_signs) 
# length(levels(crashes$Danger_signs)) --> 3
summary(crashes$Danger_signs) # Frecuency tables
plot(crashes$Danger_signs)
# Categorical, 3 categories, NA:42 (All in 2015)(same as Road_width), no 0 frecuencies

# 21.- Crash_type
anyNA(crashes$Crash_type) # FALSE
class(crashes$Crash_type) # Factor
levels(crashes$Crash_type) 
# length(levels(crashes$Crash_type)) --> 7
summary(crashes$Crash_type) # Frecuency tables
plot(crashes$Crash_type)
# Categorical, 7 categories, no NA, no 0 frecuencies

# 22.- Traffic
anyNA(crashes$Traffic) # TRUE
which(is.na(crashes$Traffic) ==T) # NA values. All of them in 2015 data.
intersect(which(is.na(crashes$Traffic) ==T),which(is.na(crashes$Road_width) ==T))==which(is.na(crashes$Road_width) ==T)
# length(which(is.na(crashes$Traffic) ==T)) --> 148
class(crashes$Traffic) # Factor
levels(crashes$Traffic) 
# length(levels(crashes$Traffic)) --> 3
summary(crashes$Traffic) # Frecuency tables
plot(crashes$Traffic)
# Categorical, 3 categories, NA:148 (All in 2015)(included Road_width), no 0 frecuencies

# 23.- Conc_distraction
anyNA(crashes$Conc_distraction) # FALSE
class(crashes$Conc_distraction) # Factor
levels(crashes$Conc_distraction) 
summary(crashes$Conc_distraction) # Frecuency tables
plot(crashes$Conc_distraction)
# Categorical, 2 categories, no NA, no 0 frecuencies

# 24.- Conc_alcohol_durgs
anyNA(crashes$Conc_alcohol_durgs) # FALSE
class(crashes$Conc_alcohol_durgs) # Factor
levels(crashes$Conc_alcohol_durgs) 
summary(crashes$Conc_alcohol_durgs) # Frecuency tables
plot(crashes$Conc_alcohol_durgs)
# Categorical, 2 categories, no NA, no 0 frecuencies

# 25.- Conc_speed
anyNA(crashes$Conc_speed) # FALSE
class(crashes$Conc_speed) # Factor
levels(crashes$Conc_speed) 
summary(crashes$Conc_speed) # Frecuency tables
plot(crashes$Conc_speed)
# Categorical, 2 categories, no NA, no 0 frecuencies

# 26.- Conc_tyred_sleepy_ill
anyNA(crashes$Conc_tyred_sleepy_ill) # FALSE
class(crashes$Conc_tyred_sleepy_ill) # Factor
levels(crashes$Conc_tyred_sleepy_ill) 
summary(crashes$Conc_tyred_sleepy_ill) # Frecuency tables
plot(crashes$Conc_tyred_sleepy_ill)
# Categorical, 2 categories, no NA, no 0 frecuencies

# 27.- Conc_weather
anyNA(crashes$Conc_weather) # FALSE
class(crashes$Conc_weather) # Factor
levels(crashes$Conc_weather) 
summary(crashes$Conc_weather) # Frecuency tables
plot(crashes$Conc_weather)
# Categorical, 2 categories, no NA, no 0 frecuencies