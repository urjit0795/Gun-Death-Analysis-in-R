
library(ggplot2)
library(kableExtra)
library(lares)
library(plyr)
library(reshape2)
library(digest)
library(party)
library(rpart)
library(e1071)
library(caret)
library(rpart.plot)
library(tidyverse)
library(arules)
library(arulesViz)
library(rattle)
library(tree)

##Getting the Data
gun_death.data = read.csv("gun_deaths.csv")
#Creating a Data Frame
gun_death.data.df <- data.frame(gun_death.data)

#Data Frame for Months
gun_deaths.month.df <- data.frame(gun_death.data$month)
#Table for month deaths
month_deaths <- table(gun_death.data.df$month)

#Creating a Data Frame for Month Deaths
month_deaths.df <- data.frame(month_deaths)
#Using Kable Function for creating an XML
kable(month_deaths.df, "pipe", col.names = c("Month", "Deaths"), align = c("l","c"))

#b)
#Creating a vector of Month Names
month_name <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

month_deaths.df$month_name  <- month_name 

#Plotting a Bar Chart
barplot(height = month_deaths.df$Freq, names= month_deaths.df$month_name)

#c) 
#Creating a data frame for Intent of Death
Death_Intent <- data.frame(gun_death.data.df$intent)

intent <- table(Death_Intent)

intent.df <- data.frame(intent)

ggplot(intent.df, aes(x = reorder(Death_Intent, -Freq), Freq)) + geom_bar(stat = "identity")

#d)
#Creating a data frame for Sex
sex_df <- data.frame(gun_death.data.df, sex = sample(c("M","F")))

table(gun_death.data.df$sex, gun_death.data.df$age)

T.male <- subset(gun_death.data, sex == "M")
T.female <- subset(gun_death.data , sex == "F")  

Female.df <- data.frame(T.female)

mean(T.female$age)

ggplot(gun_death.data.df) + geom_boxplot(aes(x=age, y=sex)) + coord_flip() +
  geom_boxplot(aes(x=age, y=sex)) +
  labs(x = "Age", y = "Sex", title = "Age of Gun Death victims by sex")

summary(Female.df$age)

mean(Female.df$age, na.rm = TRUE)

mean(T.male$age, na.rm = TRUE)

#e)
#No. of White Males with at least a high school education which were killed by guns in 2012
sum(gun_death.data.df$year == 2012 & gun_death.data.df$race == "White" & gun_death.data.df$education == "HS/GED" & gun_death.data.df == "M", na.rm = TRUE )


#f)
winter = sum(gun_death.data.df$month == 1 | gun_death.data.df$month == 2 | gun_death.data.df$month == 3, na.rm = TRUE)

spring = sum(gun_death.data.df$month == 4 | gun_death.data.df$month == 5 | gun_death.data.df$month == 6, na.rm = TRUE)

summer = sum(gun_death.data.df$month == 7 | gun_death.data.df$month == 8 | gun_death.data.df$month == 9, na.rm = TRUE)

fall = sum(gun_death.data.df$month == 10 | gun_death.data.df$month == 11 | gun_death.data.df$month == 12, na.rm = TRUE)



season = cut(gun_death.data.df$month, breaks = c(1,2,3,Inf)) 



gun_death.data.df$season[gun_death.data.df$month == 1 | gun_death.data.df$month == 2 | gun_death.data.df$month == 3 ] = 'Winter'
gun_death.data.df$season[gun_death.data.df$month == 4 | gun_death.data.df$month == 5 | gun_death.data.df$month == 6 ] = 'Spring'
gun_death.data.df$season[gun_death.data.df$month == 7 | gun_death.data.df$month == 8 | gun_death.data.df$month == 9 ] = 'Summer'
gun_death.data.df$season[gun_death.data.df$month == 10 | gun_death.data.df$month == 11 | gun_death.data.df$month == 12 ] = 'Fall'



table(gun_death.data.df$season)
#g
Q7 = data.frame(c(gun_death.data.df$race, gun_death.data.df$intent))



White_suicide <- sum(gun_death.data.df$race == "White" & gun_death.data.df$intent == "Suicide" , na.rm = TRUE )
White_homicide <- sum(gun_death.data.df$race == "White" & gun_death.data.df$intent == "Homicide" , na.rm = TRUE )

Black_suicide <- sum(gun_death.data.df$race == "Black" & gun_death.data.df$intent == "Suicide" , na.rm = TRUE )
Black_homicide <- sum(gun_death.data.df$race == "Black" & gun_death.data.df$intent == "Homicide" , na.rm = TRUE )

Hispanics_suicide <- sum(gun_death.data.df$race == "Hispanic" & gun_death.data.df$intent == "Suicide" , na.rm = TRUE )
Hispanics_homicide <- sum(gun_death.data.df$race == "Hispanic" & gun_death.data.df$intent == "Homicide" , na.rm = TRUE )

suicide = c(White_suicide,Black_suicide,Hispanics_suicide)

homicide = c(White_homicide,Black_homicide,Hispanics_homicide)

Intent_SH = data.frame(suicide,homicide)

names(Intent_SH) = c("Suicide", "Homicide")

barplot(height = as.matrix(Intent_SH), beside = TRUE ,col = rainbow(3))

legend("topright", c("White", "Black", "Hispanics"), cex = 1.0, fill = rainbow(3))
ifelse (White_suicide > White_homicide, "whites who are killed by guns are more likely to die because of suicide", "whites who are killed by guns are more likely to die because of homicide")

ifelse (Black_suicide > Black_homicide, "Blacks who are killed by guns are more likely to die because of suicide", "Blacks who are killed by guns are more likely to die because of homicide")

ifelse (Hispanics_suicide > Hispanics_homicide, "Hispanics who are killed by guns are more likely to die because of suicide", "Hispanics who are killed by guns are more likely to die because of homicide")



#h
#Police Involvement
gundeath_police <- sum(gun_death.data.df$police == "1")

#Police Not Involved
gundeath_wpolice <- sum(gun_death.data.df$police == "0")

gundeath_police

gundeath_wpolice

#Simple Pie Chart
slices = c(gundeath_police,gundeath_wpolice)

lbls = c("Gun_Death_Police", "Gun_Death_Wo_Police")

pie_chart = pie(slices,lbls, main = "Police Involvement in Gun Deaths")

gun_death.data.df = na.omit(gun_death.data.df)

corr_var(gun_death.data.df,police,limit = 1, top=20)

