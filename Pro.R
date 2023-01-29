dev.off() #clears the graph window
cat('\014') #clears the console
rm(list=ls()) #clears all user objects from the environment
setwd("~/Users/akshaymusuku/Desktop/jalaja/Airline satisfaction project")
library(RCurl)
library(jsonlite)
library(tidyverse)
library(ggplot2)

dataset <- 'Spring2020-survey-02 (1).json'
testdf <- jsonlite::fromJSON(dataset)
str(testdf)
View(testdf)
summary(testdf)

#~~~~~~~~~Phase 1~~~~~~~~~~~~

#The column 'Likelihood to recommend is the dependent variable in the given dataframe. To find the missing
#values in this column, which and is.na functions are used. 'which' function gives the true indices of a 
#logical object, allowing for array indices and 'is.na' is a generic function that indicates which elements
#are missing.
which(is.na(testdf$Likelihood.to.recommend))
View(testdf[4690,])

#The length command in the below command gives out the number of missing values in the mentioned column.
#Here, the number of missing values in the 'Likelihood.to.recommend' column are 26.
length(which(is.na(testdf$Likelihood.to.recommend)))

#View command is used to display the missing rows which contain the missing values in the likelihood to 
#recommend column. The rows which contains the missing values also contain missing values in all columns.
View(testdf[which(is.na(testdf$Likelihood.to.recommend)),])

#The 26 missing rows which contain NA data are deleted from the data frame in the command below.
testdf <- testdf[-which(is.na(testdf$Likelihood.to.recommend)),]
View(testdf)

View(testdf[763,])


#Here, we now check for white spaces and removing them using gsub:
testdf$Destination.City <- gsub(" +$","",testdf$Destination.City)
testdf$Origin.City <- gsub(" +$","",testdf$Origin.City)
testdf$Airline.Status <- gsub(" +$","",testdf$Airline.Status)
testdf$Age <- gsub(" +$","",testdf$Age)
testdf$Gender <- gsub(" +$","",testdf$Gender)
testdf$Price.Sensitivity <- gsub(" +$","",testdf$Price.Sensitivity)
testdf$Year.of.First.Flight <- gsub(" +$","",testdf$Year.of.First.Flight)
testdf$Flights.Per.Year <- gsub(" +$","",testdf$Flights.Per.Year)
testdf$Loyalty <- gsub(" +$","",testdf$Loyalty)
testdf$Type.of.Travel <- gsub(" +$","",testdf$Type.of.Travel)
testdf$Total.Freq.Flyer.Accts <- gsub(" +$","",testdf$Total.Freq.Flyer.Accts)
testdf$Shopping.Amount.at.Airport <- gsub(" +$","",testdf$Shopping.Amount.at.Airport)
testdf$Eating.and.Drinking.at.Airport <- gsub(" +$","",testdf$Eating.and.Drinking.at.Airport)
testdf$Class <- gsub(" +$","",testdf$Class)
testdf$Day.of.Month <- gsub(" +$","",testdf$Day.of.Month)
testdf$Flight.date <- gsub(" +$","",testdf$Flight.date)
testdf$Partner.Code <- gsub(" +$","",testdf$Partner.Code)
testdf$Partner.Name <- gsub(" +$","",testdf$Partner.Name)
testdf$Origin.State <- gsub(" +$","",testdf$Origin.State)
testdf$Destination.State <- gsub(" +$","",testdf$Destination.State)

testdf$Scheduled.Departure.Hour <- gsub(" +$","",testdf$Scheduled.Departure.Hour)
testdf$Departure.Delay.in.Minutes <- gsub(" +$","",testdf$Departure.Delay.in.Minutes)
testdf$Arrival.Delay.in.Minutes <- gsub(" +$","",testdf$Arrival.Delay.in.Minutes)
testdf$Flight.cancelled <- gsub(" +$","",testdf$Flight.cancelled)
testdf$Flight.time.in.minutes <- gsub(" +$","",testdf$Flight.time.in.minutes)
testdf$Flight.Distance <- gsub(" +$","",testdf$Flight.Distance)
testdf$Likelihood.to.recommend <- gsub(" +$","",testdf$Likelihood.to.recommend)
testdf$olong <- gsub(" +$","",testdf$olong)
testdf$olat <- gsub(" +$","",testdf$olat)
testdf$dlong <- gsub(" +$","",testdf$dlong)
testdf$dlat <- gsub(" +$","",testdf$dlat)

#Now, we need to convert the columns containing numeric data into numeric type and factor data into factor types:
testdf$Destination.City <- as.factor(testdf$Destination.City)
testdf$Origin.City <- as.factor(testdf$Origin.City)
testdf$Airline.Status <- as.factor(testdf$Airline.Status)
testdf$Gender <- as.factor(testdf$Gender)
testdf$Type.of.Travel <- as.factor(testdf$Type.of.Travel)
testdf$Class <- as.factor(testdf$Class)
testdf$Flight.cancelled <- as.factor(testdf$Flight.cancelled)
testdf$Partner.Code <- as.factor(testdf$Partner.Code)
testdf$Partner.Name <- as.factor(testdf$Partner.Name)
testdf$Destination.State <- as.factor(testdf$Destination.State)
testdf$Origin.State <- as.factor(testdf$Origin.State)


testdf$Age <- as.numeric(testdf$Age)
testdf$Price.Sensitivity <- as.numeric(testdf$Price.Sensitivity)
testdf$Year.of.First.Flight <- as.numeric(testdf$Year.of.First.Flight)
testdf$Flights.Per.Year <- as.numeric(testdf$Flights.Per.Year)
testdf$Loyalty <- as.numeric(testdf$Loyalty)
testdf$Total.Freq.Flyer.Accts <- as.numeric(testdf$Total.Freq.Flyer.Accts)
testdf$Shopping.Amount.at.Airport <- as.numeric(testdf$Shopping.Amount.at.Airport)
testdf$Eating.and.Drinking.at.Airport <- as.numeric(testdf$Eating.and.Drinking.at.Airport)
testdf$Day.of.Month <- as.numeric(testdf$Day.of.Month)
testdf$Scheduled.Departure.Hour <- as.numeric(testdf$Scheduled.Departure.Hour)
testdf$Departure.Delay.in.Minutes <- as.numeric(testdf$Departure.Delay.in.Minutes)
testdf$Arrival.Delay.in.Minutes <- as.numeric(testdf$Arrival.Delay.in.Minutes)
testdf$Flight.time.in.minutes <- as.numeric(testdf$Flight.time.in.minutes)
testdf$Flight.Distance <- as.numeric(testdf$Flight.Distance)
testdf$Likelihood.to.recommend <- as.numeric(testdf$Likelihood.to.recommend)
testdf$olong <- as.numeric(testdf$olong)
testdf$olat <- as.numeric(testdf$olat)
testdf$dlong <- as.numeric(testdf$dlong)
testdf$dlat <- as.numeric(testdf$dlat)
str(testdf)

#Creating a new column Net Promotor Score (NPS) to categorise customers into three categories named Promotors, Detractors
#and Passive. The categories are divided based on the values of Likelihood.to.recommend column.
testdf <- testdf %>%
  mutate(NPS = ifelse(Likelihood.to.recommend > 8, "Promotor", ifelse(Likelihood.to.recommend > 6, "Passive","Detractor")))
table(testdf$NPS)

#The scores which are above 8 are classified as Promotors as they help the airline in spreading positive
#words about it.
#The scores which are in the range of 7 and 8 are classified are Passive.
#The scores which are below 7 are classified as Detractors as they are most likely to stop using the 
#service and may also actively tell others not to use the service.

testdf <- testdf %>%
  mutate(LoyCol = ifelse(Loyalty > 0, "More Loyal", "Less Loyal"))
#The above categorical attribute is created to easily interpret the loyalty of the passengers. If the loyalty
#is less than 0 then they are interpreted to be less loyal and if more than 0 then more loyal.

#Creating similar categorical variable for studying different types of .
testdf <- testdf %>%
  mutate(NPSn = ifelse(Likelihood.to.recommend > 8, "1", ifelse(Likelihood.to.recommend == 7 | Likelihood.to.recommend ==8, "2", "3")))

#The scores which are above 8 are classified as 1 (Promotors) as they help the airline in spreading positive
#words about it.
#The scores which are in the range of 7 and 8 are classified are 2 (Passive).
#The scores which are below 7 are classified as 3 (Detractors) as they are most likely to stop using the 
#service and may also actively tell others not to use the service.


#Creating another categorical value to divide travellers into different age groups.
testdf <- testdf %>%
  mutate(AgeG = ifelse(Age <= 35, "YoungA", ifelse(Age > 55, "OlderA", "MiddleA")))

#The first age group consisting of ages less than 35 are classified as Young adults.
#The next age group consisting of ages between 35 and 55 are classified as middle-aged adults.
#The third age group consisting of ages greater than 55 are classified as older adults.


#Additionally, all the independent variables or other columns in the data frame are checked for any missing
#values using the same approach we have used above which is using the which and is.na functions.
which(is.na(testdf$Destination.City))
which(is.na(testdf$Origin.City))
which(is.na(testdf$Airline.Status))
which(is.na(testdf$Age))
which(is.na(testdf$Gender))
which(is.na(testdf$Price.Sensitivity))
which(is.na(testdf$Year.of.First.Flight))
which(is.na(testdf$Flights.Per.Year))
which(is.na(testdf$Loyalty))
which(is.na(testdf$Type.of.Travel))
which(is.na(testdf$Total.Freq.Flyer.Accts))
which(is.na(testdf$Shopping.Amount.at.Airport))
which(is.na(testdf$Eating.and.Drinking.at.Airport))
which(is.na(testdf$Class))
which(is.na(testdf$Day.of.Month))
which(is.na(testdf$Flight.date))
which(is.na(testdf$Partner.Code))
which(is.na(testdf$Partner.Name))
which(is.na(testdf$Origin.State))
which(is.na(testdf$Destination.State))
which(is.na(testdf$Scheduled.Departure.Hour))
which(is.na(testdf$Departure.Delay.in.Minutes))
which(is.na(testdf$Arrival.Delay.in.Minutes))
which(is.na(testdf$Flight.cancelled))
which(is.na(testdf$Flight.time.in.minutes))
which(is.na(testdf$Flight.Distance))
which(is.na(testdf$olong))
which(is.na(testdf$olat))
which(is.na(testdf$dlat))
which(is.na(testdf$dlong))
#By checking all the columns, we have come to know that, the columns 'Departure.Delay.in.Minutes', 
#'Arrival.Delay.in.Minutes', and 'Flight.time.in.minutes' have missing data in them.



#The length command is used to find all the missing data in the mentioned columns. It was found that there
#were 191 missing values in the departure delay column, 219 missing values in both arrival delay and flight time columns.
length(which(is.na(testdf$Departure.Delay.in.Minutes)))
length(which(is.na(testdf$Arrival.Delay.in.Minutes)))
length(which(is.na(testdf$Flight.time.in.minutes)))
length(which(is.na(testdf$NPS)))

#Here, view command is used to check the columns where the flight was cancelled. It was found that there 
#were 195 entries where the flights were cancelled. Therefore, these 195 rows can have missing values in
#the departure delay, arrival delay, and flight time columns as the flights were cancelled.
View(testdf[which(testdf$Flight.cancelled == 'Yes'),])
#View command is used in the below command to check for the rows which have missing values in the flight
#time in minutes column. From the above data, it can be understood that there are 195 rows which have 
#missing values for a reason, but the other rows which have missing values due to some error. Hence, from
#the data, we can understand that there are 219-195 = 24 missing values in the flight time column and other
#columns which were missing due to some error.
View(testdf[which(is.na(testdf$Flight.time.in.minutes)),])

View(testdf[which(testdf$Origin.State == 'Utah' & testdf$Destination.State == 'Wyoming'),])

View(testdf[which(testdf$Flight.cancelled == 'No' & is.na(testdf$Flight.time.in.minutes)),])

#In order to replace the missing values, the NAs for which the flights were cancelled were replaced with 
#zero beacuse the flight was cancelled.For the other values, I used a different approach.
#A simple method of dealing with small amounts of missing data in a numeric variable is to substitute the
#mean of the variable in place of each missing datum. So, in the below commands, we substituted the mean
#of a particular column in place of the missing value in the columns which have missing values. Hence, 
#the columns departure delay, arrival delay and flight time have their missing values replaced with the 
#mean of the respective column.

testdf$Departure.Delay.in.Minutes[which(testdf$Flight.cancelled == 'Yes' & is.na(testdf$Departure.Delay.in.Minutes))] = 0
testdf$Arrival.Delay.in.Minutes[which(testdf$Flight.cancelled == 'Yes' & is.na(testdf$Arrival.Delay.in.Minutes))] = 0
testdf$Flight.time.in.minutes[which(testdf$Flight.cancelled == 'Yes')] = 0



testdf$Departure.Delay.in.Minutes[is.na(testdf$Departure.Delay.in.Minutes)] <- mean(testdf$Departure.Delay.in.Minutes, na.rm = TRUE)
testdf$Arrival.Delay.in.Minutes[is.na(testdf$Arrival.Delay.in.Minutes)] <- mean(testdf$Arrival.Delay.in.Minutes, na.rm = TRUE)
testdf$Flight.time.in.minutes[is.na(testdf$Flight.time.in.minutes)] <- mean(testdf$Flight.time.in.minutes, na.rm = TRUE)

View(testdf)

#Adding a new attribute to gain more insight on how likelihood gets impacted by delay in arrival or 
#departure of a plane:
testdf <- testdf %>%
  mutate(Delay = ifelse(Departure.Delay.in.Minutes > 7 | Arrival.Delay.in.Minutes > 7, "YES", "NO"))

#Mean flight time of an average flight journey
mean(testdf$Flight.time.in.minutes)
#112.1901

#Adding a new attribute to know how a longer or shorter flight duration may impact likelihood:
testdf <- testdf %>%
  mutate(Long.Duration = ifelse(Delay == "YES" | Flight.time.in.minutes > 112, "TRUE", "FALSE"))


#~~~~~~~~~Phase 2~~~~~~~~~~~~

hist(testdf$Age)
#The histogram for the Age column is right-skewed.
hist(testdf$Price.Sensitivity)
#The histogram for the Price.Sensivity column is right-skewed.
hist(testdf$Year.of.First.Flight)
#The histogram for the Year.of.First.Flight column is symmetric.
hist(testdf$Flights.Per.Year)
#The histogram for the Flights.Per.Year column is right-skewed.
hist(testdf$Loyalty)
#The histogram for the Loyalty column is right-skewed with a outlier.
hist(testdf$Total.Freq.Flyer.Accts)
#The histogram for the Total.Freq.Flyer.Accts column is right-skewed.~~~~~~~~~~~~~~~
hist(testdf$Shopping.Amount.at.Airport)
#The histogram for the Shopping.Amount.at.Airport column is right-skewed.~~~~~~~~~~~~~~~ 
hist(testdf$Eating.and.Drinking.at.Airport)
#The histogram for the Eating.and.Drinking.at.Airport column is right-skewed.~~~~~~~~~~~~~~~ 
hist(testdf$Day.of.Month)
#The histogram for the Day.of.Month column is symmetric.
hist(testdf$Scheduled.Departure.Hour)
#The histogram for the Scheduled.Departure.Hour column is symmetric.
hist(testdf$Departure.Delay.in.Minutes)
#The histogram for the Departure.Delay.in.Minutes column is right-skewed.~~~~~~~~~~~~~~~ 
hist(testdf$Arrival.Delay.in.Minutes)
#The histogram for the Arrival.Delay.in.Minutes column is right-skewed.~~~~~~~~~~~~~~~ 
hist(testdf$Flight.time.in.minutes)
#The histogram for the Flight.time.in.minutes column is right-skewed.
hist(testdf$Flight.Distance)
#The histogram for the Flight.Distance column is right-skewed.
hist(testdf$Likelihood.to.recommend)
#The histogram for the Likelihood.to.recommend column is left-skewed.
hist(testdf$olong)
#The histogram for the olong column is left-skewed.~~~~~~~~
hist(testdf$olat)
#The histogram for the olat column is symmetric.~~~~~~~`
hist(testdf$dlong)
#The histogram for the dlong column is left-skewed.~~~~~~~~
hist(testdf$dlat)
#The histogram for the dlat column is right-skewed.~~~~~~~~~

summary(testdf)



table(testdf$Age)
#Most number of people are in the range of 35-55.
table(testdf$Destination.City)
table(testdf$Origin.City)
table(testdf$Airline.Status)
#Blue     Gold Platinum   Silver 
#6988      859      320     2089 
table(testdf$Gender)
#Female   Male 
#5883   4373 
table(testdf$Type.of.Travel)
#Business travel  Mileage tickets  Personal Travel 
#6252             858              3146 
table(testdf$Class)
#Business      Eco      Eco Plus 
#790           8382     1084 
table(testdf$Partner.Code)
# AA   AS   B6   DL   EV   F9   FL   HA   MQ   OO   OU   US   VX   WN 
#505  304  377 1639 1147  140  210   12  529 1229  947  903  115 2199 
table(testdf$Partner.Name)
table(testdf$Price.Sensitivity)
table(testdf$Year.of.First.Flight)
table(testdf$Flights.Per.Year)
table(testdf$Loyalty)
table(testdf$Type.of.Travel)
table(testdf$Total.Freq.Flyer.Accts)
table(testdf$Shopping.Amount.at.Airport)
table(testdf$Eating.and.Drinking.at.Airport)
table(testdf$Day.of.Month)
table(testdf$Likelihood.to.recommend)
table(testdf$Origin.State)
table(testdf$Destination.State)
table(testdf$Flight.cancelled)
#No   Yes 
#10061   195 
table(testdf$NPS)
#Detractors    Passive  Promotors 
#3005          3336       3915 
table(testdf$AgeG)
#MiddleA    OldA  YoungA 
#4115       2973    3168
table(testdf$Delay)
#  NO  YES 
#6323 3933 
table(testdf$Long.Duration)
#FALSE  TRUE 
#4024  6232 


prop.table(table(testdf$AgeG, testdf$NPS))
testdf$AgeG <- as.factor(testdf$AgeG)
testdf$NPS <- as.factor(testdf$NPS)
table(testdf$AgeG)
plot(testdf$AgeG, testdf$NPS)
#From the graph, we can understand that the most number of detractors were in the Older Adults category,
#followed by Young Adults. The promotors were mainly in the Middle age adults category followed by 
#younger adults. The passive were in the older adults category followed by both middle age and younger
#adults categories.

table(testdf$Airline.Status)
testdf$Airline.Status <- as.factor(testdf$Airline.Status)
plot(testdf$Airline.Status, testdf$NPS)
#From the graph plotted, it can be understood that people who travelled in the blue airline status program
#were the people who were detractors followed by platinum and  gold categories. The passive NPS scores 
#were given by dilver category followed by blue and gold categories.

testdf$Gender <- as.factor(testdf$Gender)
prop.table(table(testdf$Gender, testdf$NPS))
plot(testdf$Gender, testdf$NPS)
#From the plot, we can make out that female passengers were more likely to be detractors and male 
#passengers were more likely to be promotors.

testdf$Class <- as.factor(testdf$Class)
table(testdf$Class, testdf$NPS) #For comparing two independent variables
prop.table(table(testdf$Class, testdf$NPS)) #The following command expresses the results of the table 
#command as percentages.
plot(testdf$Class, testdf$NPS)
#From the plot, we can see that there are more detractors in the economy plus category followed by 
#economy category. The passive category is dominated by eceonomy plus and then eceonomy. The promotors 
#are most likely economy passengers. 


#~~~~~~~~~Phase 3~~~~~~~~~~~~

#In this phase, we develop different modeling techniques to analyze the data and remove unnecessary 
#variables so that we may only work with variables which directly impact the dependent variable.

#Linear Modeling

library(gdata)
library(readxl)

#The below commands comprise linear modeling applied to the data. Linear modeling is applied to only 
#some selective variables such as filtering out, for example, origin city and destination city because
#they may reflect variability in the data as they contain too many values in them. That may lead to 
#unnecessary variability which may affect inear modeling.

#Also, in linear modeling, significance of the factors are decided with the help of p-values.
#If p-value is less than 0.05, then that factor can be considered as significant or that it impacts the
#dependent variable directly. Asterisks mentioned at the side of factors provide the significance based
#on the order of 3 asterisks, 2 ,1 and a dot may imply that among all the one withe dot is least 
#significant. The factors which do not have any type of symbols at the side imply that these factors do
#not impact the dependent variable.

lmplott <- lm(formula = Likelihood.to.recommend ~ Airline.Status + Age + Gender + Price.Sensitivity + Year.of.First.Flight + Flights.Per.Year + Loyalty + Type.of.Travel + Total.Freq.Flyer.Accts + Shopping.Amount.at.Airport + Eating.and.Drinking.at.Airport + Class + Day.of.Month + Partner.Code +  Scheduled.Departure.Hour + Departure.Delay.in.Minutes + Arrival.Delay.in.Minutes + Flight.cancelled + Flight.time.in.minutes + Flight.Distance, data = testdf)
summary(lmplott)
#From the above linear modeling data, we can surmise that arrival delay, arrival status, age, gender, 
#flights per year, type of travel, eating and drinking at airport, partner code, price sensitivity, 
#loyalty, class, year of first flight, flight time and departure delay are significant factors when 
#defining the independent factors which will affect the dependent factor likelihood to recommend.

lmplot1 <- lm(formula = Likelihood.to.recommend ~ Destination.City, data = testdf)
summary(lmplot1)
#The independent variable Origin.City is statistically significant as it has a p-value of
#0.0009763. It has many values can lead to variability in the data which may make the linear model more
#complex or it can be ignored.

lmplot2 <- lm(formula = Likelihood.to.recommend ~ Origin.City, data = testdf)
summary(lmplot2)
#The independent variable Origin.City is statistically significant as it has a p-value of
#3.931e-15. It has many values can lead to variability in the data which may make the linear model more
#complex or it can be ignored.

lmplot3 <- lm(formula = Likelihood.to.recommend ~ Airline.Status, data = testdf)
summary(lmplot3)
#The independent variable Airline.Status is statistically significant as it has a p-value of
#2.2e-16 and is represented by three asterisks.

lmplot4 <- lm(formula = Likelihood.to.recommend ~ Age, data = testdf)
summary(lmplot4)
#The independent variable Age is statistically significant as it has a p-value of
#2.2e-16 and is represented by three asterisks.

lmplot5 <- lm(formula = Likelihood.to.recommend ~ Gender, data = testdf)
summary(lmplot5)
#The independent variable Gender is statistically significant as it has a p-value of
#2.2e-16 and is represented by three asterisks.

lmplot6 <- lm(formula = Likelihood.to.recommend ~ Price.Sensitivity, data = testdf)
summary(lmplot6)
#The independent variable Price.Sensitivity is statistically significant as it has a p-value of
#8.089e-13 and is represented by three asterisks.

lmplot7 <- lm(formula = Likelihood.to.recommend ~ Year.of.First.Flight, data = testdf)
summary(lmplot7)
#The independent variable Flights.Per.Year is statistically insignificant as it has a p-value of
#0.3972. Therefore, it can be ignored.

lmplot8 <- lm(formula = Likelihood.to.recommend ~ Flights.Per.Year, data = testdf)
summary(lmplot8)
#The independent variable Flights.Per.Year is statistically significant as it has a p-value of
#2.2e-16 and is represented by three asterisks.

lmplot9 <- lm(formula = Likelihood.to.recommend ~ Loyalty, data = testdf)
summary(lmplot9)
#The independent variable Loyalty is statistically significant as it has a p-value of
#2.2e-16 and is represented by three asterisks.

lmplot10 <- lm(formula = Likelihood.to.recommend ~ Type.of.Travel, data = testdf)
summary(lmplot10)
#The independent variable Type.of.Travel is statistically significant as it has a p-value of
#2.2e-16 and is represented by three asterisks.

lmplot11 <- lm(formula = Likelihood.to.recommend ~ Total.Freq.Flyer.Accts, data = testdf)
summary(lmplot11)
#The independent variable Total.Freq.Flyer.Accts is statistically significant as it has a p-value of
#2.2e-16 and is represented by three asterisks.

lmplot12 <- lm(formula = Likelihood.to.recommend ~ Shopping.Amount.at.Airport, data = testdf)
summary(lmplot12)
#The independent variable Shopping.Amount.at.Airport is statistically significant as it has a p-value of
#0.003935 and is represented by two asterisks.

lmplot13 <- lm(formula = Likelihood.to.recommend ~ Eating.and.Drinking.at.Airport, data = testdf)
summary(lmplot13)
#The independent variable Eating.and.Drinking.at.Airport is statistically significant as it has a p-value of
#4.731e-14 and is represented by three asterisks.

lmplot14 <- lm(formula = Likelihood.to.recommend ~ Class, data = testdf)
summary(lmplot14)
#The independent variable Class is statistically significant as it has a p-value of
#1.899e-07 and is represented by three asterisks.

lmplot15 <- lm(formula = Likelihood.to.recommend ~ Day.of.Month, data = testdf)
summary(lmplot15)
#The independent variable Day.of.Month is statistically significant as it has a p-value of
#0.01057. It is represented by a single asterisk.

lmplot16 <- lm(formula = Likelihood.to.recommend ~ Flight.date, data = testdf)
summary(lmplot16)
#The independent variable Flight.date is statistically significant as it has a p-value of
#0.0146. It has many values can lead to variability in the data which may make the linear model more
#complex or it can be ignored.

lmplot17 <- lm(formula = Likelihood.to.recommend ~ Partner.Code, data = testdf)
summary(lmplot17)
#The independent variable Partner.Code is statistically significant as it has a p-value of
#2.2e-16. It has many values can lead to variability in the data which may make the linear model more
#complex or it can be ignored.

lmplot18 <- lm(formula = Likelihood.to.recommend ~ Partner.Name, data = testdf)
summary(lmplot18)
#The independent variable Partner.Name is statistically significant as it has a p-value of
#2.2e-16. It has many values can lead to variability in the data which may make the linear model more
#complex or it can be ignored.

lmplot19 <- lm(formula = Likelihood.to.recommend ~ Origin.State, data = testdf)
summary(lmplot19)
#The independent variable Origin.State is statistically significant as it has a p-value of
#2.2e-16. It has many values can lead to variability in the data which may make the linear model more
#complex or it can be ignored.

lmplot20 <- lm(formula = Likelihood.to.recommend ~ Destination.State, data = testdf)
summary(lmplot20)
#The independent variable Destination.State is statistically significant as it has a p-value of
#0.003517. It has many values can lead to variability in the data which may make the linear model more
#complex or it can be ignored.

lmplot21 <- lm(formula = Likelihood.to.recommend ~ Scheduled.Departure.Hour, data = testdf)
summary(lmplot21)
#The independent variable Scheduled.Departure.Hour is statistically insignificant as it has a p-value of
#0.35 and is represented by no asterisks. Therefore, this factor can be ignored.

lmplot22 <- lm(formula = Likelihood.to.recommend ~ Departure.Delay.in.Minutes, data = testdf)
summary(lmplot22)
#The independent variable Departure.Delay.in.Minutes is statistically significant as it has a p-value of
#2e-16 and is represented by three asterisks.

lmplot23 <- lm(formula = Likelihood.to.recommend ~ Arrival.Delay.in.Minutes, data = testdf)
summary(lmplot23)
#The independent variable Arrival.Delay.in.Minutes is statistically significant as it has a p-value of
#2e-16 and is represented by three asterisks.

lmplot24 <- lm(formula = Likelihood.to.recommend ~ Flight.cancelled, data = testdf)
summary(lmplot24)
#The independent variable Flight.cancelled is statistically significant as it has a p-value of 1.67e-05
#and is represented by three asterisks.

lmplot25 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance, data = testdf)
summary(lmplot25)
#The independent variable olong is statistically significant as it has a p-value of 0.02151 and is 
#represented by one asterisks. Though it is significant, its significance level is represented by only
#one asterisk, so in these type of cases the particular factor may be ignored.

lmplot26 <- lm(formula = Likelihood.to.recommend ~ olong, data = testdf)
summary(lmplot26)
#The independent variable olong is statistically significant as it has a p-value of 0.007753 and is 
#represented by two asterisks.

lmplot27 <- lm(formula = Likelihood.to.recommend ~ olat, data = testdf)
summary(lmplot27)
#The independent variable olat is statistically significant as it has a p-value of 0.000544 and is 
#represented by three asterisks.

lmplot28 <- lm(formula = Likelihood.to.recommend ~ dlong, data = testdf)
summary(lmplot28)
#The independent variable dlong is significant as it has a p-value of 0.01467 but has low significance 
#as it is represented by only one asterisk.

lmplot29 <- lm(formula = Likelihood.to.recommend ~ dlat, data = testdf)
summary(lmplot29)
#The independent variable dlat cannot be taken into account as it has a p-value of 0.6609. So, it is 
#statistically insignificant.

lmplot30 <- lm(formula = Likelihood.to.recommend ~ Delay, data = testdf)
summary(lmplot30)
#The independent variable Delay can be taken into account as it has a p-value of 2.2e-16. So, it is 
#statistically significant.

lmplot31 <- lm(formula = Likelihood.to.recommend ~ Long.Duration, data = testdf)
summary(lmplot31)
#The independent variable Long.Duration can be taken into account as it has a p-value of 2.2e-16. So, it is 
#statistically significant.

lmplot32 <- lm(formula = Likelihood.to.recommend ~ LoyCol, data = testdf)
summary(lmplot32)
#The independent variable LoyCol can be taken into account as it has a p-value of 2.2e-16. So, it is 
#statistically significant.

#By filtering out the insignificant variables and other complex variables which may vary the data and 
#may skew the results, we can make a linear model with significant variables so that we know which 
#significant variables may impact the dependent variable and by how much.

lmplott1 <- lm(formula = Likelihood.to.recommend ~ Origin.State + Destination.State + Airline.Status + AgeG + Gender + Price.Sensitivity + Flights.Per.Year + Loyalty + Type.of.Travel + Shopping.Amount.at.Airport + Eating.and.Drinking.at.Airport + Class + Departure.Delay.in.Minutes + Arrival.Delay.in.Minutes + Flight.cancelled + Flight.time.in.minutes + Flight.Distance, data = testdf)
summary(lmplott1)
#In the above command, we used linear modelling by considering all independent attributes. By running the 
#summary command, we get to know the significant attributes and how significant each attribute is.



lmplotf <- lm(formula = Likelihood.to.recommend ~ Airline.Status + AgeG + Gender + Price.Sensitivity + Flights.Per.Year + Loyalty + Type.of.Travel + Eating.and.Drinking.at.Airport + Class + Delay + Flight.cancelled + Long.Duration + Flight.Distance + LoyCol, data = testdf)
summary(lmplotf)
#The above linear model is the final model because we only considered significant attributes which affect 
#the dependent variable. The adjusted r-squared value here is 0.42 and the p-value is < 2.2e-16.

str(testdf)
View(testdf)

#Above,we have done predictive modeling on the data. The modeling technique used was Linear modeling. In Linear Modeling,
#we try to know how different independent variables may effect the dependent variable. This modeling technique gives adjusted
#R-squared and p-values which tell us how the independent variables account for the variability in the dependent variable and 
#significance of the independent variables.The p-value should be less than 0.05 for a variable to significant.
#The adjusted r-squared is high for Airline.Status and Type.of.Travel and the p-values also suggested that they were signifacant.






#Association Rules Mining

install.packages('arules')
install.packages('arulesViz')
library(arules)
library(arulesViz)
library(tidyverse)
dfX <- as(testdf, "transactions")
#In order to implement association rules mining, the data set has to be converted into a dataframe of taransactions data
#type. In order do that, the above command helps us in doing that certain operation.
#But, an error occured as we try to do that. As this particular transactions data type requires the variables to be in 
#factor data type, errors occur, so we have to convert them into factor data types.

testdf$Price.Sensitivity <- as.factor(testdf$Price.Sensitivity)
testdf$Total.Freq.Flyer.Accts <- as.factor(testdf$Total.Freq.Flyer.Accts)
testdf$Shopping.Amount.at.Airport <- as.factor(testdf$Shopping.Amount.at.Airport)
testdf$Departure.Delay.in.Minutes <- as.factor(testdf$Departure.Delay.in.Minutes)
testdf$Arrival.Delay.in.Minutes <- as.factor(testdf$Arrival.Delay.in.Minutes)

testdf1 <- testdf[,-c(4,7,35,32,25,23,22)]
#In the above command, age and NPSn columns were removed and replaced with columns NPS and AgeG would 
#be more useful in knowing and understanding the output. The new columns categorise the age and scores,
#so they can help in further understanding the different groups.


View(testdf1)

#The below commands are used to discretize or convert the columns into factor types for association rules
#mining modeling technique.
discretizeDF(testdf1, methods = NULL, default = NULL)
testdf1 <- testdf1 %>% mutate_all(as.factor)


#The given dataset is converted into a new dataframe of arules transactions class containing sparse
#matrix configuration which will be helpful while applying association rules modeling technique.
dfX <- as(testdf1, "transactions")
dfX

View(dfX)

inspect(dfX)

itemFrequency(dfX)

itemFrequencyPlot(dfX)

#Apriori is a specific algorithm that R uses to scan the dataset for appropriate rules. It uses a iterative
#approach known as level-wise search. In the below command, apriori algorithm is used on dfX sparse
#matrix by taking the support as 5% and confidence as 50%. The algorithm is also given the appearance of
#parameter where the lhs can be a combination of any objects among the matrix and rhs has to be such that
#the NPS class should be for Detractors.
ruleset <- apriori(dfX, parameter = list(support = 0.05, confidence = 0.5), appearance = list(default="lhs",rhs=("NPS=Detractor")))

#Inspect and InspectDT are used to review the output of the association rules apriori rulesets.
inspect(ruleset)
inspect(head(ruleset,20))
inspectDT(ruleset)
plot(ruleset, jitter = 0)
#In the above predictive model, we considered every independent variable, to know how they are going to
#impact the dependent variable, regardless of its significance. So, when we inspected the ruleset, there
#were rules which included Likelihood to recommend in the top rules. So, in order to know which attributes
#influenced the likelihood, we need to eliminate likelihood attribute and focus on how the variables are 
#impacting Net promotor score.
#Also, a scatter plot is used to plot the above rules graphically. A jitter is applied in the command
#to avoid overplotting.



#In the below commands, insignificant and unnecessary columns are deleted to create a new dataframe, so
#we can get accurate output rules to know more about how the detractors review or recommend.
detdf <- testdf1[,-c(14:16,22)]
View(detdf)
str(detdf)
detdfX <- as(detdf, "transactions")
ruleset1 <- apriori(detdfX, parameter = list(support = 0.05, confidence = 0.5), appearance = list(default="lhs",rhs=("NPS=Detractor")))
inspect(head(ruleset1,10))
inspectDT(ruleset1)
plot(ruleset1, jitter = 0)
#The below command sorts the ruleset according to the lift obtained.
inspect(sort(head(ruleset1,10), y="lift"))
#Also, a scatter plot is used to plot the above rules graphically. A jitter is applied in the command
#to avoid overplotting.

#Overall, we can figure out that the ruleset that the highest lift is 3.2, which gives a set of unique rules
#which we can work on. Most importantly, the rules contain the attributes which mainly influence the 
#dependent variable. Here, the dependent variable is Detractors in the NPS. So, we have now got to know 
#which attributes mainly affected the passengers to give a low recommendation score.


#The below ruleset is for Passive category in the NPS. This ruleset has a lift of 1.28 and comparitively 
#has lesser rules than compared to Detractors. Passive mainly stay in this category as they are not actively
#promoting or bad-mouthing the airlines but provide a low enough score not to be in promotors.
#The attributes mentioned in the rules can be used to understand how the score lessened.
ruleset2 <- apriori(detdfX, parameter = list(support = 0.05, confidence = 0.4), appearance = list(default="lhs",rhs=("NPS=Passive")))
inspect(head(ruleset2,10))
inspectDT(ruleset2)
inspect(sort(head(ruleset2,10), y="lift"))
plot(ruleset2, jitter = 0)
#Also, a scatter plot is used to plot the above rules graphically. A jitter is applied in the command
#to avoid overplotting.

#Coverting the numeric attributes into numeric type which have been converted to factor type:
testdf1$Age <- as.numeric(testdf1$Age)
testdf1$Price.Sensitivity <- as.numeric(testdf1$Price.Sensitivity)
testdf1$Flights.Per.Year <- as.numeric(testdf1$Flights.Per.Year)
testdf1$Loyalty <- as.numeric(testdf1$Loyalty)
testdf1$Total.Freq.Flyer.Accts <- as.numeric(testdf1$Total.Freq.Flyer.Accts)
testdf1$Shopping.Amount.at.Airport <- as.numeric(testdf1$Shopping.Amount.at.Airport)
testdf1$Eating.and.Drinking.at.Airport <- as.numeric(testdf1$Eating.and.Drinking.at.Airport)
testdf1$Day.of.Month <- as.numeric(testdf1$Day.of.Month)
testdf1$Scheduled.Departure.Hour <- as.numeric(testdf1$Scheduled.Departure.Hour)
testdf1$Flight.Distance <- as.numeric(testdf1$Flight.Distance)
testdf1$Likelihood.to.recommend <- as.numeric(testdf1$Likelihood.to.recommend)
testdf1$olong <- as.numeric(testdf1$olong)
testdf1$olat <- as.numeric(testdf1$olat)
testdf1$dlong <- as.numeric(testdf1$dlong)
testdf1$dlat <- as.numeric(testdf1$dlat)
str(testdf1)

detdf$Price.Sensitivity <- as.numeric(detdf$Price.Sensitivity)
detdf$Flights.Per.Year <- as.numeric(detdf$Flights.Per.Year)
detdf$Loyalty <- as.numeric(detdf$Loyalty)
detdf$Total.Freq.Flyer.Accts <- as.numeric(detdf$Total.Freq.Flyer.Accts)
detdf$Shopping.Amount.at.Airport <- as.numeric(detdf$Shopping.Amount.at.Airport)
detdf$Eating.and.Drinking.at.Airport <- as.numeric(detdf$Eating.and.Drinking.at.Airport)
detdf$Day.of.Month <- as.numeric(detdf$Day.of.Month)
detdf$Scheduled.Departure.Hour <- as.numeric(detdf$Scheduled.Departure.Hour)
detdf$Flight.Distance <- as.numeric(detdf$Flight.Distance)
detdf$olong <- as.numeric(detdf$olong)
detdf$olat <- as.numeric(detdf$olat)
detdf$dlong <- as.numeric(detdf$dlong)
detdf$dlat <- as.numeric(detdf$dlat)
str(detdf)






#Support Vector Machines
install.packages("caret")
library(caret)
install.packages("kernlab")
library(kernlab)
library(ggplot2)
install.packages("e1071")
library(e1071)

View(detdf)
#The reason SVMs are considered a supervised learning technique is that we train the algorithm on an 
#initial set of data (the supervised phase) and then we test it out on a brand-new set of data. If the 
#training we accomplished worked well, then the algorithm should be able to predict the right outcome 
#most of the time in the test data. This is the basic strategy of supervised machine learning: Have a 
#substantial number of training cases that the algorithm can use to discover and mimic the underlying 
#pattern, and then use the results of that process on a test data set in order to find out how well the 
#algorithm and parameters perform in a cross- validation. Cross-validation, in this instance, refers to 
#the process of verifying that the trained algorithm can carry out is prediction or classification task 
#accurately on novel data.

#The data is partitioned into trainList by partitioning 65% data into it.
trainList <- createDataPartition(y=detdf$NPS, p=.65,list=FALSE)

#Here, the trainList containing 65% data is loaded into trainData and the other 35% data is loaded into 
#testData.
trainData <- detdf[trainList,]
testData <- detdf[-trainList,]

#The below commands illustrate the number of observations and the number of columns in the dataset.
dim(trainData)
#6668   27
dim(testData)
#3588   27

#The below command is used to get the output of trainData by using SVMs. #The first parameter is: 
#kernel=“rbfdot”. The rbfdot designation refers to the radial basis function. 
#The dot in the name refers to the mathematical idea of a dot product, which is a way of multiplying 
#vectors together to come up with a single number such as a distance value. In simplified terms, the 
#radial basis function kernel takes the set of inputs from each row in a data set and calculates a 
#distance value based on the combination of the many variables in the row. The weighting of the 
#different variables in the row is adjusted by the algorithm in order to get the maximum separation of 
#distance between the premium cases and the ideal cases. The kpar argument refers to a variety of parameters
#that can be used to control the operation of the radial basis function kernel. In this case, we are 
#depending on the goodwill of the designers of this algorithm by specifying automatic.
#The C argument refers to the so-called cost of constraints.
svmOutput <- ksvm(NPS ~ ., data = trainData, kernel = "rbfdot", kpar = "automatic", C=5, cross = 3, prob.model = TRUE)
svmOutput

#This command is used to predict the output of testData by using the above operation which was used for
#figuring the output of trainData.
svmPred <- predict(svmOutput, testData)

str(svmPred)
head(svmPred)

confusionMatrix(svmPred, testData$NPS)
#The confusion matrix is generated by using model output generated from a model based on the training 
#data set as the parameters for prediction, and uses the test data which the support vectors have never 
#seen before, to generate predictions, and it requests votes or probabilities from the prediction process. 









#~~~~~~~~~Phase 4~~~~~~~~~~~~

#Mapping low satisfaction routes
#Here, in this phase, we plot the routes of passengers who have given low satisfaction scores on the map.

mapdt <- testdf %>%
  filter(NPS=="Detractor")

mapdt$Price.Sensitivity <- as.numeric(mapdt$Price.Sensitivity)
mapdt$Total.Freq.Flyer.Accts <- as.numeric(mapdt$Total.Freq.Flyer.Accts)
mapdt$Shopping.Amount.at.Airport <- as.numeric(mapdt$Shopping.Amount.at.Airport)
mapdt$Departure.Delay.in.Minutes <- as.numeric(mapdt$Departure.Delay.in.Minutes)
mapdt$Arrival.Delay.in.Minutes <- as.numeric(mapdt$Arrival.Delay.in.Minutes)

View(mapdt)
str(mapdt)

mapdt <- mapdt[,-c(32,34)]
View(mapdt)



library(ggplot2)
library(gdata)
library(zipcode)
library(ggmap)
library(maps)

us <- map_data("state")
us

usMap <- borders("state", colour="red", fill="grey")


plot1 <- ggplot() + usMap + geom_curve(data=mapdt, aes(x=olong, y=olat, xend=dlong, yend=dlat), col="#3C1776", size=.5, curvature=0.2) + geom_point(data=mapdt, aes(x=olong, y=olat), colour="red", size=1.5) + geom_point(data=mapdt, aes(x=dlong, y=dlat), colour="red") + theme(axis.line=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks=element_blank(), plot.title=element_text(hjust=0.5, size=12)) + ggtitle("Low Satisfaction Routes")
plot1

table(mapdt$Origin.State, mapdt$NPS)
#From the above command's output, it can be understood that the passsengers who flew from California, 
#Texas, Illinois and Georgia were the most number of detractors.
table(mapdt$Destination.State, mapdt$NPS)
#From the above command's output, it can be understood that the passsengers who flew to California, 
#Texas, Illinois and Georgia were the most number of detractors, similar to passengers who flew from 
#those particular states.

#It can be observed that passengers who flew from or to certain states like California, Texas, Illinois
#and Georgia were the maximum number of detractors.

LowSatR <- mapdt %>%
  filter(Origin.State == "California" | Destination.State == "California")
View(LowSatR)

ggplot() + usMap + geom_point(data = LowSatR, aes(x = olong, y = olat), col = "#3C1776") +
   geom_curve(data=LowSatR, aes(x = olong, y = olat, xend = dlong, yend = dlat), col = "#f5af3d", size = 1, curvature = .2)+
  theme(panel.background = element_rect(fill="white"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank() + coord_map()) + ggtitle("California - Low Satisfaction Routes")
#The above plot depicts the routes taken by passengers who fly from California state.


usMap <- borders("state", colour="black", fill="grey")
LowSatR1 <- mapdt %>%
  filter(Origin.State == "Texas" | Destination.State == "Texas")
View(LowSatR1)

ggplot() + usMap + geom_point(data = LowSatR1, aes(x = olong, y = olat), col = "#3C1776") +
  geom_curve(data=LowSatR1, aes(x = olong, y = olat, xend = dlong, yend = dlat), col = "#FC044C", size = 1, curvature = .2)+
  theme(panel.background = element_rect(fill="white"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank() + coord_map()) + ggtitle("Texas - Low Satisfaction Routes")
#The above plot depicts the routes taken by passengers who fly from Texas state.


LowSatR2 <- mapdt %>%
  filter(Origin.State == "Illinois" | Destination.State == "Illinois")
View(LowSatR2)

ggplot() + usMap + geom_point(data = LowSatR2, aes(x = olong, y = olat), col = "#3C1776") +
  geom_curve(data=LowSatR2, aes(x = olong, y = olat, xend = dlong, yend = dlat), col = "#04FC8F", size = 1, curvature = .2)+
  theme(panel.background = element_rect(fill="white"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank() + coord_map()) + ggtitle("Illinois - Low Satisfaction Routes")
#The above plot depicts the routes taken by passengers who fly from Illinois state.


LowSatR3 <- mapdt %>%
  filter(Origin.State == "Georgia" | Destination.State == "Georgia")
View(LowSatR3)

ggplot() + usMap + geom_point(data = LowSatR3, aes(x = olong, y = olat), col = "#3C1776") +
  geom_curve(data=LowSatR3, aes(x = olong, y = olat, xend = dlong, yend = dlat), col = "#FCF804", size = 1, curvature = .2)+
  theme(panel.background = element_rect(fill="white"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank() + coord_map()) + ggtitle("Georgia - Low Satisfaction Routes")
#The above plot depicts the routes taken by passengers who fly from Georgia state.


#In order to depict a more simple plot, we can filter the data further with respect to other attributes.
LowSatR4 <- mapdt %>%
  filter(Origin.City == "New York, NY")
View(LowSatR4)

ggplot() + usMap + geom_point(data = LowSatR4, aes(x = olong, y = olat), col = "#3C1776") +
  geom_curve(data=LowSatR4, aes(x = olong, y = olat, xend = dlong, yend = dlat), col = "#1D5F01", size = 1, curvature = .2)+
  theme(panel.background = element_rect(fill="white"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank() + coord_map()) + ggtitle("New York - Low Satisfaction Routes")
#In the plot here, we have filtered the data for detractors who flew from the origin city which is New 
#York. This plot depicts a more simple plot where the passengers flew from New York city.



#~~~~~~~~~Phase 5~~~~~~~~~~~~

#This phase deals with the data we obtained from phases 3 and 4 in order to gain insights on why certain
#trips have low satisfaction. 



#In order to provide actionable insights, we have to subset data to eliminate the skewness related with 
#taking all of the data. The data can be sampled based on various attributes like age, gender, loyalty,
#by origin or destination et.
#Here, in this data, to provide more understanding insights, data is sampled based on the airlines status.

prop.table(table(testdf$Airline.Status, testdf$NPS))
#From the output of the above command, we can observe that most of the detractors, passive and promotors
#are in the blue airline status category and all the remaining passengers are scattered across all categories
#dominated by silver passengers in promotors and followed by others in all categories.


View(testdf)
acttdf <- testdf[,-c(4,7,16:18,35,32,25,23,22)]

actdf <- acttdf %>%
  filter(Origin.State == "California" | str_trim(Destination.State) == "California")

View(actdf)
str(actdf)
#By studying Association Rules Mining, we can understand that some independent variables were more 
#impactful or important in influencing the dependent variable.
#Some of them can be seen listed in the ruleset1 summary.
#Running the inspect command to know or understand more about them:
#inspect(sort(head(ruleset1,10), y="lift"))

#We now filter data such that the dataset contains information or details of passengers travelling to
#or from California to study why the passengers relating to this state consists of more detractors.


table(actdf$Airline.Status, actdf$NPS)
#There were 489 passengers in the blue airline status category, the maximum in any airline status category,
#followed by 35 passengers in the gold airline status category.


table(actdf$Type.of.Travel)
#Business travel  Mileage tickets  Personal Travel 
#  1256              168             639 
table(actdf$NPS, actdf$Type.of.Travel)
prop.table(table(actdf$NPS, actdf$Type.of.Travel))
#            Business travel Mileage tickets Personal Travel
#Detractor      0.07707222      0.01211827      0.18177412
#Passive        0.18468250      0.03005332      0.10324770
#Promotor       0.34706738      0.03926321      0.02472128

#We observe that passengers who used the airlines for personal travel were not satisfied with the travel 
#and gave low recommendation scores. They were the most number of detractors.
#While, most number of business travellers were promotors, the passive category has its maximum number 
#of passengers from business travellers too.


table(actdf$AgeG)
#MiddleA  OlderA  YoungA 
# 828      602      633 
table(actdf$AgeG, actdf$NPS)
#          Detractor Passive Promotor
#MiddleA       135     233      460
#OlderA        272     196      134
#YoungA        152     227      254
prop.table(table(actdf$AgeG, actdf$NPS))
#         Detractor    Passive   Promotor
#MiddleA 0.06543868 0.11294232 0.22297625
#OlderA  0.13184683 0.09500727 0.06495395
#YoungA  0.07367911 0.11003393 0.12312167
#From the above output, we can notice that the maximum number of detractors are in the older adults age 
#group followed by younger adults. While the passive category is dominated by younger adults and then 
#followed by middle age adult passenger categories.

table(actdf$AgeG, actdf$Type.of.Travel, actdf$NPS)
#The older age passengers who travelled for personal travel were the maximum among the detractors.

table(actdf$Gender, actdf$NPS)
#The detractors were more among women when compared to men.

table(actdf$Price.Sensitivity, actdf$NPS)
#Most number of dteractors are in the price sensitivity category of 1. So, we may infer that some passengers
#did not give accurate reviews or there were some other attributes which affect the detractors.

table(actdf$LoyCol, actdf$NPS)
prop.table(table(actdf$LoyCol, actdf$NPS))
#Most number of passengers who were detractors had a loyalty score of less than 1 (less loyal) when
#compared to more loyal passengers who are only 93 passengers who were detractors.

table(actdf$Shopping.Amount.at.Airport, actdf$NPS)
table(actdf$Eating.and.Drinking.at.Airport, actdf$NPS)
#From the outputs of the above commands, it can be observed that the amount spent by the passengers 
#varies significantly. So, it cannot be related to detractors and how spending relates to low customer
#satisfaction scores.

table(actdf$Class, actdf$NPS)
#           Detractor Passive Promotor
#Business        30      39       97
#Eco            469     548      658
#Eco Plus        60      69       93
prop.table(table(actdf$Class, actdf$NPS))
#          Detractor    Passive   Promotor
#Business 0.01454193 0.01890451 0.04701890
#Eco      0.22733883 0.26563257 0.31895298
#Eco Plus 0.02908386 0.03344644 0.04507998
#It can be observed form the above output that there were more number of passengers in the economic class
#and the number of passengers who gave low customer satisfaction scores are also high among the economic class.

table(actdf$Day.of.Month, actdf$NPS)
#No correlation can be obtained from the day of the month attribute and the customer satisfaction scores.

table(actdf$Scheduled.Departure.Hour, actdf$NPS)
#No correlation can be obtained from the scheduled departure hor attribute and the customer 
#satisfaction scores.

table(actdf$Flight.cancelled, actdf$NPS)
#The flight cancellations did not impact the customer satisfaction scores as passengers whose flights
#did not get cancelled were more among the number of detractors.

table(actdf$Flight.Distance, actdf$NPS)
#From the outputs of the above commands, it can be observed that the flight distance varies 
#significantly. So, it cannot be related to detractors and how flight distance relates to low customer
#satisfaction scores.

table(actdf$Delay, actdf$NPS)
#Delay cannot be correlated with low customer satisfaction scores as the detractors are almost equally
#distributed among the passengers whose flights got delayed and the passengers whose flights were not
#delayed.

table(actdf$Long.Duration, actdf$NPS)
#The passengers who travelled a longer duration or had a long journey were among the maximum number
#of detractors.

table(actdf$Type.of.Travel, actdf$Gender, actdf$NPS)
prop.table(table(actdf$Type.of.Travel, actdf$Gender, actdf$NPS))
#From the output of the above commands, we can observe that the female passengers who travelled for 
#personal reasons were the maximum percentage among the detractors.

table(actdf$Airline.Status, actdf$Type.of.Travel, actdf$NPS, actdf$Gender)
#It can be observed that female passengers who used personal travel and flew in the blue airline status 
#category contributed towards low customer satisfaction scores more than anyone.

table(actdf$AgeG, actdf$LoyCol, actdf$Class, actdf$NPS)
#From the above output, it can be made out that older adult passengers who flew in economy class and tried
#other airlines were less loyal and contributed towards low customer satisfaction scores more than anyone.

table(actdf$Long.Duration, actdf$NPS, actdf$AgeG, actdf$Gender)
#It can be observed that older female passengers who had longer duration of flight journey gave 
#low customer satisfaction scores compared to others.

table(actdf$Long.Duration, actdf$NPS, actdf$AgeG, actdf$Class)
#From the above output, it can be made out that older adult passengers who flew in economy class and tried
#other airlines and had a longer flight duration had contributed towards low customer satisfaction 
#scores more than anyone.
str(actdf)
ggplot(actdf,aes(Loyalty, Flights.Per.Year)) + geom_line(color="orange", size=1, alpha=0.7, linetype=1)
#From the above plot, we can interpret that as loyalty increases, the passengers flying number of flights decreases.
#It means that people less loyal to the airline tarvel more number of times than people who travelled who
#are more loyal. So, the detractors are mostly due to travellers who travel a different number of airlines.


pl1<-ggplot(actdf,aes(Likelihood.to.recommend,Airline.Status))
pl1<-pl1+geom_boxplot(color="black",outlier.colour = "red",fill="yellow")
pl1<-pl1+ggtitle('Likelihood to recommend and Airline Status')+labs(x="Likelihood to Recommend",y="Airline Status")
pl1
#From the above plot, it can be inferred that passengers who travelled mainly in the blue airlines status 
#were more likely detractors compared to other airline status categories.

#In summary, we can infer from the above data that passengers travelling from and to California of older
#age, travelling for personal reasons in blue airline status category were the most number of detractors.
#Some other attributes also affect the dependent variable NPS.



#2
#We now take another dataframe to study how other attributes are going to affect the dependent variable. Here,
#we filter data such that we can study the passengers travelling in blue airline status and economy class categories.

act2df <- acttdf %>%
  filter(str_trim(Airline.Status) == "Blue" & str_trim(Class) == "Eco")

View(act2df)

#By studying Association Rules Mining, we can understand that some independent variables were more 
#impactful or important in influencing the dependent variable.
#Some of them can be seen listed in the ruleset1 summary.
#Running the inspect command to know or understand more about them:
#inspect(sort(head(ruleset1,10), y="lift"))

#We now filter data such that the dataset contains information or details of passengers travelling on
#blue airline status and economy class to study why the passengers relating to these conditions 
#consist of more detractors.


table(act2df$Airline.Status, act2df$NPS)
#There were 2157 passengers in the blue airline status category, who were detractors.


table(act2df$Type.of.Travel)
#Business travel  Mileage tickets  Personal Travel 
#  3171              542             1980 
table(act2df$NPS, act2df$Type.of.Travel)
prop.table(table(act2df$NPS, act2df$Type.of.Travel))
#           Business travel Mileage tickets Personal Travel
#Detractor     0.107676093     0.018619357     0.252590901
#Passive       0.199016336     0.045845776     0.086597576
#Promotor      0.250307395     0.030739505     0.008607061

#We observe that passengers who used the airlines for personal travel were not satisfied with the travel 
#and gave low recommendation scores. They were the most number of detractors.
#While, most number of business travellers were promotors, the passive category has its maximum number 
#of passengers from business travellers too.


table(act2df$AgeG)
#MiddleA  OlderA  YoungA 
# 2083    1731    1879 
table(act2df$AgeG, act2df$NPS)
#          Detractor Passive Promotor
#MiddleA       547     662      874
#OlderA        997     482      252
#YoungA        613     743      523
prop.table(table(act2df$AgeG, act2df$NPS))
#         Detractor    Passive   Promotor
#MiddleA 0.09608291 0.11628315 0.15352187
#OlderA  0.17512735 0.08466538 0.04426489
#YoungA  0.10767609 0.13051115 0.09186721
#From the above output, we can notice that the maximum number of detractors are in the older adults age 
#group followed by younger adults. While the passive category is dominated by younger adults and then 
#followed by middle age adult passenger categories.

table(act2df$AgeG, act2df$Type.of.Travel, act2df$NPS)
#The older age passengers who travelled for personal travel were the maximum among the detractors.

table(act2df$Gender, act2df$NPS)
#The detractors were more among women when compared to men.

table(act2df$Price.Sensitivity, act2df$NPS)
#Most number of dteractors are in the price sensitivity category of 1. So, we may infer that some passengers
#did not give accurate reviews or there were some other attributes which affect the detractors.

table(act2df$LoyCol, act2df$NPS)
prop.table(table(act2df$LoyCol, act2df$NPS))
#Most number of passengers who were detractors had a loyalty score of less than 1 (less loyal) when
#compared to more loyal passengers who are only 340 passengers who were detractors.

table(act2df$Shopping.Amount.at.Airport, act2df$NPS)
table(act2df$Eating.and.Drinking.at.Airport, act2df$NPS)
#From the outputs of the above commands, it can be observed that the amount spent by the passengers 
#varies significantly. So, it cannot be related to detractors and how spending relates to low customer
#satisfaction scores.

table(act2df$Class, act2df$NPS)
#            Detractor Passive Promotor
#Business         0       0        0
#Eco           2157    1887     1649
#Eco Plus         0       0        0
prop.table(table(act2df$Class, act2df$NPS))
#         Detractor   Passive  Promotor
#Business 0.0000000 0.0000000 0.0000000
#Eco      0.3788864 0.3314597 0.2896540
#Eco Plus 0.0000000 0.0000000 0.0000000
#It can be observed form the above output that there were more number of passengers in the economic class
#and the number of passengers who gave low customer satisfaction scores are also high among the economic class.

table(act2df$Day.of.Month, act2df$NPS)
#No correlation can be obtained from the day of the month attribute and the customer satisfaction scores.

table(act2df$Scheduled.Departure.Hour, act2df$NPS)
#No correlation can be obtained from the scheduled departure hor attribute and the customer 
#satisfaction scores.

table(act2df$Flight.cancelled, act2df$NPS)
#The flight cancellations did not impact the customer satisfaction scores as passengers whose flights
#did not get cancelled were more among the number of detractors.

table(act2df$Flight.Distance, act2df$NPS)
#From the outputs of the above commands, it can be observed that the flight distance varies 
#significantly. So, it cannot be related to detractors and how flight distance relates to low customer
#satisfaction scores.

table(act2df$Delay, act2df$NPS)
#Delay cannot be correlated with low customer satisfaction scores as the detractors are almost equally
#distributed among the passengers whose flights got delayed and the passengers whose flights were not
#delayed.

table(act2df$Long.Duration, act2df$NPS)
#The passengers who travelled a longer duration or had a long journey were among the maximum number
#of detractors.

table(act2df$Type.of.Travel, act2df$Gender, act2df$NPS)
prop.table(table(act2df$Type.of.Travel, act2df$Gender, act2df$NPS))
#From the output of the above commands, we can observe that the female passengers who travelled for 
#personal reasons were the maximum percentage among the detractors.

table(act2df$Airline.Status, act2df$Type.of.Travel, act2df$NPS, act2df$Gender)
#It can be observed that female passengers who used personal travel and flew in the blue airline status 
#category contributed towards low customer satisfaction scores more than anyone.

table(act2df$AgeG, act2df$LoyCol, act2df$Class, act2df$NPS)
#From the above output, it can be made out that older adult passengers who flew in economy class and tried
#other airlines were less loyal and contributed towards low customer satisfaction scores more than anyone.

table(act2df$Long.Duration, act2df$NPS, act2df$AgeG, act2df$Gender)
#It can be observed that older female passengers who had longer duration of flight journey gave 
#low customer satisfaction scores compared to others.

table(act2df$Long.Duration, act2df$NPS, act2df$AgeG, act2df$Class)
#From the above output, it can be made out that older adult passengers who flew in economy class and tried
#other airlines and had a longer flight duration had contributed towards low customer satisfaction 
#scores more than anyone.

table(act2df$Origin.State, act2df$NPS)
#It can be observed that passengers flying from California, Texas and some other states had given the 
#lowest customer satisfaction scores.

table(act2df$Destination.State, act2df$NPS)
#It can be observed that passengers flying to California, Texas and some other states had given the 
#lowest customer satisfaction scores.

ggplot(data=act2df, aes(x=Likelihood.to.recommend,y=Gender, fill=Type.of.Travel),position="dodge") +geom_col()+
  ggtitle("Likelihood to recommend Vs Type of travel")+labs(x="Recommendation score (1-10)",y="Number of Travellers") +
  scale_fill_manual(values = c("Business travel" = "lightgreen","Mileage tickets" = "red","Personal Travel"= "grey"))
#From the above plots and output, it can be observed that the female passengers who travelled for personal reasons were
#the passengers who rated low satisfaction scores.


#In the above data we have filtered the passengers travelling in the blue airline status and 
#in the economy class. So, from the above outputs it can be inferred that older female passengers who
#are travelling to and from California, Illinois, Texas and Georgia and had longer duration flights
#are the maximum detractors.




#3
act3df <- acttdf %>%
  filter(str_trim(Gender) == "Female" & str_trim(AgeG) == "OlderA")

View(act3df)

#By studying Association Rules Mining, we can understand that some independent variables were more 
#impactful or important in influencing the dependent variable.
#Some of them can be seen listed in the ruleset1 summary.
#Running the inspect command to know or understand more about them:
#inspect(sort(head(ruleset1,10), y="lift"))

#We now filter data such that the dataset contains information or details of female passengers who
#are of age greater than 55 to study why the passengers relating to these conditions 
#consist of more detractors.


table(act3df$Airline.Status, act3df$NPS)
#There were 764 passengers in the blue airline status category, who were detractors.


table(act3df$Type.of.Travel)
#Business travel Mileage tickets Personal Travel 
#   570             124            1113 
table(act3df$NPS, act3df$Type.of.Travel)
prop.table(table(act3df$NPS, act3df$Type.of.Travel))
#            Business travel Mileage tickets Personal Travel
#Detractor     0.054233536     0.009407858     0.411178749
#Passive       0.116768124     0.028776978     0.177089098
#Promotor      0.144438296     0.030437189     0.027670172
#We observe that passengers who used the airlines for personal travel were not satisfied with the travel 
#and gave low recommendation scores. They were the most number of detractors.
#While, most number of business travellers were promotors, the passive category has its maximum number 
#of passengers from business travellers too.


table(act3df$AgeG)
#MiddleA  OlderA  YoungA 
#    0     1807     0
table(act3df$AgeG, act3df$NPS)
#           Detractor Passive Promotor
#MiddleA         0       0        0
#OlderA        858     583      366
#YoungA          0       0        0
prop.table(table(act3df$AgeG, act3df$NPS))
#       Detractor    Passive   Promotor
#MiddleA 0.0000000 0.0000000 0.0000000
#OlderA  0.4748201 0.3226342 0.2025457
#YoungA  0.0000000 0.0000000 0.0000000
#From the above output, we can notice that the almost 50% of the older passengers were detractors.

table(act3df$AgeG, act3df$Type.of.Travel, act3df$NPS)
#The older age passengers who travelled for personal travel were the maximum among the detractors.

table(act3df$Gender, act3df$NPS)
#The detractors were more among women when compared to men.

table(act3df$Price.Sensitivity, act3df$NPS)
#Most number of dteractors are in the price sensitivity category of 1. So, we may infer that some passengers
#did not give accurate reviews or there were some other attributes which affect the detractors.

table(act3df$LoyCol, act3df$NPS)
prop.table(table(act3df$LoyCol, act3df$NPS))
#Most number of passengers who were detractors had a loyalty score of less than 1 (less loyal) when
#compared to more loyal passengers who are only 51 passengers who were detractors.

table(act3df$Shopping.Amount.at.Airport, act3df$NPS)
table(act3df$Eating.and.Drinking.at.Airport, act3df$NPS)
#From the outputs of the above commands, it can be observed that the amount spent by the passengers 
#varies significantly. So, it cannot be related to detractors and how spending relates to low customer
#satisfaction scores.

table(act3df$Class, act3df$NPS)
#            Detractor Passive Promotor
#Business        41      41       32
#Eco            684     428      277
#Eco Plus       133     114       57
prop.table(table(act3df$Class, act3df$NPS))
#         Detractor   Passive  Promotor
#Business 0.02268954 0.02268954 0.01770891
#Eco      0.37852795 0.23685667 0.15329275
#Eco Plus 0.07360266 0.06308799 0.03154400
#It can be observed form the above output that there were more number of passengers in the economic class
#and the number of passengers who gave low customer satisfaction scores are also high among the economic class.

table(act3df$Day.of.Month, act3df$NPS)
#No correlation can be obtained from the day of the month attribute and the customer satisfaction scores.

table(act3df$Scheduled.Departure.Hour, act3df$NPS)
#No correlation can be obtained from the scheduled departure hor attribute and the customer 
#satisfaction scores.

table(act3df$Flight.cancelled, act3df$NPS)
#The flight cancellations did not impact the customer satisfaction scores as passengers whose flights
#did not get cancelled were more among the number of detractors.

table(act3df$Flight.Distance, act3df$NPS)
#From the outputs of the above commands, it can be observed that the flight distance varies 
#significantly. So, it cannot be related to detractors and how flight distance relates to low customer
#satisfaction scores.

table(act3df$Delay, act3df$NPS)
#Delay cannot be correlated with low customer satisfaction scores as the detractors are almost equally
#distributed among the passengers whose flights got delayed and the passengers whose flights were not
#delayed.

table(act3df$Long.Duration, act3df$NPS)
#The passengers who travelled a longer duration or had a long journey were among the maximum number
#of detractors.

table(act3df$Type.of.Travel, act3df$Gender, act3df$NPS)
prop.table(table(act3df$Type.of.Travel, act3df$Gender, act3df$NPS))
#From the output of the above commands, we can observe that the female passengers who travelled for 
#personal reasons were the maximum percentage among the detractors.

table(act3df$Airline.Status, act3df$Type.of.Travel, act3df$NPS, act3df$Gender)
#It can be observed that female passengers who used personal travel and flew in the blue airline status 
#category contributed towards low customer satisfaction scores more than anyone.

table(act3df$AgeG, act3df$LoyCol, act3df$Class, act3df$NPS)
#From the above output, it can be made out that older adult passengers who flew in economy class and tried
#other airlines were less loyal and contributed towards low customer satisfaction scores more than anyone.

table(act3df$Long.Duration, act3df$NPS, act3df$AgeG, act3df$Gender)
#It can be observed that older female passengers who had longer duration of flight journey gave 
#low customer satisfaction scores compared to others.

table(act3df$Long.Duration, act3df$NPS, act3df$AgeG, act3df$Class)
#From the above output, it can be made out that older adult passengers who flew in economy class and tried
#other airlines and had a longer flight duration had contributed towards low customer satisfaction 
#scores more than anyone.

table(act3df$Origin.State, act3df$NPS)
#It can be observed that passengers flying from California, Texas and some other states had given the 
#lowest customer satisfaction scores.

table(act3df$Destination.State, act3df$NPS)
#It can be observed that passengers flying to California, Texas and some other states had given the 
#lowest customer satisfaction scores.

ggplot(data=act3df, aes(x=Type.of.Travel,y=Likelihood.to.recommend, fill=Airline.Status),position="dodge") +geom_col()+
  ggtitle("Likelihood to recommend Vs Airline Status")+labs(x="Airline Status",y="Scores (10-1)") +
  scale_fill_manual(values = c("Blue" = "Purple","Silver" = "blue", "Gold"="yellow","Platinum"="white"))
#It can be seen from the above graph that passengers who travlled in the blue airlnine status and tarveleld for 
#personal reasons were more likely to give less customer satisfaction scores.

ggplot(act3df,aes( Loyalty,Flight.Distance)) + geom_line(color="steelblue", size=1, alpha=0.7, linetype=1)
#It can be observed from the above graph that passengers who travelled different airlines or were less loyal 
#were more likely to travel long distances. In turn, the passengers who travelled for long duration and long distances
#were the people who gave low satisfaction scores.

#In the above data we have filtered the passengers who were female and of older age that is, age greater than 55.
#So, from the above outputs it can be inferred that these passengers were less loyal and travelling in the 
#economy class of blue airline category and travelling to and from California, Illinois, Texas and 
#Georgia and had longer duration flights are the maximum detractors.





