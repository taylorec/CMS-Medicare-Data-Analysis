##### Nursing Home Affiliated Entity Performance Measures

library(tidyr)
library(dplyr)

df <- read.csv('Affiliated Entity Performance.csv')

colnames(df)
dim(df) # number of rows, columns
str(df)
any(is.na(df))

### Columns to keep
columns.keep <- c('Affiliated.entity', 'Number.of.facilities', 'Number.of.Special.Focus.Facilities..SFF.', 'Percentage.of.facilities.with.an.abuse.icon',
                  'Percent.of.facilities.classified.as.for.profit', 'Average.overall.5.star.rating', 'Average.health.inspection.rating', ' Average.staffing.rating',
                  'Average.quality.rating', 'Average.total.nurse.hours.per.resident.day', 'Average.number.of.fines', 'Average.percentage.of.short.stay.residents.who.made.improvements.in.function', 
                  'Average.percentage.of.long.stay.residents.experiencing.one.or.more.falls.with.major.injury', 'Average.percentage.of.long.stay.residents.whose.ability.to.move.independently.worsened', 
                  'Affiliated.entity', 'Average.percentage.of.long.stay.residents.who.have.symptoms.of.depression', 'Average.percentage.of.long.stay.residents.who.used.antianxiety.or.hypnotic.medication')

Entity.Performance<- (df[, (names(df) %in% columns.keep)])

ncol(Entity.Performance) # number of columns

anyNA(Entity.Performance) # check if NA values exist

# Count number of NA values per column
na_count <-
  sapply(Entity.Performance, function(y)
    sum(length(which(is.na(
      y
    )))))
na_df <- data.frame(na_count)
View(na_df) # View columns needed for data cleaning


### Data Cleaning
Entity.Performance$Average.total.nurse.hours.per.resident.day <- ifelse(is.na(Entity.Performance$Average.total.nurse.hours.per.resident.day),        #if value is NA
                             							      mean(Entity.Performance$Average.total.nurse.hours.per.resident.day,na.rm=T), #replace with mean
                                                                        Entity.Performance$Average.total.nurse.hours.per.resident.day)               #else keep value

Entity.Performance$Average.percentage.of.short.stay.residents.who.made.improvements.in.function <- ifelse(is.na(Entity.Performance$Average.percentage.of.short.stay.residents.who.made.improvements.in.function),        #if value is NA
                             							                                        mean(Entity.Performance$Average.percentage.of.short.stay.residents.who.made.improvements.in.function,na.rm=T), #replace with mean
                                                                                                          Entity.Performance$Average.percentage.of.short.stay.residents.who.made.improvements.in.function)               #else keep value

Entity.Performance$Average.percentage.of.long.stay.residents.experiencing.one.or.more.falls.with.major.injury <- ifelse(is.na(Entity.Performance$Average.percentage.of.long.stay.residents.experiencing.one.or.more.falls.with.major.injury),        #if value is NA
                             							                                                      mean(Entity.Performance$Average.percentage.of.long.stay.residents.experiencing.one.or.more.falls.with.major.injury,na.rm=T), #replace with mean
                                                                                                                        Entity.Performance$Average.percentage.of.long.stay.residents.experiencing.one.or.more.falls.with.major.injury)               #else keep value

Entity.Performance$Average.percentage.of.long.stay.residents.whose.ability.to.move.independently.worsened <- ifelse(is.na(Entity.Performance$Average.percentage.of.long.stay.residents.whose.ability.to.move.independently.worsened),            #if value is NA
                             							                                                      mean(Entity.Performance$Average.percentage.of.long.stay.residents.whose.ability.to.move.independently.worsened,na.rm=T), #replace with mean
                                                                                                                        Entity.Performance$Average.percentage.of.long.stay.residents.whose.ability.to.move.independently.worsened)               #else keep value

Entity.Performance$Average.percentage.of.long.stay.residents.who.have.symptoms.of.depression <- ifelse(is.na(Entity.Performance$Average.percentage.of.long.stay.residents.who.have.symptoms.of.depression),            #if value is NA
                             							                                         mean(Entity.Performance$Average.percentage.of.long.stay.residents.who.have.symptoms.of.depression,na.rm=T), #replace with mean
                                                                                                           Entity.Performance$Average.percentage.of.long.stay.residents.who.have.symptoms.of.depression)               #else keep value

Entity.Performance$Average.percentage.of.long.stay.residents.who.used.antianxiety.or.hypnotic.medication <- ifelse(is.na(Entity.Performance$Average.percentage.of.long.stay.residents.who.used.antianxiety.or.hypnotic.medication),            #if value is NA
                             							                                                     mean(Entity.Performance$Average.percentage.of.long.stay.residents.who.used.antianxiety.or.hypnotic.medication,na.rm=T), #replace with mean
                                                                                                                       Entity.Performance$Average.percentage.of.long.stay.residents.who.used.antianxiety.or.hypnotic.medication)               #else keep value


#New count of number of NA values per column
na_count <-
  sapply(Entity.Performance, function(y)
    sum(length(which(is.na(
      y
    )))))
na_df <- data.frame(na_count)
View(na_df) # View columns needed for data cleaning


### Exploratory Data Analysis

# Number of facilities in this data: 24,360 facilities
sum(Entity.Performance$Number.of.facilities)

# Percentage of Affiliated Entities with Special Focus Facilities 8.8%
count(subset(Entity.Performance,subset=Number.of.Special.Focus.Facilities..SFF.>0))/nrow(Entity.Performance)*100

# Percentage of facilities with an abuse icon
mean(Entity.Performance$Percentage.of.facilities.with.an.abuse.icon) # mean percentage is 8.8%
median(Entity.Performance$Percentage.of.facilities.with.an.abuse.icon) # median percentage is 3.2%
hist(Entity.Performance$Percentage.of.facilities.with.an.abuse.icon) # distribution is heavily right skewed 

# Percentage of facilities classified as for profit
mean(Entity.Performance$Percent.of.facilities.classified.as.for.profit) # mean percentage is 78.8%
median(Entity.Performance$Percent.of.facilities.classified.as.for.profit) # median percentage is 100%

# Average overall 5 start rating
mean(Entity.Performance$Average.overall.5.star.rating) # mean rating is 2.78
median(Entity.Performance$Average.overall.5.star.rating) median rating is 2.70
hist(Entity.Performance$Average.overall.5.star.rating) # distribution is slightly right skewed
shapiro.test(Entity.Performance$Average.overall.5.star.rating) # non-normal distribution

# Average health inspection rating
mean(Entity.Performance$Average.health.inspection.rating) # mean rating is 2.73
median(Entity.Performance$Average.health.inspection.rating) #median rating is 2.70
hist(Entity.Performance$Average.health.inspection.rating) # distribution is slightly right skewed
shapiro.test(Entity.Performance$Average.health.inspection.rating) # non-normal distribution 

# Average quality rating
mean(Entity.Performance$Average.quality.rating) # mean rating is 3.49
median(Entity.Performance$Average.quality.rating) # median rating is 3.50
hist(Entity.Performance$Average.quality.rating) # distribution is slightly left skewed
shapiro.test(Entity.Performance$Average.quality.rating) # non-normal distribution

# Average total nurse hours per resident day per facility
mean(Entity.Performance$Average.total.nurse.hours.per.resident.day) # mean is 3.77
median(Entity.Performance$Average.total.nurse.hours.per.resident.day) # median is 3.60
hist(Entity.Performance$Average.total.nurse.hours.per.resident.day) # distribution is slightly right skewed
shapiro.test(Entity.Performance$Average.total.nurse.hours.per.resident.day) # non-normal distribution

# Average number of civil monetary penalties (CMPs) across all facilities
mean(Entity.Performance$Average.number.of.fines) # mean is 2.28
median(Entity.Performance$Average.number.of.fines) # median is 1.8
hist(Entity.Performance$Average.number.of.fines) # distribution is heavily skewed right skewed

# Average percentage of short-stay residents who made improvements in function
mean(Entity.Performance$Average.percentage.of.short.stay.residents.who.made.improvements.in.function) # mean percentage is 76.15
median(Entity.Performance$Average.percentage.of.short.stay.residents.who.made.improvements.in.function) # median percentage is 77.00
hist(Entity.Performance$Average.percentage.of.short.stay.residents.who.made.improvements.in.function, breaks=20) 

# Average percentage of long-stay residents experiencing one or more falls with major injury
mean(Entity.Performance$Average.percentage.of.long.stay.residents.experiencing.one.or.more.falls.with.major.injury) # mean percentage is 3.33
median(Entity.Performance$Average.percentage.of.long.stay.residents.experiencing.one.or.more.falls.with.major.injury) # median percentage is 3.30
hist(Entity.Performance$Average.percentage.of.long.stay.residents.experiencing.one.or.more.falls.with.major.injury)

# Average percentage of long-stay residents whose ability to move independently worsened
mean(Entity.Performance$Average.percentage.of.long.stay.residents.whose.ability.to.move.independently.worsened) # mean percentage is 15.52
median(Entity.Performance$Average.percentage.of.long.stay.residents.whose.ability.to.move.independently.worsened) # median percentage is 15.45
hist(Entity.Performance$Average.percentage.of.long.stay.residents.whose.ability.to.move.independently.worsened, breaks=20)

# Average percentage of long-stay residents who have symptoms of depression
mean(Entity.Performance$Average.percentage.of.long.stay.residents.who.have.symptoms.of.depression) # mean percentage is 8.80
median(Entity.Performance$Average.percentage.of.long.stay.residents.who.have.symptoms.of.depression) # median percentage is 4.2
hist(Entity.Performance$Average.percentage.of.long.stay.residents.who.have.symptoms.of.depression) # Distribution is heavily right skewed

# Average percentage of long-stay residents who used antianxiety or hypnotic medication
mean(Entity.Performance$Average.percentage.of.long.stay.residents.who.used.antianxiety.or.hypnotic.medication) # mean percentage is 19.56
median(Entity.Performance$Average.percentage.of.long.stay.residents.who.used.antianxiety.or.hypnotic.medication) # median percentage is 19.10
hist(Entity.Performance$Average.percentage.of.long.stay.residents.who.used.antianxiety.or.hypnotic.medication)

## Correlation Analysis

# No correlation exists between abuse icon and for-profit entities
cor(Entity.Performance$Percentage.of.facilities.with.an.abuse.icon, Entity.Performance$Percent.of.facilities.classified.as.for.profit) # 0.18 Correlation

# Weak negative correlation exists between overall rating and for-profit entities
cor(Entity.Performance$Average.overall.5.star.rating, Entity.Performance$Percent.of.facilities.classified.as.for.profit) # -0.37 Correlation

# Moderate negative correlation exists between abuse icon and five-star rating
cor(Entity.Performance$Percentage.of.facilities.with.an.abuse.icon, Entity.Performance$Average.overall.5.star.rating) # -0.477

# Very Strong correlation exists between health inspection rating and five-star rating
cor(Entity.Performance$Average.health.inspection.rating, Entity.Performance$Average.overall.5.star.rating) # 0.91 Correlation

# Moderate correlation exists between quality rating and five-star rating
cor(Entity.Performance$Average.quality.rating, Entity.Performance$Average.overall.5.star.rating) # 0.55 Correlation

# Weak correlation exists between quality rating and health inspection
cor(Entity.Performance$Average.quality.rating, Entity.Performance$Average.health.inspection.rating) # 0.30 Correlation

# Weak correlation exists between nursing hours and quality rating
cor(Entity.Performance$Average.quality.rating, Entity.Performance$Average.total.nurse.hours.per.resident.day) # 0.29 Correlation

# Strong correlation exists between nursing hours and five-star rating
cor(Entity.Performance$Average.total.nurse.hours.per.resident.day, Entity.Performance$Average.overall.5.star.rating) # 0.60 Correlation

# Weak correlation between number of fines and abuse icon
cor(Entity.Performance$Average.number.of.fines, Entity.Performance$Percentage.of.facilities.with.an.abuse.icon) # 0.22 Correlation

# Weak correlation between number of fines and quality rating
cor(Entity.Performance$Average.number.of.fines, Entity.Performance$Average.quality.rating) # -0.14 Correlation

# Moderately-weak negative corralation between number of fines and five-star rating
cor(Entity.Performance$Average.number.of.fines, Entity.Performance$Average.overall.5.star.rating) # -0.39 Correlation

# Weak correlation between resident function improvements and nursing hours
cor(Entity.Performance$Average.percentage.of.short.stay.residents.who.made.improvements.in.function, Entity.Performance$Average.total.nurse.hours.per.resident.day) # 0.11 Correlation

# Weak correlation between resident function improvements and five-star rating
cor(Entity.Performance$Average.percentage.of.short.stay.residents.who.made.improvements.in.function, Entity.Performance$Average.overall.5.star.rating) # 0.22 Correlation

# Weak correlation between number of resident falls and abuse icon
cor(Entity.Performance$Average.percentage.of.long.stay.residents.experiencing.one.or.more.falls.with.major.injury, Entity.Performance$Percentage.of.facilities.with.an.abuse.icon) # -0.11 Correlation

# Weak correlation between resident falls and five-star rating
cor(Entity.Performance$Average.percentage.of.long.stay.residents.experiencing.one.or.more.falls.with.major.injury, Entity.Performance$Average.overall.5.star.rating) # 0.06 Correlation

# Weak to moderate correlation between resident function improvements and resident abilities worsened
cor(Entity.Performance$Average.percentage.of.short.stay.residents.who.made.improvements.in.function, Entity.Performance$Average.percentage.of.long.stay.residents.whose.ability.to.move.independently.worsened) # -0.31 Correlation

# Weak correlation between resident abilities worsening and five-star rating
cor(Entity.Performance$Average.percentage.of.long.stay.residents.whose.ability.to.move.independently.worsened, Entity.Performance$Average.overall.5.star.rating) # -0.14 Correlation

# No correlation between residents with symptoms of depression and residents who use antianxiety or hypnotic medication
cor(Entity.Performance$Average.percentage.of.long.stay.residents.who.have.symptoms.of.depression, Entity.Performance$Average.percentage.of.long.stay.residents.who.used.antianxiety.or.hypnotic.medication) # 0.03 Correlation

library(corrplot)
cor_df <- Entity.Performance[,-c(1,2,3,5,11,12,13,14,15)]
cor_matrix <- cor(cor_df)
corrplot(cor_matrix, tl.col="blue", tl.srt=45)

### Linear Regression on five-star rating
res <- lm(Average.overall.5.star.rating~., data=cor_df)
summary(res)  # Multiple R-squared: 0.9336; predictor variables are statistically significant
library(car)
vif(res)      # Variance Inflation Factor: no multicollinearity detected 

par(mfrow = c(2,2))
plot(res)

# plot of residuals and actual values follows the linear regression assumptions
plot(res$fitted.values, res$Average.overall.5.star.rating)

# normality of residuals
hist(res$residuals)
plot(res, 2)

# Homoscedasticity testing follows the linear regression assumptions
plot(res, 1)
plot(res, 3)
library(lmtest)
bptest(res)

### Model Assumptions
# Entity five-star rating is statistically signficantly modeled against health inspection and quality ratings, 
# nursing hours per resident, and average number of entity fines. 
# Model Adjusted R-squared: 0.9331 with p-value: < 2.2e-16
# Health inspection rating has the highest coefficient for entity five-star rating
