#-----------------------libraries, wd, seed, import data------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(caret)
library(ggplot2)
library(corrplot)

#setwd and seed
setwd("C:/Users/VPL/Desktop/Data Science/Ubiqum/Module 3")
set.seed(123)

#import data
concatenated_factors <- read.csv("concatenated_factors.csv")
concatenated_websites <- read.csv("concatenated_websites.csv")

summary(concatenated_factors)
summary(concatenated_websites)
head(concatenated_factors)
head(concatenated_websites)
str(concatenated_factors)
str(concatenated_websites)
which(apply(concatenated_factors, 2, var) == 0)
which(apply(concatenated_factors, 1, var) == 0)

#-------------------------------clean-----------------------

#na
length(which(is.na(concatenated_factors)))
length(which(concatenated_factors == "?"))
length(which(concatenated_factors == ""))
length(which(concatenated_factors == " "))
length(which(concatenated_factors == "none"))

length(which(is.na(concatenated_websites)))
length(which(concatenated_websites == "?"))
length(which(concatenated_websites == ""))
length(which(concatenated_websites == " "))
length(which(concatenated_websites == "none"))

#Unneeded variables
cf_clean <- concatenated_factors
cf_clean$sonyxperia <- NULL
cf_clean$nokialumina <- NULL
cf_clean$htcphone <- NULL
cf_clean$sonycampos <- NULL
cf_clean$nokiacampos <- NULL
cf_clean$htccampos <- NULL
cf_clean$sonycamneg <- NULL
cf_clean$nokiacamneg <- NULL
cf_clean$htccamneg <- NULL
cf_clean$sonycamunc <- NULL
cf_clean$nokiacamunc <- NULL
cf_clean$htccamunc <- NULL
cf_clean$sonydispos <- NULL
cf_clean$nokiadispos <- NULL
cf_clean$htcdispos <- NULL
cf_clean$sonydisneg <- NULL
cf_clean$nokiadisneg <- NULL
cf_clean$htcdisneg <- NULL
cf_clean$sonydisunc <- NULL
cf_clean$nokiadisunc <- NULL
cf_clean$htcdisunc <- NULL
cf_clean$sonyperpos <- NULL
cf_clean$nokiaperpos <- NULL
cf_clean$htcperpos <- NULL
cf_clean$sonyperneg<- NULL
cf_clean$nokiaperneg <- NULL
cf_clean$htcperneg <- NULL
cf_clean$sonyperunc <- NULL
cf_clean$nokiaperunc <- NULL
cf_clean$htcperunc <- NULL
head(cf_clean)


#---------------------Data transformation--------------------------------------------

cf_weight <- cf_clean
cf_weight <- mutate(cf_weight, iphonepos = (iphonecampos + iphonedispos + iphoneperpos + iosperpos) * 10)
cf_weight <- mutate(cf_weight, iphoneneg = (iphonecamneg + iphonedisneg + iphoneperneg + iosperneg) * -10)
cf_weight <- mutate(cf_weight, iphoneunc = (iphonecamunc + iphonedisunc + iphoneperunc + iosperunc))
cf_weight <- mutate(cf_weight, iphoneSentiment = (iphonepos + iphoneneg + iphoneunc)) 
                    
cf_weight <- mutate(cf_weight, samsungpos = (samsungcampos + samsungdispos + samsungperpos + googleperpos) * 10)
cf_weight <- mutate(cf_weight, samsungneg = (samsungcamneg + samsungdisneg + samsungperneg + googleperneg) * -10)
cf_weight <- mutate(cf_weight, samsungunc = (samsungcamunc + samsungdisunc + samsungperunc + googleperunc))
cf_weight <- mutate(cf_weight, samsungSentiment = (samsungpos + samsungneg + samsungunc))
head(cf_weight)



iphonelargematrix <- cf_weight[c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 31, 32, 33)]
head(iphonelargematrix)

samsunglargematrix <- cf_weight[c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 34, 35, 36, 37)]
head(samsunglargematrix)

#variance
which(apply(iphonelargematrix, 2, var) == 0)
which(apply(iphonelargematrix, 1, var) == 0)
iphone <- iphonelargematrix[-c(which(apply(iphonelargematrix, 1, var) == 0)),]
which(apply(samsunglargematrix, 2, var) == 0)
samsung <- samsunglargematrix[-c(which(apply(samsunglargematrix, 1, var) == 0)),]
head(iphone)

hist(iphone$iphoneSentiment)
hist(iphone$iphoneSentiment, xlim=c(1200,1300), ylim=c(0,100))
hist(samsung$samsungSentiment)
iphone_wol <- filter(iphone, iphoneSentiment < 1200)
samsung_wol <- filter(samsung, samsungSentiment < 400)
samsung_end <- distinct(samsung_wol)
iphone_end <- distinct(iphone_wol)

hist(iphone_end$iphoneSentiment)
hist(samsung_end$samsungSentiment)
summary(iphone_end)
corrplot(cor(iphone_end), method = "number")
