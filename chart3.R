#create scatterplot with average human development index # 
#for each country from 1990-2017 on y axis and average 
#percent renewable energy from 1990-2017 on x axis
#overlay with average human development index # 
#for each country from 1990-2017 on y axis

library(ggplot2)
library(dplyr)
library(tidyverse)

data_HDI <- read.csv("data/Human_development_index_(HDI).csv")
data_re <- read.csv("data/Renewable_Energy_Production.csv")

data_HDI <- data.frame(data_HDI)

meanHDI <- data.frame(Country=data_HDI[,1], Mean_HDI=rowMeans(data_HDI[,-1]))
write.csv(meanHDI, "data/meanHDI.csv", row.names = TRUE)

data_re$Indicator.Code <- NULL
data_re$Indicator.Name <- NULL
data_re$Country.Code <- NULL
meanRE <- data.frame(Country=data_re[,1], Mean_re=rowMeans(data_re[,-1]))
write.csv(meanRE, "data/meanRE.csv", row.names = TRUE)

both_RE_HDI<- merge(meanHDI, meanRE)
write.csv(meanRE, "data/both_RE_HDI.csv", row.names = TRUE)

createScatterPlot <- function(both_RE_HDI) {
  ggplot(both_RE_HDI, aes(both_RE_HDI$Mean_HDI, both_RE_HDI$Mean_re, color = both_RE_HDI$Mean_HDI)) + 
    geom_point(shape = 16, size = 3, alpha = .5) + 
    theme_minimal() +
    scale_color_gradient(low = "#0091ff", high = "#f0650e") +
    xlab("Average HDI Value") +
    ylab("Average % Renewable Energy") +
    ggtitle("Correlation Between Average RE and HDI") + 
    theme(legend.position="none")
  }
