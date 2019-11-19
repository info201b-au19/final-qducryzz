# I decided to create a scatterplot with the average human development index number (from 0 to 1) of a country 
# for each country from 1990-2017 on the x axis and the same country's average percent renewable energy from 
# 1990-2017 on the y axis. I believed that the amount of wealth and development a country has would directly
# correlate with the ratio of renewable energy used in that country and a correlation scatter plot would be 
# the best way to display that. However, I did not realize that there would be an inverse correlation- that 
# is, the lower a country's HDI, the more % renewable energy that country uses. At first, this was surprising, 
# but it later made sense to me, because wealthier countries like the United States tend to be more industrialized 
# and therefore output more Co2 than third world countries that don't have factories or mass production. 

library(ggplot2)
library(dplyr)
library(tidyverse)

createAveragesPlot <- function(both_RE_HDI$Mean_HDI, both_RE_HDI$Mean_re) {
  gg <- ggplot(both_RE_HDI, aes(both_RE_HDI$Mean_HDI, both_RE_HDI$Mean_re, color = both_RE_HDI$Mean_HDI)) +
          geom_point(shape = 16,  size = 3, alpha = .5) +
          theme_minimal() +
          scale_color_gradient(low = "#000000", high = "#ffa500") +
          xlab("Average HDI Value") +
          ylab("Average % Renewable Energy") +
          ggtitle("Correlation Between Average RE and HDI") + 
          theme(legend.position="left")
  gg + labs(color="HDI (0-1)")
}
