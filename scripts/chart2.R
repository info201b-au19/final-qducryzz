library(dplyr)
library(ggplot2)
library(plotly)

## Total and all CO2 outputs are measured in million metric tons
my_data <- read.csv(file = "data/CombinedQoLData.csv", stringsAsFactors = FALSE)

x <- my_data %>%
  filter(Year == 2014) %>%
  summarise(total = sum(Total)) %>%
  pull(total)

Total2014 <- my_data %>%
  filter(Year == 2014) %>%
  arrange(-Total)

s <- 0
pop <- 0
for(i in 1:length(Total2014$Total)){
 s <- s + Total2014$Total[i]
 pop <- pop + Total2014$Pop[i]
 if(s/x > 0.6){
   break
 }
}

## Nations responsible for 90% of total CO2 emmisions, home to 4.5 billion people
Top <- Total2014$Country[1:i]

TopNations <- my_data[my_data$Country %in% Top,]
TopNations1950 <- TopNations %>%
  filter(Year >= 1960)

meanLife <- TopNations1950 %>%
  group_by(Year) %>%
  summarise(mean = mean(LifeTime))
TopNations1950 <- left_join(TopNations1950, meanLife, by = "Year")

p <- ggplot(TopNations1950, mapping = aes(x = Year)) +
  geom_line(aes(y = Total, color = Country, group = Country)) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 20000, name = "Life Expectancy (Years0")) +
  geom_line(aes(y = mean * 20000)) +
  ggtitle("CO2 Emissions from 1960 - 2014") +
  xlab("CO2 (Million Metric Tons)")
p


