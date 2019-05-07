#QA rainfall data
#check whether spikes are real events
#1. Identify spikes using ggplotly

library(plotly)
library(ggplot2)
library(zoo)

#Load rainfall data from load_data script
head(rain)

#Load Scarhouse data
head(Scarhouse_zoo)


#gauge data
test<-autoplot(rain)
(testly<-ggplotly(test)) #omg this works