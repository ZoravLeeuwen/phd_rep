#Cleaning data using a Hampel identifier
#https://www.r-bloggers.com/cleaning-time-series-and-other-data-streams/
#https://www.r-bloggers.com/moving-window-filters-and-the-pracma-package/
#https://cran.r-project.org/web/packages/pracma/pracma.pdf
library(pracma)
library(ggplot2)
library(plotly)

#Load level_data
head(level_data)

#choose a test period

#Find a period of the data with an obvious outlier using plotly
p<-autoplot(level_data$LB8U)
p<-p+theme_bw()
(ply<-ggplotly(p,dynamicTicks=TRUE))

#e.g. around LB8U July 27th
#date range to test
mystart<-as.POSIXct("2018-06-20",tz="GMT")
myend<-as.POSIXct("2018-09-20",tz="GMT")
test_data<-window(level_data$LB8U,start=mystart,end=myend)
autoplot(test_data)

#fill in NA with median value (don't want to just use last value as errors are likely to occur around NAs)
mymed<-median(na.omit(test_data)) #calculate median
test_data_filled<-na.fill(test_data,fill=as.list(mymed))
autoplot(test_data_filled)
#if want to know whether nay NAs remaining
any(is.na(test_data_filled))


#Try hampel filter
cleaned<-hampel(as.vector(coredata(test_data_filled)),k=10,t=3) # have ti omit NAs for this to work, should probably fill them instead

plot(as.vector(coredata(test_data_filled)),type="l") #plot original data
lines(cleaned$y,type="l",col="darkred") #plot cleaned data over top
#OR plot removed points
points(cleaned$ind,as.vector(coredata(test_data_filled[cleaned$ind])),pch=12,col="darkred")


