#Data QA - change point in mean detection
#https://rpubs.com/richkt/269908

#load level data
head(level_data)

#Know there is a change around July 27th on LB8U - but this will be difficult to detect as just before an event

#Have a go following the example from rpubs

y_ts    <- ts(rnorm(500,mean=1,sd=.5)) # random signal without a changepoint
y_ts_CP <- ts(c(rnorm(250,mean=1,sd=.5), rnorm(250,mean=2,sd=1))) # rand signal w\ changepoint

#using my data
#e.g. around LB8U July 27th
#date range to test
mystart<-as.POSIXct("2018-06-20",tz="GMT")
myend<-as.POSIXct("2018-09-20",tz="GMT")
test_data<-window(level_data$LB8U,start=mystart,end=myend)
#autoplot(test_data)

#fill in NA with median value (don't want to just use last value as errors are likely to occur around NAs)
mymed<-median(na.omit(test_data)) #calculate median
test_data_filled<-na.fill(test_data,fill=as.list(mymed))

y_ts<-test_data_filled #LB8U
y_ts_CP<-test_data_filled #LB8L



options(warn=-1)
library(changepoint)

cptfn <- function(data, pen) {
  ans <- cpt.mean(data, test.stat="Normal", method = "PELT", penalty = "Manual", pen.value = pen) 
  length(cpts(ans)) +1
}

# evaluate and plot results:
plot.new()
frame()
par(mfcol=c(2,2))
# run cptfn for the signal with a known change point
pen.vals <- seq(0, 12,.2)
elbowplotData <- unlist(lapply(pen.vals, function(p) 
  cptfn(data = y_ts, pen = p)))
plot.ts(y_ts,type='l',col='red',
        xlab = "time",
        ylab = " Y(t)",
        main = "Stationary signal (constant mean)")
plot(pen.vals,elbowplotData, 
     xlab = "PELT penalty parameter",
     ylab = " ",
     main = " ")

# run cptfn for the signal with a known change point
elbowplotData <- unlist(lapply(pen.vals, function(p) 
  cptfn(data = y_ts_CP, pen = p)))
plot.ts(y_ts_CP,type='l',col='red',
        xlab = "time",
        ylab = " Y(t)",
        main = "Change in mean signal")
plot(pen.vals,elbowplotData,
     xlab = "PELT penalty parameter",
     ylab = " ",
     main = " ")

penalty.val <- 8 # this value is determined from elbow plots

cptm_stationary <- cpt.mean(y_ts,    penalty='Manual',pen.value=penalty.val,method='PELT') 
cpts_stationary <- cpts(cptm_stationary) # change point time points

cptm_CP         <- cpt.mean(y_ts_CP, penalty='Manual',pen.value=penalty.val,method='PELT') 
cpts_CP         <- cpts(cptm_CP) # change point time points
cpts_CP

plot.new()
frame()
par(mfcol=c(1,2))
plot(cptm_stationary,
     xlab = "time",
     ylab = " Y(t)",
     main = "Change in mean signal")
plot(cptm_CP)
