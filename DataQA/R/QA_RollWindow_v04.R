#DATA QA

#Check level data by plotting it every x number of days.Make a QA note and remove any incorrect levels

#clear variables
rm(list=ls())


#Load level_data from load data script - make sure you have the version you want to update (ie latest QA'd data)
library(zoo)
combined_data_zoo<-level_data
head(combined_data_zoo,5)

#make sure the data is stored as numeric, not character
mode(combined_data_zoo)<-"numeric"
head(combined_data_zoo)
#make a copy to QA
combined_data_zoo_QA<-combined_data_zoo
#combined_data_zoo_QA$QAnote<-NA
#head(combined_data_zoo_QA)
#make a copy to 'clean'
#or (need to decide which one to keep)
combined_data_zoo_cleaned<-combined_data_zoo

#################################################
#to keep updating the same data after first time
combined_data_zoo<-combined_data_zoo_cleaned
mode(combined_data_zoo)<-"numeric"
#################################################

#Convert zoo to dataframe for using the interactive plotting tool
combined_data<-data.frame(datetime=index(combined_data_zoo),as.data.frame(combined_data_zoo))
head(combined_data)


#list logger names
loggers<-colnames(combined_data_zoo)
loggers

############################################################################################################################################
#Step 1: cycle through x time steps and plot data, click to save points (always click twice, once for start of range, once for end of range)

#####################################START AND END DATES############################

#choose number of plots you want to go through, e.g. 4 weekly
timespan_days<-50
timespan<-(timespan_days*24*60)

#####################################################################
#Choose which logger
loggers
logger_nr<-9################################### Change for the logger
loggers[logger_nr]

#plot whole dataset
plot(combined_data$datetime,combined_data[,(logger_nr+1)],type="h")

#####################################################################
#work out number of plots
#start date
startdate<-start(na.omit(combined_data_zoo[,logger_nr])) 
enddate<-end(na.omit(combined_data_zoo[,logger_nr])) 
startdate
enddate
duration<-enddate-startdate
duration
nrplots<-as.numeric(duration)/timespan_days
nrplots<-ceiling(nrplots) #round up to nearest integer
nrplots
######################################################################################################
#Want to keep y axis constant, and plot a dashed line showing baseflow level
#Set ylim and line for baseflow
#set ylim 
ylim<-range(combined_data_zoo[,logger_nr],na.rm=TRUE)#set y lim to be the same between plots
ylim

#add abline at baseline (maximum density)
#to get index of maximum density
data_density<-density(na.omit(combined_data_zoo[,logger_nr]))
max_freq_index<-which.max(data_density$y)
#maximum density value, ie baseline
baseline<-data_density$x[max_freq_index]
baseline

########################################################################################################
#set up empty variables to save clicks in
rowstodelete_from<-numeric()#create empty variable first
rowstodelete_to<-numeric()

#Loop for plotting
#click first date of range, then second date of range (ie left to right)
#just press escape in cases where I don't want to delete any of the data!
i<-0 #for one specific event

par(mfrow=c(1,1))

attach(combined_data) #have to do this on datadrame not zoo object
for (i in 0:nrplots){
#for (i in 0:10){ #just for testing

 
   from<-(startdate + (i*timespan*60))
  from
  from_id<-match(from,combined_data$datetime)
 
  to<-startdate + ((i+1)*timespan*60) # times by 60 as done in seconds
  to 
  to_id<-match(to,combined_data$datetime)
  #if to is later than last date of data set it to last date of data
  if (to>enddate){
    to_id<-match(enddate,combined_data$datetime)}
  
  t<-NULL
  t<-try(plot(combined_data$datetime[from_id:to_id],combined_data[from_id:to_id,(logger_nr+1)],type="l",main=loggers[logger_nr],ylim=ylim)) #try to catch any errors
  if (inherits(t,"try-error")) { #if plotting not succesful then set as NA
    dev.off()
    rowstodelete_from[i+1]<-NA
    rowstodelete_to[i+1]<-NA
     } else { #otherwise launch coordinate chooser
      abline(h=baseline,col="blue",lty=2)
       coords<-locator(type="l")#store points and draw lines, ie underline the portion of the data I don't want and save the coordinates in coords  
      if (length(coords)>0){ #if don't select any coordinates...
      rowstodelete_from[i+1]<-coords$x[1] 
      rowstodelete_to[i+1]<-coords$x[2]
       #new row
      }else{
        rowstodelete_from[i+1]<-NA #...then set to NA
        rowstodelete_to[i+1]<-NA 
      }
        
    }
  
  
}

#if wont get it within loop then
#rowstodelete_from[i]<-coords$x[1] 
#rowstodelete_to[i]<-coords$x[2]
#OR if already converted to posixct
#rowstodelete_from[i+1]<-as.POSIXct(coords$x[1] ,origin="1970-01-01",tz="GMT")
#rowstodelete_to[i+1]<-as.POSIXct(coords$x[2],origin="1970-01-01",tz="GMT")

#convert to posixct
rowstodelete_from<-as.POSIXct(rowstodelete_from,origin="1970-01-01",tz="GMT")
rowstodelete_to<-as.POSIXct(rowstodelete_to,origin="1970-01-01",tz="GMT")
rowstodelete_from
rowstodelete_to

#cheat if won't read in the last date
#rowstodelete_to[17]<-enddate



hours_deleted<-rowstodelete_to-rowstodelete_from #should be approximately 1 day
hours_deleted
#############################################################################################################################################
#########Create a variable for each logger which stores the dates from clicks
#print/store the dates to be plotted

inspect_dates_from<-na.omit(rowstodelete_from)
inspect_dates_to<-na.omit(rowstodelete_to)
# 
# assign(paste("inspect_dates_from_",loggers[logger_nr],sep=""),na.omit(rowstodelete_from)) #create variable name using logger
# assign(paste("inspect_dates_to_",loggers[logger_nr],sep=""),na.omit(rowstodelete_to))
# 
# print(paste("inspect_dates_from_",loggers[logger_nr],sep=""))
# print(paste("inspect_dates_to_",loggers[logger_nr],sep=""))

inspect_dates_from
inspect_dates_to

################################################################################################################################################
#Step 2: Make a QA note in the data and replace erroneous values with NA
################################################ Do something with dates flagged up ###########################
#from the QA analysis (should have saved variables)
# inspect_dates_from_LB5U<-as.POSIXct(c("2018-02-05 09:09:21","2018-02-23 00:51:19","2018-06-10 06:31:10","2018-06-20 00:23:46","2018-07-31 20:47:24","2018-09-26 03:29:30","2018-10-07 13:40:49","2018-10-15 14:44:05"),tz="GMT")
# inspect_dates_to_LB5U<-as.POSIXct(c( "2018-02-05 22:56:00","2018-02-23 14:07:21","2018-06-13 06:58:43","2018-06-22 07:30:21","2018-08-02 07:29:20","2018-09-29 04:58:17","2018-10-08 20:17:49","2018-10-16 12:09:59"),tz="GMT")
# 
# inspect_dates_from_LB5M<-as.POSIXct("2018-09-26 21:45:46",tz="GMT")
# inspect_dates_to_LB5M<-as.POSIXct("2018-09-29 07:25:27",tz="GMT")
# 
# inspect_dates_from_LB5L<-as.POSIXct(c("2017-06-05 10:00:22","2017-12-11 15:07:34","2018-02-13 17:46:46"),tz="GMT")
# inspect_dates_to_LB5L<-as.POSIXct(c("2017-06-05 15:37:09","2017-12-12 16:38:23","2018-02-14 18:46:59"),tz="GMT")
# 
# inspect_dates_from_LB7U<-as.POSIXct(c("2017-10-06 04:57:16","2017-11-24 02:55:49","2017-12-05 04:26:39","2018-06-12 22:19:15","2018-06-18 05:02:22"),tz="GMT")
# inspect_dates_to_LB7U<-as.POSIXct(c("2017-10-06 17:42:41","2017-11-24 21:48:38","2017-12-06 00:20:42","2018-06-13 22:48:51","2018-06-19 03:29:30"),tz="GMT")
# 
# inspect_dates_from_LB7L<-as.POSIXct(c( "2018-03-17 10:04:15","2018-05-04 08:34:26","2018-06-12 21:51:29"),tz="GMT")
# inspect_dates_to_LB7L<-as.POSIXct(c( "2018-03-18 08:00:45","2018-05-05 04:59:06","2018-06-13 22:51:42"),tz="GMT")
# 
# inspect_dates_from_LB7L<-as.POSIXct(c( "2018-03-17 10:04:15","2018-05-04 08:34:26","2018-06-12 21:51:29"),tz="GMT")
# inspect_dates_to_LB7L<-as.POSIXct(c( "2018-03-18 08:00:45","2018-05-05 04:59:06","2018-06-13 22:51:42"),tz="GMT")
# 
# inspect_dates_from_LB8U<-as.POSIXct("2017-06-30 10:05:29",tz="GMT")
# inspect_dates_to_LB8U<-as.POSIXct("2017-06-30 18:45:58",tz="GMT")
# 
# inspect_dates_from_MCFU<-as.POSIXct("2018-06-10 09:39:58",tz="GMT")
# inspect_dates_to_MCFU<-as.POSIXct("2018-06-11 11:10:48",tz="GMT")
# 
# inspect_dates_from_MCU<-as.POSIXct("2017-12-08 18:41:40",tz="GMT")
# inspect_dates_to_MCU<-as.POSIXct("2017-12-09 17:08:48",tz="GMT")



#####Plot flagged up dates
#Need to manually change logger!
#####################################################################
#Choose which logger
#loggers
#logger_nr<-1################################### Change for the logger
#loggers[logger_nr]
#####################################################################
#Same logger as plotted above
i=2 #Choose a date window to plot 
library(lubridate)
from<-round_date(inspect_dates_from[i],unit="minute") #round to nearest minute
to<-round_date(inspect_dates_to[i],unit="minute")

from
to
#Extend plotting window (if required)

from_ext<-from-5*24*60*60 
to_ext<-to+5*24*60*60
from_ext

#plot
library(ggplot2)
autoplot.zoo(window(combined_data_zoo[,logger_nr],start=from,end=to),facets=NULL)+ggtitle(loggers[logger_nr])+ylab("Stage (m)")

#plot extended time window
autoplot.zoo(window(combined_data_zoo[,logger_nr],start=from_ext,end=to_ext),facets=NULL)+ggtitle(loggers[logger_nr])
#plot extended time window with other loggers
autoplot.zoo(window(combined_data_zoo,start=from_ext,end=to_ext))+ggtitle(loggers[logger_nr])
autoplot.zoo(window(combined_data_zoo,start=from_ext,end=to_ext),facets=NULL)+ggtitle(loggers[logger_nr])


#plot so can clicker dates
attach(combined_data) #have to do this on datadrame not zoo object
from_id<-match(from_ext,combined_data$datetime)
to_id<-match(to_ext,combined_data$datetime)
#if to is later than last date of data set it to last date of data
if (to_ext>enddate){
  to_id<-match(enddate,combined_data$datetime)}  
plot(combined_data$datetime[from_id:to_id],combined_data[from_id:to_id,(logger_nr+1)],type="l",main=loggers[logger_nr])
coords<-locator(type="l")#store points and draw lines, ie underline the portion of the data I don't want and save the coordinates in coords  

###############################
#First time only
#create a dataframe with delete from, delete to, and a notes column

#QA_dates<-data.frame(matrix(ncol=5,nrow=0),stringsAsFactors = FALSE)
#colnames(QA_dates)<-c("date_from","date_to","delete","QA_note","logger")
#QA_dates
##############################

#Then
QA_dates<-rbind(QA_dates,data.frame(date_from=coords$x[ !c(TRUE,FALSE) ],date_to=coords$x[ c(TRUE,FALSE) ],delete="yes",QA_note="data download",logger=loggers[logger_nr],stringsAsFactors=FALSE)) #the TRUE FALSE bit recursively selects odd, and then even rows of coords$x, which is the start and the end of each bit for deletion.

#Manually change notes for rows if required
#NOTE - need to manually change the QA note and delete yes/no if not same for each date period in coords.
#e.g.
#QA_dates$delete[2]<-"yes"
#QA_dates$QA_note[6]<-"Sudden 0.04m jump in level after short data gap"
QA_dates

#QA_dates<-QA_dates[1,] # i fmade a mistake can subset data to delete rows: myData[-c(2, 4, 6), ] woudl delte rows 2, 4 and 6.

#Repeat for each listed pair of dates, but do not create the dataframe again!!

#########################################################################
#########################################################################
#Merge QA note into zoo object, one for each logger
data_QA<-combined_data_zoo[,logger_nr]
head(data_QA)

#Convert to POSIXct and round to nearest minute
QA_dates$date_from<-round_date(as.POSIXct(QA_dates$date_from,origin="1970-01-01 00:00:00",tz="GMT"),unit="minute") 
QA_dates$date_to<-round_date(as.POSIXct(QA_dates$date_to,origin="1970-01-01 00:00:00",tz="GMT"),unit="minute")
QA_dates

#################################
#TO MANUALLY CHANGE QA_dates
#QA_dates$date_from<-as.POSIXct("2019-03-16 16:53:00",tz="GMT")
#QA_dates$date_to<-as.POSIXct("2019-03-16 16:55:00",tz="GMT")
################################

#save
today<-format(Sys.time(),"%Y_%m_%d_%H_%M_%S")
today
saveRDS(QA_dates, file=paste("//foe-data-30/a263/gyzrvl/Documents - Y Drive/Analysis/R/TimeSeriesR/QA/QA_notes_",loggers[logger_nr],"_",today,".Rda", sep=""))
#read
#QA_dates<-readRDS(file=paste("//foe-data-30/a263/gyzrvl/Documents - Y Drive/Analysis/R/TimeSeriesR/QA/QA_notes_",loggers[logger_nr],"_2019_01_22.Rda", sep=""),refhook=NULL)
QA_dates
#Create a zoo object to merge
#create date range
i<-2 #event
dates<-seq(from=QA_dates$date_to[i],to=QA_dates$date_from[i],by="mins") #swap date_from and date_to round of not working
head(dates)

#OR manually
#dates<-seq(from=as.POSIXct("2019-03-27 12:27:00"),to=as.POSIXct("2019-03-27 12:34:00"),by="mins")

#crate zoo object using date range
QA_data<-zoo(x=QA_dates[i,3:4],order.by=dates) #note and delete columns
head(QA_data,20)

#merge with data for LB5U
#first time
    data_QA<-merge(level_data[,logger_nr],QA_data,all=TRUE,fill=NA)
    data_QA
    data_QA_copy<-data_QA #backup

#set delete column to same as zoo object just created
data_QA$delete[dates]<-QA_data$delete
#set QA note column to same as zoo object just created
data_QA$QA_note[dates]<-QA_data$QA_note
#check
data_QA[dates]

#and delete levels if appropriate
if (QA_dates[i,3]=="yes"){
print(head(combined_data_zoo_cleaned[dates,logger_nr])) #before
combined_data_zoo_cleaned[dates,logger_nr]<-"NA"
print(head(combined_data_zoo_cleaned[dates,logger_nr]))} #after

#save data with QA note
today<-format(Sys.time(),"%Y_%m_%d_%H_%M_%S")
today
saveRDS(data_QA, file=paste("//foe-data-30/a263/gyzrvl/Documents - Y Drive/Analysis/R/TimeSeriesR/QA/Data_with_QANote_",loggers[logger_nr],"_",today,".Rda", sep=""))
#today<-"2019_01_22_12_11_00" #to read this particular file
#LB5U_data_QA<-readRDS(file=paste("//foe-data-30/a263/gyzrvl/Documents - Y Drive/Analysis/R/TimeSeriesR/QA/Data_with_QANote_",loggers[logger_nr],"_",today,".Rda", sep=""),refhook=NULL)

#save data with incorrect levels replaced with NA
saveRDS(combined_data_zoo_cleaned, file=paste("//foe-data-30/a263/gyzrvl/Documents - Y Drive/Analysis/R/TimeSeriesR/QA/CleanedData_",loggers[logger_nr],"_",today,".Rda", sep=""))
#file
paste("//foe-data-30/a263/gyzrvl/Documents - Y Drive/Analysis/R/TimeSeriesR/QA/CleanedData_",loggers[logger_nr],"_",today,".Rda", sep="")

#plot over top of each other to see deleted data in red
plot(na.omit(combined_data_zoo[,logger_nr]),col="red")
lines(na.omit(combined_data_zoo_cleaned[,logger_nr]),col="black")
