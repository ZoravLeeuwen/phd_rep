#DATA QA

#Check level data when downloads were made, remove any artificial levels

#clear variables
rm(list=ls())

##Reading saved data back in
#first update this## - done

#combined_data<-readRDS(file="//foe-data-30/a263/gyzrvl/Documents - Y Drive/Analysis/R/TimeSeriesR/Master_Data/COV/level_data_2018_10_08.Rda", refhook = NULL)
#open from load data script
library(zoo)
combined_data_zoo<-level_data
head(combined_data_zoo,5)

#for rainfall
combined_data_zoo<-rain_15min_mm


###########################List of dates data was downloaded on###########################
#then get list of dates data was downloaded (by listing folders)
DL_dates<-list.dirs("//foe-data-30/a263/gyzrvl/Documents - Y Drive/Field Data/1. Level Data",recursive = "FALSE",full.names="FALSE")
# #For rainfall data
# DL_dates<-list.dirs("//foe-data-30/a263/gyzrvl/Documents - Y Drive/Field Data/3. Rain gauge data",recursive = "FALSE",full.names="FALSE")
# DL_dates
#OR

################################MANUALLY CHANGE!!!!!!!!!!!!!!!!!!!!!!################
DL_dates<-DL_dates[1:35] #change this if more dates!!!!!!!
DL_dates

DL_dates_posixct<-as.POSIXct(DL_dates,format='%Y-%m-%d',tz="GMT")
DL_dates_posixct

#For rainfall, dates from field notebook:
DL_dates_posixct<-as.POSIXct(c("18-01-2017","15-02-2017","28-02-2017","06-07-2017","09-08-2017","01-09-2017","19-10-2017","15-11-2017","13-10-2017","07-12-2017","18-05-2018","28-09-2018","01-10-2018","11-10-2018","04-12-2018","05-12-2018"),format='%d-%m-%Y',tz="GMT") #"18-05-2018" added in from folder list
DL_dates_posixct

sort(DL_dates_posixct)
########################USe these as 'peak' for plotting window############
#convert to zoo
#combined_data_zoo<-read.zoo(combined_data) #rds already is zoo
#need to convert zoo data to dataframe
combined_data<-data.frame(datetime=index(combined_data_zoo),as.data.frame(combined_data_zoo))
head(combined_data)

#combined data, just tribs
#tribs_zoo<-read.zoo(combined_data[,1:10])
#MC_zoo<-read.zoo(combined_data[,c(1,11:13)],tz="GMT")

#list logger names
loggers<-colnames(combined_data_zoo)
loggers
#make a copy to 'clean'
combined_data_zoo_cleaned<-combined_data_zoo

#set up ylim for plotting
#ylim<-range(coredata(tribs_zoo),finite=TRUE)
#ylim_MC<-range(coredata(MC_zoo),finite=TRUE)

#work out start and end dates for week with DL dates in the middle
myfrom<-DL_dates_posixct-60*60*24*1.5
myto<-DL_dates_posixct+60*60*24*1.5


#plot just the time window
nr=8 #choose which dl event #couldnt plot event 5,6,12,13

#events cleaned on 9/10/18: 28,27,26,25,24,23,22,21,20,19,18,17,16,15,14,13,12 #re-do has been throwing errors at removing data stage so will not have been successful in deleting all data.could be useful to identify which loggers dl on which dates to avoid this
#events re-cleaned after solving error on 10/10/18: 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1
#events cleaned on 18/01/19 : 3,4,5,6,7,8,9,10,11,12,13,14:32


DL_dates_posixct[nr]
#plot(window(tribs_zoo,start=myfrom[nr],end=myto[nr]),ylim=ylim)

rowstodelete_from<-numeric()#create empty variable first
rowstodelete_to<-numeric()

#first plot
#attach(as.data.frame(tribs_zoo))
#plot(window(tribs_zoo,start=myfrom[nr],end=myto[nr]),ylim=ylim)

#or plot dataframe
from=match(myfrom[nr],combined_data$datetime)
to=match(myto[nr],combined_data$datetime)

#to get closest date (if required)
library(birk)
from<-which.closest(combined_data$datetime,myfrom[nr])
to<-which.closest(combined_data$datetime,myto[nr])

from
to
#plot(combined_data$datetime[from:to],combined_data$LB5U[from:to],type="h") #plot within the loop

#save coordinates for each plot (must do in right order, ie down the rows of the plot)
#then try in a loop

#just press escape in cases where I don't want to delete any of the data!
i<-1 #for one specific logger
DL_dates_posixct

attach(combined_data) #have to do this on datadrame not zoo object
for (i in 1:(length(combined_data)-1)){
  t<-NULL
  t<-try(plot(combined_data$datetime[from:to],combined_data[from:to,i+1],type="l",main=loggers[i],xaxt="n")) #try to catch any errors
  if (inherits(t,"try-error")) { #if plotting not succesful then set as NA
    dev.off()
    rowstodelete_from[i]<-NA
    rowstodelete_to[i]<-NA
     } else { # otherwise plot x axis, showing whole date then launch coordinate chooser
       r<-as.POSIXct(round(range(combined_data$datetime),"hours")) #plot x axis
       axis.POSIXct(3,at=seq(r[1],r[2],by="day"),format="%b %d") #plot x axis - dates at top
       axis.POSIXct(1,at=seq(r[1],r[2],by="day"),format="%b %d",labels=FALSE) #plot x axis - big dash for dates at bottom
       axis.POSIXct(1,at=seq(r[1],r[2],by="hour"),format="%H:%M",tcl=-0.2) #plot x axis - hours at bottom
       coords<-locator(type="l")#store points and draw lines, ie underline the portion of the data I don't want and save the coordinates in coords  
      if (length(coords)>0){ #if don't select any coordinates...
      rowstodelete_from[i]<-coords$x[1] 
      rowstodelete_to[i]<-coords$x[2]
      }else{
        rowstodelete_from[i]<-NA #...then set to NA
        rowstodelete_to[i]<-NA 
      }
        
    }
  
  
}




#convert to posixct
rowstodelete_from<-as.POSIXct(rowstodelete_from,origin="1970-01-01",tz="GMT")
rowstodelete_to<-as.POSIXct(rowstodelete_to,origin="1970-01-01",tz="GMT")
rowstodelete_from
rowstodelete_to


hours_deleted<-rowstodelete_to-rowstodelete_from #should be approximately 1 day
hours_deleted

##########################################Loop to replace values with 'NA'
#Put in a loop # watch the figures for any errors
#j<-8 #for just one event
for (j in 1:(length(combined_data)-1)){

#####extract range of dates from window
#to_delete<-window(tribs_zoo$LB5U,start=rowstodelete_from[1],end=rowstodelete_to[1])

  #write if statement so skip loggers for which there is no data
  if (is.na(rowstodelete_from[j])){
   print(paste("no data for these dates for",loggers[j]))
    }else{
      to_delete<-window(combined_data_zoo[,j],start=rowstodelete_from[j],end=rowstodelete_to[j])
      #plot(to_delete)
      dates_to_delete<-index(to_delete)
      
      #then use this to index the combined data
      #check it really is the same data
      plot(window(combined_data_zoo[,j],start=myfrom[nr],end=myto[nr]),main=loggers[j])
      lines(combined_data_zoo[dates_to_delete,j],col="red") #not selecting the right dates :(
      
      #then replace values in tribs_zoo with "NA"
      
      # replace window on copy 
      combined_data_zoo_cleaned[dates_to_delete,j]<-NA
      #check by plotting
      lines(window(combined_data_zoo_cleaned[,j],start=myfrom[nr],end=myto[nr]),col="blue")
    }
}

#save the cleaned data every now and again
##Saving the data as R variable
today<-format(Sys.time(),"%Y_%m_%d_%H_%M_%S")
today

saveRDS(combined_data_zoo_cleaned, file=paste("//foe-data-30/a263/gyzrvl/Documents - Y Drive/Analysis/R/TimeSeriesR/Master_Data/COV/level_data_",today,"_cleaned.Rda",sep=""))
#for rainfall
saveRDS(combined_data_zoo_cleaned, file=paste("//foe-data-30/a263/gyzrvl/Documents - Y Drive/Analysis/R/TimeSeriesR/Master_Data/COV/rainfall_data_",today,"_cleaned.Rda",sep=""))

##Reading saved data back in
combined_data_zoo_cleaned<-readRDS(file="//foe-data-30/a263/gyzrvl/Documents - Y Drive/Analysis/R/TimeSeriesR/Master_Data/COV/level_data_2018_10_08_cleaned.Rda", refhook = NULL)
head(combined_data_zoo_cleaned,5)
tail(combined_data_zoo_cleaned,5)
#to check try plotting cleaned data over original data
plot(combined_data_zoo$LB5U,col="red")
lines(combined_data_zoo_cleaned$LB5U,col="black")

#Will need to write a way to append new data to the cleaned data rather than having to clean it all over again each time.

