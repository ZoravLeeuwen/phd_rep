#QA Before data
#Load before data
library(ggplot2)
library(scales)

head(before_data)
ggplot(data=before_data,aes(x=Index,y=LB5U))+geom_line()

#to do on level_data instead of before_data
#before_data<-level_data

#make copy to clean
before_data_cleaned<-before_data


#names of loggers
loggers<-colnames(before_data)
loggers

#work out start and end dates


#nrweeks<-length(na.omit(before_data[,1]))/(7*24*60)
#nrweeks
#####################################################################################################################
#choose number of plots you want to go through, e.g. 4 weekly
timespan<-(4*7*24*60)
timespan_days<-28

##############################################################################################################


#####################################################################
logger_nr<-1################################### Change for the logger
loggers[logger_nr]
#####################################################################
#Plot whole series
ggplot(data=before_data,aes(x=Index,y=before_data[,logger_nr]))+geom_line()

#start date
startdate<-start(na.omit(before_data[,logger_nr])) 
enddate<-end(na.omit(before_data[,logger_nr])) 
duration<-enddate-startdate
duration
nrplots<-as.numeric(duration)/timespan_days
nrplots<-ceiling(nrplots) #round up to nearest integer
nrplots


#set ylim 
ylim<-range(before_data[,logger_nr],na.rm=TRUE)#set y lim to be the same between plots
ylim

#add abline at baseline (maximum density)
#to get index of maximum density
data_density<-density(na.omit(before_data[,logger_nr]))
max_freq_index<-which.max(data_density$y)
#maximum density value, ie baseline
baseline<-data_density$x[max_freq_index]
baseline


for (i in 0:nrplots){ #loop through i from 0 to nrplots

  myfrom<-startdate + (i*timespan*60)
  myfrom
  myto<-startdate + ((i+1)*timespan*60) # times by 60 as done in seconds
  myto 

  data<-window(before_data,start=myfrom,end=myto) #select window

  print(ggplot(data=data,aes(x=Index,y=data[,logger_nr]))+geom_line()+ggtitle(i)+ylim(ylim)+geom_hline(yintercept=baseline,linetype="dashed",color="blue")+scale_x_datetime(date_breaks="4 days")+ylab(loggers[logger_nr])) #plot window 
  
  }

#To clear all plots
#graphics.off()

