#download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",method="curl","activity.zip")
## cleaning entire environment
#rm(list=ls())
# data readin and adding a timestamp variable
setwd("git//RepData_PeerAssessment1")
data <- read.csv(unz("activity.zip", "activity.csv"))

data$interval<-as.character(data$interval)
for (i in 1:nrow(data))
{
        if(nchar(data$interval[i])<2) 
        {data$interval[i]<-paste("000",data$interval[i],sep="")}
        else if (nchar(data$interval[i])<3)
        {data$interval[i]<-paste("00",data$interval[i],sep="")}
        else if (nchar(data$interval[i])<4)
        {data$interval[i]<-paste("0",data$interval[i],sep="")}      
}
data$time<-paste(data$interval,data$date)
data$time<-strptime(data$time,"%H%M %F")
data$interval<-as.factor(data$interval)
data$date<-as.factor(data$date)


library(plyr) #this package provides function ddply
# create summaries mean, median, sum for daily data with NA removed
daily<-ddply(data,.(date),# ddply creates a data frame from data splited by variable date
                summarise, # uses function summarise
                        mean=mean(steps,na.rm=TRUE),
                        median=median(steps,na.rm=TRUE),
                        sum=sum(steps,na.rm=TRUE)
              )

library(ggplot2)
ggplot(daily)+
        geom_histogram(aes(sum),binwidth=1000)

# create summaries mean, median, sum for interval data with NA removed
perminute<-ddply(data,.(interval),# ddply creates a data frame from data splited by variable date
             summarise, # uses function summarise
             mean=mean(steps,na.rm=TRUE),
             median=median(steps,na.rm=TRUE),
             sum=sum(steps,na.rm=TRUE)
)
#creating time variable from interval
perminute$time<-strptime(perminute$interval,"%H%M")
#converting time variable to chron, in order to plot it reasonably
library(chron)
perminute$time<-times(format(perminute$time, "%H:%M:%S"))
Sys.setenv(TZ='GMT')#necessary to avoid time shift
#ploting the time series of average steps per 5 minute interval
ggplot(perminute)+
        geom_line(aes(x=time,y=mean))+#line
        scale_x_chron(format="%H:%M")#timescale

#location and filling of missing data
#count missing data
count_<-sum(!complete.cases(data$steps))
#find location of missing data
logical_index<-!complete.cases(data$steps)
#create a vector of factors
xt<-data$interval[logical_index]
#obtain appropriate medians
yt<-perminute$median[xt]
#create new data 
data2<-data
#fill it with missing values
data2$steps[logical_index]<-yt

daily2<-ddply(data2,.(date),# ddply creates a data frame from data splited by variable date
             summarise, # uses function summarise
             mean=mean(steps),
             median=median(steps),
             sum=sum(steps) 
)

ggplot(daily2)+
        geom_histogram(aes(sum),binwidth=1000)
daily2$old_sum<-daily$sum
ggplot(daily2)+
        geom_histogram(aes(old_sum),fill="red",binwidth=1000)+
        geom_histogram(aes(sum),fill="black",binwidth=1000)


is.weekend<- function(x){ 
        if (weekdays(x,abbreviate=T)=="sob" ||weekdays(x,abbreviate=T)=="nie")
        {
                return(TRUE)  
        }
        else {
                return(FALSE)
        }
}

for (i in 1:dim(data2)[1])
{
        if (is.weekend(data2$time[i])){
                data2$weekend[i]<-"Weekend"
        }
        else{
                data2$weekend[i]<-"Weekday"
        }
        
}
###
new_data<-data2
new_data$time<-NULL
new_data$date<-NULL
perminute2<-ddply(new_data,.(interval,weekend),summarise, mean=mean(steps))
perminute2$time<-strptime(perminute2$interval,"%H%M")
perminute2$time<-times(format(perminute2$time, "%H:%M:%S"))

perminute2$weekend<-as.factor(perminute2$weekend)
ggplot(perminute2,aes(time,mean))+geom_line()+scale_x_chron(format="%H:%M")+facet_grid(weekend~.)






