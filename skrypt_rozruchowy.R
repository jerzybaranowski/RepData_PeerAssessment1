download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",method="curl","activity.zip")
data <- read.csv(unz("activity.zip", "activity.csv"))
data$interval<-as.char(data$interval)
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

sum_steps<-aggregate(data$steps,list(data$date),sum,na.rm=TRUE)
mean_steps<-aggregate(data$steps,list(data$date),mean,na.rm=TRUE)
median_steps<-aggregate(data$steps,list(data$date),median,na.rm=TRUE)