proj1<-function(){
    library(date)
    library(reshape2)
    #Project 1 for Reproducible Research
    #


    #Read in the activity data
    #unzip data from http://github.com/bikegeek/RepData_PeerAssessment1 and save
    #to the working directory (same directory as this script)
    df<-read.csv("activity.csv",header=TRUE)
    data<-na.omit(df)
    data$steps <- as.numeric(data$steps)


    melted <- melt(data,id=c(2:3),measure=c(1),na.rm=TRUE)
    #print(head(melted,10))
    sumsteps <- dcast(melted, date~variable,sum)
    print("sum:")
    print(sumsteps)
    meansteps <- dcast(melted,date~variable,mean)
    print("Mean:")
    print(meansteps)
    mediansteps <- dcast(melted,date~variable,sd, na.rm=TRUE)
    print("Median:")
    print(mediansteps)

   total <- merge(sumsteps, meansteps, by ="date")
   total2 <- merge(total, mediansteps, by="date")
   names(total2) <- c("Date", "Total Steps", "Mean Steps", "Median Steps")
   print(total2)

}
