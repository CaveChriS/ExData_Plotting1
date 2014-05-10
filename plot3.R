plot3 <- function() {
        ## read in data file
        thisTable <- read.table(file="household_power_consumption.txt", header=TRUE,sep=";",colClasses="character")
        
        ## transform date column to date type
        thisTable <- transform(thisTable, Date=as.Date(thisTable$Date,format="%d/%m/%Y"))
        
        ## transform time column to POSIX time type
        thisTable <- transform(thisTable, Time=strptime(thisTable$Time, format="%H:%M:%S"))
        
        ## get a subset of the rows based on date
        thisTableDateRowsWanted <- subset(thisTable, thisTable$Date == "2007-02-01" | thisTable$Date == "2007-02-02")
        
        ## add the full date time as desired
        thisTableDateRowsWanted <- cbind(thisTableDateRowsWanted, paste(thisTableDateRowsWanted$Date, format(thisTableDateRowsWanted$Time, "%H:%M:%S")))
        names(thisTableDateRowsWanted)[10] <- "DateandTime"
        
        ## add the day of the week
        thisTableDateRowsWanted <- cbind(thisTableDateRowsWanted, weekdays(thisTableDateRowsWanted$Date))
        names(thisTableDateRowsWanted)[11] <- "DayOfWeek"
        
        ## open graphics device to write to
        png(filename="plot3.png",bg="transparent")
        
        ## draw the sub metering 1 line and create base graph
        plot(as.POSIXlt(thisTableDateRowsWanted$DateandTime),thisTableDateRowsWanted$Sub_metering_1,type="l",xlab="",ylab="Energy sub metering",col="black")

        ## draw sub metering 2 line
        lines(as.POSIXlt(thisTableDateRowsWanted$DateandTime),as.numeric(thisTableDateRowsWanted$Sub_metering_2),type="l",col="red")
        
        ## draw sub metering 3 line
        lines(as.POSIXlt(thisTableDateRowsWanted$DateandTime),as.numeric(thisTableDateRowsWanted$Sub_metering_3),type="l",col="blue")
        
        ## add legend
        legend("topright", col=c("black","red","blue"),lty=1,legend=c("Sub_metering_1 ","Sub_metering_2 ", "Sub_metering_3 "), cex=0.95)
        
        ## close graphics device
        dev.off()
        
}