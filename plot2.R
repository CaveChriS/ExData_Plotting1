plot2 <- function() {
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
        png(filename="plot2.png",bg="transparent")
        
        ## draw appropriate graph
        with(thisTableDateRowsWanted, plot(as.POSIXlt(DateandTime),Global_active_power,type="l",xlab="",xact="n",ylab="Global Active Power (kilowatts)"))
        
        ## close graphics device
        dev.off()
        
}