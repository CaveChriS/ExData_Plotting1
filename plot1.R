plot1 <- function() {
        ## read in data file
        thisTable <- read.table(file="household_power_consumption.txt", header=TRUE,sep=";",colClasses="character")
        
        ## transform date column to date type
        thisTable <- transform(thisTable, Date=as.Date(thisTable$Date,format="%d/%m/%Y"))
        
        ## transform time column to POSIX time type
        thisTable <- transform(thisTable, Time=strptime(thisTable$Time, format="%H:%M:%S"))
        
        ## get a subset of the rows based on date
        thisTableDateRowsWanted <- subset(thisTable, thisTable$Date == "2007-02-01" | thisTable$Date == "2007-02-02")
        
        ## open graphics device to write to
        png(filename="plot1.png",bg="transparent")
        
        ## run the histogram (part of the base plotting system)
        hist(as.numeric(thisTableDateRowsWanted$Global_active_power), col="red", xlab="Global Active Power (kilowatts)", main="Global Active Power")
        
        ## shut off device
        dev.off()
}