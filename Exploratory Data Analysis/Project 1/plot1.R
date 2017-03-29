
library(sqldf)
library(chron)

# Set the filePath in your own computer 
filePath = "data/household_power_consumption.txt"

if (!exists("dat")) {
    dat = read.csv.sql(filePath,sep = ";", na.strings = "?",
                            sql = "select * from file where Date in ('1/2/2007','2/2/2007')")
    dat$Date = as.Date(totalDat$Date,format = "%d/%m/%Y")
    dat$Time = chron(times = totalDat$Time)
    dat$DateTime = as.POSIXct(paste(dat$Date,dat$Time),
                              format="%Y-%m-%d %H:%M:%S")
}
png("plot1.png")
hist(dat$Global_active_power, col = "red", 
     xlab = "Global Active Power (kilowatts)", 
     main = "Global Active Power")
dev.off()