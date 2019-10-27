
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

png("plot4.png")
par("mfrow"=c(2,2))
# Plot1
plot(dat$DateTime,dat$Global_active_power,type="n",
     ylab="Global Active Power",xlab="")
lines(dat$DateTime,dat$Global_active_power)
# Plot2
with(dat,plot(DateTime,Voltage,type="n"))
lines(dat$DateTime,dat$Voltage)
# Plot3
plot(dat$DateTime,dat$Sub_metering_1,type="n",
     ylab="Energy sub metering",xlab="")
lines(dat$DateTime,dat$Sub_metering_1,col="black")
lines(dat$DateTime,dat$Sub_metering_2,col="red")
lines(dat$DateTime,dat$Sub_metering_3,col="blue")
legend("topright",lty=c(1,1,1),
       col=c("black","red","blue"),
       legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
# Plot4
plot(dat$DateTime,dat$Global_reactive_power,type="n",
     ylab="Global Reactive Power",xlab="")
lines(dat$DateTime,dat$Global_reactive_power)
dev.off()