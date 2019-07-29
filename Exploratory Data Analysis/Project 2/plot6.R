# Compare emissions from motor vehicle sources in Baltimore City with emissions
# from motor vehicle sources in Los Angeles County, California (𝚏𝚒𝚙𝚜 ==
# "𝟶𝟼𝟶𝟹𝟽"). Which city has seen greater changes over time in motor vehicle
# emissions?

# Test whether data.frames exist in your working env
if(!exists("NEI")) {
    NEI = readRDS('Source_Classification_Code.rds')
}
if(!exists("SCC")) {
    SCC = readRDS('summarySCC_PM25.rds')
}

# Extract SCC related to motor vehicle accroding to Short.Name
motor.index = grep("[Mm]otor",NEI$Short.Name)
motor.scc = NEI[motor.index,1]

# Extract observations corresponding to Baltimore and Los Angeles
baltimore = subset(SCC, fips == "24510" & SCC %in% motor.scc)
angeles = subset(SCC, fips == "06037" & SCC %in% motor.scc) 

# Calculate emissions for both cities
plot.baltimore = with(baltimore,aggregate(Emissions,list(year),FUN=sum))
plot.angeles = with(angeles,aggregate(Emissions,list(year),FUN=sum))

# Scale the value for both dataframe
names(plot.baltimore) = c("Year", "Emission")
names(plot.angeles) = c("Year", "Emission")
plot.baltimore$Emission = scale(plot.baltimore$Emission)
plot.angeles$Emission = scale(plot.angeles$Emission)

png("plot6.png")
rng = range(rbind(plot.angeles,plot.baltimore)[,2])

with(plot.baltimore,
     plot(Year,Emission,main = "Emission of Motor Vehicle", 
          type = "n", ylim = rng, ylab = "Emission (Scaled)"))
with(plot.angeles,
     lines(Year,Emission,col="antiquewhite3",lwd=2))
with(plot.baltimore,
     lines(Year,Emission,col="brown1",lwd=2))
legend("bottomright", legend = c("Baltimore", "Los Angeles"), 
       col = c("brown1", "antiquewhite3"),
       lty = c(1,1), lwd = c(2,2))

dev.off()



