# How have emissions from motor vehicle sources changed from 1999–2008 in
# Baltimore City?

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

# Extract observations related to motor and Baltimore
bm.emission = subset(SCC, fips == "24510" & SCC %in% motor.scc)

# Calculate emissions for each year, construct dataframe for plotting
plot.df = with(bm.emission, aggregate(Emissions,list(year),FUN=sum))
names(plot.df) = c("Year", "Emission")

png("plot5.png")
with(plot.df,plot(Year,Emission,type = "l",lwd=2))
title("Motor Emission in Baltimore")
dev.off()
