# make a plot showing the total PM2.5 emission from all sources for each of the
# years 1999, 2002, 2005, and 2008

# Test whether data.frames exist in your working env
if(!exists("NEI")) {
    NEI = readRDS('Source_Classification_Code.rds')
}
if(!exists("SCC")) {
    SCC = readRDS('summarySCC_PM25.rds')
}

# Calculate emissions for each year
emission = with(SCC,tapply(Emissions, year, sum))

png('plot1.png')
plot(x = names(emission),emission,
     type = "l",
     ylab = "Total Emission",
     xlab = "",
     main = "Total PM2.5 Emission Per Year")
text(names(emission),emission*1.005,labels = names(emission))
dev.off()