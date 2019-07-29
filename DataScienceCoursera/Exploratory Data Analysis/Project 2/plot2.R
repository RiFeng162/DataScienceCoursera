# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
# (𝚏𝚒𝚙𝚜 == "𝟸𝟺𝟻𝟷𝟶") from 1999 to 2008?

# Test whether data.frames exist in your working env
if(!exists("NEI")) {
    NEI = readRDS('Source_Classification_Code.rds')
}
if(!exists("SCC")) {
    SCC = readRDS('summarySCC_PM25.rds')
}

# Extract observations of Baltimore
baltimore = SCC[SCC$fips=='24510',]

# Calculate emissions of each year
emission.baltimore = with(baltimore,tapply(Emissions, year, sum))

png('plot2.png')
plot(x = names(emission.baltimore),emission.baltimore,
     type = "l",
     ylab = "Total Emission",
     xlab = "",
     main = "Total PM2.5 Emission of Baltimore")
text(names(emission.baltimore),emission.baltimore*1.005,labels = names(emission.baltimore))
dev.off()