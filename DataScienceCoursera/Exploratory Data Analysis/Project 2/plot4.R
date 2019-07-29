# Across the United States, how have emissions from coal combustion-related
# sources changed from 1999–2008?

# Test whether data.frames exist in your working env
if(!exists("NEI")) {
    NEI = readRDS('Source_Classification_Code.rds')
}
if(!exists("SCC")) {
    SCC = readRDS('summarySCC_PM25.rds')
}

# Choose EI.Sector to find coal combustion-related source
coal.index = grep("Comb(ustion)?.*Coal",NEI$EI.Sector)
coal.source = NEI[coal.index,1]

# Extract observations related to coal combustion
coal.emission = subset(SCC, SCC %in% coal.source)

# Calculate emissions for each year, construct dataframe for plotting
plot.df = with(coal.emission,aggregate(Emissions,list(year),FUN=sum))
names(plot.df) = c("Year", "Emission")

png("plot4.png")
with(plot.df,plot(Year,Emission,type = "l"))
title("Coal Combustion-related Emission")
dev.off()
