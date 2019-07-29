# Of the four types of sources indicated by the 𝚝𝚢𝚙𝚎 (point, nonpoint,
# onroad, nonroad) variable, which of these four sources have seen decreases in
# emissions from 1999–2008 for Baltimore City? Which have seen increases in
# emissions from 1999–2008?

library(ggplot2)
# Test whether data.frames exist in your working env
if(!exists("NEI")) {
    NEI = readRDS('Source_Classification_Code.rds')
}
if(!exists("SCC")) {
    SCC = readRDS('summarySCC_PM25.rds')
}

# Extract observations of baltimore
baltimore = SCC[SCC$fips=='24510',]

# Splite observations into groups
baltimore$yeartype = paste(baltimore$year, baltimore$type,sep = '.')
baltimore.emission = with(baltimore,tapply(Emissions,yeartype,sum))

# Construct dataframe for plot
baltimore.emission = data.frame(baltimore.emission)
yeartype = strsplit(row.names(baltimore.emission),'\\.')
for (i in 1:length(yeartype)) {
    baltimore.emission$year[i] = yeartype[[i]][1]
    baltimore.emission$type[i] = yeartype[[i]][2]
}

# Making the plot
png('plot3.png')
ggplot(baltimore.emission, aes(x = year, y = baltimore.emission, group = type)) + 
    geom_line() +
    facet_wrap(~type, ncol = 2) +
    labs(x = '', y = 'Emission', title = 'Emission for different types in Baltimore')
dev.off()


