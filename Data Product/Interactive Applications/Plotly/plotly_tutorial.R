library(plotly)

# make scatter plot
?plot_ly
plot_ly(data = mtcars, x = ~mpg, y = ~wt, type = "scatter")

  # pay attention to ~ for x,y
plot_ly(data = mtcars, x = ~wt, y = ~mpg, mode = "markers",
        color = ~as.factor(cyl)) # add color attributes

plot_ly(data = mtcars, x = ~wt, y = ~mpg, mode = "markers",
        color = ~disp)

plot_ly(data = mtcars, x = ~wt, y = ~mpg, type = "scatter",
        color = ~factor(cyl), size = ~hp) # add size attribute

 # 3d scatter plot
temp <- rnorm(100, 100, 5)
dtime <- 1:100
pressure <- rnorm(100)
plot_ly(x = ~temp, y = ~pressure, z = ~dtime,
        type = "scatter3d", color = ~temp)

# line graph
data("airmiles")
str(airmiles) # data is time-series object
plot_ly(x = time(airmiles), y = airmiles, mode = "line",
        type = "scatter") # in this situation, ~ is not neccessary?
  # plot multiple lines
library(tidyr)
library(dplyr)
data("EuStockMarkets")
str(EuStockMarkets) # time-series object with more dimensions
stock <- as.data.frame(EuStockMarkets) %>%
  gather(index, price) %>% # transform from short-format to long-format
  mutate(time = rep(time(EuStockMarkets), 4)) # add time-stamp
plot_ly(data = stock, x = ~time, y = ~price, color = ~index,
        type = "scatter", mode = "lines") # line/lines all acceptable?

# histogram
  # only 1 variable is needed
plot_ly(x = ~xvar, type = "historgram")

# boxplot
  # used for comparing different data set
plot_ly(data = iris, y = ~Sepal.Length, color = ~ Species,
        type = "box")

# heatmap
 # used to represent 3d plot in 2d format with color represent 3rd-dim
terrin1 <- matrix(rnorm(100*100), 100, 100)
plot_ly(z = ~terrin1, type = "heatmap")

# 3d-surface
terrin2 <- matrix(sort(rnorm(100*100)), 100, 100)
plot_ly(z = ~ terrin2, type = "surface")

# Choropleth - 分级统计图
  # choropleth illustrates data across geographic areas by shading regions with different colors
state_pop <- data.frame(State = state.abb, Pop = as.vector(state.x77[,1]))
state_pop$hover <- with(state_pop, paste(State, "<br>", 
                                         "Population:", Pop))
borders <- list(toRGB("red"))
map_options <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

plot_ly(z = ~state_pop$Pop, text = ~state_pop$hover, locations = ~state_pop$State, 
        type = 'choropleth', locationmode = 'USA-states', 
        color = state_pop$Pop, colors = 'Blues', marker = list(line = borders)) %>%
  layout(title = 'US Population in 1975', geo = map_options)

# transform from ggplot2 to plotly
d <- diamonds[sample(nrow(diamonds), 1000), ]
p <- ggplot(data = d, aes(x = carat, y = price)) +
  geom_point(aes(text = paste("Clarity:", clarity))) + 
  geom_smooth(aes(color = cut)) + facet_wrap(~cut)

gg <- ggplotly(p)













