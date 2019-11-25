# Plotly tutorial
# link:https://plotly-r.com/introduction.html

library(plotly)

  # load the data
data(diamonds, package = "ggplot2")
diamonds
plot_ly(diamonds, x = ~cut) # the y-axis is frequency
plot_ly(diamonds, x = ~cut, y = ~clarity)
plot_ly(diamonds, 
        x = ~cut, 
        color = ~clarity,  # define how data values map to visual properties
        colors = "Accent") # plural form: define the range of a visual property

# create plotly directly
diamonds %>%
  plot_ly(x = ~cut) %>% # create a plotly-object
  add_histogram() %>% # specify the layer explicitly 
  group_by(cut) %>%
  summarise(n = n()) %>% # manipulate the data underlying plotly object
  add_text(text = ~scales::comma(n),
           y = ~n,
           textposition = "top middle",
           cliponaxis = FALSE) %>%
  plotly_data() # access underlying data

# create plotly transformed from ggplot2
p <- ggplot(diamonds, aes(x = log(carat), y = log(price))) + 
  geom_hex(bins = 100)

p <- ggplot(diamonds, aes(x = log(price), color = clarity)) + 
  geom_freqpoly() +
  facet_wrap(~cut)

m <- lm(log(price) ~ log(carat), data = diamonds)
diamonds <- modelr::add_residuals(diamonds, m)  # column resid is the price removing the influence of carat
p <- ggplot(diamonds, aes(x = clarity, y = resid, color = clarity)) + 
  ggforce::geom_sina(alpha = 0.1) +   # "::" load the package but not attach
  stat_summary(fun.data = "mean_cl_boot", color = "black") + 
  facet_wrap(~cut)  

p
ggplotly(p)
toWebGL(ggplotly(p))  # WebGL is more efficient at rendering lots of points

# one geometry per group
library(lubridate)
econ <- economics %>%
  mutate(yr = year(date), mnth = month(date))
econ %>%
  group_by(yr) %>%
  plot_ly(x = ~mnth, y = ~uempmed) %>%
  add_lines(text = ~yr) # one trace

plot_ly(econ, x = ~mnth, y = ~uempmed) %>%
  add_lines(color = ~ordered(yr))   # multiple traces

plot_ly(econ, x = ~mnth, y = ~uempmed) %>%
  add_lines(split = ~yr, color = I("black"))

# 
p <- plot_ly(diamonds, x = ~cut, color = ~clarity, colors = "Accent")
plotly_json(p)  # show the underlying JSON syntax of plotly
  # it can be found, under "data", there are 8 elements (traces),
  # under each elements, there are "type" info and other attributes.
schema()  # show attributes for each trace type

subplot(plot_ly(mpg, x = ~cty, y = ~hwy, name = "default"),
        plot_ly(mpg, x = ~cty, y = ~hwy) %>%
          add_markers(alpha = 0.2, name = "alpha"))   # use alpha to solve overplotting

###### add_markers ######
p <- plot_ly(mpg, x = ~cty, y = ~hwy, alpha = 0.5)
subplot(add_markers(p, color = ~cyl, showlegend = FALSE),
        add_markers(p, color = ~cyl, showlegend = FALSE) %>%
          colorbar(title = "Viridis"),
        add_markers(p, color = ~factor(cyl)))
  # color
  # symbol
  # stroke and span
  # size 
  # dotplots and error bars 
    # dotplot is similar to scatter plots while one variable is categorical
m <- lm(Sepal.Length ~ Sepal.Width * Petal.Length * Petal.Width, 
        data = iris)
broom::tidy(m) %>%  # format coefficients in a data frame
  mutate(term = forcats::fct_reorder(term, estimate)) %>%
  plot_ly(x = ~estimate, y = ~term) %>%
  add_markers(error_x = ~list(value = std.error),
              color = I("black"), # I() means "as is", not mapping
              hoverinfo = "x"
              )

###### Lines ######
 # including add_lines(), add_paths(), add_segments()










