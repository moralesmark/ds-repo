# libraries ----
library(tidyverse)

# data ----
url <- 'https://raw.githubusercontent.com/OHI-Science/data-science-training/master/data/gapminder.csv'
gapminder <- read_csv(url) # View(gapminder)

# ggplot: after filter by country ----
country_plot <- function(cntry, folder = "."){

#cntry <- "Zambia"
png <- paste0("gdp", cntry, ".png")
cat("country_plot(", cntry, ") -> ", png, "\n")

g <- gapminder %>%
  filter(country == cntry) %>%
  ggplot(aes(x = year, y = gdpPercap)) +
  geom_point() +
  geom_smooth() +
  labs(title = cntry)

ggsave(file.path(folder, png), g)

}

### check out these function
### message, cat, error, sprintf  

### Let's create a folder called developed
### to duplicate a line in a mac, option + cmd + down arrow
dir.create("developed")
dir.create("developing")

### now we want to seperate depending on whether the country is developed or not
# ggplot: after filter by country ----
is_developed <- function(cntry, threshold = 12000){
  gapminder %>%
    filter(country == cntry) %>%
    summarize(
      mean_gdp = mean(gdpPercap)) %>%
    .$mean_gdp >= threshold
}


for(i in unique(gapminder$country)) {
  
  if(is_developed(i)){
    country_plot(i, "developed")
      
    } else {
      country_plot(i, "developing")
  }
}

