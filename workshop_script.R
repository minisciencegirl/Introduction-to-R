# Introduction to R with Amy and Travis
# 2019-09-20

library(tidyverse)

gapminder_data <- read.csv("data/gapminder_data.csv")

head(gapminder_data)

gapminder_select <- select(gapminder_data, country, continent, year)

gapminder_2 <- gapminder_data %>%
  select(country, continent, year)

x <- ...


# filter for country being Canada
gapminder_canada <- gapminder_data %>%
  filter(country == "Canada")

# filter for life expectancy above 50
gapminder_le <- gapminder_data %>%
  filter(lifeExp >= 50)

# filter for life expectancy > 50 and < 10
gapminder_or <- gapminder_data %>%
  filter(lifeExp < 10 | lifeExp > 50)

#filter on multiple columns
gapminder_or <- gapminder_data %>%
  filter(lifeExp < 70 & gdpPercap > 1000)


# formula for ddp in billions
# gdpPercap * pop / 10^9

gapminder_bil <- gapminder_data %>%
  mutate(gdpBil = gdpPercap * pop / 10^9)

# piping from functions
gapminder_pipe <- gapminder_data %>%
  select(country, continent) %>%
  filter(continent == "Asia")


# average life expectancy for each country
gapminder_avg <- gapminder_data %>%
  group_by(country) %>%
  summarise(avg_lifeExp = mean(lifeExp))

# another group_by and summarise example
gapminder_avgGDP <- gapminder_data %>%
  group_by(country) %>%
  summarise(avgGDP = mean(gdpPercap), sdGDP = sd(gdpPercap)) %>%
  filter(avgGDP > 1000)


# Combine tables with left_join()



