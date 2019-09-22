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
fruits_1 <- read.csv("data/fruits_table1.csv")
fruits_2 <- read.csv("data/fruits_table2.csv")

fruits_join <- left_join(fruits_1, fruits_2, by = "FruitID")


# tidyr section -----------------------------------------------------------

fship <- read_csv("data/The_Fellowship_Of_The_Ring.csv")

ttow <- read_csv("data/The_Two_Towers.csv")

rking <- read_csv("data/The_Return_Of_The_King.csv")

# Combine the three dataframe together using dplyr function bind_rows
lotr_untidy <- dplyr::bind_rows(fship, ttow, rking)

# Inspect your data
head(lotr_untidy)
tail(lotr_untidy)
str(lotr_untidy)
View(lotr_untidy)

lotr_tidy <- lotr_untidy %>%
  gather(key = "Gender", value = "Words", Female, Male)

lotr_untidy <- lotr_tidy %>%
  spread(key = "Gender", value = "Words")

storms <- read_csv("data/hurricanes.csv")

storms.sep <- storms %>%
  separate(date, c("year", "month", "day"), sep = "-") %>%
  View()


# plotting with ggplot2
ggplot(gapminder_data, aes(gdpPercap, lifeExp)) +
  geom_point()

ggplot(gapminder_data, aes(year, lifeExp, colour = continent)) +
  geom_point()

# making a line plot
ggplot(gapminder_data, aes(year, lifeExp, colour = continent, by = country)) +
  geom_line()

# adding points to the line plot
ggplot(gapminder_data, aes(year, lifeExp, colour = continent, by = country)) +
  geom_line() +
  geom_point()


# colouring the points all black
ggplot(gapminder_data, aes(year, lifeExp, colour = continent, by = country)) +
  geom_point(colour = "black") +
  geom_line()

# making gdp plot
ggplot(gapminder_data, aes(gdpPercap, lifeExp, colour = continent)) +
  geom_point()

# scaling and setting alpha
ggplot(gapminder_data, aes(gdpPercap, lifeExp, colour = continent)) +
  geom_point(alpha = 0.5) +
  scale_x_log10()


ggplot(gapminder_data, aes(gdpPercap, lifeExp)) +
  geom_point(alpha = 0.5, colour = "blue") +
  scale_x_log10()


# smooth line of best fit
ggplot(gapminder_data, aes(gdpPercap, lifeExp)) +
  geom_point() +
  scale_x_log10() +
  geom_smooth(method = "lm")


# making a facet plot
gapminder_small <- gapminder_data %>%
  filter(country %in% c("Canada", "United States", "France", "Australia"))

ggplot(gapminder_small, aes(year, lifeExp, colour = continent)) +
  geom_line() +
  facet_wrap(~country)

# tweaking the appearance
ggplot(gapminder_small, aes(year, lifeExp, colour = continent)) +
  geom_line() +
  facet_wrap(~country) +
  labs(title = "Figure 1", x = "Year", y = "Life Expectancy", colour = "Continent") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


ggplot(gapminder_small, aes(year, lifeExp, colour = continent)) +
  geom_line() +
  facet_wrap(~country) +
  labs(title = "Figure 1", x = "Year", y = "Life Expectancy", colour = "Continent") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, color = "red"),
        axis.ticks.x = element_blank())

ggsave(filename = "facet_lifeExp_year.png", scale = 2)

write_csv()

