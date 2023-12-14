library(tidyverse)
gapminder_data <- read.csv("gapminder_data.csv")
gapminder_data <- read_csv("gapminder_data.csv")
summarize(gapminder_data, averageLifeExp = mean(lifeExp))
gapminder_data %>% summarize(averageLifeExp = mean(lifeExp))

gampminder_data_summarize <- gapminder_data %>% summarize(averageLifeExp = mean(lifeExp))

gapminder_data %>% 
  summarize(recent_year = max(year))
gapminder_data %>% 
  filter(year == 2007) %>% 
  summarize(average = mean(lifeExp))

head(gapminder_data)

#average GDP for the first year
gapminder_data %>% 
  #Filter for first year in data
  filter(year == min(year)) %>% 
  #averaging GDP
  summarize(average = mean(gdpPercap))

gapminder_data %>% 
  group_by(year) %>% 
  summarize(average = mean(lifeExp))

gapminder_data %>% 
  group_by(continent) %>% 
  summarize(average = mean(lifeExp))

head(gapminder_data)

gapminder_data %>% 
  group_by(continent) %>% 
  summarize(average = mean(lifeExp), min=min(lifeExp))


gapminder_data %>% 
  mutate(gdp = pop*gdpPercap)


gapminder_data %>% 
  mutate(gdp = pop*gdpPercap, popInMill=pop/100000)

gapminder_data %>% 
  select(pop, year)

gapminder_data %>% 
  select(country,continent,year,lifeExp)


gapminder_data_2007 <- read_csv("gapminder_data.csv") %>% 
  filter(year == 2007 & continent == "Americas") %>% 
  select(-year, -continent)



head(gapminder_data_2007)

co2_data <- read_csv("co2-un-data.csv", skip=2,
                     col_names = c("region", "country","year","series","value","footnotes","source"))
head(co2_data)

co2_emissions <- co2_data %>% 
  select(country,year,series,value) %>% 
  mutate(series = recode(series, 
                         "Emissions (thousand metric tons of carbon dioxide)" = "total emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year == 2005) %>% 
  select(-year)


gapmind_2007 <- inner_join(gapminder_data_2007, co2_emissions, by="country")

ggplot(gapmind_2007, aes(x=gdpPercap, y=per_capita_emissions))+
  geom_point()+
  labs(x = "GDP (per capita)", y = "CO2 emitted (per capita)", title = "Strong association between a nation's GDP and CO2 production")
#cheese