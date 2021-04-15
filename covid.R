url_rec = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"

url_con = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

url_dead = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

df_rec <- readr::read_csv(url(url_rec))

df_con <- readr::read_csv(url(url_con))

df_dead <- readr::read_csv(url(url_dead))

require(tidyverse)

colnames(df_rec)[1:4]

x <- df_con %>%
  rename(State = `Province/State`,
         Country = `Country/Region`) %>%
  pivot_longer(contains("/"), names_to = "date", values_to = "cases") %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  group_by(Country, date) %>%
  summarize(cases = sum(cases)) %>%
  filter()
  filter(Country %in% c("US", "China", "United Kingdom"))


ggplot(x) +
  geom_point(aes(x = date, y = cases, color = Country)) +
  #scale_y_log10() +
  theme_classic()
