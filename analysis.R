#' ---
#' title: "Covid-19 Latest Data"
#' author: "Jordan Ayala"
#' ---


#+ setup, include=FALSE
suppressMessages(library(tidyverse))
library(lubridate)
library(readr)
knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(warning = FALSE)

data_dir <- "COVID-19/csse_covid_19_data/csse_covid_19_time_series/"

datasets <- tribble(
  ~file, ~dataset,
  "time_series_19-covid-Confirmed.csv", "confirmed",
  "time_series_19-covid-Deaths.csv", "deaths",
  "time_series_19-covid-Recovered.csv", "recovered"
)


raw_data <- datasets %>%
  mutate(data = map(file, ~ read_csv(file.path(data_dir, .x), col_names = T))) %>%
  unnest(data)


df <- raw_data %>%
  pivot_longer(-c(file, dataset, `Province/State`, `Country/Region`, Lat, Long), names_to = "date", values_to = "cases") %>%
  mutate(date = lubridate::mdy(date)) %>%
  transmute(region = `Country/Region`, province = `Province/State`, dataset = dataset, lat = Lat, long = Long, date = date, cases = cases) %>%
  mutate(region = str_replace(region, "Iran.*", "Iran")) %>% # Join Iran, since changed the region recently
  mutate(region = str_replace(region, ".*China.*", "China")) %>% # Join China, since changed the region recently
  mutate(region = str_replace(region, "Republic of Korea", "Korea, South")) %>% # Join China, since changed the region recently
  group_by(dataset, region, date, province, lat, long) %>%
  summarise(cases = sum(cases, na.rm = T)) %>% # Sum aforementioned regions
  ungroup() %>%
  pivot_wider(names_from = dataset, values_from = cases) %>%
  mutate(active = confirmed - deaths - recovered) %>%
  pivot_longer(-c(region, date, province, lat, long), names_to = "dataset", values_to = "cases")

by_region <- df %>%
  filter(cases != 0) %>%
  group_by(dataset, region, date) %>%
  summarise(cases = sum(cases, na.rm = T), last_report = max(date)) %>% # Sum all for a single date
  summarise(cases = last(cases, order_by = date), last_report = max(last_report)) %>% # Last for a single date
  ungroup() %>%
  pivot_wider(names_from = dataset, values_from = cases)


"%notin%" <- Negate("%in%")

most_cases <- by_region %>%
  filter(region %notin% c("Others")) %>%
  top_n(6, confirmed) %>%
  arrange(desc(confirmed))

df_most_cases <- df %>%
  filter(region %in% most_cases$region) %>%
  group_by(dataset, region, date, province) %>%
  summarise(cases = sum(cases))

df_most_cases_region <- df_most_cases %>%
  summarise(cases = sum(cases))

#+ most_cases
most_cases %>%
  knitr::kable()


#'
#+ plot

df_most_cases %>%
  filter(region %in% c("China")) %>%
  pivot_wider(names_from = dataset, values_from = cases) %>%
  top_n(5, confirmed) %>%
  pivot_longer(c(confirmed, deaths, recovered), names_to = "dataset", values_to = "cases") %>%
  ggplot(aes(x = date, y = cases, color = province)) +
  geom_line() +
  scale_x_date(breaks = scales::pretty_breaks(10)) + 
  facet_wrap(vars(dataset), ncol = 1, scales = "free_y") +
  labs(title = "Cases in China")

df_most_cases_region %>%
  filter(region %notin% c("China")) %>%
  filter(date >= ymd("2020-02-20")) %>%
  ggplot(aes(x = date, y = cases, color = region)) +
  geom_line() +
  geom_point() +
  facet_wrap(vars(dataset), ncol = 1, scales = "free_y") +
  labs(title = "Cases around the world")

last_date <- max(df$date)

last_data <- df %>%
  filter(date == last_date)

write_csv(last_data, "last_ts.csv")
