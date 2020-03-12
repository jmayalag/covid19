#' ---
#' title: "Covid-19"
#' author: ""
#' ---


#+ setup, include=FALSE
suppressMessages(library(tidyverse))
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
  ungroup()


by_region <- df %>%
  filter(cases != 0) %>%
  group_by(dataset, region, date) %>%
  summarise(cases = sum(cases, na.rm = T)) %>% # Sum all for a single date
  summarise(cases = last(cases, order_by = date), first_case = min(date)) %>% # Last for a single date
  mutate(first_case = min(first_case)) %>%
  ungroup() %>%
  pivot_wider(names_from = dataset, values_from = cases)


"%notin%" <- Negate("%in%")

most_cases <- by_region %>%
  filter(region %notin% c("Others")) %>%
  top_n(6, confirmed) %>%
  arrange(desc(confirmed))

df_most_cases <- df %>%
  filter(region %in% most_cases$region) %>%
  group_by(dataset, region, date) %>%
  summarise(cases = sum(cases))

#+ most_cases
most_cases %>%
  knitr::kable()


#'
#+ plot
df_most_cases %>%
  filter(region %notin% c("China")) %>%
  ggplot(aes(x = date, y = cases, color = region)) +
  geom_line() +
  facet_wrap(vars(dataset), ncol = 1, scales = "free_y")

df %>% pivot_wider(names_from = dataset, values_from = cases) %>% View()
