#try to use johns hopkins : not working 

library(tidyverse)
jhu_url <- paste("https://raw.githubusercontent.com/CSSEGISandData/", 
                 "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
                 "time_series_19-covid-Confirmed.csv", sep = "")

us_confirmed_long_jhu <- read_csv(jhu_url) %>% rename(province = "Province/State", 
                                                      country_region = "Country/Region") %>% pivot_longer(-c(province, 
                                                                                                             country_region, Lat, Long), names_to = "Date", values_to = "cumulative_cases") %>% 
        # adjust JHU dates back one day to reflect US time, more or
        # less
        mutate(Date = mdy(Date) - days(1)) %>% filter(country_region == 
                                                              "US") %>% arrange(province, Date) %>% group_by(province) %>% 
        mutate(incident_cases = c(0, diff(cumulative_cases))) %>% 
        ungroup() %>% select(-c(country_region, Lat, Long, cumulative_cases)) %>% 
        filter(str_detect(province, "Diamond Princess", negate = TRUE))

#getting data from covid trackinig project 
install.packages(c("httr", "jsonlite"))
install.packages('lubridate')
library(httr)
library(jsonlite)
library(lubridate)
library(tidyverse)

covid = GET("https://covidtracking.com/api/v1/us/daily.json") #get as json format 
covid.historic <- fromJSON("https://covidtracking.com/api/v1/us/daily.json")
covid.historic <- as.tibble(covid.db)

covid.historic$date <- as.Date(as.character(covid.historic$date),format='%Y%m%d')

head(covid.historic)
View(covid.historic)
names(covid.historic)
summary(covid.historic$onVentilatorCurrently)

covid.historic %>% 
        ggplot() +
        aes(x=date, y=onVentilatorCurrently) +
        geom_line(fill='black')

covid.historic %>% 
        ggplot() +
        aes(x=date, y=hospitalizedCurrently) +
        geom_line(fill='black')

covid.state.historic <- fromJSON("https://covidtracking.com/api/v1/states/daily.json")
View(covid.state.historic)
covid.state.historic <- as_tibble(covid.state.historic)
covid.state.historic$date <- as.Date(as.character(covid.state.historic$date),format='%Y%m%d')
covid.state.historic$date
names(covid.state.historic)

covid.state.historic %>% 
        filter(state=='CA') %>% 
        ggplot(aes(x=date)) +
        geom_bar(aes(y=positive), stat = 'identity', fill='lightblue')+
        geom_line(aes(y=positive), color='red')

covid.state.historic %>% 
        ggplot(aes(x=date)) +
        geom_line(aes(y=positive), color='red')

covid.state.historic %>% 
        filter(state=='AL') %>% 
        ggplot() +
        aes(x=date, y=hospitalizedCurrently) +
        geom_line()

covid.state.historic %>% 
        filter(state=='CA') %>% 
        ggplot() +
        aes(x=date, y=deathIncrease) +
        geom_col()

covid.state.historic %>% 
        filter(state=='CA') %>% 
        ggplot() +
        aes(x=date, y=deathIncrease) +
        geom_point()

class(covid.historic$date)
as.Date(covid.historic$date,'%y%m%d')
?as.Date

#tidycovid19 package -does not work... for some reason 
#https://joachim-gassen.github.io/tidycovid19/
remotes::install_github("joachim-gassen/tidycovid19")
# Suggestion by AndreaPi (issue #19)
library(tidyverse)
library('tidycovid19')
library(zoo)

df <- download_merged_data(cached = TRUE, silent = TRUE)

df %>%
        filter(iso3c == "ITA") %>%
        mutate(
                new_cases = confirmed - lag(confirmed),
                ave_new_cases = rollmean(new_cases, 7, na.pad=TRUE, align="right")
        ) %>%
        filter(!is.na(new_cases), !is.na(ave_new_cases)) %>%
        ggplot(aes(x = date)) +
        geom_bar(aes(y = new_cases), stat = "identity", fill = "lightblue") +
        geom_line(aes(y = ave_new_cases), color ="red") +
        theme_minimal()

merged <- download_merged_data(cached = TRUE, silent = TRUE)
plot_covid19_spread(
        merged, highlight = c("ITA", "ESP", "GBR", "FRA", "DEU", "USA", "BRA", "MEX"),
        intervention = "lockdown"
)

plot_covid19_stripes(
        per_capita = TRUE, 
        population_cutoff = TRUE, 
        sort_countries = "magnitude"
)

#kieranhealy.org method 
library(tidyverse)
library(lubridate)
library(here)
library(janitor)
library(socviz)
library(ggrepel)
library(paletteer)


## Download today's excel file, saving it to data/ and reading it in
get_ecdc_data <- function(url = "https://www.ecdc.europa.eu/sites/default/files/documents/",
                          fname = "COVID-19-geographic-distribution-worldwide-", 
                          date = lubridate::today(), 
                          ext = "xlsx", 
                          dest = "data") {
        
        target <-  paste0(url, fname, date, ".", ext)
        message("target: ", target)
        
        destination <- fs::path(here::here("data"), paste0(fname, date), ext = ext)
        message("saving to: ", destination)
        
        tf <- tempfile(fileext = ext)
        curl::curl_download(target, tf)
        fs::file_copy(tf, destination)
        
        switch(ext, 
               xls = janitor::clean_names(readxl::read_xls(tf)),
               xlsx = janitor::clean_names(readxl::read_xlsx(tf))
        )
}                          


coalesce_join <- function(x, y, 
                          by = NULL, suffix = c(".x", ".y"), 
                          join = dplyr::full_join, ...) {
        joined <- join(x, y, by = by, suffix = suffix, ...)
        # names of desired output
        cols <- dplyr::union(names(x), names(y))
        
        to_coalesce <- names(joined)[!names(joined) %in% cols]
        suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
        # remove suffixes and deduplicate
        to_coalesce <- unique(substr(
                to_coalesce, 
                1, 
                nchar(to_coalesce) - nchar(suffix_used)
        ))
        
        coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
                joined[[paste0(.x, suffix[1])]], 
                joined[[paste0(.x, suffix[2])]]
        ))
        names(coalesced) <- to_coalesce
        
        dplyr::bind_cols(joined, coalesced)[cols]
}

covid_raw <- get_ecdc_data(url = "https://www.ecdc.europa.eu/sites/default/files/documents/",
                           fname = "COVID-19-geographic-disbtribution-worldwide-",
                           ext = "xlsx")
covid_raw

#this is the code that I wanted to use. but I do not know how i did it 
# https://kieranhealy.org/blog/archives/2020/03/21/covid-19-tracking/


focus_cn <- c("CHN", "GBR", "USA", "IRN", "JPN",
              "KOR", "ITA", "FRA", "ESP")


cov_curve %>%
        filter(iso3 %in% focus_cn) %>% ## focus on just a few countries, defined above
        mutate(end_label = recode(end_label, `United States` = "USA",
                                  `Iran, Islamic Republic of` = "Iran", 
                                  `Korea, Republic of` = "South Korea", 
                                  `United Kingdom` = "UK")) %>%
        ggplot(mapping = aes(x = days_elapsed, y = cu_deaths, 
                             color = cname, label = end_label, 
                             group = cname)) + 
        geom_line(size = 0.8) + 
        geom_text_repel(nudge_x = 1.1,
                        nudge_y = 0.1, 
                        segment.color = NA) + 
        guides(color = FALSE) + 
        scale_color_manual(values = prismatic::clr_darken(paletteer_d("ggsci::category20_d3"), 0.2)) +
        scale_y_continuous(labels = scales::comma_format(accuracy = 1), 
                           breaks = 2^seq(4, 12),
                           trans = "log2") + 
        labs(x = "Days Since 10th Confirmed Death", 
             y = "Cumulative Number of Deaths (log scale)", 
             title = "Cumulative Deaths from COVID-19, Selected Countries", 
             subtitle = paste("Data as of", format(max(cov_curve$date), "%A, %B %e, %Y")), 
             caption = "Kieran Healy @kjhealy / Data: ECDC") + 
        theme(plot.title = element_text(size = rel(2), face = "bold"),
              plot.subtitle = element_text(size = rel(1.5)),
              axis.text.y = element_text(size = rel(2)),
              axis.title.x = element_text(size = rel(1.5)),
              axis.title.y = element_text(size = rel(1.5)),
              axis.text.x = element_text(size = rel(2)),
              legend.text = element_text(size = rel(2))
        )
