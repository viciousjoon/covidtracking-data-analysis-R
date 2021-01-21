#getting data from covid trackinig project 
install.packages(c("httr", "jsonlite"))
install.packages('lubridate')
library(httr)
library(jsonlite)
library(lubridate)
library(tidyverse)

mylib <- function() {
        library(httr)
        library(jsonlite)
        library(lubridate)
        library(tidyverse)      
}
mylib()

covid = GET("https://covidtracking.com/api/v1/us/daily.json") #get as json format 
#national data - USA
covid.historic <- fromJSON("https://covidtracking.com/api/v1/us/daily.json")
covid.historic <- as.tibble(covid.historic)
covid.historic$date <- as.Date(as.character(covid.historic$date),format='%Y%m%d')

head(covid.historic)
View(covid.historic)
names(covid.historic)
summary(covid.historic$onVentilatorCurrently)

covid.historic %>% 
        ggplot() +
        aes(x=date, y=deathIncrease) +
        geom_col(fill='blue')

covid.historic %>% 
        ggplot() +
        aes(x=date, y=hospitalizedCurrently) +
        geom_col(color='skyblue') 

#state data 
covid.state.historic <- fromJSON("https://covidtracking.com/api/v1/states/daily.json")
covid.state.historic <- as_tibble(covid.state.historic)
covid.state.historic$date <- as.Date(as.character(covid.state.historic$date),format='%Y%m%d')
names(covid.state.historic)

covid.state.historic %>% 
        filter(state %in% c('CA','AL','AZ','TX','FL')) %>% 
        ggplot(aes(x=date)) +
        geom_bar(aes(y=positive), stat = 'identity', fill='lightblue')+
        geom_line(aes(y=positive), color='red') #not pretty looking graph...

covid.state.historic %>% 
        filter(state %in% c('CA','AL','AZ','TX','FL')) %>%
        ggplot(aes(x=date)) +
        geom_line(aes(y=hospitalizedCumulative, group = state, color=state))   

#current hospitalization by state
covid.state.historic %>% 
        filter(hospitalizedCurrently>1000) %>% 
        ggplot(aes(x=date)) +
        geom_line(aes(y=hospitalizedCurrently, group = state, color=state))   

str(covid.state.historic)
summary(covid.state.historic$hospitalizedCurrently)

covid.state.historic %>% 
        filter(state %in% c('CA','AL','AZ','TX','FL','NY')) %>%
        ggplot(aes(x=date)) +
        geom_col(aes(y=hospitalizedCurrently, fill=state)) +
        facet_grid(.~state)

#high state
covid.state.historic %>% 
        filter(state %in% c("AL",'AZ','IN','NJ',"NY","TX")) %>% 
        ggplot(aes(x=date)) +
        geom_area(aes(y=hospitalizedCurrently, fill=state)) +
        facet_wrap(.~state) +
        labs(y='Number of Patients', x='Date', title = 'Number of Hospitalized COVID-19 Patients by State', subtitle = paste("Data as of", format(max(covid.state.historic$date), "%A, %B %e, %Y")), caption = "Joon Kim @joonkimmd / Data: covidtracking.com") 

#all state 
covid.state.historic %>% 
        ggplot(aes(x=date)) +
        geom_area(aes(y=hospitalizedCurrently, fill=state)) +
        scale_y_continuous(limits = c(0,1500)) +
        facet_wrap(.~state) +
        labs(y='Number of Patients', x='Date', title = 'Number of Hospitalized COVID-19 Patients by State')

#all state 
covid.state.historic %>%
        ggplot(aes(x=date)) +
        geom_line(aes(y=hospitalizedCurrently, color=state)) +
        scale_y_continuous(limits = c(0,3000)) +
        facet_wrap(.~state) +
        labs(y='Number of Patients', x='Date', title = 'Number of Hospitalized COVID-19 Patients by State', subtitle = paste("Data as of", format(max(covid.state.historic$date), "%A, %B %e, %Y")), caption = "Joon Kim @joonkimmd / Data: covidtracking.com")  +
        theme_minimal()

#select dates 
covid.state.historic %>%
        ggplot(aes(x=date)) +
        geom_area(aes(y=hospitalizedCurrently, fill=state)) +
        scale_y_continuous(limits = c(0,1000)) +
        scale_x_date(limits = c(Sys.Date() - 60, Sys.Date()))+
        facet_wrap(.~state) +
        labs(y='Number of Patients', x='Date', title = 'Number of Hospitalized COVID-19 Patients by State', subtitle = paste("Data as of", format(max(covid.state.historic$date), "%A, %B %e, %Y")), caption = "Joon Kim @joonkimmd / Data: covidtracking.com")  +
        theme_minimal()

#selected states 
covid.state.historic %>%
        filter(state %in% c('AZ','CO','IL','IN','MD','MI','MN','NC','NJ','NY','OH','OK','PA','TN','TX','UT','WI','FL',"AR",'AL','GA')) %>%
        ggplot(aes(x=date)) +
        geom_area(aes(y=hospitalizedCurrently, fill=state)) +
        scale_y_continuous(limits = c(0,4000)) +
        facet_wrap(.~state) +
        labs(y='Number of Patients', x='Date', title = 'Number of Hospitalized COVID-19 Patients by State', subtitle = paste("Data as of", format(max(covid.state.historic$date), "%A, %B %e, %Y")), caption = "Joon Kim @joonkimmd / Data: covidtracking.com")    +
        theme_minimal()

#less than 10000 key figure
covid.state.historic %>%
        filter(state %in% c('AZ','CO','CT','GA','IL','IN','MD','MI','MN','NC','NJ','NY','OH','OK','PA','TN','UT','WI',"AR",'AL','MA','MO','KY','MO','NC','TX','NV')) %>%
        ggplot(aes(x=date)) +
        geom_area(aes(y=hospitalizedCurrently, fill=state)) +
        scale_y_continuous(limits = c(0,8000)) +
        facet_wrap(.~state) +
        labs(y='Number of Patients', x='Date', title = 'Number of Hospitalized COVID-19 Patients by State', subtitle = paste("Data as of", format(max(covid.state.historic$date), "%A, %B %e, %Y")), caption = "Joon Kim @joonkimmd / Data: covidtracking.com")    +
        theme_minimal()

#no y limit selected state 
covid.state.historic %>%
        filter(state %in% c('AZ','CO','IL','IN','MD','MI','MN','NC','NJ','NY','OH','OK','PA','TN','TX','WI',"AR",'AL','MA','MO','CA','TX')) %>%
        ggplot(aes(x=date)) +
        geom_area(aes(y=hospitalizedCurrently, fill=state)) +
        facet_wrap(.~state) +
        labs(y='Number of Patients', x='Date', title = 'Number of Hospitalized COVID-19 Patients by State', subtitle = paste("Data as of", format(max(covid.state.historic$date), "%A, %B %e, %Y")), caption = "Joon Kim @joonkimmd / Data: covidtracking.com")    +
        theme_minimal()



covid.state.historic %>%
        ggplot(aes(x=date)) +
        geom_line(aes(y=hospitalizedCurrently, color=state)) +
        scale_y_continuous(limits = c(0,3000)) +
        facet_wrap(.~state) +
        labs(y='Number of Patients', x='Date', title = 'Number of Hospitalized COVID-19 Patients by State')+
        theme_minimal()

covid.state.historic %>% 
        ggplot(aes(x=date)) +
        geom_line(aes(y=hospitalizedCurrently, group = state, color=state)) +
        theme_minimal()

covid.state.historic %>% 
        ggplot(aes(x=date)) +
        geom_line(aes(y=positive, group = state, color=state)) +
        theme_minimal()

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
