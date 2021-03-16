## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(rvest)
library(ggplot2)
library(dplyr)
library(lubridate)


## ----package-updates-by-last-update-------------------------------------------

url = 'https://cran.r-project.org/web/packages/available_packages_by_date.html'

CRANpage <- read_html(url)
tbls <- html_nodes(CRANpage, "table") # since HTML is in table; no need to scrape td/tr elements
table1 <- html_table(tbls[1], fill = TRUE)
dd <- data.frame(table1[1])

#house cleaning
dd$Date <- as.Date(dd$Date)

### simple graph
ggplot(dd, aes(x = Date)) +
  geom_dotplot(binwidth = 12) +
  labs(x = "Dates",
       y = "Number of packages updates by Year of last update") +
  scale_x_date(date_breaks = "2 years",
               date_labels = "%Y/%m",
               limits = as.Date(c("2005-01-01", 
                                  format(Sys.Date(), "%Y-%m-%d"))))


## ----updates-by-year, echo=FALSE----------------------------------------------

# updates by year
dd_y <- dd %>%
  mutate( PYear= year(Date)) %>%
  select (PYear) %>%
  group_by(PYear) %>%
  summarise(
    nof = n()
  )


