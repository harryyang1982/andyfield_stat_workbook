library(tidyverse)

metallica <- c("Lars", "James", "Jason", "Kirk")

metallica

metallica <- metallica[metallica != "Jason"]

metallica

metallicaNames <- c("Lars", "James", "Kirk", "Rob")
metallicaAges <- c(47, 47, 48, 46)

metallica <- tibble(metallicaNames, metallicaAges)
metallica

name <- c("Lars Ulrich","James Hetfield", "Kirk Hammett", "Rob Trujillo", "Jason Newsted", "Cliff Burton", "Dave Mustaine")

# Numeric variables stored as double
songs_written <-  c(111, 112, 56, 16, 3, 11, 6)
net_worth <- c(300000000, 300000000, 200000000, 20000000, 40000000, 1000000, 20000000)

# Numeric variables stored as integer
songs_written_int <-  c(111L, 112L, 56L, 16L, 3L, 11L, 6L)
net_worth_int <- c(300000000L, 300000000L, 200000000L, 20000000L, 40000000L, 1000000L, 20000000L)

# Date variables
birth_date <- c("1963-12-26", "1963-08-03", "1962-11-18", "1964-10-23", "1963-03-04", "1962-02-10", "1961-09-13") %>% lubridate::ymd()

death_date <- c(NA, NA, NA, NA, NA, "1986-09-27", NA) %>% 
  lubridate::ymd()

# Logical variables
current_member <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)

# Factor variables
instrument <- c(2, 0, 0, 1, 1, 1, 0) %>% factor(levels = 0:2, labels = c("Guitar", "Bass", "Drums"))


instrument <- c("Drums", "Guitar", "Guitar", "Bass", "Bass", "Bass", "Guitar") %>%
  forcats::as_factor() %>%
  forcats::fct_relevel("Guitar", "Bass", "Drums")

levels(instrument)

metallica <- tibble(name, birth_date, death_date, instrument, current_member, songs_written, net_worth)

metallica
