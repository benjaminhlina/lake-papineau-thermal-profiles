

# load packages ----
library(dplyr)

library(readr)



# load logger rds ----

temp_loggers <- read_rds(here::here("Saved R Data", 
                                    "papineau_lake_temp_loggers.rds"))


glimpse(temp_loggers)
rec_3 <- temp_loggers %>% 
  filter(receiver_name == 3)



write_csv(rec_3, file = here::here("Saved R data", 
                                   "receiver_3_temp_loggers_2018-2020.csv"))

