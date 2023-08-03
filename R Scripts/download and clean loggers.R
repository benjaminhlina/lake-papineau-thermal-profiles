# load temp logger data and match up metadata ----
# load packages ----
library(dplyr)
library(lubridate)
library(janitor)
library(purrr)
library(readr)
library(stringr)

# import metadata ----

md_loggers <- read_csv(here::here("Data", 
                                  "Logger metadata", 
                                  "logger_metadata.csv"))

glimpse(md_loggers)

md_loggers$import_order <- md_loggers$import_order %>% 
  as.character()

md_loggers <- md_loggers %>% 
  arrange(import_order)

# import loggers -----

file.list <- list.files(here::here("Data", "Exports from LoggerVue"), 
                        full.names = TRUE,
                        pattern = "*.csv")


file.list

# names(file.list) <- list.files(file.list) %>% 
#   str_replace(".csv$", "")
# use setNames to retunr object but not 
# store file names under a name just so the names can be assigned
# file.list <- set_names(file.list, file.list) 

file.list
# use map_df funciton from purrr to essentiall proform -----
# a for loop to import and combine into a single dataframe ----
# only needed when you need an id-column with the file-names 

pl_loggers <- map(file.list,read_csv, id = "import_order") %>%
  clean_names()
# view data after import 
glimpse(pl_loggers)

ls()
fs::dir_ls()
file.list




# join metadata and logger massive dataframe together ----

temp_loggers <- pl_loggers %>% 
  left_join(md_loggers, by = "import_order")



# rename columns -----
temp_loggers <- temp_loggers %>% 
  rename(date_time = date_yyyy_mm_dd_time_hh_mm_ss, 
         temp = temperature_c)

glimpse(temp_loggers)

# check and change date and time into posixct ----

is.POSIXct(temp_loggers$date_time)


# remove the first 13 observations per each 


temp_loggers <- temp_loggers %>% 
  group_by(import_order) %>% 
  arrange(date_time) %>% 
  slice(22:n()) %>% 
  slice(1:(n()-8)) %>% 
  ungroup()


# filter(row_number() != 1 & row_number() != n())



temp_split_1 <- split(ts, f = as.factor(ts$import_order))


lapply(temp_split_1, head, 10)




# temp_split <- split(temp_loggers, f = as.factor(temp_loggers$import_order))

# 
# temp_id <- length(temp_split)
# 
# str(tail(temp_split$))
# temp_list <- list()
# 
# for (i in 1:temp_id) {
#   
#   df_1 <- temp_split[[i]] %>% 
#     arrange(date_time)
#   
#   
#   df_2 <- df_1[-c(1:13), ]
#   
#   temp_list[[i]] <- df_2
# }
# 
# glimpse(temp_list)
# 
# temp_merge <- as_tibble(do.call(rbind, temp_list))


# it's already poxicit so no need to mess with it 

temp_loggers <- temp_loggers %>% 
  mutate(jdate = lubridate::yday(date_time), 
         day = day(date_time), 
         month = month(date_time), 
         year = year(date_time), 
         daily = make_date(year = year, 
                           month = month,
                           day = day))

glimpse(temp_loggers)

temp_loggers <- temp_loggers %>% 
  group_by(daily, depth) %>% 
  mutate(mdt = mean(temp)) %>% 
  ungroup()



t3 <- temp_loggers %>% 
  filter(receiver_name == 3 & year %in% c(2020))

tj <- t3 %>% 
  filter(month %in% c(6))

ss <- tj %>% 
  group_by(daily, depth) %>% 
  summarise(mdts = mean(mdt)) %>% 
  ungroup()

ss


View(ss)
glimpse(t3)

# export merged database ---
write_rds(temp_loggers, here::here("Saved R Data", 
                                   "papineau_lake_temp_loggers.rds"))

