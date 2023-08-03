# load and seperate loggers by loction ----
# load packages -----
# load temp logger data and match up metadata ----
# load packages ----
library(dplyr)
library(ggplot2)
library(forcats)
library(lubridate)
library(janitor)
library(magrittr)
library(purrr)
library(readr)
library(rLakeAnalyzer)
library(stringr)

# load logger rds ----

temp_loggers <- read_rds(here::here("Saved R Data", 
                                    "papineau_lake_temp_loggers.rds"))

glimpse(temp_loggers)

temp_loggers$depth %<>%
  as.character() %>% 
  as.numeric()

unique(temp_loggers$depth)
# split loggers up by receivers they are on ----
s_temp_loggers <- split(temp_loggers, temp_loggers$receiver_name)

glimpse(s_temp_loggers)

rec_3_loggers <- s_temp_loggers$`3`
rec_5_loggers <- s_temp_loggers$`5`
rec_15_loggers <- s_temp_loggers$`15`
rec_3_loggers

glimpse(rec_15_loggers)
unique(rec_3_loggers$depth)



rec_15_loggers %<>% 
  mutate(day = floor_date(date_time, unit = "day"))
date 
lim_15_2018 <- rec_15_loggers %>% 
  filter(date_time >= "2018-7-20" & date_time <= "2018-9-30")


lim_15_2019 <- rec_15_loggers %>% 
  filter(date_time >= "2019-6-15" & date_time <= "2019-7-03")
  
lim_15 <- rbind(lim_15_2018, lim_15_2019)

lim_15_s <- lim_15 %>% 
  group_by(depth) %>%
  summarise(mt = mean(temp),
            sd = sd(temp), 
            var = var(temp))

lim_15_s
lim <- rec_15_loggers %>% 
  filter(date_time >= "2019-6-16" & date_time <= "2019-7-01") 

lim
# filter(date_time >= "2018-7-20" & date_time <= "2018-9-01") 
tail(lim)




ggplot(data = lim, aes(x = temp, y = depth)) + 
  geom_point(aes(colour = depth)) +
  scale_x_continuous(position = "top") + 
  scale_y_reverse() + 
  stat_smooth(method = "lm", se = TRUE, fill = NA, show.legend = TRUE, 
              formula = y ~ 1 + poly(x, 3, raw = TRUE), colour = "black", 
              orientation = "y")

ggplot(data = lim_15_s, aes(x = mt, y = depth)) + 
  # geom_line(colour = "#000000", size = 1.5) +
  geom_point(aes(fill = depth), size = 7, shape = 21) +
  scale_x_continuous(position = "top") + 
  scale_y_reverse() + 
  scale_fill_continuous(trans = "reverse") + 
  stat_smooth(method = "lm", se = TRUE, fill = NA, show.legend = TRUE, 
              formula=y ~ 1+poly(x, 3, raw = TRUE), colour = "black", 
              orientation = "y")



thermo.depth(wtr = lim_15_s$mt, depths = lim_s$depth)

lim_3 <- rec_3_loggers %>% 
  filter(date_time >= "2018-7-20" & date_time <= "2018-9-1") %>%  
  group_by(depth) %>%
  summarise(mt = mean(temp),
            sd = sd(temp), 
            var = var(temp))

tail(lim_3)
lim_3
thermo.depth(wtr = lim_3$mt, depths = c(2, 4, 6, 10, 20 ))
