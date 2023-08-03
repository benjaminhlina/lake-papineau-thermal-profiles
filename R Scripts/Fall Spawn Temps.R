
# load packages ----
library(dplyr)
library(ggplot2)
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

# view loggers -----
str(temp_loggers)


fall_temp_loggers <- temp_loggers %>% 
  filter(daily >= "2018-09-01" & daily <= "2018-12-01") %>% 
  filter(mdt <= 14.25)


# plot filtered fall temps -----

p <- ggplot(data = fall_temp_loggers, aes(x = date_time, 
                                          y = temp, group = depth)) + 
  geom_line(aes(colour = depth)) + 
  scale_color_discrete(name = "Depth") +
  facet_wrap(.~ receiver_name, nrow = 3, ncol = 1, scales = "free_x") + 
  # scale_y_continuous(expand = expand_scale(mult = c(0.1, 0.135)),
  #                    breaks = seq(0, 40, 10)) +
  theme_bw() +
  theme(axis.text = element_text(size = 13, colour = "black", face = "bold"), 
        axis.title = element_text(size = 16, face = "bold"),
        axis.line = element_line(size = 0.8),
        strip.background = element_blank(), 
        # panel.grid.major = element_line(colour = "grey", size = 0.5), 
        strip.text = element_text(size = 16, face = "bold"), 
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 13, face = "bold")) + 
  labs(y = expression(bold("Temperature " (degree~C))), 
       x = "Date")
p


# determine the first day 14.25 or 12.0 shows up ----

spawn_temp <- fall_temp_loggers %>% 
  filter(mdt >= 8.5 & mdt <= 13.5) %>% 
  filter(depth == c(2, 4)) %>% 
  arrange(daily) 

spawn_temp

p1 <- ggplot(data = spawn_temp, aes(x = date_time, 
                                          y = temp, group = depth)) + 
  geom_line(aes(colour = depth)) + 
  scale_color_discrete(name = "Depth") +
  facet_wrap(.~ receiver_name, nrow = 3, ncol = 1, scales = "free_x") + 
  # scale_y_continuous(expand = expand_scale(mult = c(0.1, 0.135)),
  #                    breaks = seq(0, 40, 10)) +
  theme_bw() +
  theme(axis.text = element_text(size = 13, colour = "black", face = "bold"), 
        axis.title = element_text(size = 16, face = "bold"),
        axis.line = element_line(size = 0.8),
        strip.background = element_blank(), 
        # panel.grid.major = element_line(colour = "grey", size = 0.5), 
        strip.text = element_text(size = 16, face = "bold"), 
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 13, face = "bold")) + 
  labs(y = expression(bold("Temperature " (degree~C))), 
       x = "Date")
p1

