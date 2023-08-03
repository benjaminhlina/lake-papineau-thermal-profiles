# load and seperate loggers by loction ----
# load packages -----
# load temp logger data and match up metadata ----
# load packages ----
library(broom)
library(dplyr)
library(ggplot2)
library(ggvis)
library(ggthemes)
library(ggridges)
library(lubridate)
library(janitor)
library(magrittr)
library(purrr)
library(readr)
library(stringr)

# load logger rds ----

temp_loggers <- read_rds(here::here("Saved R Data", 
                                    "papineau_lake_temp_loggers.rds"))


glimpse(temp_loggers)
# split loggers up by receivers they are on ----
s_temp_loggers <- split(temp_loggers, temp_loggers$receiver_name)

glimpse(s_temp_loggers)

rec_3_loggers <- s_temp_loggers$`3`
rec_5_loggers <- s_temp_loggers$`5`
rec_15_loggers <- s_temp_loggers$`15`

temp_loggers_19 <- temp_loggers %>% 
  filter(year == 2019)


# glimpse them all 

# rec_15_loggers$depth %<>%
#   as.factor()
#   
# glimpse(rec_15_loggers)  
# levels(rec_15_loggers$depth)
# 
# ggplot(data = rec_15_loggers, aes(x = date_time, y = temp, group = depth)) + 
#   geom_line(aes(colour = depth))
# 
# 
# 



# plot them all with each receiver seperate graphs -----
p <- ggplot(data = temp_loggers, aes(x = date_time, y = temp, group = depth)) + 
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



p1 <- ggplot(data = temp_loggers, aes(x = date_time, y = temp)) +   
  facet_wrap(.~ receiver_name, nrow = 3, ncol = 1)
p1

glimpse(temp_loggers)

temp_loggers <- temp_loggers %>% 
  mutate(week = week(date_time))

glimpse(temp_loggers)

# more plots ------

p2 <- ggplot(data = temp_loggers_19, 
             aes(x = jdate, y = mdt, group = depth)) + 
  geom_line(aes(colour = depth), size = 1) + 
  scale_x_continuous(breaks = c(seq(0, 370, 30)),
                     limits = c(0, 370), 
                     labels = c("Jan", "Feb", "Mar", 
                                "Apr", "May", "Jun", 
                                "Jul", "Aug", "Sept", 
                                "Oct", "Nov", "Dec", 
                                "Jan")) +
  # scale_y_continuous(breaks = c(seq(0, 30, 5)),
  #                    limits = c(0, 28.6)) + 
  scale_color_continuous(name = "Depth") + 
  facet_wrap(.~ receiver_name, nrow = 3, ncol = 1, scales = "free_x") +
  theme_classic() +
  theme(legend.justification = c(1, 0),
        legend.position = c(0.10, 0.75), 
        strip.background = element_blank(), 
        strip.text = element_text(size = 12, face = "bold")) + 
  labs(x = "Month", 
       y = expression("Mean Daily Temperature " ( degree~C)))

p2

# save plot -----

ggsave(plot = p, filename = here::here("Plots",
                                       "Papineau_Lake_Temperatures.pdf"),
       width = 8.34,
       height = 4.37 * 3)       


ggsave(plot = p2, filename = here::here("Plots",
                                       "Papineau_Lake_Temperatures_adjusted.pdf"),
       width = 8.34,
       height = 4.37 * 3) 
