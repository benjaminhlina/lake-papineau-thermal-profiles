

# load packages ----
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(here)
library(plotly)
library(tidyr)
library(tibble)


# load logger rds ----

temp_loggers <- read_rds(here::here("Saved R Data", 
                                    "papineau_lake_temp_loggers.rds"))


glimpse(temp_loggers)
temp_loggers <- temp_loggers %>% 
  mutate(basin = case_when(receiver_name == 3 ~ "East Basin", 
                           receiver_name == 5 ~ "West Basin", 
                           receiver_name == 15 ~ "North Basin"))


temp_loggers <- temp_loggers %>% 
  mutate(basin = factor(basin, levels = c("East Basin", 
                                          "West Basin", 
                                          "North Basin")))
may <- temp_loggers %>% 
  filter(month == 5)



glimpse(may)
may <- may %>% 
  mutate(month_day = paste(month, day, sep = "-"))



summary_may <- may %>% 
  group_by(receiver_name, basin, daily, jdate, depth) %>% 
  summarise(mean_daily_temp = mean(temp)) %>% 
  ungroup()






glimpse(summary_may)

ggplot(data = may, aes(x = jdate, y = mdt)) + 
  geom_line(aes(
    # linetype = as.factor(depth), 
    colour = as.factor(depth)), size = 1.0) + 
  facet_wrap(.~ basin) + 
  scale_y_continuous(limits = c(2, 14), 
                     breaks = seq(2, 14, 2)) + 
  scale_x_continuous(breaks = c(seq(120, 150, 10)),
                     limits = c(120, 160), 
                     labels = c("05-01", 
                                "May 10th", 
                                "May 20th", 
                                "May 30th")) +  
  scale_colour_discrete(name = "Depth (m)") + 
  coord_cartesian(ylim = c(3.5, 13)) + 
  theme_classic(base_size = 15) + 
  theme(axis.text = element_text(colour = "black")) + 
  labs(y = expression(bold("Temperature " (degree~C))), 
       x = "Date")




p <- ggplot(data = summary_may, aes(x = jdate, y = mean_daily_temp)) + 
  geom_line(aes(
    # linetype = as.factor(depth), 
    colour = as.factor(depth)), size = 1.0) + 
  facet_wrap(.~ basin) + 
  scale_y_continuous(limits = c(2, 14), 
                     breaks = seq(2, 14, 2)) + 
  scale_x_continuous(breaks = c(seq(120, 150, 10)),
                     limits = c(120, 152), 
                     labels = c("May 1st", 
                                "May 10th", 
                                "May 20th", 
                                "May 30th")) +  
  scale_colour_discrete(name = "Depth (m)") + 
  coord_cartesian(ylim = c(3.5, 13.25)) + 
  theme_classic(base_size = 15) + 
  theme(axis.text = element_text(colour = "black")) + 
  labs(y = expression(bold("Temperature " (degree~C))), 
       x = "Date")


# ggsave(p, filename = here("Plots", 
#                           "Mean Daily Temp-May-Papineau Lake.png"), 
#        height = 6.37, width = 6.34 * 3)


p



# 
# temp_merge <- temp_merge %>% 
#   mutate(basin = case_when(rec_name == 3 ~ "East Basin", 
#                            rec_name == 5 ~ "West Basin", 
#                            rec_name == 15 ~ "North Basin"))
# 
# temp_merge <- temp_merge %>% 
#   mutate(basin = factor(basin, levels = c("East Basin", 
#                                           "West Basin", 
#                                           "North Basin")))


glimpse(temp_merge)
ggplot() + 
  geom_tile(data = temp_merge, aes(x = jdate, y = depth, fill = temp)) +
  facet_wrap(.~ basin) + 
  scale_y_reverse() + 
  scale_x_continuous(breaks = c(seq(120, 150, 10)),
                     limits = c(120, 152), 
                     labels = c("May 1st", 
                                "May 10th", 
                                "May 20th", 
                                "May 30th")) + 
  scale_fill_gradient2(midpoint = 15,
                       high = scales::muted("red"),
                       low = scales::muted("blue"),
                       name = expression("Temperature (" *degree*C*")")) +
  
  labs(x = "Date", 
       y = "Depth (m)") + 
  # coord_cartesian(expand = FALSE) +
  theme_classic(base_size = 18) + 
  theme(legend.title.align = 0.5, 
        axis.text = element_text(colour = "black")) 
