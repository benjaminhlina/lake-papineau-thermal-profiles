

# load packages ----
library(broom)
library(dplyr)
library(fishualize)
library(ggplot2)
library(gganimate)
library(ggvis)
library(ggthemes)
library(ggridges)
library(lubridate)
library(janitor)
library(purrr)
library(readr)
library(stringr)
library(tweenr)
library(transformr)

library(av)
library(tidyr)

# load logger rds ----

temp_loggers <- read_rds(here::here("Saved R Data", 
                                    "papineau_lake_temp_loggers.rds"))


smooth_fit <- read_rds(here::here("Saved R Data", 
                                  "loess_temp_raster_model.rds"))
# look at the temp_logger str ----

glimpse(temp_loggers)



rec_num <- unique(temp_loggers$receiver_name)




temp_list <- list()

# only do 2019 -----


tl_2019 <- temp_loggers %>% 
  filter(year == 2019)


for (rec in 1:length(rec_num)) {
  
  df <- tl_2019 %>% 
    filter(receiver_name == rec_num[rec]) %>% 
    arrange(depth)
  
  
  
  
  estimate_temp_by_date <- function(target_date, target_depth) {
    data_for_date <- df %>% 
      filter(daily == target_date) %>%
      arrange(depth)
    
    # approx() is one way to do a linear interpolation
    approx(data_for_date$depth, data_for_date$temp, xout = target_depth)$y
  }
  
  
  
  
  
  temp_interp_depth <- crossing(
    tibble(date = unique(df$daily), 
           jdate = unique(df$jdate)
    ),
    tibble(rec_name = unique(df$receiver_name)),
    # depths can now be any value
    tibble(depth = seq(1, 20, length.out = 100))
    
  ) %>%
    group_by(date, 
             jdate,
             rec_name) %>%
    mutate(temp = estimate_temp_by_date(date[1], depth)) %>% 
    ungroup()
  
  
  
  
  
  # estimate_temp_by_depth <- function(target_depth, target_date) {
  #   df_1 <- temp_interp_depth %>%
  #     filter(depth == target_depth) %>%
  #     arrange(date)
  #   
  #   approx(df_1$date, df_1$temp, xout = target_date)$y
  # }
  # 
  # 
  # 
  # 
  # 
  # estimate_temp_by_depth(target_depth = 5,
  #                        target_date = seq(ymd("2018-07-02"),
  #                                          ymd("2019-08-15"), by = 1))
  # head(temp_interp_depth, n = 1)
  # 
  # 
  # temp_raster <- crossing(
  #   # dates can now be any value
  #   tibble(date = seq(ymd("2018-07-20"), ymd("2019-07-31"), by = 1)),
  #   # depths must be the same as in temp_interp_depth
  #   tibble(depth = unique(temp_interp_depth$depth))
  # ) %>%
  #   group_by(depth) %>%
  #   mutate(temp = estimate_temp_by_depth(depth[1], date)) %>% 
  #   ungroup()
  
  # ggplot(data = temp_interp_depth, aes(x = jdate, y = depth, colour = temp)) + 
  #   geom_point() + 
  #   scale_y_reverse() + 
  #   scale_x_continuous(breaks = c(seq(0, 370, 30)),
  #                      limits = c(0, 370),
  #                      labels = c("Jan", "Feb", "Mar",
  #                                 "Apr", "May", "Jun",
  #                                 "Jul", "Aug", "Sept",
  #                                 "Oct", "Nov", "Dec",
  #                                 "Jan")) +
  #   scale_colour_gradient2(midpoint = 15, 
  #                          high = scales::muted("red"), 
  #                          low = scales::muted("blue"))
  
  
  temp_list[[rec]] <- temp_interp_depth
  
  
  
}




temp_merge <- do.call(rbind, temp_list) %>% 
  as_tibble()




glimpse(temp_merge)

temp_merge <- temp_merge %>% 
  filter(!temp == is.na(temp))


ggplot(data = temp_merge, aes(x = jdate, y = depth, fill = temp)) + 
  geom_tile() +
  facet_wrap(.~ rec_name, scales = "free") + 
  scale_y_reverse() + 
  scale_x_continuous(breaks = c(seq(0, 370, 30)),
                     limits = c(0, 370),
                     labels = c("Jan", "Feb", "Mar",
                                "Apr", "May", "Jun",
                                "Jul", "Aug", "Sept",
                                "Oct", "Nov", "Dec",
                                "Jan")) +
  scale_fill_gradient2(midpoint = 15,
                       high = scales::muted("red"),
                       low = scales::muted("blue"),
                       name = expression("Temperature (" *degree*C*")")) +
  
  # scale_fill_viridis_c(name = expression("Temperature (" *degree*C*")")) + 
  labs(x = "Month", 
       y = "Depth (m)") + 
  coord_cartesian(expand = FALSE) +
  theme_classic(base_size = 18) + 
  theme(legend.title.align = 0.5, 
        axis.text = element_text(colour = "black")) 






# new idea of depth -----

rec_3 <- temp_merge %>% 
  filter(rec_name == "3")




results_merged <- read_rds(here::here("Saved R data", 
                                      "depth_binned_results.rds")) %>% 
  as_tibble()


results_merged
results_merged %<>%
  arrange(species, tag_id, time_bins)


results_merged <- results_merged %>% 
  filter(species == "lt") %>% 
  mutate(jdate = yday(date))




ggplot() + 
  geom_tile(data = rec_3, aes(x = jdate, y = depth, fill = temp)) +
  geom_boxplot(data = results_merged, aes(x = jdate, y = sensor_value, 
                                          group = month)) + 
  scale_y_reverse() + 
  scale_x_continuous(breaks = c(seq(0, 370, 30)),
                     limits = c(0, 370),
                     labels = c("Jan", "Feb", "Mar",
                                "Apr", "May", "Jun",
                                "Jul", "Aug", "Sept",
                                "Oct", "Nov", "Dec",
                                "Jan")) +
  scale_fill_gradient2(midpoint = 15,
                       high = scales::muted("red"),
                       low = scales::muted("blue"),
                       name = expression("Temperature (" *degree*C*")")) +
  
  
  # scale_fill_viridis_c(name = expression("Temperature (" *degree*C*")")) + 
  labs(x = "Month", 
       y = "Depth (m)") + 
  # coord_cartesian(expand = FALSE) +
  theme_classic(base_size = 18) + 
  theme(legend.title.align = 0.5, 
        axis.text = element_text(colour = "black")) 




# this is just for thermocline ------





thermo <- temp_merge %>% 
  filter(date >= "2018-07-22" & date <= "2018-10-15") 

thermo_1 <- temp_merge %>% 
  filter(date >= "2019-04-01")



thermo_merged <- rbind(thermo, thermo_1)


thermo_merged <- thermo_merged %>%
  mutate(basin = case_when(rec_name == 3 ~ "East Basin",
                           rec_name == 5 ~ "West Basin",
                           rec_name == 15 ~ "North Basin"))

thermo_merged <- thermo_merged %>%
  mutate(basin = factor(basin, levels = c("East Basin",
                                          "West Basin",
                                          "North Basin")))


pdf(here::here("Plots",
               "thermoclines_basin_date.pdf"),
    width = 10, height = 10)

date_id <- unique(thermo_merged$date)

date_id

for (d in 1:length(date_id)) {


  df_1 <- thermo_merged %>%
    filter(date == date_id[d]) %>%

    ggplot(aes(x = temp, y = depth)) +
    geom_line(size = 1) +
    facet_wrap(.~ basin, scales = "fixed") +
    scale_x_continuous(position = "top",
                      limits = c(0, 27.5),
                      breaks = seq(0, 25, 5)) +
    scale_y_reverse() +
    theme(plot.title = element_text(hjust = 0.5), 
          panel.border = element_rect(colour = "black", fill = NA, 
                                      size = 1)) +
    labs(title = date_id[d],
         x = expression("Temperature (" *degree*C*")"),
         y = "Depth (m)") -> p
  print(p)


}


dev.off()


glimpse(temp_loggers)

glimpse(thermo_merged)

thermo_merged <- thermo_merged %>%
  mutate(basin = case_when(rec_name == 3 ~ "East Basin",
                           rec_name == 5 ~ "West Basin",
                           rec_name == 15 ~ "North Basin"))

thermo_merged <- thermo_merged %>%
  mutate(basin = factor(basin, levels = c("East Basin",
                                          "West Basin",
                                          "North Basin")))


unique(thermo_merged$date)

p15 <- ggplot(data = thermo_merged, aes(x = temp, y = depth)) + 
  geom_line(size = 1) + 
  facet_wrap(.~ basin) + 
  scale_x_continuous(position = "top") + 
  scale_y_reverse() + 
  theme(plot.title = element_text(hjust = 0.5), 
        panel.border = element_rect(colour = "black", fill = NA, 
                                    size = 1)) + 
  labs(x = expression("Temperature (" *degree*C*")"), 
       y = "Depth (m)") +
  transition_time(thermo_merged$date) +
  #   # shadow_wake(wake_length = 0.1, alpha = FALSE) +
  ggtitle(" {frame_time}")

p15    
length(unique(thermo_merged$date))

b <- animate(p15, 
             # duration = 137.5,
             nframes = 275,
             # fps = 5,
             renderer = ffmpeg_renderer())

c <- animate(p15, 
             # duration = 122.5, 
             # fps = 1,
             nframes = 275)

anim_save("2019_thermolcine_per_day.mp4", animation = b
)
anim_save("2019_thermolcine_per_day.gif", animation = c
)


temp_loggers %>% 
  group_by(receiver_name) %>% 
  summarise(min(daily), max(daily), n_distinct(daily))


temp_loggers %<>% 
  group_by(receiver_name) %>% 
  mutate(day_in_year = as.numeric(daily - floor_date(daily, unit = "years"), 
                                  unit = "days"), 
         date_label = ymd("2019-01-01") + day_in_year) %>% 
  ungroup()


glimpse(temp_loggers)



# smooth_fit <- loess(temp ~ day_in_year + depth + as.factor(receiver_name), 
#                     data = temp_loggers, 
#                     span = 0.2)
# 
# beepr::beep()
# glimpse(temp_loggers)
# ggplot(data = temp_loggers, aes(x = jdate, y = depth, colour = temp)) + 
#   geom_point() +
#   facet_wrap(.~ receiver_name) + 
#   scale_y_reverse() + 
#   scale_x_continuous(breaks = c(seq(0, 370, 30)),
#                      limits = c(0, 370),
#                      labels = c("Jan", "Feb", "Mar",
#                                 "Apr", "May", "Jun",
#                                 "Jul", "Aug", "Sept",
#                                 "Oct", "Nov", "Dec",
#                                 "Jan")) +
#   scale_colour_gradient2(midpoint = 15,
#                        high = scales::muted("red"),
#                        low = scales::muted("blue"),
#                        name = expression("Temperature (" *degree*C*")")) +
#   
#   # scale_fill_viridis_c(name = expression("Temperature (" *degree*C*")")) + 
#   labs(x = "Month", 
#        y = "Depth (m)") + 
#   coord_cartesian(expand = FALSE) +
#   theme_classic(base_size = 18) + 
#   theme(legend.title.align = 0.5)
glimpse(smooth_fit)

temp_raster_smooth <- crossing(
  tibble(date = seq(ymd("2018-07-20"), ymd("2019-07-20"), by = 1)),
  tibble(depth = seq(1, 20, length.out = 100)),
  tibble(receiver_name = unique(temp_loggers$receiver_name))
) %>%
  mutate(
    day_in_year = as.numeric(date - floor_date(date, unit = "years"), 
                             unit = "days"),
    temp = predict(
      smooth_fit, 
      newdata = tibble(day_in_year = day_in_year, depth = depth, 
                       receiver_name = receiver_name)
    )
  )

glimpse(temp_raster_smooth)


temp_raster_smooth %<>% 
  filter(!temp == is.na(temp))
ggplot(data = temp_raster_smooth, aes(x = day_in_year, 
                                      y = depth, fill = temp)) + 
  geom_raster() +
  scale_y_reverse() + 
  facet_wrap(.~ receiver_name, scales = "free") + 
  scale_x_continuous(breaks = c(seq(0, 370, 30)),
                     limits = c(0, 370),
                     labels = c("Jan", "Feb", "Mar",
                                "Apr", "May", "Jun",
                                "Jul", "Aug", "Sept",
                                "Oct", "Nov", "Dec",
                                "Jan")) +
  scale_fill_gradient2(midpoint = 15,
                       high = scales::muted("red"),
                       low = scales::muted("blue"),
                       name = expression("Temperature (" *degree*C*")")) +
  
  # scale_fill_viridis_c(name = expression("Temperature (" *degree*C*")")) + 
  labs(x = "Month", 
       y = "Depth (m)") + 
  # coord_cartesian(expand = FALSE) +
  theme_classic(base_size = 18) + 
  theme(legend.title.align = 0.5, 
        axis.text = element_text(colour = "black")) 

