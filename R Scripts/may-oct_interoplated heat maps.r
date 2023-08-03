

# load packages ----
library(broom)
library(dplyr)
library(fishualize)
library(ggplot2)
library(gganimate)
library(ggvis)
library(here)
library(ggthemes)
library(ggridges)
library(lubridate)
library(janitor)
library(purrr)
library(readr)
library(stringr)
library(tweenr)
library(transformr)
library(tidyr)

# load logger rds ----

temp_loggers <- read_rds(here::here("Saved R Data", 
                                    "papineau_lake_temp_loggers.rds"))


smooth_fit <- read_rds(here::here("Saved R Data", 
                                  "loess_temp_raster_model.rds"))
# look at the temp_logger str ----

glimpse(temp_loggers)








temp_list <- list()

# only do 2019 -----

tl <- temp_logg

tl <- temp_loggers %>% 
  filter(year %in% c(2019, 2020))

rec_num <- unique(tl$receiver_name)
glimpse(tl)

for (rec in 1:length(rec_num)) {
  
  df <- tl %>% 
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
           # jdate = unique(df$jdate)
    ),
    tibble(rec_name = unique(df$receiver_name)),
    # depths can now be any value
    tibble(depth = seq(1, 20, length.out = 100))
    
  ) %>%
    group_by(date, 
             # jdate,
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



temp_merge <- temp_merge %>%
  mutate(basin = case_when(rec_name == 3 ~ "East Basin",
                           rec_name == 5 ~ "West Basin",
                           rec_name == 15 ~ "North Basin"))

temp_merge <- temp_merge %>%
  mutate(basin = factor(basin, levels = c("East Basin",
                                          "West Basin",
                                          "North Basin")))



temp_merge <- temp_merge %>% 
  mutate(jdate = yday(date), 
         year = year(date), 
         month = month(date))

glimpse(temp_merge)


 may_oct <- temp_merge %>% 
  filter(month >= 5 & month <= 10
         & temp != is.na(temp)) 

 unique(may_oct$month)
 
 
 
 unique(is.na(may_oct$temp))
 
 
 
 # june <- temp_merge %>% 
 #   filter(month == 6) 
 # ms <- june %>% 
 #   filter(date %in% as.Date("2020-06-30")) %>% 
 #   select(date, basin, depth, temp)
 # 
 # View(ms)
 # 
 
 
may_oct %>% 
  filter(jdate == 244) %>% 
  arrange(date) 



# %>% 
  # tail()

min(unique(may_oct$jdate))
max(unique(may_oct$date))

p <- ggplot(data = may_oct, aes(x = jdate, y = depth, fill = temp)) + 
  geom_tile() +
  lemon::facet_rep_grid(year ~ basin,
                        repeat.tick.labels = TRUE, scales = "free_y") +
  scale_y_reverse(
    # breaks = rev(seq(0, 20, 5)),
    # limits = rev(c(0, 20))
  ) + 
  
  scale_x_continuous(
    breaks = c(121, 152, 182, 213, 244, 274, 304), 
                     limits = c(121, 304),
                     labels = c("May", "Jun",
                                "Jul", "Aug", "Sept",
                                "Oct", "Nov")) +
  scale_fill_viridis_c(option = "B", 
                       name = "Temperature (°C)") +
  # scale_fill_gradient2(midpoint = 15,
  #                      high = scales::muted("red"),
  #                      low = scales::muted("blue"),
  #                      name = expression("Temperature (" *degree*C*")")) +
  
  labs(x = "Month", 
       y = "Depth (m)") + 
  coord_cartesian(expand = FALSE) +
  theme_classic(base_size = 18) + 
  theme(legend.title.align = 0.5, 
        axis.text = element_text(colour = "black")) 


p

# ggsave(p, filename = here("Plots",
#                           "Heatmap-May-Oct_Papineau Lake.png"),
#        height = 6.37 * 2, width = 6.34 * 3)




