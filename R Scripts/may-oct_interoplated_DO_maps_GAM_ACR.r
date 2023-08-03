

# load packages ----
library(broom)
library(dplyr)
library(fishualize)
library(ggplot2)
library(gganimate)
library(itsadug)
library(ggvis)
library(here)
library(mgcv)
library(ggthemes)
library(ggridges)
library(lubridate)
library(janitor)
library(purrr)
library(readr)
library(stringr)
library(scico)
library(tweenr)
library(transformr)
library(tidyr)
library(tibble)

# load logger rds ----


# smooth_fit <- read_rds(here::here("Saved R Data", 
#                                   "loess_temp_raster_model.rds"))
# look at the temp_logger str ----

glimpse(temp_loggers)


# bring in DO data -----

do <- read_csv(here("Data", 
                    "do_profile.csv"))



glimpse(do)



# combine date and time 
do <- do %>% 
  mutate(date_time = dmy_hms(paste(date, time, sep = " "))
  ) %>% 
  select(date_time, date:temp_c) %>% 
  mutate(basin = factor(basin, levels = c("East", 
                                          "West", 
                                          "North")), 
         jdate = yday(date_time), 
         ) %>% 
  group_by(basin, depth_m) %>% 
  mutate(start_event = if_else(jdate == 143, true = TRUE, 
                               false = FALSE)) %>% 
  ungroup()








# 
# do %>% 
#   filter(depth_m <= 20) %>% 
#   ggplot(aes(y = depth_m, x = date_time)) +
#   geom_point(size = 4, aes(fill = do_mg_l), colour = "black", 
#              stroke = 0.8, shape = 21) + 
#   lemon::facet_rep_wrap(.~ basin,
#                         repeat.tick.labels = TRUE, scales = "free_y") +
#   scale_y_reverse(
#     breaks = rev(seq(0, 20, 2.5)),
#     # limits = rev(c(0, 20))
#   ) + 
#   scale_fill_gradient2(
#     midpoint = 8, 
#     low = scales::muted("red"), 
#     high = scales::muted("blue"), 
#     name = expression(paste("Dissolved Oxygen (mg ", L^-1, ")")),
#     breaks = seq(0, 16, 2)
#   ) 
# 




m <- bam(do_mg_l ~ jdate * depth_m * basin +
           ti(jdate, depth_m, by = loc,
              bs = c("cs", "cs"), 
              k = c(3, 21)) + 
           
           s(depth_m, by = loc, bs = "cs", k = 22), 
         method = "fREML", 
         family = Gamma(link = "identity"),
         # family = gaussian(link = "inverse"),
         data = do)


par(mfrow = c(2, 2))
gam.check(m)
summary(m)
broom.mixed::glance(m)


r1 <- start_value_rho(m, plot = FALSE, lag = 2)
r1

m1 <- bam(do_mg_l ~ jdate * depth_m * basin +  
            ti(jdate, depth_m, by = loc, 
               bs = c("cs", "cs"), 
               k = c(3, 21)) + 
            
            s(depth_m, by = loc,
              bs = "cs", k = 22), 
          method = "fREML",
          rho = r1,
          AR.start = do$start_event, 
          
          family = Gamma(link = "identity"),
          # correlation = corAR1(value = 1),
          data = do)


gam.check(m1)
summary(m1)
broom.mixed::glance(m1)





do_raster_smooth <- crossing(
  # tibble(date = unique(df$daily)),
  tibble(jdate = seq(143, 230, 1)),
  # tibble(year = unique(df$year)), 
  tibble(basin = unique(do$basin),
         loc = unique(do$loc)),
  # depths can now be any value
  tibble(depth_m = seq(1, 20, length.out = 100))
  
) %>%
  
  mutate(
    do = predict(
      m1,
      newdata = tibble(
        # date = date,
        jdate = jdate,
        # day_in_year,  
        # year = year,
        loc = loc,
        basin = basin,
        depth_m = depth_m, 
      )
    )
  )





glimpse(do_raster_smooth)




ggplot(data = do_raster_smooth, 
       aes(y = depth_m, x = jdate, fill = do)) +
  geom_tile() + 
  # geom_point(size = 4, aes(fill = do_mg_l), colour = "black", 
  #            stroke = 0.8, shape = 21) + 
  lemon::facet_rep_wrap(.~ basin,
                        repeat.tick.labels = TRUE, scales = "free_y") +
  scale_y_reverse(
    # breaks = rev(seq(0, 20, 2.5)),
    # limits = rev(c(0, 20))
  ) + 
  scale_fill_gradient2(
    midpoint = 8.5, 
    low = scales::muted("red"), 
    high = scales::muted("blue"), 
    name = expression(paste("Dissolved Oxygen (mg ", L^-1, ")")),
    breaks = seq(0, 16, 2)
  ) +   
  scale_x_continuous(
    breaks = c(143, 152, 182, 213 
               # 244, 274, 304
               ), 
    limits = c(143, 
               # 304
               230
               ),
    labels = c("May", "Jun",
               "Jul", "Aug"
               # "Sept",
               # "Oct", "Nov"
               )) + 
  coord_cartesian(expand = FALSE) +
  theme_bw(base_size = 18) + 
  theme(legend.title.align = 0.5, 
        panel.grid = element_blank(), 
        axis.text = element_text(colour = "black")) + 
  labs(x = "Date", 
       y = "Depth (m)")
  
  
