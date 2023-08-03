

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

temp_loggers <- read_rds(here::here("Saved R Data", 
                                    "papineau_lake_temp_loggers.rds"))

# 
# smooth_fit <- read_rds(here::here("Saved R Data", 
#                                   "loess_temp_raster_model.rds"))
# look at the temp_logger str ----

glimpse(temp_loggers)






# only do 2019 -----

tl <- temp_loggers %>% 
  filter(year %in% c(2019, 2020))





glimpse(tl)
tl <- tl %>% 
  filter(month >= 5 & month <= 10) 


# tl <- tl %>% 
#   mutate(hours = floor_date(date_time, unit = "hour"))
# 
# glimpse(tl)
# 
# 
# tl_h <- tl %>% 
#   group_by(receiver_name, depth, jdate, hours) %>% 
#   summarise(mht = mean(temp)) %>% 
#   ungroup()
# 
# glimpse(tl_h)


# unique(tl_h$jdate)


tl_d <- tl %>% 
  group_by(receiver_name, depth, jdate, daily, year) %>% 
  summarise(mdt  = mean(temp)) %>% 
  ungroup()

tl_d <- tl_d %>% 
  mutate(day = as.numeric(daily))


# 
# tl %>% 
#   summarise(min(daily), 
#             max(daily), n_distinct(daily))
# unique(tl$jdate)
# unique(tl$daily)
# unique( as.numeric(tl$daily - floor_date(tl$daily, unit = "years"), 
#                    unit = "days"))



# tl_yearless <- tl %>%
#   mutate(
#     day_in_year = as.numeric(tl$daily - floor_date(tl$daily, unit = "years"), 
#                              unit = "days"),
#     date_label = ymd("2019-01-01") + day_in_year
#   ) 
# 
# glimpse(tl_yearless)

# tl_2019 <- tl %>% 
#   filter(year == 2019)
# 
# glimpse(tl_2019)


# 
# tl_2019_s <- tl_2019 %>% 
#   group_by(daily, jdate, receiver_name, detph) %>% 
#   summarise()
# glimpse(tl)



# 
# length(unique(tl$jdate))
# acf(residuals(m))
# 
# 
# ?s
# length(tl_h$mht[-100])
# length(tl_h$mht[-1])
# glimpse(tl_h)
# 
# #lag 1
# cor(tl_h$mht[-100], tl_h$mht[-1])
# # lag high at 1 
# # 
# # lag 2  
# cor(tl_h$mht[-(99:100)], tl_h$mht[-(1:2)])
# # very high at 0.99
# 
# 
# acf(tl_h$mht, lag.max = 3, plot = FALSE)
# 
# 
# #lag 1
# cor(tl_d$mdt[-100], tl_d$mdt[-1])
# # lag high at 1 
# # 
# # lag 2  
# cor(tl_d$mdt[-(99:100)], tl_d$mdt[-(1:2)])
# # very high at 0.99
# 
# 
# acf(tl_h$mht, lag.max = 100, plot = FALSE)
# 
# 
# 
# acf(tl_d$mdt, lag.max = 200)
# 
# data(simdat)
# glimpse(simdat)
glimpse(tl_d)

tl_d <- tl_d %>% 
  group_by(receiver_name, depth, year) %>% 
  mutate(start_event = if_else(daily %in% ymd(c("2019-05-01", "2020-05-01")),
                               true = TRUE, 
                               false = FALSE)) %>% 
  ungroup()


n <- tl_d %>% 
  filter(daily == "2020-05-02")



tl_d <- tl_d %>% 
  group_by(receiver_name, depth) %>% 
  mutate(start_event_2 = if_else(jdate == 121, true = TRUE, 
                               false = FALSE)) %>% 
  ungroup()

# 
# tl_h <- tl_h %>% 
#   group_by(receiver_name, depth) %>% 
#   mutate(start_event = if_else(jdate == 121, true = TRUE, 
#                                false = FALSE)) %>% 
#   ungroup()
# 
# 
# 
# 
# 
# # model isn't seperating based on basin 
# 
# 
# m <- bam(mdt ~ jdate * depth * receiver_name + 
#            te(jdate, depth) + 
#            s(depth, bs = "cc", k = 6),
#            # s(receiver_name, k = 3) +
#          # s(jdate, bs = "cc", k = 185),
#          # rho = r1,
#          # AR.start = tl_d$start_event, 
#          # correlation = corAR1(value = 1),
#          data = tl_d)
# acf(residuals(m), lag.max = 1000)
# acf(resid_gam(m), lag.max = 10000)
# acf_resid(m)
# r1 <- start_value_rho(m, plot=TRUE,lag = 2)
# r1
# 
# m1 <- bam(mdt ~ jdate + depth + receiver_name + 
#             te(jdate, depth) + 
#            s(depth, bs = "cc", k = 6),
#            # s(receiver_name, k = 3) +
#            # s(jdate, bs = "cc", k = 185),
#          rho = r1,
#          AR.start = tl_d$start_event, 
#          # correlation = corAR1(value = 1),
#          data = tl_d)
# 
# 
# 
# 
# summary(m)
# 
# 
# 
# 
# acf(resid(m1), lag.max = 1000)
# acf_resid(m1, split_pred = c("receiver_name", "depth"), n = 6)
# acf_resid(resid_gam(m1))
# 
# acf_resid(m2, split_pred = c("receiver_name", "depth"))
# 
# acf_resid(m, split_pred = c("receiver_name", "depth"))
# 
# 
# 
# AIC(m)
# AIC(m1)
# 
# 
# 
# 
# beepr::beep()
# # gam.check(m)
# beepr::beep()
# 
# plot(m1)


# # 
# m1 <- loess(
#   temp ~ jdate + depth + receiver_name,
#   data = tl,
#   span = 0.2
# )

glimpse(tl_d)

rec_num <- unique(tl_d$receiver_name)

temp_list <- list()

# rec <- 1
for (rec in 1:length(rec_num)) {
  
  df <- tl_d %>% 
    filter(receiver_name == rec_num[rec]) %>% 
    arrange(depth)
  m <- bam(mdt ~ day * depth +
             te(day, depth) + 
             s(depth, bs = "cs", k = 5), 
           # s(receiver_name, k = 3) +
           # s(jdate, bs = "cc", k = 185),
           # rho = r1,
           # AR.start = tl_d$start_event, 
           # correlation = corAR1(value = 1),
           data = df)
  
  r1 <- start_value_rho(m, plot = FALSE, lag = 2)
  r1
  
  
  m1 <- bam(mdt ~ day * depth +   
              te(day, depth) + 
              s(depth, bs = "cs", k = 5), 
            # s(receiver_name, k = 3) +
            # s(jdate, bs = "cc", k = 185),
            rho = r1,
            AR.start = df$start_event, 
            # correlation = corAR1(value = 1),
            data = df)
  
  
  
  
  # acf(resid_gam(m1), lag.max = 500)
  
  
  temp_raster_smooth <- crossing(
    # tibble(date = unique(df$daily)),
    tibble(day = unique(df$day)),
    # tibble(year = unique(df$year)),
    tibble(receiver_name = unique(df$receiver_name)),
    # depths can now be any value
    tibble(depth = seq(1, 20, length.out = 100))
    
  ) %>%
    
    mutate(
      temp = predict(
        m1,
        newdata = tibble(
          # date = date,
          day = day,
          # day_in_year,  
          # year = year,
          receiver_name = receiver_name,
          depth = depth, 
        )
      )
    )
  
  
  

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
  
  
  temp_list[[rec]] <- temp_raster_smooth
  
  
  
}


glimpse(tl)

# library(fitdistrplus)
# 
# descdist(tl$mdt)
# hist(tl$mdt)



temp_merge <- do.call(rbind, temp_list) %>% 
  as_tibble()

temp_merge <- temp_merge %>% 
  mutate(daily = as.Date(day, origin = "1970-01-01"))
glimpse(temp_merge)


model_list <- list()


for (rec in 1:length(rec_num)) {
  
  df <- tl_d %>% 
    filter(receiver_name == rec_num[rec]) %>% 
    arrange(depth)
  m <- bam(mdt ~ day * depth +
             te(day, depth) + 
             s(depth, bs = "cs", k = 5), 
           # s(receiver_name, k = 3) +
           # s(jdate, bs = "cc", k = 185),
           # rho = r1,
           # AR.start = tl_d$start_event, 
           # correlation = corAR1(value = 1),
           data = df)
  
  r1 <- start_value_rho(m, plot = FALSE, lag = 2)
  r1
  
  
  m1 <- bam(mdt ~ day * depth +   
              te(day, depth) + 
              s(depth, bs = "cs", k = 5), 
            # s(receiver_name, k = 3) +
            # s(jdate, bs = "cc", k = 185),
            rho = r1,
            AR.start = df$start_event, 
            # correlation = corAR1(value = 1),
            data = df)
  
model_list[[rec]] <- m1


}



model_list


lapply(model_list, summary)















# temp_merge <- temp_merge %>% 
#   filter(!temp == is.na(temp))



temp_merge <- temp_merge %>%
  mutate(basin = case_when(receiver_name == 3 ~ "East Basin",
                           receiver_name == 5 ~ "West Basin",
                           receiver_name == 15 ~ "North Basin"))

temp_merge <- temp_merge %>%
  mutate(basin = factor(basin, levels = c("East Basin",
                                          "West Basin",
                                          "North Basin")))


# 
temp_merge <- temp_merge %>%
  mutate(jdate = yday(daily),
         year = year(daily),
         month = month(daily))

glimpse(temp_merge)





p <- ggplot(data = temp_merge, aes(x = jdate, y = depth, fill = temp)) + 
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
  # scale_fill_gradient2(
  #   midpoint = 15,
  #   high = scales::muted("red"),
  #   low = scales::muted("blue"),
  #   name = "Temperature (°C)",
  #   breaks = seq(5, 25, 5)
  # ) +
  # scale_fill_viridis_c(option = "B",
  #                      name = "Temperature (°C)",
  #                      breaks = seq(5, 25, 5)) +
  scale_fill_scico(
    palette =
    "roma"
    # "romaO"
    # "vik"
    # "vikO"
    ,
      midpoint = 15,
      name = "Temperature (°C)",
      breaks = seq(5, 25, 5),
      direction = -1,
  ) +
  labs(x = "Month", 
       y = "Depth (m)") + 
  coord_cartesian(expand = FALSE) +
  theme_classic(base_size = 18) + 
  theme(legend.title.align = 0.5, 
        axis.text = element_text(colour = "black")) 


p



# # 
# ggsave(p, filename = here("Plots",
#                           "Heatmap-May-Oct_Papineau Lake_GAM_year_red_blue.png"),
#        height = 6.37 * 2, width = 6.34 * 3)




