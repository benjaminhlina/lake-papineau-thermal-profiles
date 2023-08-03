
# load packages ----
{
  library(broom)
  library(dplyr)
  library(fishualize)
  library(ggplot2)
  library(glmmTMB)
  library(gganimate)
  library(lme4)
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
  library(tweenr)
  library(transformr)
  library(tidyr)
  library(tibble)
  source(here("R Scripts", 
              "julian_date_reorder.r"))
}


# load logger rds ----

temp_loggers <- read_rds(here::here("Saved R Data", 
                                    "papineau_lake_temp_loggers.rds"))

# 
# smooth_fit <- read_rds(here::here("Saved R Data", 
#                                   "loess_temp_raster_model.rds"))
# look at the temp_logger str ----

glimpse(temp_loggers)

unique(temp_loggers$year)



# only do 2019 -----

tl <- temp_loggers 
# filter(year %in% c(2018, 2019, 2020, 2021))





glimpse(tl)
tl <- tl %>% 
  # filter(month >= 5 & month <= 10) %>% 
  mutate(basin = factor(case_when(receiver_name == 3 ~ "East", 
                                  receiver_name == 5 ~ "West", 
                                  receiver_name == 15 ~ "North"), 
                        levels = c("East", 
                                   "West", 
                                   "North"))
  )


tl
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
  group_by(receiver_name, basin, depth, jdate, month, daily, year) %>% 
  summarise(mdt  = mean(temp), 
            sem = sd(temp)/ sqrt(n())) %>% 
  ungroup()

glimpse(tl_d)


tl_dd <- tl %>% 
  group_by(receiver_name, basin, depth, jdate, month, daily, year) %>% 
  summarise(mdt  = mean(temp), 
            sem = sd(temp)/ sqrt(n()), 
            min_temp = min(temp), 
            max_temp = max(temp), 
            med_temp = median(temp), 
            # ran_temp = slice_sample(., n = 1, replace = FALSE) %>% 
            #   .$temp
  ) %>% 
  ungroup() %>% 
  mutate(doy = days(daily, 
                    # end = "02-28")
  )) %>% 
  arrange(doy)


tl_dd

write_rds(tl_dd, file = here("Saved Data", 
                             "daily_temp_range_measured.rds"))
# tl_dm <- tl_d %>% 
#   group_by(basin, month) %>% 
#   summarise(
#     mes = mean(mdt)
#   ) %>% 
#   ungroup()




tl_d <- tl_d %>% 
  mutate(doy = days(daily, 
                    # end = "02-28")
  )) %>% 
  arrange(doy)


tl_d %>% 
  filter(jdate == 121)




glimpse(tl_d)

tl_d <- tl_d %>% 
  group_by(receiver_name, basin, depth, year) %>% 
  mutate(start_event = if_else(doy  == 1, true = TRUE, 
                               false = FALSE)) %>% 
  ungroup() %>% 
  arrange(year, start_event)


fitdistrplus::descdist(tl_d$mdt)

ggplot(data = tl_d, aes(x = mdt)) + 
  geom_histogram()



# ggplot(data = tl_d, aes(x = doy, y = depth)) + 
#   geom_point(aes(colour = mdt), size = 3) + 
#   lemon::facet_rep_wrap(.~ receiver_name,
#                         repeat.tick.labels = TRUE, scales = "free_y") +
#   scale_y_reverse(
#     breaks = rev(seq(0, 20, 2.5)),
#     # limits = rev(c(0, 20))
#   ) + 
#   # 
#   
#   scale_x_continuous(
#     breaks = c(1, 31, 61, 92, 123, 153, 
#                184, 214, 245, 276, 304, 335),
#     limits = c(1, 366),
#     labels = c( "May", "Jun",
#                 "Jul", "Aug", "Sept",
#                 "Oct", "Nov", "Dec",
#                 "Jan", "Feb", "Mar", 
#                 "Apr")) +
#   scale_colour_gradient2(
#     midpoint = 15, 
#     high = scales::muted("red"), 
#     low = scales::muted("blue"), 
#     name = "Temperature (°C)",
#     breaks = seq(5, 25, 5)
#   ) + 
#   # scale_fill_viridis_c(option = "C", 
#   #                      name = "Temperature (°C)",
#   #                      breaks = seq(5, 25, 5)) +
#   # scale_fill_gradient2(mi3 = scales::muted("red"),
#   #                      low = scales::muted("blue"),
#   #                      name = expression("Temperature (" *degree*C*")")) +
#   
#   labs(x = "Month", 
#        y = "Depth (m)") + 
#   coord_cartesian(expand = FALSE) +
#   theme_classic(base_size = 18) + 
#   theme(legend.title.align = 0.5, 
#         axis.text = element_text(colour = "black")) 

unique(tl_d$year)

tl_d <- tl_d %>% 
  mutate(year = as.factor(as.character(year)), 
         receiver_name = as.factor(as.character(receiver_name)))

tl_d
gammas <- fitdistrplus::fitdist(tl_d$mdt, distr = "gamma", method = "mme")
plot(gammas)
norm <- fitdistrplus::fitdist(tl_d$mdt, distr = "norm", method = "mle")
plot(norm)


# model -----
m <- bam(mdt ~ depth * basin + 
           s(doy,
             by = basin,
             k = 15,
             bs = "cc") +
           s(doy,
             by = depth,
             k = 12,
             bs = "cc") +
           s(depth,
             bs = "cr",
             by = basin,
             k = 6) +
           s(year, by = basin, bs = "re", k = 4) +
           ti(doy, depth,
              by = basin,
              k = c(15, 6),
              bs = c("cc", "cr")
           ),
         family = Gamma(link = "log"),
         method = "fREML", 
         data = tl_d, 
         select = TRUE, 
         discrete = TRUE
)

r1 <- start_value_rho(m, plot = TRUE, lag = 2)
r1

acf(resid_gam(m), main="acf(resid(m))")
par(mfrow = c(2, 2))
gam.check(m)
summary(m)
beepr::beep()

length(unique(tl_d$jdate))

glimpse(tl_d)


# ------- cprrect model ------

m1 <- bam(mdt ~ depth * basin + 
            s(doy,
              by = basin,
              k = 15,
              bs = "cc") +
            s(doy,
              by = depth,
              k = 14,
              bs = "cc") +
            s(depth,
              bs = "cr",
              by = basin,
              k = 6) +
            s(year, by = basin, bs = "re", k = 4) +
            ti(doy, depth,
               by = basin,
               k = c(15, 6),
               bs = c("cc", "cr")
            ),
          # family = gaussian(link = "inverse"),
          family = Gamma(link = "log"),
          method = "fREML",
          # knots = list(doy = c(15, 351)),
          data = tl_d,
          select = TRUE,
          discrete = TRUE,
          rho = r1,
          AR.start = tl_d$start_event
)
# m2 <- bam(mdt ~ depth * basin + 
#             s(jdate,
#               by = basin,
#               k = 15,
#               bs = "cc") +
#             s(jdate,
#               by = depth,
#               k = 14,
#               bs = "cc") +
#             s(depth,
#               bs = "cr",
#               by = basin,
#               k = 6) +
#             s(year, bs = "re", k = 4) +
#             ti(jdate, depth,
#                by = basin,
#                k = c(15, 6),
#                bs = c("cc", "cr")
#             ),
#           # family = gaussian(link = "inverse"),
#           family = Gamma(link = "log"),
#           method = "fREML",
#           data = tl_d,
#           select = TRUE,
#           discrete = TRUE,
#           rho = r1,
#           AR.start = tl_d 
#           $start_event
# )


r1 <- start_value_rho(m1, plot=TRUE)
acf_resid(m1)
gam.check(m1)
# plot(m1)
# ?bam
summary(m1)
# beepr::beep()


summary_param <- tidy(m1,parametric = TRUE)
summary_smooth <- tidy(m1,parametric = FALSE)
summary_param
summary_smooth
anova.gam(m1)
m1_overall <- anova.gam(m1)


glance(m1)
overall_parm <- m1_overall$pTerms.table %>% 
  as_tibble(rownames = "terms") %>% 
  clean_names()

overall_parm



# 
# overall_parm %>%
#   openxlsx::write.xlsx(here("Results",
#                             "GAMM_water_temp_oveall_parm_jan_dec.xlsx"))
# 
# summary_param %>%
#   openxlsx::write.xlsx(here("Results",
#                             "GAMM_water_temp_param_jan_dec.xlsx"))
# summary_smooth %>%
#   openxlsx::write.xlsx(here("Results",
#                             "GAMM_water_temp_smooth_jan_dec.xlsx"))
# glance(m1) %>%
#   openxlsx::write.xlsx(here("Results",
#                             "GAMM_model_fit_jan_dec.xlsx"))

temp_raster_smooth <- crossing(
  # tibble(date = unique(df$daily)),
  tibble(doy = unique(tl_d$doy)),
  tibble(year = unique(tl_d$year)),
  tibble(basin = unique(tl_d$basin)),
  # depths can now be any value
  tibble(depth = seq(1, 20, length.out = 300))
  
) %>%
  
  mutate(
    temp = predict.gam(
      m1,
      newdata = tibble(
        # date = date,
        doy = doy,
        # day_in_year,  
        year = year,
        basin = basin,
        depth = depth, 
      )
    )
  )


glimpse(temp_raster_smooth)
temp_raster_smooth <- temp_raster_smooth %>%
  mutate(temp = exp(1) ^ temp)



lmes <- glmmTMB(mdt ~ depth * basin + (1 | year) + 
                  ar1(depth * basin + 0 | year),
                data = tl_d, 
                family = Gamma(link = "log")
)


# plot(lmes)

res1 <- DHARMa::simulateResiduals(lmes)
plot(res1)

car::Anova(lmes)

summary(lmes)

# 
# glance(lmes) %>%
#   openxlsx::write.xlsx(here("Results",
#                             "GLMM_water_temp_model_fit_jan_dec.xlsx"))
# 
# tidy(lmes) %>%
#   openxlsx::write.xlsx(here("Results",
#                             "GLMM_water_temp_individual_jan_dec.xlsx"))
# 
# tidy(car::Anova(lmes)) %>%
#   openxlsx::write.xlsx(here("Results",
#                             "GLMM_water_temp_overall_jan_dec.xlsx"))

temp_sums <- temp_raster_smooth %>% 
  group_by(doy, year, basin) %>% 
  summarise(
    mean_temp = mean(temp), 
    sem_temp = sd(temp) / sqrt(n()),
    min_temp = min(temp), 
    max_temp = max(temp), 
    med_temp = median(temp), 
    # rand_temp = sample_n(temp)
  ) %>% 
  ungroup()

temp_sums


write_rds(temp_sums, here("Saved Data", 
                          "daily_temp_range.rds"))


temp_surface <- temp_raster_smooth %>% 
  filter(depth == 1) 

cols <- rev(rainbow(6)[-6])

# glimpse(temp_merge)


p <- ggplot() + 
  geom_tile(data = temp_raster_smooth, 
            aes(x = doy, y = depth, fill = temp)) +
  # geom_point(data = tl_d,
  #            aes(x = jdate, y = depth, colour = mdt), size = 3) +
  lemon::facet_rep_wrap(.~ basin,
                        repeat.tick.labels = TRUE, scales = "free_y") +
  scale_y_reverse(
    breaks = rev(seq(0, 20, 2.5)),
    # limits = rev(c(0, 20))
  ) + 
  scale_x_continuous(
    breaks = c(1, 31, 61, 92, 123, 153, 
               184, 214, 245, 276, 304, 335),
    limits = c(1, 366),
    labels = c("May", "Jun",
               "Jul", "Aug", "Sept",
               "Oct", "Nov", "Dec",
               "Jan","Feb", "Mar", 
               "Apr")) +
  # scale_x_continuous(
  #   breaks = c(121, 152, 182, 213, 244, 274, 304), 
  #   limits = c(121, 304),
  #   labels = c("May", "Jun",
  #              "Jul", "Aug", "Sept",
  #              "Oct", "Nov")) +
  # scale_fill_gradient2(
  #   midpoint = 15,
  #   mid = "white",
  #   
  #   high = scales::muted("red"),
#   low = scales::muted("blue"),
#   name = "Temperature (°C)",
#   breaks = seq(5, 30, 5)
# ) +

scale_fill_gradientn(colours = cols,
                     name = "Temperature (°C)",
                     breaks = seq(0, 30, 5)
) +
  # scale_colour_gradient2(
  #   midpoint = 15, 
  #   high = scales::muted("red"), 
  #   low = scales::muted("blue"), 
  #   name = "Temperature (°C)",
  #   breaks = seq(5, 25, 5)
  # ) + 
  # scale_fill_viridis_c(option = "C", 
  #                      name = "Temperature (°C)",
  #                      breaks = seq(5, 25, 5)) +
  
labs(x = "Month",
     y = "Depth (m)") + 
  coord_cartesian(expand = FALSE) +
  theme_classic(base_size = 18) + 
  theme(legend.title.align = 0.5, 
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(colour = "black")) 


p

# temp_list

# glimpse(temp_raster_smooth)


ggsave(p, filename = here("Plots",
                          "Heatmap-Jan-Dec_GAM_rainbow_hundrith.png"),
       height = 6.37 * 1.75, width = 6.34 * 3)

# 
# 
# ggsave(p, filename = here("Plots",
#                           "Heatmap-May-Oct_Papineau Lake_GAM_red_blue.pdf"),
#        height = 6.37 * 2, width = 6.34 * 3)

# ---- thermocline depth ---- 

glimpse(temp_raster_smooth)

thermocline <- temp_raster_smooth %>% 
  filter(between(doy, 14, 183) & 
           between(temp, 14.75, 15.25)) %>% 
  group_by(doy, year, basin) %>% 
  summarise(
    max_depth = max(depth)
  ) %>% 
  ungroup()
  # group_by(doy, basin) %>% 
  # # summarise(
  # #   mean_max_d = mean(max_depth), 
  # #   sem = sd(max_depth) / sqrt(n())
  # # ) %>% 
  # ungroup()

thermocline

tapply(thermocline$mean_max_d, thermocline$basin, max)
ggplot(data = thermocline, aes(x = doy, y = max_depth, colour = basin)) + 
  geom_point(size = 3) + 
  geom_line(linewidth = 1) + 
  scale_y_reverse(breaks = rev(seq(0, 7, 1))) + 
  facet_wrap(. ~ year) + 
  scale_colour_viridis_d(
    begin = 0.25, 
    end = 0.75, 
    name = "Basin", 
    option = "B"
  ) + 
  theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank()
  ) + 
  labs(
    x = "Date", 
    y = "Depth (m)"
  )

# ---- Model selection ----


m2 <- update(m, ~ depth * basin + 
               s(doy,
                 by = basin,
                 k = 15,
                 bs = "cc") +
               s(doy,
                 by = depth,
                 k = 12,
                 bs = "cc") +
               s(depth,
                 bs = "cr",
                 by = basin,
                 k = 6) +
               s(year, by = basin, bs = "re", k = 4)
             # ti(doy, depth,
             #    by = basin,
             #    k = c(15, 6),
             #    bs = c("cc", "cr")
)

m3 <- update(m, ~ depth * basin + 
               s(doy,
                 by = basin,
                 k = 15,
                 bs = "cc") +
               s(doy,
                 by = depth,
                 k = 12,
                 bs = "cc") +
               s(depth,
                 bs = "cr",
                 by = basin,
                 k = 6)
             # s(year, by = basin, bs = "re", k = 4)
             # ti(doy, depth,
             #    by = basin,
             #    k = c(15, 6),
             #    bs = c("cc", "cr")
)

m4 <- update(m, ~ depth * basin + 
               s(doy,
                 by = basin,
                 k = 15,
                 bs = "cc") +
               s(doy,
                 by = depth,
                 k = 12,
                 bs = "cc")
             # s(depth,
             #   bs = "cr",
             #   by = basin,
             #   k = 6) +
             # s(year, by = basin, bs = "re", k = 4)
             # ti(doy, depth,
             #    by = basin,
             #    k = c(15, 6),
             #    bs = c("cc", "cr")
)

m5 <- update(m, ~ depth * basin + 
               s(doy,
                 by = basin,
                 k = 15,
                 bs = "cc")
             # s(doy,
             #   by = depth,
             #   k = 12,
             #   bs = "cc") +
             # s(depth,
             #   bs = "cr",
             #   by = basin,
             #   k = 6) +
             # s(year, by = basin, bs = "re", k = 4)
             # ti(doy, depth,
             #    by = basin,
             #    k = c(15, 6),
             #    bs = c("cc", "cr")
)

m6 <- update(m, ~ depth * basin  
             # s(doy,
             #   by = basin,
             #   k = 15,
             #   bs = "cc") +
             # s(doy,
             #   by = depth,
             #   k = 12,
             #   bs = "cc") +
             # s(depth,
             #   bs = "cr",
             #   by = basin,
             #   k = 6) +
             # s(year, by = basin, bs = "re", k = 4)
             # ti(doy, depth,
             #    by = basin,
             #    k = c(15, 6),
             #    bs = c("cc", "cr")
)

m7 <- update(m, ~ 
               # depth * 
               # basin + 
               s(doy,
                 by = basin,
                 k = 15,
                 bs = "cc") +
               s(doy,
                 by = depth,
                 k = 12,
                 bs = "cc") +
               s(depth,
                 bs = "cr",
                 by = basin,
                 k = 6) +
               s(year, by = basin, bs = "re", k = 4) +
               ti(doy, depth,
                  by = basin,
                  k = c(15, 6),
                  bs = c("cc", "cr")
               )
)

m8 <- update(m, ~ 
               depth + 
               # basin + 
               s(doy,
                 by = basin,
                 k = 15,
                 bs = "cc") +
               s(doy,
                 by = depth,
                 k = 12,
                 bs = "cc") +
               s(depth,
                 bs = "cr",
                 by = basin,
                 k = 6) +
               s(year, by = basin, bs = "re", k = 4) + 
               ti(doy, depth,
                  by = basin,
                  k = c(15, 6),
                  bs = c("cc", "cr"))
)

m9 <- update(m, ~ basin + 
               s(doy,
                 by = basin,
                 k = 15,
                 bs = "cc") +
               s(doy,
                 by = depth,
                 k = 12,
                 bs = "cc") +
               s(depth,
                 bs = "cr",
                 by = basin,
                 k = 6) +
               s(year, by = basin, bs = "re", k = 4)
             # ti(doy, depth,
             #    by = basin,
             #    k = c(15, 6),
             #    bs = c("cc", "cr")
)

m10 <- update(m, ~ depth * basin + 
                s(doy,
                  by = basin,
                  k = 15,
                  bs = "cc") +
                s(doy,
                  by = depth,
                  k = 12,
                  bs = "cc") +
                s(depth,
                  bs = "cr",
                  by = basin,
                  k = 6) +
                # s(year, by = basin, bs = "re", k = 4)
                ti(doy, depth,
                   by = basin,
                   k = c(15, 6),
                   bs = c("cc", "cr"))
)

m11 <- update(m, ~ depth * basin + 
                s(doy,
                  by = basin,
                  k = 15,
                  bs = "cc") +
                s(doy,
                  by = depth,
                  k = 12,
                  bs = "cc") +
                # s(depth,
                #   bs = "cr",
                #   by = basin,
                #   k = 6) +
                # s(year, by = basin, bs = "re", k = 4)
                ti(doy, depth,
                   by = basin,
                   k = c(15, 6),
                   bs = c("cc", "cr"))
)

m12 <- update(m, ~ depth * basin + 
                s(doy,
                  by = basin,
                  k = 15,
                  bs = "cc") +
                # s(doy,
                #   by = depth,
                #   k = 12,
                #   bs = "cc") +
                # s(depth,
                #   bs = "cr",
                #   by = basin,
                #   k = 6) +
                # s(year, by = basin, bs = "re", k = 4)
                ti(doy, depth,
                   by = basin,
                   k = c(15, 6),
                   bs = c("cc", "cr"))
)

m13 <- update(m, ~ depth * basin + 
                # s(doy,
                #   by = basin,
                #   k = 15,
                #   bs = "cc") +
                # s(doy,
                #   by = depth,
                #   k = 12,
                #   bs = "cc") +
                # s(depth,
                #   bs = "cr",
                #   by = basin,
              #   k = 6) +
              # s(year, by = basin, bs = "re", k = 4)
              ti(doy, depth,
                 by = basin,
                 k = c(15, 6),
                 bs = c("cc", "cr"))
)

m14 <- update(m, ~ 
                depth +
                # * 
                # basin  
                # s(doy,
                #   by = basin,
                #   k = 15,
                #   bs = "cc") +
                # s(doy,
                #   by = depth,
                #   k = 12,
                #   bs = "cc") +
                # s(depth,
              #   bs = "cr",
              #   by = basin,
              #   k = 6) +
              # s(year, by = basin, bs = "re", k = 4)
              ti(doy, depth,
                 by = basin,
                 k = c(15, 6),
                 bs = c("cc", "cr"))
)

m15 <- update(m, ~ 
                # depth +  
                # * 
                #  basin + 
                # s(doy,
                #   by = basin,
                #   k = 15,
                #   bs = "cc") +
                # s(doy,
                #   by = depth,
                #   k = 12,
                #   bs = "cc") +
              # s(depth,
              #   bs = "cr",
              #   by = basin,
              #   k = 6) +
              # s(year, by = basin, bs = "re", k = 4)
              ti(doy, depth,
                 by = basin,
                 k = c(15, 6),
                 bs = c("cc", "cr"))
)

m16 <- update(m, ~ depth * basin + 
                s(doy,
                  by = basin,
                  k = 15,
                  bs = "cc") +
                # s(doy,
                #   by = depth,
                #   k = 12,
                #   bs = "cc") +
                s(depth,
                  bs = "cr",
                  by = basin,
                  k = 6) 
              # s(year, by = basin, bs = "re", k = 4)
              # ti(doy, depth,
              #    by = basin,
              #    k = c(15, 6),
              #    bs = c("cc", "cr")
)

m17 <- update(m, ~ depth * basin + 
                # s(doy,
                #   by = basin,
                #   k = 15,
                #   bs = "cc") +
                # s(doy,
                #   by = depth,
                #   k = 12,
                #   bs = "cc") +
                s(depth,
                  bs = "cr",
                  by = basin,
                  k = 6) 
              # s(year, by = basin, bs = "re", k = 4)
              # ti(doy, depth,
              #    by = basin,
              #    k = c(15, 6),
              #    bs = c("cc", "cr")
)

m18 <- update(m, ~ depth * basin + 
                s(doy,
                  by = basin,
                  k = 15,
                  bs = "cc") +
                # s(doy,
                #   by = depth,
                #   k = 12,
                #   bs = "cc") +
                s(depth,
                  bs = "cr",
                  by = basin,
                  k = 6) 
              # s(year, by = basin, bs = "re", k = 4)
              # ti(doy, depth,
              #    by = basin,
              #    k = c(15, 6),
              #    bs = c("cc", "cr")
)

m16 <- update(m, ~ depth * basin + 
                s(doy,
                  by = basin,
                  k = 15,
                  bs = "cc") +
                # s(doy,
                #   by = depth,
                #   k = 12,
                #   bs = "cc") +
                s(depth,
                  bs = "cr",
                  by = basin,
                  k = 6) 
              # s(year, by = basin, bs = "re", k = 4)
              # ti(doy, depth,
              #    by = basin,
              #    k = c(15, 6),
              #    bs = c("cc", "cr")
)

m16 <- update(m, ~ depth * basin + 
                s(doy,
                  by = basin,
                  k = 15,
                  bs = "cc") +
                # s(doy,
                #   by = depth,
                #   k = 12,
                #   bs = "cc") +
                s(depth,
                  bs = "cr",
                  by = basin,
                  k = 6) 
              # s(year, by = basin, bs = "re", k = 4)
              # ti(doy, depth,
              #    by = basin,
              #    k = c(15, 6),
              #    bs = c("cc", "cr")
)

m16 <- update(m, ~ depth * basin + 
                s(doy,
                  by = basin,
                  k = 15,
                  bs = "cc") +
                # s(doy,
                #   by = depth,
                #   k = 12,
                #   bs = "cc") +
                s(depth,
                  bs = "cr",
                  by = basin,
                  k = 6) 
              # s(year, by = basin, bs = "re", k = 4)
              # ti(doy, depth,
              #    by = basin,
              #    k = c(15, 6),
              #    bs = c("cc", "cr")
)

m16 <- update(m, ~ depth * basin + 
                s(doy,
                  by = basin,
                  k = 15,
                  bs = "cc") +
                # s(doy,
                #   by = depth,
                #   k = 12,
                #   bs = "cc") +
                s(depth,
                  bs = "cr",
                  by = basin,
                  k = 6) 
              # s(year, by = basin, bs = "re", k = 4)
              # ti(doy, depth,
              #    by = basin,
              #    k = c(15, 6),
              #    bs = c("cc", "cr")
)




# create model list for model selection ------
model_list <- list(m, m1, m2, 
                   m3, m4, m5, m6, m7,
                   m8, m9, m10, m11, m12, 
                   m13, m14, m15, m16
                   # m19, m20, m21
)
# give the elements useful names
names(model_list) <- c("m", 
                       "m1", "m2",
                       "m3", "m4", "m5", "m6", "m7",
                       "m8", "m9", "m10", "m11", 
                       "m12", "m13", "m14", "m15", 
                       "m16"
                       # "m19", "m20", "m21"
)
glance(m)

# get the summaries using `lapply

summary_list <- lapply(model_list, function(x) tidy(x, parametric = TRUE))
glance_list <- lapply(model_list, glance)

glance_summary <- map_df(glance_list, ~as.data.frame(.x), .id = "id") %>% 
  mutate(model = lapply(model_list, formula) %>%
           as.character() 
  ) %>% 
  dplyr::select(model, id:df.residual) %>% 
  arrange(AIC)


glance_summary

glance_summary <- glance_summary %>% 
  mutate(model = case_when(id %in% c("m1") ~ paste(model, 
                                                   "ACF", 
                                                   sep = " + "),
                           TRUE ~ model), 
         delta_AIC = AIC - first(AIC),
         AIC_weight = exp(-0.5 * delta_AIC) / sum(exp(-0.5 * delta_AIC))
  ) %>% 
  dplyr::select(model:AIC, delta_AIC, AIC_weight, BIC:df.residual)


glance_summary


# 
glance_summary %>%
  openxlsx::write.xlsx(here("Results",
                            "Mode_Selection_Jan-Dec.xlsx"))


# determine seasonality time points when things become isothermal 
# 
# 
# ------ thermal habitat change with 0.34 -----

temp_raster_smooth <- crossing(
  temp_raster_smooth,
  tibble(year_pred = seq(2025, 2075, 10))
) %>% 
  mutate(
    pred_temp = case_when(
      year_pred == 2035 ~ temp + 0.34, 
      year_pred == 2045 ~ temp + 0.34 * 2, 
      year_pred == 2055 ~ temp + 0.34 * 3, 
      year_pred == 2065 ~ temp + 0.34 * 4, 
      year_pred == 2075 ~ temp + 0.34 * 5, 
      TRUE ~ temp
    )
  )

temp_raster_smooth

p10 <- ggplot() + 
  geom_tile(data = temp_raster_smooth, 
            aes(x = doy, y = depth, fill = pred_temp)) +
  
  facet_grid(year_pred ~ basin) +
  scale_y_reverse(
    breaks = rev(seq(0, 20, 2.5)),
    # limits = rev(c(0, 20))
  ) + 
  scale_x_continuous(
    breaks = c(1, 31, 61, 92, 123, 153, 
               184, 214, 245, 276, 304, 335),
    limits = c(1, 366),
    labels = c("May", "Jun",
               "Jul", "Aug", "Sept",
               "Oct", "Nov", "Dec",
               "Jan","Feb", "Mar", 
               "Apr")) +
  
  scale_fill_gradientn(colours = cols,
                       name = "Temperature (°C)",
                       breaks = seq(0, 30, 5)
  ) +
  
  labs(x = "Month",
       y = "Depth (m)") + 
  coord_cartesian(expand = FALSE) +
  theme_classic(base_size = 18) + 
  theme(legend.title.align = 0.5, 
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(colour = "black")) 
p10

ggsave(p10, filename = here("Plots",
                          "Heatmap-Jan-Dec_GAM_rainbow_FINAL_clim_chg.png"),
       height = 6.37 * 2.5, width = 6.34 * 2.5)
