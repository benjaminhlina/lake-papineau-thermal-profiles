
# load packages ----
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
  filter(month >= 5 & month <= 10) %>% 
  mutate(basin = factor(case_when(receiver_name == 3 ~ "East", 
                                  receiver_name == 5 ~ "West", 
                                  receiver_name == 15 ~ "North"), 
                        levels = c("East", 
                                   "West", 
                                   "North")
  )
  )



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
  group_by(receiver_name, basin, depth, jdate, daily, year) %>% 
  summarise(mdt  = mean(temp)) %>% 
  ungroup()

glimpse(tl_d)



glimpse(tl_d)

tl_d <- tl_d %>% 
  group_by(receiver_name, basin, depth, year) %>% 
  mutate(start_event = if_else(jdate == 121, true = TRUE, 
                               false = FALSE)) %>% 
  ungroup() %>% 
  arrange(year, start_event)


fitdistrplus::descdist(tl_d$mdt)

ggplot(data = tl_d, aes(x = mdt)) + 
  geom_histogram()



ggplot(data = tl_d, aes(x = jdate, y = depth)) + 
  geom_point(aes(colour = mdt), size = 3) + 
  lemon::facet_rep_wrap(.~ receiver_name,
                        repeat.tick.labels = TRUE, scales = "free_y") +
  scale_y_reverse(
    breaks = rev(seq(0, 20, 2.5)),
    # limits = rev(c(0, 20))
  ) + 
  
  scale_x_continuous(
    breaks = c(121, 152, 182, 213, 244, 274, 304), 
    limits = c(121, 304),
    labels = c("May", "Jun",
               "Jul", "Aug", "Sept",
               "Oct", "Nov")) +
  scale_colour_gradient2(
    midpoint = 15, 
    high = scales::muted("red"), 
    low = scales::muted("blue"), 
    name = "Temperature (°C)",
    breaks = seq(5, 25, 5)
  ) + 
  # scale_fill_viridis_c(option = "C", 
  #                      name = "Temperature (°C)",
  #                      breaks = seq(5, 25, 5)) +
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
           s(jdate,
             by = basin,
             k = 13,
             bs = "cc") +
           s(jdate,
             by = depth,
             k = 12,
             bs = "cc") +
           s(depth,
             bs = "cr",
             by = basin,
             k = 6) +
           s(year, by = basin, bs = "re", k = 4) +
           ti(jdate, depth,
              by = basin,
              k = c(14, 6),
              bs = c("cc", "cr")
           ),
         # family = Gamma(link = "inverse"),
         method = "fREML", 
         data = tl_d, 
         select = TRUE, 
         discrete = TRUE
)

r1 <- start_value_rho(m, plot = TRUE, lag = 2)
r1
r1 <- start_value_rho(m1, plot=TRUE)
acf(resid_gam(m), main="acf(resid(m))")
par(mfrow = c(2, 2))
gam.check(m)
summary(m)
beepr::beep()

length(unique(tl_d$jdate))

glimpse(tl_d)


# ------- cprrect model ------

m1 <- bam(mdt ~ depth * basin + 
            s(jdate,
              by = basin,
              k = 15,
              bs = "cc") +
            s(jdate,
              by = depth,
              k = 14,
              bs = "cc") +
            s(depth,
              bs = "cr",
              by = basin,
              k = 6) +
            s(year, by = basin, bs = "re", k = 4) +
            ti(jdate, depth,
               by = basin,
               k = c(15, 6),
               bs = c("cc", "cr")
            ),
          # family = gaussian(link = "inverse"),
          family = Gamma(link = "log"),
          method = "fREML",
          knots = list(jdate = c(136, 290)), 
          data = tl_d,
          select = TRUE,
          discrete = TRUE,
          rho = r1,
          AR.start = tl_d 
          $start_event
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



# acf_resid(m1)
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




overall_parm %>%
  openxlsx::write.xlsx(here("Results",
                            "GAMM_water_temp_oveall_parm.xlsx"))

summary_param %>%
  openxlsx::write.xlsx(here("Results",
                            "GAMM_water_temp_param.xlsx"))
F23 %>%
  openxlsx::write.xlsx(here("Results",
                            "GAMM_water_temp_smooth.xlsx"))
glance(m1) %>%
  openxlsx::write.xlsx(here("Results",
                            "GAMM_model_fit.xlsx"))

temp_raster_smooth <- crossing(
  # tibble(date = unique(df$daily)),
  tibble(jdate = unique(tl_d$jdate)),
  tibble(year = unique(tl_d$year)),
  tibble(basin = unique(tl_d$basin)),
  # depths can now be any value
  tibble(depth = seq(1, 20, length.out = 100))
  
) %>%
  
  mutate(
    temp = predict.gam(
      m1,
      newdata = tibble(
        # date = date,
        jdate = jdate,
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


glance(lmes) %>%
  openxlsx::write.xlsx(here("Results",
                           "GLMM_water_temp_model_fit.xlsx"))

tidy(lmes) %>%
  openxlsx::write.xlsx(here("Results",
                            "GLMM_water_temp_individual.xlsx"))

tidy(car::Anova(lmes)) %>%
  openxlsx::write.xlsx(here("Results",
                            "GLMM_water_temp_overall.xlsx"))






cols <- rev(rainbow(6)[-6])




# glimpse(temp_merge)


p <- ggplot() + 
  geom_tile(data = temp_raster_smooth, 
            aes(x = jdate, y = depth, fill = temp)) +
  # geom_point(data = tl_d,
  #            aes(x = jdate, y = depth, colour = mdt), size = 3) +
  lemon::facet_rep_wrap(.~ basin,
                        repeat.tick.labels = TRUE, scales = "free_y") +
  scale_y_reverse(
    breaks = rev(seq(0, 20, 2.5)),
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
  scale_fill_gradientn(colours = cols,
    name = "Temperature (°C)",
    breaks = seq(0, 30, 5)
  ) +
  # scale_fill_viridis_c(option = "C", 
  #                      name = "Temperature (°C)",
  #                      breaks = seq(5, 25, 5)) +
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


# p

# temp_list

glimpse(temp_raster_smooth)


ggsave(p, filename = here("Plots",
                          "Heatmap-May-Oct_GAM_rainbow_FINAL.png"),
       height = 6.37 * 1.75, width = 6.34 * 3)

# 
# 
# ggsave(p, filename = here("Plots",
#                           "Heatmap-May-Oct_Papineau Lake_GAM_red_blue.pdf"),
#        height = 6.37 * 2, width = 6.34 * 3)
