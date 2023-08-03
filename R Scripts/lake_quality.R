# ---- load packages ----- 
{
  library(dplyr)
  library(ggplot2)
  library(here)
  library(lubridate)
  library(readr)
  library(tidyr)
}


# --- bring in data ---- 

df <- read_csv(here("data", 
                    "lake quality", 
                    "summary_vlmp_kenauk.csv"))


df <- df %>% 
  mutate(
    date = mdy(date)
  )



df_long <- df %>% 
  pivot_longer(
    cols = -c(date, site), 
    names_to = "metric", 
    values_to = "value"
  )


df_long



ggplot(data = df_long, aes(x = date, y = value)) + 
  geom_point(aes(colour = site), size = 3) + 
  facet_wrap(. ~ metric, scales = "free_y") + 
  scale_colour_viridis_d(name = "Site", end = 0.65, begin = 0.35) + 
  theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank()
  ) + 
  labs(
    x = "Date",
    y = ""
  )
