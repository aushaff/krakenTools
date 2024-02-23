min_dt <- 1435705200
max_dt <- 1498863600
interval <- 5
ap <- "XBTEUR"

out <- down_tick_data(pair_in = ap,
                      min_dt = min_dt,
                      max_dt = max_dt)

library(dplyr)
View(out)

out %>%
  summarise(min_dt = min(out$unix_time),
            max_dt = max(out$unix_time))
