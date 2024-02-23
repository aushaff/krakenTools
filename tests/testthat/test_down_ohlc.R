min_dt <- 1435705200
max_dt <- 1498863600
interval <- 5
ap <- "XBTEUR"

out <- down_ohlc(asset_pair = ap,
                 min_date = min_dt,
                 max_date = max_dt,
                 interval = interval)

# View(out)

out$result
