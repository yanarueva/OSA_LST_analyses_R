# -----------------------------------------------------------------
plot_raw<- function (df, elev_range) {
  ggplot(df, aes(x = date, y = lst, color = landcover)
  ) +
    # geom_point(alpha = 0.4) +            # optional: show the raw points
    geom_jitter(size = 0.3, alpha = 0.25) +
    geom_smooth(method = "loess",        # "loess" for local smoothing
                span = 0.1,              # adjust smoothness (0-1)
                se = FALSE,
                fill = "#4e4f52",
                linewidth = 0.9) +               # display confidence interval
    scale_colour_manual(
      values = c("darkgreen", "sienna2")
    ) +
    scale_y_continuous(limits = c(5, 35), breaks = seq(5, 35, by = 5)
    ) +
    # scale_x_date(date_breaks = "2 months",  date_labels = "%Y %b")+
    # scale_y_continuous(limits = c(5, 40), breaks = seq(5, 40, by = 5))+
    annotate("rect", xmin = as.Date("2023-11-01"), xmax = as.Date("2024-04-01"), ymin = -Inf, ymax = Inf, alpha = .2
    ) +
    annotate("text", x = as.Date("2023-12-01"), y = 30, label = "El Niño"
    ) +
    labs(
      x = "Date", 
      y = "LST (°C)", 
      title = paste("Daily LST time-series for forest and open land at",elev_range, "m")
    ) +
    theme_classic()
}

# -------------------------------------------------------------------
clean_lst_data <- function(df) {
  df |>
    rename(
      date = interval_from,
      lst = LST_B0_mean
    ) |> 
    mutate(
      date = as.Date(date, format = "%Y-%m-%d"),
      # day of year
      doy = as.integer(strftime(date, "%j")),
      # Convert Kelvin to Celcius
      lst = lst - 273.15,
      landcover = factor(landcover, labels = c("forest", "open_land"))
    ) |>
    select(c(date, doy, poly_id, landcover, lst, elv, uid, check)) |>
    filter(date >= as.Date("2020-10-01")) 
}


# -----------------------------------------------------------------
plot_monthly<- function (df, elev_range) {
  p <- ggplot(
    df,
    aes( 
      x= month_date, 
      y = lst_monthly_mean,
      color = landcover)
  ) +
    geom_line(
      aes(color = landcover),
      linewidth = 0.6 
    ) +
    scale_colour_manual(
      values = c("darkgreen", "sienna2")
    ) +
    scale_y_continuous(limits = c(10, 30), breaks = seq(10, 30, by = 5)
    ) +
    annotate("rect", xmin = as.Date("2023-11-01"), xmax = as.Date("2024-04-01"), ymin = -Inf, ymax = Inf, alpha = .2
    ) +
    annotate("text", x = as.Date("2023-12-01"), y = 28, label = "El Niño"
    ) +
    labs(
      x = "Date",
      y = "LST",
      title = paste("Monthly mean LST time-series for forest and open land at",elev_range, "m")
    ) +
    theme_classic()
}

# ----------------------------------------------------------------------
monthly <- function(df) {
  df |>
    mutate(
      year  = year(date),
      month = month(date)
    ) |>
    group_by(landcover,year, month) |>
    summarise(
      lst_monthly_mean = mean(lst, na.rm = TRUE),
      n_points = n(),    # optional (how many records per month)
    ) |>
    ungroup() |>
    # optional: create a proper first-of-month date
    mutate(month_date = as.Date(sprintf("%04d-%02d-01", year, month)))
}
# -----------------------------------------------------------------------

plot_bfast_change <- function(df, landcover_choice, elev_range, y_bound = c(14,28)) {
  #  monthly_summarize
  df <- df |>
    filter(landcover == landcover_choice)
  
  ts <- ts(df$lst_monthly_mean, frequency = 12, start = c(2020,10))
  bfm <- bfastmonitor(ts, formula = response ~ harmon, start = c(2022,10))
  
  plot(
    bfm,
    ylab = "LST",
    ylim = y_bound,
    sub = paste (
      landcover_choice,
      "at", 
      elev_range, 
      "with magnitude", 
      bfm$magnitude)
  )
}

