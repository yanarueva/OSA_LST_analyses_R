clean_LST_data <- function(df, landcov_value) {
  date_col <- "LST.DISPLAY.DAY.TIME.C0.date" #column containing the date
  lst_col  <- "LST.DISPLAY.DAY.TIME.C0.mean" #column containing the LST values
  df %>%
    # select only the date and LST columns
    select(all_of(c(date_col, lst_col))) %>%
    # rename the columns
    rename(
      date = !!date_col,
      LST_mean = !!lst_col
    ) %>%
    # convert date to Date format
    mutate(
      date = as.Date(date),
      # day of year
      doy = as.integer(strftime(date, "%j")),
      # add landcover column
      landcov = landcov_value,
      # Convert Kelvin to Celcius
      LST_C = LST_mean - 273.15
    ) %>%
    filter(date >= as.Date("2020-10-28"))
}

# -----------------------------------------------------------------

plotLST <- function (df, elev_range)  {
  ggplot(df, aes(x = date, y = LST_C, color = landcov)) +
    annotate("rect", xmin = as.Date("2023-11-01"), xmax = as.Date("2024-04-01"), ymin = -Inf, ymax = Inf, alpha = .2)+
    annotate("text", x = as.Date("2023-12-01"), y = 50, label = "El Niño")+
    # geom_point(alpha = 0.4) +            # optional: show the raw points
    geom_line(alpha = 0.8, size = 0.3) +
    geom_smooth(method = "loess",        # "loess" for local smoothing
                span = 0.1,              # adjust smoothness (0-1)
                se = TRUE,
                fill = "#4e4f52",
                size = 0.8) +               # display confidence interval
    scale_colour_manual(
      values = c("darkgreen", "sienna2")
    )+
    # scale_x_date(date_breaks = "2 months",  date_labels = "%Y %b")+
    scale_y_continuous(limits = c(10, 50), breaks = seq(10, 50, by = 5))+
    labs(x = "Date", y = "LST (°C)", title = paste("Forest and low vegetation comparison pair at",elev_range, "m")) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right",                  # keep it outside on the right
          legend.justification = c("left", "top"),    # align legend box to its top edge)
          legend.key = element_rect(fill = "white"), # white key boxes
          legend.title = element_blank())   
}

# -----------------------------------------------------------------
lst_bind <- function(bb1, bb2) {
  paired_bb <- rbind(bb1, bb2)
  paired_bb$landcov <- factor(paired_bb$landcov, labels = c("Forest", "Low vegetation"))
  return(paired_bb)
}