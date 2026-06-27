library(terra)
library(ggplot2)
library(dplyr)
library(viridis)
library(tidyterra)

# load elevation data (continuous raster)
lst_hot_day <- rast("D:/Conservation_Biology/2025_Osa_Conservation/DATA_YN/LST/Request_builder/LST_hot_day_06_05_2023.tiff")
lst_hot_night <- rast("D:/Conservation_Biology/2025_Osa_Conservation/DATA_YN/LST/Request_builder/LST_hot_night_07_05_2023.tiff")
lst_cold_day <- rast("D:/Conservation_Biology/2025_Osa_Conservation/DATA_YN/LST/Request_builder/LST_cold_day_28_08_2023.tiff")
lst_cold_night <- rast("D:/Conservation_Biology/2025_Osa_Conservation/DATA_YN/LST/Request_builder/LST_cold_night_29_08_2023.tiff")


lst_stack <- c(lst_cold_day, lst_cold_night, lst_hot_day, lst_hot_night)
lst_stack[lst_stack == 0] <- NA
# Display facets
ggplot() +
  geom_spatraster(data = lst_stack) +
  facet_wrap(~lyr, ncol = 4) +
  scale_fill_gradientn(
    colours = plasma(100),         # Use plasma palette
    limits = c(28000, 33000),      # Set min/max
    na.value = "transparent",
    oob = scales::squish          # ensures values outside the range are capped
  ) +
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

# load elevation data (continuous raster)
elv <- rast("D:/Conservation_Biology/2025_Osa_Conservation/DATA_YN/elevation_30_YN.tif") 
elv <- project(elv,lst_hot_day)

aspect <- terrain (elv, v = "aspect", unit = "degrees")
plot(aspect, col = rainbow(8))

elv <- resample(elv, lst_hot_day, method = "bilinear")
aspect <- resample(aspect, lst_hot_day, method = "bilinear")

# pts <- vect("D:/Conservation_Biology/2025_Osa_Conservation/GIS_projects/Sampling_design_R/DATA/points_low_veg_forest_5367.gpkg")
pts <- vect("data/points_low_veg_forest_5367_v2.gpkg")
pts <- project(pts,lst_hot_day)


plot(c(lst_hot_day, lst_hot_night, lst_cold_day, lst_cold_night), range = c(28000,33000),col = plasma(100))

plot(pts, cex = 0.5)

# Extract lst values to the points
pts$lst_cold_day <- (extract(lst_cold_day, pts)[,2])/100
pts$lst_cold_night <- (extract(lst_cold_night, pts)[,2])/100
pts$lst_hot_day <- (extract(lst_hot_day, pts)[,2])/100
pts$lst_hot_night <- (extract(lst_hot_night, pts)[,2])/100

# extract elevation and aspect data as well
pts$elv <- extract(elv, pts)[,2]
pts$aspect <- extract(aspect, pts)[,2]
pts

# convert our spatial points to a dataframe in order to plot the results
df <- as.data.frame(pts)
df

df$landcover <- factor(df$landcover,
                       levels = c(10, 30),
                       labels = c ("Forest", "Low vegetation"))

# -----------------------------------------------------------------------
# plot the data
gg_hot_day <- ggplot(df, aes(x = elv, y = lst_hot_day, colour = landcover)) +
  geom_point(size = 1, alpha = 0.8) +
  geom_smooth(method = "lm") +
  scale_colour_manual(
    values = c("Forest" = "darkgreen", "Low vegetation" = "sienna2")) +
  labs(
    x = "Elevation (m)",   # x-axis label
    y = "Land Surface Temperature (K)",                   # y-axis label
    colour = "Landcover Type",             # legend title
    title = "Daytime LST for the hottest day 2023",   # plot title
    subtitle = "(06.05.2023)",
  ) 
gg_hot_day


# -----------------------------------------------------------------------
# plot the data
gg_hot_night <- ggplot(df, aes(x = elv, y = lst_hot_night, colour = landcover)) +
  geom_point(size = 1, alpha = 0.8) +
  geom_smooth(method = "lm") +
  scale_colour_manual(
    values = c("Forest" = "darkgreen", "Low vegetation" = "sienna2")) +
  labs(
    x = "Elevation (m)",   # x-axis label
    y = "Land Surface Temperature (K)",                   # y-axis label
    colour = "Landcover Type",             # legend title
    title = "Nightime LST for the hottest day 2023",   # plot title
    subtitle = "(07.05.2023 01:30AM)",
  ) 
gg_hot_night

# -----------------------------------------------------------------------
# plot the data
gg_cold_day <- ggplot(df, aes(x = elv, y = lst_cold_day, colour = landcover)) +
  geom_point(size = 1, alpha = 0.8) +
  geom_smooth(method = "lm") +
  scale_colour_manual(
    values = c("Forest" = "darkgreen", "Low vegetation" = "sienna2")) +
  labs(
    x = "Elevation (m)",   # x-axis label
    y = "Land Surface Temperature (K)",                   # y-axis label
    colour = "Landcover Type",             # legend title
    title = "Daytime LST for the coldest day 2023",   # plot title
    subtitle = "(28.08.2023 12:30PM)",
  ) 
gg_cold_day

# -----------------------------------------------------------------------
# plot the data
gg_cold_night <- ggplot(df, aes(x = elv, y = lst_cold_night, colour = landcover)) +
  geom_point(size = 1, alpha = 0.8) +
  geom_smooth(method = "lm") +
  scale_colour_manual(
    values = c("Forest" = "darkgreen", "Low vegetation" = "sienna2")) +
  labs(
    x = "Elevation (m)",   # x-axis label
    y = "Land Surface Temperature (K)",                   # y-axis label
    colour = "Landcover Type",             # legend title
    title = "Nightime LST for the coldest day 2023",   # plot title
    subtitle = "(29.08.2023 1:30AM)",
  ) 
gg_cold_night


# -----------------------------------------------------------------------
gg_aspect <- ggplot(df, aes(x = aspect, y = lst_hot_day, colour = landcover))+
  geom_point(size = 1, alpha = 0.8) +
  scale_colour_manual(
    values = c("Forest" = "darkgreen", "Low vegetation" = "sienna2"))+
  geom_smooth(method = "lm")
  
gg_aspect

# plot the data
# gg_cold <- ggplot(df, aes(x = elv, y = lst_cold, colour = landcover)) +
#   geom_point(size = 1) +
#   geom_smooth(formula = y ~ x, method = "lm") +
#   scale_colour_manual(
#     values = c("Forest" = "darkgreen", "Low vegetation" = "sienna2")) +
#   labs(
#     x = "Elevation (m)",   # x-axis label
#     y = "Land Surface Temperature (K)",                   # y-axis label
#     colour = "Landcover Type",                # y-axis label
#     colour = "Landcover Type",             # legend title
#     title = "Coldest day 2023",   # plot title
#     subtitle = "(28.08.2023)",
#   )
# gg_cold

# ##################################################################
library(tidyr)
library(dplyr)

df_long <- df %>%
  pivot_longer(
    cols = c(lst_hot_day, lst_hot_night, lst_cold_day, lst_cold_night),
    names_to = "time_type",
    values_to = "lst"
  )

df_long <- df_long %>%
  mutate(
    # Remove the "lst_" prefix first
    time_type = sub("^lst_", "", time_type)
  ) %>%
  separate(
    col = time_type, 
    into = c("temp_type", "time_of_day"), 
    sep = "_"
  )

df_long <- df_long %>%
  mutate(
    temp_type = recode(temp_type, hot = "Hot", cold = "Cold"),
    time_of_day = recode(time_of_day, day = "Day", night = "Night")
  )
ggplot(df_long, aes(x = elv, y = lst, colour = landcover)) +
  geom_point(size = 0.5, alpha = 0.8) +
  geom_smooth(method = "lm", linewidth = 0.6) +
  scale_colour_manual(values = c("Forest" = "darkgreen", "Low vegetation" = "sienna2")) +
  facet_grid(temp_type ~ time_of_day) +  # Hot/Cold as rows, Day/Night as columns
  theme_minimal()

# ##################################################################
library(tidyr)
library(dplyr)
library(ggplot2)

# Convert wide to long
df_long <- df %>%
  pivot_longer(
    cols = c(lst_hot_day, lst_hot_night, lst_cold_day, lst_cold_night),
    names_to = "time_type",
    values_to = "lst"
  ) 

ggplot(df_long, aes(x = elv, y = lst, colour = landcover)) +
  geom_point(size = 0.5, alpha = 0.8) +
  geom_smooth(method = "lm") +
  scale_colour_manual(
    values = c("Forest" = "darkgreen", "Low vegetation" = "sienna2")
  ) +
  labs(
    x = "Elevation (m)",
    y = "Land Surface Temperature (K)",
    colour = "Landcover Type"
  ) +
  facet_wrap(~ time_type, ncol = 4) +  # 2 columns layout
  theme_bw()
