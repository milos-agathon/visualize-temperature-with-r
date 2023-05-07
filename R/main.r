#############################################
# VISUALIZE CLIMATE DATA WITH R
# Milos Popovic 2023/05/03
#############################################
main_dir <- getwd()
dir.create("weather")
weather_dir_path <- main_dir |>
    paste0("/", "weather")
setwd(weather_dir_path)

# 0. INSTALL & LOAD LIBRARIES
#----------------------------

# install.packages("devtools")
# devtools::install_github("https://github.com/ErikKusch/KrigR")

# libraries we need
libs <- c(
    "KrigR", "tidyverse", "tidyr",
    "sf", "giscoR", "classInt",
    "RColorBrewer", "gganimate"
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
    install.packages(libs[!installed_libs])
}

# load libraries

invisible(lapply(libs, library, character.only = T))

# 1. QUERY TEMPERATURE DATA
#---------------------------

my_api <- ****** # PLEASE INSERT YOUR API USER ID
my_key <- ****** # PLEASE INSERT YOUR API KEY

start_date <- "2022-01-01"
end_date <- "2023-01-01"

poland_sf <- giscoR::gisco_get_countries(
    country = "PL",
    resolution = 10
)

poland_temp <- KrigR::download_ERA(
    Variable = "2m_temperature",
    DataSet = "era5-land",
    DateStart = start_date,
    DateStop = end_date,
    TResolution = "month",
    TStep = 1,
    Dir = weather_dir_path,
    FileName = "poland_2m_temperature",
    Extent = as(poland_sf, "Spatial"),
    API_User = my_api,
    API_Key = my_key
)

poland_temp[["X6"]] |>
    as.data.frame(xy = T, na.rm = T) |>
    ggplot() +
    geom_tile(aes(x = x, y = y, fill = X6)) +
    coord_sf() +
    scale_fill_viridis_c(option = "plasma") +
    theme_void()

# 2. NC TO DATAFRAME
#-------------------
poland_temp_df <- as.data.frame(
    poland_temp,
    xy = T, na.rm = T
)

head(poland_temp_df)

poland_temp_long <- poland_temp_df |>
    tidyr::pivot_longer(
        !c(x, y),
        names_to = "layer",
        values_to = "value"
    )

head(poland_temp_long)

# 3. GET DATES
#-------------

poland_temp_long$ord <- sub(
    ".", "", poland_temp_long$layer
)

poland_temp_long$ord <- as.numeric(
    as.character(poland_temp_long$ord)
)

datum <- seq(
    as.Date(start_date),
    by = "month", length.out = max(
        poland_temp_long$ord
    )
)

ord <- 1:max(poland_temp_long$ord)

dates_df <- data.frame(
    ord, datum
)

poland_temp_dates <- poland_temp_long |>
    dplyr::left_join(dates_df, "ord") |>
    dplyr::mutate(
        celsius = value - 273.15
    ) |>
    dplyr::select(
        -layer, -ord, -value
    )

# 4. BREAKS
#----------

vmin <- min(poland_temp_dates$celsius)
vmax <- max(poland_temp_dates$celsius)

breaks <- classInt::classIntervals(
    poland_temp_dates$celsius,
    n = 14,
    style = "pretty"
)$brks

# 5. COLOR
#----------

cols <- colorRampPalette(rev(RColorBrewer::brewer.pal(
    11, "Spectral"
)))

# 6. MAP
#-------
crsLAEA <-
    "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

poland_map <- ggplot(poland_temp_dates) +
    geom_tile(aes(x = x, y = y, fill = celsius)) +
    facet_wrap(~datum) +
    scale_fill_gradientn(
        name = "Celsius degree",
        colours = cols(15),
        limits = c(vmin, vmax),
        breaks = breaks
    ) +
    coord_sf(crs = crsLAEA) +
    guides(
        fill = guide_legend(
            direction = "horizontal",
            keyheight = unit(1.5, units = "mm"),
            keywidth = unit(10, units = "mm"),
            title.position = "top",
            label.position = "bottom",
            title.hjust = .5,
            label.hjust = .5,
            nrow = 1,
            byrow = T
        )
    ) +
    theme_minimal() +
    theme(
        axis.line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "top",
        legend.title = element_text(
            size = 11, color = "grey10"
        ),
        legend.text = element_text(
            size = 10, color = "grey10"
        ),
        plot.title = element_text(
            size = 20, color = "grey10",
            hjust = .5, vjust = -3
        ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(
            c(t = 1, r = 0, l = 0, b = 0), "lines"
        )
    ) +
    labs(
        x = "",
        y = "",
        title = "Monthly temperature in Poland"
    )

print(poland_map)

# 7. ANIMATE
#-----------

poland_map <- ggplot(poland_temp_dates) +
    geom_tile(aes(x = x, y = y, fill = celsius)) +
    scale_fill_gradientn(
        name = "Celsius degree",
        colours = cols(15),
        limits = c(vmin, vmax),
        breaks = breaks
    ) +
    coord_sf(crs = crsLAEA) +
    guides(
        fill = guide_legend(
            direction = "horizontal",
            keyheight = unit(1.5, units = "mm"),
            keywidth = unit(10, units = "mm"),
            title.position = "top",
            label.position = "bottom",
            title.hjust = .5,
            label.hjust = .5,
            nrow = 1,
            byrow = T
        )
    ) +
    theme_minimal() +
    theme(
        axis.line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(
            size = 11, color = "grey10"
        ),
        legend.text = element_text(
            size = 10, color = "grey10"
        ),
        plot.title = element_text(
            size = 20, color = "grey10",
            hjust = .5, vjust = -3
        ),
        plot.subtitle(
            size = 40, color = "#c43c4e",
            hjust = .5, vjust = -1
        ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(
            c(t = 1, r = 0, l = 0, b = 0), "lines"
        )
    ) +
    labs(
        x = "",
        y = "",
        title = "Monthly temperature in Poland",
        subtitle = "{as.Date(frame_time)}"
    )

timelapse_poland_map <- poland_map +
    transition_time(time = as.Date(datum)) +
    enter_fade() +
    exit_fade() +
    ease_aes("linear", interval = .2)

animated_temp_map <- gganimate::animate(
    timelapse_poland_map,
    nframes = 65,
    duration = 20,
    start_pause = 3,
    end_pause = 30,
    height = 7,
    width = 7,
    res = 300,
    fps = 15,
    renderer = gifski_renderer(loop = T)
)

gganimate::anim_save(
    "poland_temperature.gif", animated_temp_map
)
