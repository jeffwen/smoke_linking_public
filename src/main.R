library(sf)
library(ggplot2)
library(cowplot)
library(dplyr)
library(data.table)
library(stringr)
library(fst)
library(lubridate)
library(pbmcapply)
library(pbapply) # for suppression match
library(grid) # plot legend
library(terra)
library(patchwork)
library(raster)
library(ggalluvial)
library(ggpubr)
library(ggtext)


sf::sf_use_s2(FALSE)
setwd("~/Stanford/projects/smoke_linking_public/")

DEFAULT_COLOR <- "#737373"

base_theme <-
  theme_classic(base_size = 12) +
  theme(
    panel.spacing = unit(1, "lines"),
    strip.background = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "transparent"), # transparent panel bg
    plot.background = element_rect(fill = "transparent", color = NA), # transparent plot bg
    panel.grid.major = element_blank(), # remove major gridlines
    panel.grid.minor = element_blank(), # remove minor gridlines
    legend.background = element_blank(), # element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_blank() # element_rect(fill='transparent', color=NA) #transparent legend panel,
  )

map_theme <- theme_void() +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_blank(),
    plot.subtitle = element_markdown(size = 8),
    legend.title = element_markdown(size = 8),
    legend.text = element_text(size = 7)
  )

`%not_in%` <- purrr::negate(`%in%`)
wrap_label <- function(x) str_wrap(str_replace_all(x, "_", " "), width = 20) ## wrap long facet labels

## Data import and setup ----
### grid cell data ----
grid <- st_read("data/grid/grid_10km_wgs84/grid_10km_wgs84.shp")

grid_transform <- grid %>%
  st_transform(crs = "epsg:5070")

### population data ----
grid_pop_dt <- readRDS("data/clean/population/grid_pop_dt.rds")
grid_pop_dt <- grid_pop_dt[, `:=`(year = as.numeric(as.character(year)))]

### globfire data ----
globfire_df <- st_read("data/clean/globfire_na_final_area_2006-2020.shp")

globfire_final_df <- globfire_df %>%
  arrange(IDate) %>%
  st_transform(crs = "epsg:5070") %>%
  mutate(
    fire_centroid_x = as.vector(sf::st_coordinates(st_centroid(st_geometry(.)))[, 1]), # store the centroid for dist calc later
    fire_centroid_y = as.vector(sf::st_coordinates(st_centroid(st_geometry(.)))[, 2])
  )

### na shape ----
na_shape <- st_read("data/etc/na_shape/na_shape.shp") %>%
  st_transform("epsg:5070")

### us shape ----
conus_bbox <- sf::st_bbox(c(xmin = -127.089844, ymin = 22.066785, xmax = -66.533203, ymax = 50.120578), crs = st_crs(4326)) %>%
  st_as_sfc()

us_shape <- tigris::nation(resolution = "20m") %>%
  st_transform(crs = st_crs("epsg:4326")) %>%
  st_crop(conus_bbox) %>%
  st_transform(crs = st_crs("epsg:5070"))

### state shapes ----
states <- tigris::states(cb = TRUE)
states_transform_df <- states %>%
  st_transform(crs = st_crs("epsg:5070")) %>%
  st_make_valid()

### ca shape ----
ca_shape <- states %>%
  filter(STUSPS == "CA") %>%
  st_transform(crs = st_crs("epsg:5070"))

### western states shape ----
western_states <- c(
  "CA", "OR", "WA", "ID", "MT", "CO",
  "NV", "UT", "AZ", "NM", "WY"
)

western_shape <- states %>%
  filter(STUSPS %in% western_states) %>%
  st_transform("epsg:5070")

### grid to state mapping ----
grid_state_dt <- grid_transform %>%
  dplyr::select(ID) %>%
  st_centroid() %>%
  st_join(states_transform_df %>%
    dplyr::select(STUSPS, STATEFP, NAME)) %>%
  st_drop_geometry() %>%
  rename(
    grid_state = STUSPS,
    grid_state_fips = STATEFP,
    grid_state_name = NAME
  ) %>%
  as.data.table()

### hysplit globfire match (matches specific fires with trajectories) ----
fire_hysplit_init_match_dt <- readRDS(file = "data/clean/fire_hysplit_init_match_dt.rds")

### hysplit initialization points ----
hysplit_init_distinct <- readRDS("data/hysplit_init/hms_hysplit_initialization_distinct_20060419-20201231.rds") %>%
  dplyr::select(-height) %>%
  distinct() %>%
  mutate(
    lon_i = lon,
    lat_i = lat,
    traj_dt_i = lubridate::ymd_hm(paste(ymd, hour, minute), tz = "UTC"),
    traj_id = paste(lon_i, lat_i, traj_dt_i, sep = "_")
  ) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant") %>%
  st_transform(crs = st_crs(globfire_final_df)) %>%
  st_filter(na_shape) %>% ## note that some fires are outside the NA continent shape...these will show as <NA> fires
  mutate(
    hysplit_init_x = as.vector(sf::st_coordinates(st_geometry(.))[, 1]), # store the x and y to calculate distance
    hysplit_init_y = as.vector(sf::st_coordinates(st_geometry(.))[, 2])
  )




# Main text figures ----
## Figure 1 ----
plot_date_str <- "2018-07-29"
CA_BOUNDS <- c(-124.48200299999999, -114.131211, 32.528832, 42.009502999999995)
FIRE_ID_LIST <- c(21889734, 21889754, 21889779) ## carr fire, ranch, ferguson
CA_BBOX <- st_bbox(ca_shape)

### sat image ----
sat_img_file <- "data/sat_img/goes_20180729.png"

sat_raster <- raster::brick(sat_img_file)
crs(sat_raster) <- crs("EPSG:4326")
extent(sat_raster) <- CA_BOUNDS
sat_raster <- projectRaster(sat_raster, crs = "EPSG:5070")

# used to generate sat plot with ca + nv?
ca_nv_shape <- states %>%
  filter(STUSPS %in% c("CA", "NV")) %>%
  st_transform(crs = st_crs("epsg:5070"))

# crop and mask to only show cali + nv shape
sat_raster <- mask(sat_raster, ca_nv_shape)
sat_raster <- crop(sat_raster, extent(ca_nv_shape))

ca_bbox_shape <- ca_shape$geometry %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf()
sat_raster <- mask(sat_raster, ca_bbox_shape)
sat_raster <- crop(sat_raster, extent(ca_bbox_shape))

p_sat <- RStoolbox::ggRGB(sat_raster, r = 1, g = 2, b = 3) +
  map_theme +
  geom_sf(data = ca_nv_shape, fill = NA, color = NA) +
  coord_sf(
    xlim = c(CA_BBOX$xmin, CA_BBOX$xmax),
    ylim = c(CA_BBOX$ymin, CA_BBOX$ymax)
  )


### HMS smoke polygons ----
hms_smoke_df <- read_sf("data/hms_smoke/hms_smoke20180729.shp") %>%
  st_set_crs("epsg:4326") %>%
  st_transform(crs = st_crs("epsg:5070")) %>%
  st_crop(ca_shape)

globfire_ca_df <- globfire_df %>%
  filter(IDate <= plot_date_str & FDate >= plot_date_str) %>%
  st_transform(crs = st_crs("epsg:5070")) %>%
  st_make_valid() %>%
  st_filter(ca_shape) %>%
  mutate(
    fire_centroid_x = as.vector(sf::st_coordinates(st_centroid(st_geometry(.)))[, 1]),
    fire_centroid_y = as.vector(sf::st_coordinates(st_centroid(st_geometry(.)))[, 2])
  )

p_hms_smoke <- ggplot() +
  map_theme +
  geom_sf(data = ca_shape, fill = NA, lwd = 0.1) +
  geom_sf(data = globfire_ca_df, fill = "red", color = NA) +
  geom_sf(data = hms_smoke_df, fill = "gray90", color = "gray90", alpha = 0.2, lwd = 0.2) +
  geom_curve(
    aes(
      xend = globfire_ca_df %>% filter(Id == 21889779) %>% pull(fire_centroid_x),
      yend = globfire_ca_df %>% filter(Id == 21889779) %>% pull(fire_centroid_y),
      x = -2150000, y = 1700000
    ),
    angle = 90, size = 0.3, color = "gray70", curvature = -0.25
  ) +
  # arrow=ggplot2::arrow(length = unit(0.025, "npc"))) +
  geom_text(aes(x = -2150000, y = 1700000), label = "Active\nfire", hjust = "outside", nudge_y = -50000, size = 2.5) +
  geom_segment(
    aes(
      xend = -1900000, yend = 1700000,
      x = -1800000, y = 1500000
    ),
    size = 0.3, color = "gray70"
  ) +
  # arrow=ggplot2::arrow(length = unit(0.025, "npc"))) +
  geom_text(aes(x = -1800000, y = 1500000), label = "Smoke\nplume", hjust = "outside", nudge_y = -50000, size = 2.5) +
  coord_sf(
    xlim = c(CA_BBOX$xmin, CA_BBOX$xmax),
    ylim = c(CA_BBOX$ymin, CA_BBOX$ymax)
  )

### smoke pm ----
formatted_date_str <- format.Date(plot_date_str, "%Y%m%d")
smokepm_df <- readRDS(sprintf("data/10km_smoke_days/smokePM_predictions_10km_%s.rds", formatted_date_str))

grid_smokepm_df <- grid_transform %>%
  left_join(smokepm_df, by = c("ID" = "grid_id_10km")) %>%
  st_crop(ca_shape) %>%
  mutate(
    smokePM_pred_coded = ifelse(smokePM_pred < 0, 0, smokePM_pred),
    smokePM_pred_capped = ifelse(smokePM_pred_coded >= 100, 100, smokePM_pred_coded)
  )

p_smokepm <- ggplot() +
  map_theme +
  theme(
    legend.position = "right",
    legend.title = element_markdown(size = 8),
  ) +
  geom_sf(data = ca_shape, fill = NA, lwd = 0.1) +
  geom_sf(data = grid_smokepm_df %>% filter(date == ymd(plot_date_str)), aes(fill = smokePM_pred_capped), color = NA) +
  scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0, 100), breaks = seq(0, 100, by = 25), labels = c(seq(0, 75, by = 25), "100+")) +
  labs(fill = "Smoke PM<sub>2.5</sub><br>(&mu;g/m<sup>3</sup>)") +
  coord_sf(
    xlim = c(CA_BBOX$xmin, CA_BBOX$xmax),
    ylim = c(CA_BBOX$ymin, CA_BBOX$ymax)
  )

### combine first row ----
p1_first_row <- plot_grid(p_sat, p_hms_smoke, p_smokepm + theme(legend.position = "none"),
  nrow = 1
)
p1_first_row_legend <- get_legend(p_smokepm)
p1_first_row <- plot_grid(p1_first_row, p1_first_row_legend,
  rel_widths = c(1, .14), nrow = 1
)

### hysplit trajectories ----
hysplit_plot_date_list <- seq.Date(from = ymd(plot_date_str) - 6, to = ymd(plot_date_str), by = "day") # need traj points from before first fire day of interest

hysplit_traj_files_list <- list.files(path = "data/traj_example", pattern = ".rds", recursive = T, full.names = T)
hysplit_df <- pbmclapply(hysplit_traj_files_list, function(x) {
  if (as.Date(str_split(str_split(x, "_")[[1]][4], ".rds")[[1]][1]) %in% hysplit_plot_date_list) {
    temp_df <- readRDS(x) %>%
      as.data.table()

    # filter and remove rained out or grounded traj points and get distance to successive points
    temp_df <- temp_df[, `:=`(traj_id = paste(lon_i, lat_i, traj_dt_i, sep = "_"))][order(traj_id, height_i, hour_along)][, `:=`(
      cumsum_precip = cumsum(rainfall),
      cummin_height = cummin(height)
    ),
    by = c("traj_id", "height_i")
    ][(cumsum_precip == 0) & (cummin_height > 0)] %>%
      as.data.frame() %>%
      dplyr::select(traj_dt, hour_along, lon, lat, height, lon_i, lat_i, traj_dt_i, height_i, traj_id) # select conflicts with raster so call from dplyr

    return(temp_df)
  }
}) %>%
  bind_rows() %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant") %>%
  mutate(date_i = as.Date(traj_dt_i)) %>%
  st_transform(st_crs("epsg:5070"))

## filter to just traj points related to specific fire
hysplit_filtered_df <- hysplit_df %>%
  st_crop(ca_shape) %>%
  left_join(st_drop_geometry(fire_hysplit_init_match_dt[, c("Id", "traj_id")]), by = "traj_id") %>%
  filter(Id %in% FIRE_ID_LIST) %>%
  mutate(
    fire_name = case_when(
      Id == 21889734 ~ "Carr Fire",
      Id == 21889754 ~ "Ranch Fire (Mendocino Complex)",
      Id == 21889779 ~ "Ferguson Fire"
    ),
    fire_name = ordered(fire_name, levels = c("Carr Fire", "Ranch Fire (Mendocino Complex)", "Ferguson Fire")),
    color_scale = 1.01
  )

hysplit_filtered_df$x <- st_coordinates(hysplit_filtered_df)[, 1]
hysplit_filtered_df$y <- st_coordinates(hysplit_filtered_df)[, 2]

## calculate num traj points at same coordinate to generate color scale
hysplit_color_scale_overlap_dt <- hysplit_filtered_df %>%
  st_drop_geometry() %>%
  as.data.table()
hysplit_color_scale_overlap_dt[, .(count_traj_points = .N), by = c("x", "y", "height_i")][order(-count_traj_points)]

hysplit_color_range <- scales::alpha(c("black"), alpha = c(0.13, 1))

hysplit_color_scale_data <- data.frame(x = 1:100, y = 1:100, color = seq(from = 0, to = 1, length.out = 100))
p_temp_hysplit_color_scale <- ggplot() +
  map_theme +
  theme(
    legend.position = "right",
    legend.title = element_markdown(size = 8),
    strip.text.x = element_blank(),
  ) +
  geom_line(data = hysplit_color_scale_data, aes(x = x, y = y, color = color), alpha = 0) +
  scale_color_gradient(
    name = "Number of<br>overlapping<br>trajectories",
    low = hysplit_color_range[1], high = hysplit_color_range[2],
    breaks = c(0, 0.5, 1),
    labels = c(1, 5, "10+"),
    guide = "colorbar"
  )
p_temp_hysplit_color_scale_legend <- cowplot::get_legend(p_temp_hysplit_color_scale)

## set up globfire polygon with correct name
globfire_label_plot_df <- globfire_df %>%
  filter(Id %in% FIRE_ID_LIST) %>%
  mutate(
    fire_name = case_when(
      Id == 21889734 ~ "Carr Fire",
      Id == 21889754 ~ "Ranch Fire (Mendocino Complex)",
      Id == 21889779 ~ "Ferguson Fire"
    ),
    fire_name = ordered(fire_name, levels = c("Carr Fire", "Ranch Fire (Mendocino Complex)", "Ferguson Fire")),
    fire_name_linebreak = case_when(
      Id == 21889734 ~ "Carr\nFire",
      Id == 21889754 ~ "Ranch Fire\n(Mendocino\nComplex)",
      Id == 21889779 ~ "Ferguson\nFire"
    )
  ) %>%
  st_transform(crs = st_crs("epsg:5070")) %>%
  mutate(
    fire_centroid_x = as.vector(sf::st_coordinates(st_centroid(st_geometry(.)))[, 1]),
    fire_centroid_y = as.vector(sf::st_coordinates(st_centroid(st_geometry(.)))[, 2]),
    offset_x = case_when(
      Id == 21889734 ~ -120000,
      Id == 21889754 ~ -100000,
      Id == 21889779 ~ -190000
    ),
    offset_y = case_when(
      Id == 21889734 ~ -10000,
      Id == 21889754 ~ 230000,
      Id == 21889779 ~ 220000
    )
  )

p_traj <- ggplot() +
  map_theme +
  theme(
    legend.position = "right",
    legend.title = element_markdown(size = 8),
    strip.text.x = element_blank(),
  ) +
  geom_sf(data = ca_shape, fill = NA, lwd = 0.1) +
  geom_path(
    data = hysplit_filtered_df %>% filter(
      traj_dt >= ymd(plot_date_str) - 6,
      date_i <= ymd(plot_date_str),
      height_i %in% c(500, 1500, 2500),
      traj_dt < ymd(plot_date_str) + 1
    ),
    aes(x = x, y = y, group = interaction(traj_id, height_i), color = color_scale), size = 0.03, alpha = 0.13
  ) +
  facet_wrap(~fire_name, labeller = as_labeller(wrap_label)) +
  scale_color_gradient(
    name = "Number of<br>overlapping<br>trajectories",
    low = hysplit_color_range[1], high = hysplit_color_range[2],
    guide = "colorbar"
  ) +
  geom_sf(data = globfire_label_plot_df, fill = "red", color = NA, lwd = 0.1) +
  geom_text(
    data = globfire_label_plot_df,
    aes(
      x = globfire_label_plot_df %>% pull(fire_centroid_x) + offset_x,
      y = globfire_label_plot_df %>% pull(fire_centroid_y) + offset_y,
      label = fire_name_linebreak
    ),
    hjust = 0, nudge_y = -50000, size = 2.5
  ) +
  coord_sf(
    xlim = c(CA_BBOX$xmin, CA_BBOX$xmax),
    ylim = c(CA_BBOX$ymin, CA_BBOX$ymax)
  )

## replace placeholder legend with custom legend
p_traj_plot_table <- ggplot_gtable(ggplot_build(p_traj))
grob_legend_idx <- which(sapply(p_traj_plot_table$grobs, function(x) x$name) == "guide-box")
p_traj_plot_table$grobs[[grob_legend_idx]] <- p_temp_hysplit_color_scale_legend
p_traj <- ggpubr::as_ggplot(p_traj_plot_table)

### contributed smokepm ----
SAVE_PATH <- "data/clean/fire_smokepm/"
WINDOW_SIZE <- 3
year_ <- year(plot_date_str)
month_ <- str_pad(month(plot_date_str), 2, pad = "0")
temp_fire_smokepm_dt <- read_fst(str_glue("{SAVE_PATH}{WINDOW_SIZE}0km/{year_}-{month_}_{WINDOW_SIZE}0km_focal_grid_fire_smokepm_agg.fst"), as.data.table = T)

specific_fire_smokepm_dt <- temp_fire_smokepm_dt[fire_id %in% FIRE_ID_LIST & !is.na(contrib_smokePM), ] %>%
  as.data.frame() %>%
  left_join(grid_transform, by = c("ID")) %>%
  mutate(contrib_smokePM_capped = ifelse(contrib_smokePM >= 100, 100, contrib_smokePM)) %>%
  st_as_sf() %>%
  mutate(
    fire_name = case_when(
      fire_id == 21889734 ~ "Carr Fire",
      fire_id == 21889754 ~ "Ranch Fire (Mendocino Complex)",
      fire_id == 21889779 ~ "Ferguson Fire"
    ),
    fire_name = ordered(fire_name, levels = c("Carr Fire", "Ranch Fire (Mendocino Complex)", "Ferguson Fire"))
  )

p_contrib_smokepm <- ggplot() +
  map_theme +
  theme(
    legend.position = "right",
    legend.title = element_markdown(size = 8),
    strip.text.x = element_blank()
  ) +
  geom_sf(data = ca_shape, fill = NA, lwd = 0.1) +
  geom_sf(data = specific_fire_smokepm_dt %>% filter(date == ymd(plot_date_str)) %>% st_crop(ca_shape), aes(fill = contrib_smokePM_capped), color = NA) +
  scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0, 100), breaks = seq(0, 100, by = 25), labels = c(seq(0, 75, by = 25), "100+")) +
  facet_wrap(~fire_name, labeller = as_labeller(wrap_label)) +
  geom_sf(data = globfire_label_plot_df, fill = "red", color = NA, lwd = 0.1) +
  labs(fill = "Contributed<br>smoke PM<sub>2.5</sub><br>(&mu;g/m<sup>3</sup>)") +
  coord_sf(
    xlim = c(CA_BBOX$xmin, CA_BBOX$xmax),
    ylim = c(CA_BBOX$ymin, CA_BBOX$ymax)
  )

### final plotting ----
p_fig1 <- plot_grid(p1_first_row, p_traj, p_contrib_smokepm, ncol = 1)
save_plot("output/fig1/fig1.pdf", plot = p_fig1, base_width = 5.4, base_height = 7.8, scale = 1.15, device = cairo_pdf)





## Figure 2 ----
## Hillsboro OR monitor timeseries and satellite imagery
GRIDCELL_ID <- 201452
MONITOR_ID <- 410670004
START_DATE <- "2020-09-04"
END_DATE <- "2020-09-20"

## monitoring station smoke pm (get lat lon...)
SAVE_PATH <- "data/smokepm_training/"
smokepm_training_df <- readRDS(str_glue("{SAVE_PATH}smokePM_training.rds")) %>%
  dplyr::select(id, date, grid_id_10km, lat, lon, smokePM) %>%
  left_join(grid_state_dt, by = c("grid_id_10km" = "ID")) %>%
  mutate(monitor_id_str = stringr::str_pad(id, 9, pad = "0")) %>%
  filter(
    monitor_id_str == MONITOR_ID,
    date >= START_DATE,
    date <= END_DATE
  ) %>%
  tidyr::complete(
    date = seq.Date(min(date), max(date), by = "day"),
    fill = list(smokePM = 0, lon = -122.9849, lat = 45.5671)
  )

## epa monitoring station data (both total PM and smoke PM)
epa_monitor_pm_df <- readRDS("data/pm25/station_smokePM.rds") %>%
  mutate(monitor_id_str = stringr::str_pad(id, 9, pad = "0")) %>%
  filter(
    monitor_id_str == MONITOR_ID,
    date >= START_DATE,
    date <= END_DATE
  )

## fire smoke pm
SAVE_PATH <- "data/clean/fire_smokepm/"
WINDOW_SIZE <- 3
MONTH_YEAR_LIST <- unique(zoo::as.yearmon(seq.Date(from = ymd(START_DATE), to = ymd(END_DATE), by = "days")))

fire_smokepm_dt <- pbmclapply(MONTH_YEAR_LIST, function(MONTH_YEAR) {
  year_ <- year(MONTH_YEAR)
  month_ <- str_pad(month(MONTH_YEAR), 2, pad = "0")

  temp_fire_smokepm_dt <- read_fst(str_glue("{SAVE_PATH}{WINDOW_SIZE}0km/{year_}-{month_}_{WINDOW_SIZE}0km_focal_grid_fire_smokepm_agg.fst"), as.data.table = T)

  # filter for specific monitor and matched fire_id
  temp_fire_smokepm_dt <- temp_fire_smokepm_dt[ID == GRIDCELL_ID &
    !is.na(fire_id) &
    date >= START_DATE &
    date <= END_DATE]

  return(temp_fire_smokepm_dt)
}) %>% bind_rows()

total_contributed_smokePM_dt <- fire_smokepm_dt[, .(total_contrib_smokePM = sum(contrib_smokePM, na.rm = T)),
  by = c("ID", "date")
]

## get major fire name and fill in 0 for no obs dates
fire_specific_contributed_smokePM_filled_df <- fire_smokepm_dt[, `:=`(fire_name = fcase(
  fire_id == "24332628", "August Complex Fire",
  fire_id == "24332764", "Dolan Fire",
  fire_id == "24461328", "Slater Fire",
  fire_id == "24332939", "Santiam Fire",
  fire_id == "24461771", "Bobcat Fire",
  fire_id == "24462263", "Riverside Fire",
  fire_id == "24462488", "Holiday Farm Fire",
  fire_id == "24462610", "Archie Creek Fire",
  fire_id == "24332608", "Red Salmon Complex Fire",
  fire_id == "24461320", "South Obenchain Fire",
  rep_len(TRUE, length(fire_id)), "Other fires"
))][, .(total_contrib_smokePM = sum(contrib_smokePM, na.rm = T)),
  by = c("fire_name", "ID", "date")
] %>%
  tidyr::complete(fire_name, date, fill = list(total_contrib_smokePM = 0))

### generate timeseries plot ----
color_pal <- RColorBrewer::brewer.pal(12, name = "Set3")

p_timeseries <- ggplot() +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.line = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_markdown(hjust = 0.5, vjust = 1),
    legend.text = element_markdown(),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, 0, 0, 0),
  ) +
  geom_vline(aes(xintercept = as.Date("2020-09-10")), linetype = 1, color = "gray90") +
  geom_text(aes(x = as.Date("2020-09-10"), y = 280), label = "Sept. 10, 2020", size = 3, hjust = 1, nudge_x = -.25) +
  geom_vline(aes(xintercept = as.Date("2020-09-13")), linetype = 1, color = "gray90") +
  geom_text(aes(x = as.Date("2020-09-13"), y = 280), label = "Sept. 13, 2020", size = 3, hjust = 1, nudge_x = -.25) +
  geom_area(data = fire_specific_contributed_smokePM_filled_df, aes(x = date, y = total_contrib_smokePM, fill = fire_name)) +
  geom_line(data = smokepm_training_df, aes(x = date, y = smokePM, linetype = "smokepm"), color = "black") +
  geom_line(data = epa_monitor_pm_df, aes(x = date, y = pm25, linetype = "pm"), color = "black") +
  labs(x = "", y = "PM<sub>2.5</sub> (&mu;g/m<sup>3</sup>)", fill = "Smoke source") +
  scale_x_date(labels = scales::date_format("%b %d %Y"), date_breaks = "2 days") +
  scale_linetype_manual("",
    labels = c(
      "smokepm" = "Hillsboro EPA station<br>calculated smoke PM<sub>2.5</sub>",
      "pm" = "Hillsboro EPA station<br>total PM<sub>2.5</sub>"
    ),
    values = c("smokepm" = 3, "pm" = 2)
  ) +
  guides(linetype = guide_legend(ncol = 1)) +
  scale_fill_brewer(palette = "Set3")

### plot map of nearby fires ----
hillsboro_fire_polygons <- globfire_final_df %>%
  mutate(Id = as.character(Id)) %>%
  filter(Id %in% fire_smokepm_dt$fire_id[str_length(fire_smokepm_dt$fire_id) < 10]) %>% # filter for fires with globfire ID
  left_join(fire_smokepm_dt[, c("fire_id", "fire_name")], by = c("Id" = "fire_id"))

hillsboro_hysplit_init <- hysplit_init_distinct %>%
  filter(traj_id %in% fire_smokepm_dt$fire_id[str_length(fire_smokepm_dt$fire_id) > 10]) # filter for fires with hysplit init traj ID

hillsboro_monitor <- smokepm_training_df %>%
  dplyr::select(monitor_id_str, lat, lon) %>%
  unique() %>%
  st_as_sf(coords = c("lon", "lat"), crs = "epsg:4326") %>%
  st_transform("epsg:5070")

ca_pnw_shape <- states %>%
  filter(STUSPS %in% c("CA", "OR", "WA", "NV")) %>%
  st_transform(crs = st_crs("epsg:5070"))

p_local_fires <- ggplot() +
  map_theme +
  geom_sf(data = ca_pnw_shape, fill = NA, lwd = 0.1) +
  geom_sf(data = hillsboro_fire_polygons, aes(fill = fire_name), color = NA) +
  geom_sf(data = hillsboro_hysplit_init, color = color_pal[[6]], size = .1) +
  geom_sf(data = hillsboro_monitor, fill = "black", color = "black", size = 2, shape = 23) +
  scale_fill_brewer(palette = "Set3") +
  geom_segment(
    aes(
      xend = st_bbox(hillsboro_monitor)$xmax - 15000, yend = st_bbox(hillsboro_monitor)$ymax + 15000,
      x = st_bbox(hillsboro_monitor)$xmax - 100000, y = st_bbox(hillsboro_monitor)$ymax + 150000
    ),
    size = 0.3, color = "gray70"
  ) +
  geom_richtext(aes(x = st_bbox(hillsboro_monitor)$xmax - 100000, y = st_bbox(hillsboro_monitor)$ymax + 150000),
    label = "Hillsboro EPA<br>monitoring station", size = 3, fill = "white", label.color = NA
  )

### sat image 1 ----
sat_img_file <- "data/sat_img/snapshot-2020-09-10T00_00_00Z.tiff"

sat_raster <- terra::rast(sat_img_file)
sat_raster <- terra::project(sat_raster, "EPSG:5070")
sat_raster <- mask(sat_raster, ca_pnw_shape)

STATES_BBOX <- st_bbox(ca_pnw_shape)
p_sat_1 <- RStoolbox::ggRGB(sat_raster, r = 1, g = 2, b = 3) +
  map_theme +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_sf(data = ca_pnw_shape, fill = NA, lwd = 0.1) +
  geom_sf(data = hillsboro_fire_polygons, aes(fill = fire_name), color = NA) +
  geom_sf(data = hillsboro_hysplit_init, color = color_pal[[6]], size = .1) +
  geom_sf(data = hillsboro_monitor, fill = "black", color = "black", size = 2, shape = 23) +
  scale_fill_brewer(palette = "Set3") +
  coord_sf(
    xlim = c(STATES_BBOX$xmin, STATES_BBOX$xmax),
    ylim = c(STATES_BBOX$ymin, STATES_BBOX$ymax)
  ) +
  labs(title = "Sept. 10, 2020")

### sat image 2 ----
sat_img_file <- "data/sat_img/snapshot-2020-09-13T00_00_00Z.tiff"

sat_raster <- terra::rast(sat_img_file)
sat_raster <- terra::project(sat_raster, "EPSG:5070")
sat_raster <- mask(sat_raster, ca_pnw_shape)

p_sat_2 <- RStoolbox::ggRGB(sat_raster, r = 1, g = 2, b = 3) +
  map_theme +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  ) +
  geom_sf(data = ca_pnw_shape, fill = NA, lwd = 0.1) +
  geom_sf(data = hillsboro_fire_polygons, aes(fill = fire_name), color = NA, show.legend = FALSE) +
  geom_sf(data = hillsboro_hysplit_init, color = color_pal[[6]], size = .1) +
  geom_sf(data = hillsboro_monitor, size = 2, fill = "black", color = "black", shape = 23) +
  scale_fill_brewer(palette = "Set3") +
  coord_sf(
    xlim = c(STATES_BBOX$xmin, STATES_BBOX$xmax),
    ylim = c(STATES_BBOX$ymin, STATES_BBOX$ymax)
  ) +
  labs(title = "Sept. 13, 2020")

p_top_row <- plot_grid(p_timeseries, labels = c("A"))
p_bottom_row <- plot_grid(p_local_fires, p_sat_1, p_sat_2, nrow = 1, labels = c("B", "C", "D"))
p_combined <- plot_grid(p_top_row, p_bottom_row, ncol = 1, rel_heights = c(1.75, 3))

save_plot("output/fig2/fig2.pdf", plot = p_combined, base_width = 7, base_height = 7, scale = 1.7, device = cairo_pdf)
save_plot("output/fig2/fig2.png", plot = p_combined, base_width = 7, base_height = 7, scale = 1.7, bg = "white")





## Figure 3 ----

## asthma ed estimates from heft-neal et al. 2023
ASTHMA_ED_COEFF <- 0.01712942 / 100000
ASTHMA_ED_SE <- 0.000828155 / 100000
ASTHMA_ED_BASE_RATE <- 0.9585968 / 100000

SAVE_PATH <- "data/clean/fire_smokepm/"
WINDOW_SIZE <- 3

## iterate over year months to calculate total smoke at grid cells
MONTH_YEAR_LIST <- unique(zoo::as.yearmon(seq.Date(from = ymd("2006-04-19"), to = ymd("2020-12-31"), by = "days")))

total_smokepm_dt <- pbmclapply(MONTH_YEAR_LIST, function(MONTH_YEAR) {
  year_ <- year(MONTH_YEAR)
  month_ <- str_pad(month(MONTH_YEAR), 2, pad = "0")

  temp_fire_smokepm_dt <- read_fst(str_glue("{SAVE_PATH}{WINDOW_SIZE}0km/{year_}-{month_}_{WINDOW_SIZE}0km_focal_grid_fire_smokepm_agg.fst"), as.data.table = T)

  ## aggregate smokePM to the fireID level (over days) then join pop data and aggregate to yearly smokepm over cells per fire
  temp_fire_smokepm_dt <- temp_fire_smokepm_dt[, `:=`(
    month = month(date),
    year = year(date)
  )][!is.na(contrib_smokePM),
    .(total_contrib_smokePM = sum(contrib_smokePM, na.rm = T)),
    by = c("ID", "fire_id", "month", "year")
  ][grid_pop_dt, `:=`(pop = i.pop), on = c("ID", "year")][, `:=`(
    pop_smokepm = pop * total_contrib_smokePM,
    asthma_ed_visits = pop * total_contrib_smokePM * ASTHMA_ED_COEFF,
    asthma_ed_visits_lb = pop * total_contrib_smokePM * (ASTHMA_ED_COEFF - qnorm(.95) * ASTHMA_ED_SE),
    asthma_ed_visits_ub = pop * total_contrib_smokePM * (ASTHMA_ED_COEFF + qnorm(.95) * ASTHMA_ED_SE)
  )][, .(
    affected_pop = sum(pop, na.rm = T),
    total_pop_smokePM = sum(pop_smokepm, na.rm = T),
    total_contrib_smokePM = sum(total_contrib_smokePM, na.rm = T),
    total_asthma_ed_visits = sum(asthma_ed_visits, na.rm = T),
    total_asthma_ed_visits_lb = sum(asthma_ed_visits_lb, na.rm = T),
    total_asthma_ed_visits_ub = sum(asthma_ed_visits_ub, na.rm = T)
  ),
  by = c("fire_id", "month", "year")
  ]

  return(temp_fire_smokepm_dt)
}) %>% bind_rows()

sorted_total_smokepm_dt <- total_smokepm_dt[, .(
  total_pop_smokePM = sum(total_pop_smokePM, na.rm = T),
  total_smokePM = sum(total_contrib_smokePM, na.rm = T),
  total_asthma_ed_visits = sum(total_asthma_ed_visits, na.rm = T),
  total_asthma_ed_visits_lb = sum(total_asthma_ed_visits_lb, na.rm = T),
  total_asthma_ed_visits_ub = sum(total_asthma_ed_visits_ub, na.rm = T)
),
by = c("fire_id")
][order(-total_pop_smokePM)]

### top 20 ----
# 8415759 ~ "Bugaboo Fire" c("GA3067208246020070425","GA3094108231320070505")
# 24332628 ~ "August Complex Fire" "CA3966012280920200817"
# 24332764 ~ "Dolan Fire" "CA3612312160220200819"
# 24461771 ~ "Bobcat Fire" "CA3424811795920200906"
# 22343661 ~ "Camp Fire" "CA3982012144020181108"
# 24461623 ~ "Creek Fire" "CA3720111927220200905"
# 21889754 ~ "Ranch Fire (Mendocino Complex)" "CA3924012311020180727" ## part of mendocino complex fire with 21889763 River fire
# 24332939 ~ "Santiam Fire" c("OR4482112218820200816","OR4472312167920200817")## composed of Beachie creek, Lionshead fires; P-515 fire is also part of this but separate in globfire
# 24332647 ~ "Claremont Fire (North Complex)" "CA3985812091220200817"
#
# 24332732 ~ "SCU Lightning Complex Fire" "CA3742412156820200816"
# 24332763 ~ "Castle Fire (SQF Complex)" "CA3616111845220200819"
# 24462488 ~ "Holiday Farm Fire" "OR4417212223120200908"
# 13501076 ~ "Wallow Fire" "AZ3360210944920110529"
# 24332700 ~ "Hennesey Fire" "CA3850412233720200817"
# 9879402 ~ "Basin Complex Fire" "CA3621212157820080621"
# 24462610 ~ "Archie Creek Fire" "OR4333412278820200908"
# 24462263 ~ "Riverside Fire" "OR4504912206220200908"
# 24461780 ~ "El Dorado Fire" "CA3405311699120200905"
# 24461607 ~ "Glass Fire" "CA3855412253120200927"
# 21889697 ~ "Klondike Fire" "OR4237012386020180716"
#
# ## canadian fire comes in 11th
# 20777771 ~ "CANADIAN FIRE"

top_fires_dt <- data.table(
  "rank" = 1:20,
  "fire_id" = c(
    8415759, 24332628, 24332764, 24461771, 22343661, 24461623, 21889754,
    24332939, 24332647, 24332732, 24332763, 24462488, 13501076, 24332700,
    9879402, 24462610, 24462263, 24461780, 24461607, 21889697
  ),
  "fire_name" = c(
    "Bugaboo Fire", "August Complex Fire", "Dolan Fire", "Bobcat Fire",
    "Camp Fire", "Creek Fire", "Ranch Fire (Mendocino Complex)", "Santiam Fire",
    "Claremont Fire (North Complex)", "SCU Lightning Complex Fire",
    "Castle Fire (SQF Complex)", "Holiday Farm Fire", "Wallow Fire",
    "Hennesey Fire", "Basin Complex Fire", "Archie Creek Fire", "Riverside Fire",
    "El Dorado Fire", "Glass Fire", "Klondike Fire"
  ),
  "event_id" = list(
    c("GA3067208246020070425", "GA3094108231320070505"), "CA3966012280920200817",
    "CA3612312160220200819", "CA3424811795920200906", "CA3982012144020181108",
    "CA3720111927220200905", "CA3924012311020180727",
    c("OR4482112218820200816", "OR4472312167920200817"),
    "CA3985812091220200817", "CA3742412156820200816", "CA3616111845220200819",
    "OR4417212223120200908", "AZ3360210944920110529", "CA3850412233720200817",
    "CA3621212157820080621", "OR4333412278820200908", "OR4504912206220200908",
    "CA3405311699120200905", "CA3855412253120200927", "OR4237012386020180716"
  )
)


### top 9 fires loop ----
RANKING <- 1:9

## get fire polygons for the top fires
globfire_plot_df <- globfire_df %>%
  filter(Id %in% top_fires_dt[RANKING, fire_id]) %>%
  left_join(top_fires_dt[, c("rank", "fire_id", "fire_name")], by = c("Id" = "fire_id")) %>%
  mutate(fire_name = ordered(fire_name, levels = top_fires_dt[RANKING, fire_name])) %>%
  st_transform(crs = st_crs("epsg:5070"))

## get months that we need to cover...include 6 days from last day of fire (prob need to change this to just get the year months where our fire occured)
MONTH_YEAR_LIST <- globfire_plot_df %>%
  rowwise() %>%
  mutate(date_list = list(seq.Date(from = IDate, to = FDate + 6, by = "days")))
MONTH_YEAR_LIST <- unique(zoo::as.yearmon(do.call(c, MONTH_YEAR_LIST$date_list)))

fire_smokepm_dt <- pbmclapply(MONTH_YEAR_LIST, function(MONTH_YEAR) {
  year_ <- year(MONTH_YEAR)
  month_ <- str_pad(month(MONTH_YEAR), 2, pad = "0")

  temp_fire_smokepm_dt <- read_fst(str_glue("{SAVE_PATH}{WINDOW_SIZE}0km/{year_}-{month_}_{WINDOW_SIZE}0km_focal_grid_fire_smokepm_agg.fst"), as.data.table = T)

  ## filter for specific fires
  temp_fire_smokepm_dt <- temp_fire_smokepm_dt[fire_id %in% top_fires_dt[RANKING, fire_id] & !is.na(contrib_smokePM), ]

  return(temp_fire_smokepm_dt)
}) %>% bind_rows()

fire_smokepm_dt[, `:=`(fire_id = as.numeric(fire_id))] ## need to convert string to integer

### asthma ed visits map overlays (maps by asthma_ed_visits) ----
pop_fire_smokepm_dt <- fire_smokepm_dt[top_fires_dt, `:=`(fire_name = i.fire_name), on = c("fire_id")][, `:=`(year = year(date))][, `:=`(fire_name = ordered(fire_name, levels = top_fires_dt[RANKING, fire_name]))][, `:=`(total_contrib_smokePM = sum(contrib_smokePM, na.rm = T)),
  by = c("ID", "fire_id", "year")
][grid_pop_dt, `:=`(pop = i.pop), on = c("ID", "year")][, `:=`(
  pop_smokepm = pop * contrib_smokePM,
  asthma_ed_visits = pop * contrib_smokePM * ASTHMA_ED_COEFF,
  asthma_ed_visits_lb = pop * contrib_smokePM * (ASTHMA_ED_COEFF - qnorm(.95) * ASTHMA_ED_SE),
  asthma_ed_visits_ub = pop * contrib_smokePM * (ASTHMA_ED_COEFF + qnorm(.95) * ASTHMA_ED_SE)
)]

### elapsed time plots ----
globfire_date_df <- globfire_plot_df %>%
  st_drop_geometry() %>%
  dplyr::select(IDate, FDate, fire_name, rank) %>%
  arrange(fire_name)

plot_pop_smokepm_df <- pop_fire_smokepm_dt[, .(
  total_pop_smokepm = sum(pop_smokepm, na.rm = T),
  total_contrib_smokePM = sum(contrib_smokePM, na.rm = T),
  total_asthma_ed_visits = sum(asthma_ed_visits, na.rm = T),
  total_asthma_ed_visits_lb = sum(asthma_ed_visits_lb, na.rm = T),
  total_asthma_ed_visits_ub = sum(asthma_ed_visits_ub, na.rm = T)
),
by = c("fire_name", "fire_id", "ID")
] %>%
  as.data.frame() %>%
  left_join(grid_transform, by = c("ID")) %>%
  st_as_sf()

elapsed_pop_fire_smokepm_dt <- pop_fire_smokepm_dt[globfire_date_df,
  `:=`(IDate = i.IDate, FDate = i.FDate, rank = i.rank),
  on = "fire_name"
][, `:=`(days_elapsed = date - IDate)][, .(
  start_date = median(IDate),
  total_affected_pop = sum(pop, na.rm = T),
  total_pop_smokepm = sum(pop_smokepm, na.rm = T),
  total_contrib_smokePM = sum(contrib_smokePM, na.rm = T),
  total_asthma_ed_visits = sum(asthma_ed_visits, na.rm = T),
  total_asthma_ed_visits_lb = sum(asthma_ed_visits_lb, na.rm = T),
  total_asthma_ed_visits_ub = sum(asthma_ed_visits_ub, na.rm = T),
  avg_affected_pop = mean(pop, na.rm = T),
  avg_pop_smokepm = mean(pop_smokepm, na.rm = T),
  avg_contrib_smokePM = mean(contrib_smokePM, na.rm = T)
),
by = c("fire_name", "days_elapsed")
]

## custom color scaler for map
custom_trans <- function() {
  trans <- function(x) {
    ifelse(x < 0, 0, x^(1 / 4))
  }
  inv <- function(x) {
    x^4
  }
  scales::trans_new("custom", trans, inv)
}

## labels evenly spaced on transformed units
seq(from = 0, to = (max(plot_pop_smokepm_df$total_asthma_ed_visits))^(1 / 4), length.out = 5)^4

## text with info of fire
label_text_dt <- pop_fire_smokepm_dt[globfire_date_df,
  `:=`(IDate = i.IDate, FDate = i.FDate, rank = i.rank),
  on = "fire_name"
][, `:=`(days_elapsed = date - IDate)][, .(
  rank = median(rank),
  year = median(year),
  total_days_elapsed = max(days_elapsed, na.rm = T),
  total_affected_pop = sum(pop, na.rm = T),
  total_pop_smokepm = sum(pop_smokepm, na.rm = T),
  total_contrib_smokePM = sum(contrib_smokePM, na.rm = T),
  total_asthma_ed_visits = sum(asthma_ed_visits, na.rm = T),
  total_asthma_ed_visits_lb = sum(asthma_ed_visits_lb, na.rm = T),
  total_asthma_ed_visits_ub = sum(asthma_ed_visits_ub, na.rm = T),
  avg_affected_pop = mean(pop, na.rm = T),
  avg_pop_smokepm = mean(pop_smokepm, na.rm = T),
  avg_contrib_smokePM = mean(contrib_smokePM, na.rm = T)
),
by = c("fire_name")
][order(fire_name)][, `:=`(
  rank = paste0("bold('", rank, ".')"),
  fire_name_text = paste0("bold('", fire_name, "')"),
  days_elapsed_text = paste0("bold('days elapsed:')~", total_days_elapsed),
  total_affected_pop_MN_text = paste0(round(total_affected_pop / 1e6, digits = 1), "~mn~affected~person~days"),
  total_pop_smokepm_BN_text = paste0("bold('tot. smoke exposure:')~", round(total_pop_smokepm / 1e9, digits = 1), "~bn~person~mu*g/m**3"),
  avg_contrib_smokePM_text = paste0("bold('avg. smokePM:')~", round(avg_contrib_smokePM, digits = 1), "~mu*g/m**3"),
  total_asthma_ed_visits_text = str_glue("bold('tot. excess asthma ED visits:')~{round(total_asthma_ed_visits,0)}~({round(total_asthma_ed_visits_lb,0)}*'-'*{round(total_asthma_ed_visits_ub,0)})", .envir = .SD)
)][, `:=`(
  label_text = paste0(
    fire_name_text, "~`|`~", year, "~`|`~", days_elapsed_text, "~`|`~",
    total_pop_smokepm_BN_text, "~`|`~", avg_contrib_smokePM_text
  ),
  label_text_ln_1 = paste0(rank, "~", fire_name_text, "~`|`~", year),
  label_text_ln_2 = paste0(total_pop_smokepm_BN_text),
  label_text_ln_3 = paste0(total_asthma_ed_visits_text)
)]


### generate map and elapsed time ----
Y_MAX <- max(elapsed_pop_fire_smokepm_dt$total_asthma_ed_visits)
p_pop_smokepm_list <- lapply(seq(from = 1, to = 9, by = 3), function(START_IDX) {
  FIRE_IDX <- START_IDX:(START_IDX + 2)

  p_pop_smokepm <- ggplot(data = plot_pop_smokepm_df %>% filter(fire_name %in% top_fires_dt[FIRE_IDX, fire_name])) +
    map_theme +
    theme(legend.title = element_markdown(size = 10)) +
    geom_sf(data = us_shape, fill = NA, lwd = 0.1) +
    geom_sf(aes(fill = total_asthma_ed_visits), color = NA) +
    geom_sf(
      data = globfire_plot_df %>% filter(fire_name %in% top_fires_dt[FIRE_IDX, fire_name]),
      fill = "#00B0F6", color = NA, lwd = 0.1
    ) +
    geom_sf(
      data = globfire_plot_df %>% filter(fire_name %in% top_fires_dt[FIRE_IDX, fire_name]) %>%
        rowwise() %>%
        mutate(
          bbox = purrr::map(geometry, st_bbox),
          geometry = st_as_sfc(bbox)
        ) %>%
        dplyr::select(-bbox) %>%
        st_set_crs("epsg:5070") %>%
        st_buffer(dist = 30000),
      color = "black", lwd = 0.5, linetype = "solid", fill = NA
    ) +
    scale_fill_viridis_c(
      option = "magma", direction = -1, trans = "custom",
      limits = c(0, 15),
      breaks = c(0.0, 1, 5, 10, 15),
      labels = scales::label_number(suffix = "", accuracy = 1)
    ) +
    facet_wrap(fire_name ~ ., ncol = 3) +
    labs(fill = "Total excess asthma<br>emergency dept. visits")

  save_plot(str_glue("output/fig3/pop_smokepm_maps_{FIRE_IDX[1]}-{FIRE_IDX[3]}.png"),
    plot = p_pop_smokepm, base_width = 7, base_height = 1.5, unit = "in", scale = 1.15
  )

  ## plot elpased timeline
  p_elapsed_fire_pop_smokepm <- ggplot(data = elapsed_pop_fire_smokepm_dt %>% filter(fire_name %in% top_fires_dt[FIRE_IDX, fire_name])) +
    theme_minimal() +
    theme(
      axis.line = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0.5, "lines"),
      plot.subtitle = element_text(hjust = 0.5),
      plot.margin = unit(c(1.8, 0.1, 0.3, 0.1), "in"),
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      axis.title.y = element_markdown(hjust = 0.5, vjust = 1),
      text = element_text(size = 8)
    ) +
    geom_line(aes(x = days_elapsed, y = total_asthma_ed_visits)) +

    # top line
    geom_text(
      data = label_text_dt %>% filter(fire_name %in% top_fires_dt[FIRE_IDX, fire_name]),
      aes(x = -7, y = Y_MAX * 4.1, label = label_text_ln_1),
      size = 3, hjust = 0, vjust = 0, parse = T
    ) +

    # date at origin
    geom_text(
      data = globfire_date_df %>% filter(fire_name %in% top_fires_dt[FIRE_IDX, fire_name]),
      aes(x = 0, y = -Y_MAX / 6, label = format(IDate, "%b %d"), hjust = 0, vjust = 1),
      size = 2.75, color = "gray50"
    ) +

    # smoke severity line
    geom_text(
      data = label_text_dt %>% filter(fire_name %in% top_fires_dt[FIRE_IDX, fire_name]),
      aes(
        y = -Y_MAX / 1.22, x = max(label_text_dt$total_days_elapsed) / 2,
        label = label_text_ln_2
      ),
      size = 2.75, hjust = 0.5, vjust = 0, parse = T
    ) +

    # excess asthma visits
    geom_text(
      data = label_text_dt %>% filter(fire_name %in% top_fires_dt[FIRE_IDX, fire_name]),
      aes(
        y = -Y_MAX / 1.19, x = max(label_text_dt$total_days_elapsed) / 2,
        label = label_text_ln_3
      ),
      size = 2.75, hjust = 0.5, vjust = 1, parse = T
    ) +
    facet_wrap(fire_name ~ ., ncol = 3) +
    coord_cartesian(
      ylim = c(0, 140),
      xlim = c(0, max(label_text_dt$total_days_elapsed)),
      clip = "off"
    ) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = c(20, 40, 60),
      labels = c(20, 40, 60)
    ) +
    scale_y_continuous(
      labels = scales::label_number(suffix = ""),
      breaks = seq(from = 0, to = max(elapsed_pop_fire_smokepm_dt$total_asthma_ed_visits), length.out = 3)
    ) +
    labs(x = "Days elapsed", y = "Excess asthma<br>emergency dept. visits")

  save_plot(str_glue("output/fig3/sparkline_{FIRE_IDX[1]}-{FIRE_IDX[3]}.pdf"),
    plot = p_elapsed_fire_pop_smokepm, base_width = 7, base_height = 2.5, unit = "in", scale = 1.2,
    device = cairo_pdf
  )

  ## needed to grab legend
  return(p_pop_smokepm)
})

p_fig3_legend <- cowplot::get_legend(p_pop_smokepm_list[[1]] +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(0.6, "in"),
    legend.title = element_markdown(size = 14, hjust = 0, vjust = 2),
    legend.text = element_text(size = 12)
  ))

grid.newpage()
grid.draw(p_fig3_legend)

save_plot(str_glue("output/fig3/pop_smokepm_maps_legend.pdf"),
  plot = p_fig3_legend, base_width = 7, base_height = .75, unit = "in",
  scale = 1.15, device = cairo_pdf
)






## Figure 4 ----

### burned area vs smokePM ----
## use all globfire that has smokePM calculated
## NEED TO RUN Figure 3 code first to get `total_smokepm_dt`
globfire_ba_df <- globfire_df %>%
  st_transform(crs = "epsg:5070") %>%
  st_filter(us_shape) %>%
  mutate(
    Id = as.character(Id),
    year = year(IDate),
    burned_area = st_area(geometry)
  ) %>%
  units::drop_units() %>%
  st_drop_geometry() %>%
  group_by(Id, IDate, FDate) %>%
  summarize(
    burned_area_m2 = mean(burned_area, na.rm = T),
    burned_area_acres = burned_area_m2 / 4047
  ) %>%
  left_join(total_smokepm_dt[, .(
    total_pop_smokepm = sum(total_pop_smokePM, na.rm = T),
    total_contrib_smokePM = sum(total_contrib_smokePM, na.rm = T),
    total_asthma_ed_visits = sum(total_asthma_ed_visits, na.rm = T)
  ),
  by = c("fire_id")
  ], by = c("Id" = "fire_id"))

### suppression vs smokePM ----
## use globfire that matches criteria of suppression data
globfire_suppression_df <- globfire_df %>%
  st_transform(crs = "epsg:5070") %>%
  st_filter(western_shape) %>%
  mutate(
    Id = as.character(Id),
    year = year(IDate),
    burned_area = st_area(geometry)
  ) %>%
  filter(burned_area >= units::set_units(1.214e+6, "m^2")) %>% # filter to greater than or equal to 300 acres
  st_buffer(500) # buffer after area calc for joining with fire ignition points

## read in fire suppression data from baylis and boomhower (2023)
fire_suppression_df <- read_fst(str_glue("data/clean/fires_all.fst"), as.data.table = T) %>%
  filter(
    year %in% c(2006:2020), ## suppression data only runs through 2016
    area_burned > 300, ## filter to greater than or equal to 300 acres
    fire_cost_real2017 > 0,
    fire_cost > 1,
    fire_name %not_in% c(
      "ALL ODF FIRES", ## all oregon fires for 2 years grouped together?
      "LICK" ## 2007 lick fire cost doesn't match records
    )
  ) %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326, agr = "constant") %>%
  st_transform("epsg:5070") %>%
  st_filter(western_shape) # limit to just western states (shouldnt really filter that many out)

fire_suppression_year_list <- sort(unique(fire_suppression_df$year))
globfire_year_list <- sort(unique(globfire_suppression_df$year))

globfire_suppression_joined_df <- pblapply(globfire_year_list, function(main_year_) {
  # spatial join doesnt also join on year so filter then spatial join
  if (main_year_ %in% fire_suppression_year_list) {
    ## subset to relevant year
    fire_suppression_temp_df <- fire_suppression_df %>%
      filter(year == main_year_)

    globfire_temp_df <- globfire_suppression_df %>%
      filter(year == main_year_) %>%
      mutate(Id = as.character(Id))

    ## spatial join and filter out unlikely matches starting with very tight bounds on date match
    temp_globfire_suppression_df <- globfire_temp_df %>%
      st_join(fire_suppression_temp_df) %>%
      filter(((IDate - 3) <= ignition_date) & (ignition_date <= IDate + 2)) %>%
      dplyr::select(
        Id, fire_name, IDate, FDate, ignition_date, fire_id_new, STUSPS, data_source, agencyowner, source_is_owner,
        large, natural, fuelmodel, fuelgroup, burned_area, area_burned, fire_cost, fire_cost_real2017
      )

    return(temp_globfire_suppression_df)
  }
}) %>% bind_rows()

# aggregate suppression cost data (bring in from separate globfire derived burned area df)
globfire_suppression_plot_df <- globfire_suppression_joined_df %>%
  units::drop_units() %>%
  st_drop_geometry() %>%
  group_by(Id, IDate, FDate) %>%
  summarize(
    fire_name = head(fire_name, 1),
    fire_id_new = head(fire_id_new, 1),
    burned_area_m2 = mean(burned_area, na.rm = T),
    burned_area_acres = burned_area_m2 / 4047,
    area_burned = mean(area_burned, na.rm = T),
    fire_cost = sum(unique(fire_cost), na.rm = T),
    fire_cost_real2017 = sum(unique(fire_cost_real2017), na.rm = T)
  )


### structures burned vs smokePM ----
## read in nifc data to get lat lon for some fires that have integer value lat lon in the structure data
## 515 duplicated unique ids where some are actual duplicates and others are different based on county name, fips, etc.
nifc_fires <- readr::read_csv("data/fire_locations/WFIGS_-_Wildland_Fire_Locations_Full_History.csv") %>%
  mutate(
    incident_number = str_replace(UniqueFireIdentifier, "(\\d{4})-(\\w{2})(\\w*)-(\\w*)", "\\2-\\3-\\4"),
    year = as.numeric(str_sub(UniqueFireIdentifier, start = 1, end = 4))
  ) %>%
  dplyr::select(
    UniqueFireIdentifier, year, incident_number, IncidentName,
    POOState, POOCounty, POOFips,
    X, Y, InitialLongitude, InitialLatitude,
    TotalIncidentPersonnel, EstimatedCostToDate
  ) %>%
  arrange(UniqueFireIdentifier) %>%
  group_by(
    UniqueFireIdentifier, year, incident_number,
    POOState, POOCounty, POOFips
  ) %>%
  summarize(
    X = mean(X, na.rm = T),
    Y = mean(Y, na.rm = T),
    InitialLongitude = ifelse(all(is.na(InitialLongitude)), NA, mean(InitialLongitude, na.rm = T)),
    InitialLatitude = ifelse(all(is.na(InitialLatitude)), NA, mean(InitialLatitude, na.rm = T)),
    TotalIncidentPersonnel = ifelse(all(is.na(TotalIncidentPersonnel)), NA, max(TotalIncidentPersonnel, na.rm = T)),
    EstimatedCostToDate = ifelse(all(is.na(EstimatedCostToDate)), NA, max(EstimatedCostToDate, na.rm = T))
  ) %>%
  as.data.frame()

## remove duplicated fires even after summarizing above (about 35 fires with same unique fire ids)
nifc_fires <- nifc_fires %>%
  filter(UniqueFireIdentifier %not_in% unique(nifc_fires[
    (duplicated(nifc_fires$UniqueFireIdentifier)) &
      (!is.na(nifc_fires$UniqueFireIdentifier)),
    "UniqueFireIdentifier"
  ]))

## filter to relevant years and merge in matched lat long from nifc data
## remove 53 fires that still have integer value lat lon
damaged_structure_df <- readxl::read_xlsx("data/clean/HE_Structures_Destroyed_2022.xlsx", sheet = "Data") %>%
  as.data.frame() %>%
  mutate(longitude = -1 * longitude) %>%
  rename(year = yr) %>%
  filter(year %in% c(2006:2020)) %>%
  left_join(nifc_fires, by = c("incident_number", "year")) %>%
  mutate(
    longitude = ifelse((longitude %% 1 == 0) & (!is.na(X)), X, longitude),
    latitude = ifelse((latitude %% 1 == 0) & (!is.na(Y)), Y, latitude)
  ) %>%
  filter(longitude %% 1 != 0 & latitude %% 1 != 0) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
  st_transform("epsg:5070") %>%
  st_filter(us_shape) %>%
  mutate(start_date = as.Date(start_date, format = "%m/%d/%Y"))

globfire_transform_df <- globfire_df %>%
  st_transform("epsg:5070") %>%
  st_filter(us_shape) %>%
  mutate(year = year(IDate)) %>%
  st_buffer(500) # buffer after area calc for joining with fire ignition points

damaged_structure_year_list <- sort(unique(damaged_structure_df$year))
globfire_year_list <- sort(unique(globfire_transform_df$year))

globfire_structure_joined_df <- pblapply(globfire_year_list, function(main_year_) {
  ## spatial join doesnt also join on year so filter then spatial join
  if (main_year_ %in% damaged_structure_year_list) {
    ## subset to relevant year
    damaged_structure_temp_df <- damaged_structure_df %>%
      filter(year == main_year_)

    globfire_temp_df <- globfire_transform_df %>%
      filter(year == main_year_) %>%
      mutate(Id = as.character(Id))

    ## spatial join and filter out unlikely matches starting with very tight bounds on date match
    temp_globfire_structure_df <- globfire_temp_df %>%
      st_join(damaged_structure_temp_df) %>%
      filter(((IDate - 3) <= start_date) & (start_date <= IDate + 2)) %>%
      dplyr::select(Id, IDate, FDate, start_date, incident_number, incident_name, structures_destroyed) %>%
      st_drop_geometry()

    return(temp_globfire_structure_df)
  }
}) %>% bind_rows()

globfire_structure_plot_df <- globfire_structure_joined_df %>%
  left_join(total_smokepm_dt[, .(
    total_pop_smokepm = sum(total_pop_smokePM, na.rm = T),
    total_contrib_smokePM = sum(total_contrib_smokePM, na.rm = T),
    total_asthma_ed_visits = sum(total_asthma_ed_visits, na.rm = T)
  ),
  by = c("fire_id")
  ], by = c("Id" = "fire_id"))

### burned area, suppression, structures burned data merge ----
globfire_ba_supp_struct_df <- globfire_ba_df %>%
  left_join(
    globfire_suppression_plot_df %>%
      dplyr::select(Id, IDate, FDate, fire_name, fire_id_new, area_burned, fire_cost, fire_cost_real2017),
    by = c("Id", "IDate", "FDate")
  ) %>%
  left_join(
    globfire_structure_plot_df %>%
      dplyr::select(Id, IDate, FDate, structures_destroyed),
    by = c("Id", "IDate", "FDate")
  )

## number of obs per plot
globfire_ba_supp_struct_df %>%
  filter(!is.na(burned_area_acres), !is.na(total_asthma_ed_visits)) %>%
  nrow()
globfire_ba_supp_struct_df %>%
  filter(!is.na(fire_cost_real2017), !is.na(total_asthma_ed_visits)) %>%
  nrow()
globfire_ba_supp_struct_df %>%
  filter(!is.na(structures_destroyed), !is.na(total_asthma_ed_visits)) %>%
  nrow()

long_globfire_ba_supp_struct_plot_df <- reshape2::melt(globfire_ba_supp_struct_df,
  id.vars = c("Id", "IDate", "FDate", "total_asthma_ed_visits"),
  measure.vars = c("burned_area_acres", "fire_cost_real2017", "total_contrib_smokePM", "structures_destroyed")
)


## calculate spearman rank correlation coefficient for each of the vars
mod_r_df <- lapply(
  c("burned_area_acres", "fire_cost_real2017", "structures_destroyed"),
  function(x_var) {
    temp_data <- long_globfire_ba_supp_struct_plot_df %>% filter(variable == x_var)
    mod <- cor.test(temp_data$value, temp_data$total_asthma_ed_visits, method = "spearman")

    result_df <- data.frame(
      variable = x_var,
      label = str_glue("italic(rho)~'='~{round(mod$estimate[[1]],2)}"),
      x = log(max(temp_data$value, na.rm = T)) * 0.9,
      y = 1e-11
    )

    return(result_df)
  }
) %>% bind_rows()


p_smoke_metrics_scatter <- ggplot(
  data = long_globfire_ba_supp_struct_plot_df %>%
    filter(variable != "total_contrib_smokePM") %>%
    mutate(variable = case_when(
      variable == "burned_area_acres" ~ "Burned area (natural log acres)",
      variable == "fire_cost_real2017" ~ "Fire cost (natural log 2017 dollars)",
      variable == "structures_destroyed" ~ "Structures destroyed (natural log)"
    )),
  aes(x = log(value), y = total_asthma_ed_visits)
) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-15, 0, 0, 0),
    panel.grid.minor = element_blank()
  ) +
  colorspace::scale_fill_continuous_sequential(name = "# of fires", palette = "BurgYl", trans = "log10") +
  geom_hex(bins = 50) +
  geom_text(
    data = mod_r_df %>%
      mutate(variable = case_when(
        variable == "burned_area_acres" ~ "Burned area (natural log acres)",
        variable == "fire_cost_real2017" ~ "Fire cost (natural log 2017 dollars)",
        variable == "structures_destroyed" ~ "Structures destroyed (natural log)"
      )),
    aes(x = x, y = y, label = label), color = "gray30", size = 3.5, parse = T
  ) +
  geom_smooth(formula = "y~x", method = "lm", se = FALSE, lwd = 0.3, linetype = 2, fullrange = T) +
  facet_wrap(variable ~ ., scales = "free_x") +
  labs(
    y = expression("Excess asthma emergency dept. visits"),
    x = ""
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(
    trans = "log", breaks = c(0.01, 0.1, 1, 10, 100, 1000),
    labels = as.character(c(0.01, 0.1, 1, 10, 100, 1000))
  )

save_plot("output/fig4/fig4.pdf",
  plot = p_smoke_metrics_scatter,
  base_width = 7.5, base_height = 3.5, scale = 1.15, device = cairo_pdf
)

save_plot("output/fig4/fig4.png",
  plot = p_smoke_metrics_scatter,
  base_width = 7.5, base_height = 3.5, scale = 1.15
)





## Figure 5 ----
## state alluvial and county transport maps

### state transport ratio ----
na_country_shapes <- st_read("data/etc/na_shape/na_country_shapes.shp") %>%
  st_transform(crs = st_crs("epsg:5070"))

## take US states but also add in country shapes from NA countries so that we can separate those out as sources...
globfire_matched_hysplit_init_df <- globfire_final_df %>%
  st_drop_geometry() %>%
  dplyr::select(Id, fire_centroid_x, fire_centroid_y) %>%
  mutate(Id = as.character(Id)) %>%
  filter(Id %in% unique(fire_hysplit_init_match_dt[Type == "FinalArea", Id])) %>%
  rename(lon = fire_centroid_x, lat = fire_centroid_y)

fire_state_dt <- fire_hysplit_init_match_dt[globfire_matched_hysplit_init_df,
  on = "Id",
  `:=`(lon = i.lon, lat = i.lat)
][, `:=`(
  centroid_lon = fifelse(is.na(lon), hysplit_init_x, lon),
  centroid_lat = fifelse(is.na(lat), hysplit_init_y, lat)
)] %>%
  dplyr::select(Id, IDate, FDate, Type, centroid_lon, centroid_lat) %>%
  unique() %>%
  st_as_sf(coords = c("centroid_lon", "centroid_lat"), crs = "epsg:5070", agr = "constant") %>%
  st_join(na_country_shapes %>% ## need to separate out canada and mexico fires
    rename(
      country_name = NAME_LONG,
      country_abb = BRK_A3
    ) %>%
    dplyr::select(country_name, country_abb)) %>%
  st_join(states_transform_df %>%
    dplyr::select(STUSPS, STATEFP, NAME)) %>%
  st_drop_geometry() %>%
  rename(
    fire_state = STUSPS,
    fire_state_fips = STATEFP,
    fire_state_name = NAME
  ) %>%
  mutate(
    fire_state = ifelse(country_name %not_in% c("United States"), country_abb, fire_state),
    fire_state_name = ifelse(country_name %not_in% c("United States"), country_name, fire_state_name)
  ) %>%
  dplyr::select(Id, IDate, FDate, Type, fire_state, fire_state_fips, fire_state_name) %>%
  as.data.table()

#### state proportion (with pop) ----
state_transport_smokepm_dt <- readRDS("data/clean/state_transport_popsmokepm_dt.rds")

#### pop smokepm state-state alluvial/sankey (2006-2010 & 2016-2020) ----
sorted_top_fire_states <- state_transport_smokepm_dt[(year <= 2010 | year >= 2016)][, `:=`(early_late = fifelse(year <= 2010, "Early", "Late"))][, .(
  total_contrib_smokePM = sum(total_contrib_smokePM, na.rm = T),
  total_pop_smokePM = sum(total_pop_smokePM, na.rm = T),
  total_asthma_ed_visits = sum(total_asthma_ed_visits, na.rm = T),
  total_asthma_ed_visits_lb = sum(total_asthma_ed_visits_lb, na.rm = T),
  total_asthma_ed_visits_ub = sum(total_asthma_ed_visits_ub, na.rm = T)
),
by = c("fire_state", "fire_state_name", "early_late")
][
  fire_state %not_in% c("CAN", "MEX") ## do canada and mexico separately...
][order(total_pop_smokePM, decreasing = TRUE)][
  , head(.SD, 5), early_late # top 5 US states
][, .(
  total_contrib_smokePM = sum(total_contrib_smokePM, na.rm = T),
  total_pop_smokePM = sum(total_pop_smokePM, na.rm = T),
  total_asthma_ed_visits = sum(total_asthma_ed_visits, na.rm = T),
  total_asthma_ed_visits_lb = sum(total_asthma_ed_visits_lb, na.rm = T),
  total_asthma_ed_visits_ub = sum(total_asthma_ed_visits_ub, na.rm = T)
),
by = c("fire_state", "fire_state_name", "early_late")
]

sorted_top_grid_states <- state_transport_smokepm_dt[(year <= 2010 | year >= 2016)][, `:=`(early_late = fifelse(year <= 2010, "Early", "Late"))][, .(
  total_contrib_smokePM = sum(total_contrib_smokePM, na.rm = T),
  total_pop_smokePM = sum(total_pop_smokePM, na.rm = T),
  total_asthma_ed_visits = sum(total_asthma_ed_visits, na.rm = T),
  total_asthma_ed_visits_lb = sum(total_asthma_ed_visits_lb, na.rm = T),
  total_asthma_ed_visits_ub = sum(total_asthma_ed_visits_ub, na.rm = T)
),
by = c("grid_state", "grid_state_name", "early_late")
][order(total_pop_smokePM, decreasing = TRUE)][, head(.SD, 10), early_late]

## rename source and sink states based on if they're top source or sinks
## doing this separately because early and late periods have different ordering and 'Other' always needs to be last
top_state_transport_dt_early <- state_transport_smokepm_dt[(year <= 2010), ][, `:=`(
  top_fire_state = fifelse(
    fire_state %in% c(sorted_top_fire_states[early_late == "Early"]$fire_state, "CAN", "MEX"),
    fire_state, "Other US"
  ),
  top_fire_state_name = fifelse(
    fire_state %in% c(sorted_top_fire_states[early_late == "Early"]$fire_state, "CAN", "MEX"),
    fire_state_name, "Other US"
  ),
  top_grid_state = fifelse(
    grid_state %in% sorted_top_grid_states[early_late == "Early"]$grid_state,
    grid_state, "Other US"
  ),
  top_grid_state_name = fifelse(
    grid_state %in% sorted_top_grid_states[early_late == "Early"]$grid_state,
    grid_state_name, "Other US"
  ),
  within_state = fifelse(fire_state == grid_state, "Within state", fifelse(
    fire_state %in% c("CAN", "MEX"),
    "Outside country", "Outside state"
  )),
  early_late = "Early"
)][, .(
  total_contrib_smokePM = sum(total_contrib_smokePM, na.rm = T),
  total_pop_smokePM = sum(total_pop_smokePM, na.rm = T),
  total_asthma_ed_visits = sum(total_asthma_ed_visits, na.rm = T),
  total_asthma_ed_visits_lb = sum(total_asthma_ed_visits_lb, na.rm = T),
  total_asthma_ed_visits_ub = sum(total_asthma_ed_visits_ub, na.rm = T)
),
by = c("top_fire_state", "top_fire_state_name", "top_grid_state", "top_grid_state_name", "within_state", "early_late")
][, `:=`(
  top_fire_state_name = ordered(top_fire_state_name,
    levels = c(sorted_top_fire_states[early_late == "Early"]$fire_state_name, "Canada", "Mexico", "Other US")
  ),
  top_grid_state = ordered(top_grid_state,
    levels = c(unique(sorted_top_grid_states[early_late == "Early"]$grid_state), "Other US")
  )
)][, `:=`(
  total_smoke = sum(total_contrib_smokePM, na.rm = T),
  total_pop_smoke = sum(total_pop_smokePM, na.rm = T),
  total_asthma_ed = sum(total_asthma_ed_visits, na.rm = T),
  total_asthma_ed_lb = sum(total_asthma_ed_visits_lb, na.rm = T),
  total_asthma_ed_ub = sum(total_asthma_ed_visits_ub, na.rm = T)
)][, `:=`(
  prop_smoke = sum(total_contrib_smokePM, na.rm = T) / total_smoke,
  prop_popsmoke = sum(total_pop_smokePM, na.rm = T) / total_pop_smoke,
  prop_asthma_ed_visits = sum(total_asthma_ed_visits, na.rm = T) / total_asthma_ed
),
by = c("top_fire_state", "top_fire_state_name")
][, `:=`(top_fire_state_name = paste0(top_fire_state_name, " (", round(prop_asthma_ed_visits * 100, 0), "%)"))][order(prop_asthma_ed_visits, decreasing = T)]

# store levels of top source states keep other, canada, and mexico at bottom (need to get these from dt above)
top_fire_early_levels <- unique(top_state_transport_dt_early$top_fire_state_name[top_state_transport_dt_early$top_fire_state_name %not_in% c(
  "Other US (34%)",
  "Canada (9%)",
  "Mexico (2%)"
)])

top_state_transport_dt_late <- state_transport_smokepm_dt[(year >= 2016), ][, `:=`(
  top_fire_state = fifelse(
    fire_state %in% c(sorted_top_fire_states[early_late == "Late"]$fire_state, "CAN", "MEX"),
    fire_state, "Other US"
  ),
  top_fire_state_name = fifelse(
    fire_state %in% c(sorted_top_fire_states[early_late == "Late"]$fire_state, "CAN", "MEX"),
    fire_state_name, "Other US"
  ),
  top_grid_state = fifelse(
    grid_state %in% sorted_top_grid_states[early_late == "Late"]$grid_state,
    grid_state, "Other US"
  ),
  top_grid_state_name = fifelse(
    grid_state %in% sorted_top_grid_states[early_late == "Late"]$grid_state,
    grid_state_name, "Other US"
  ),
  within_state = fifelse(fire_state == grid_state, "Within state", fifelse(
    fire_state %in% c("CAN", "MEX"),
    "Outside country", "Outside state"
  )),
  early_late = "Late"
)][, .(
  total_contrib_smokePM = sum(total_contrib_smokePM, na.rm = T),
  total_pop_smokePM = sum(total_pop_smokePM, na.rm = T),
  total_asthma_ed_visits = sum(total_asthma_ed_visits, na.rm = T),
  total_asthma_ed_visits_lb = sum(total_asthma_ed_visits_lb, na.rm = T),
  total_asthma_ed_visits_ub = sum(total_asthma_ed_visits_ub, na.rm = T)
),
by = c("top_fire_state", "top_fire_state_name", "top_grid_state", "top_grid_state_name", "within_state", "early_late")
][, `:=`(
  top_fire_state_name = ordered(top_fire_state_name, levels = c(
    sorted_top_fire_states[early_late == "Late"]$fire_state_name,
    "Canada", "Mexico", "Other US"
  )),
  top_grid_state = ordered(top_grid_state, levels = c(unique(sorted_top_grid_states[early_late == "Late"]$grid_state), "Other US"))
)][, `:=`(
  total_smoke = sum(total_contrib_smokePM, na.rm = T),
  total_pop_smoke = sum(total_pop_smokePM, na.rm = T),
  total_asthma_ed = sum(total_asthma_ed_visits, na.rm = T),
  total_asthma_ed_lb = sum(total_asthma_ed_visits_lb, na.rm = T),
  total_asthma_ed_ub = sum(total_asthma_ed_visits_ub, na.rm = T)
)][, `:=`(
  prop_smoke = sum(total_contrib_smokePM, na.rm = T) / total_smoke,
  prop_popsmoke = sum(total_pop_smokePM, na.rm = T) / total_pop_smoke,
  prop_asthma_ed_visits = sum(total_asthma_ed_visits, na.rm = T) / total_asthma_ed
),
by = c("top_fire_state", "top_fire_state_name")
][, `:=`(top_fire_state_name = paste0(top_fire_state_name, " (", round(prop_asthma_ed_visits * 100, 0), "%)"))][order(prop_asthma_ed_visits, decreasing = T)]

top_fire_late_levels <- unique(top_state_transport_dt_late$top_fire_state_name[top_state_transport_dt_late$top_fire_state_name %not_in% c(
  "Other US (30%)",
  "Canada (8%)",
  "Mexico (3%)"
)])


### generate alluvial flow plot ----
top_state_transport_dt_early_long <- to_lodes_form(
  top_state_transport_dt_early %>%
    dplyr::select(top_fire_state_name, top_grid_state, early_late, within_state, total_asthma_ed_visits),
  axes = 1:2, id = "flow"
) %>%
  mutate(
    stratum = ordered(stratum, levels = c(
      top_fire_early_levels,
      unique(sorted_top_grid_states[early_late == "Early"]$grid_state),
      "Other US",
      "Other US (34%)",
      "Canada (9%)",
      "Mexico (2%)"
    )),
    early_late = "Early\n(2006-2010)"
  )

top_state_transport_dt_late_long <- to_lodes_form(
  top_state_transport_dt_late %>%
    dplyr::select(top_fire_state_name, top_grid_state, early_late, within_state, total_asthma_ed_visits),
  axes = 1:2, id = "flow"
) %>%
  mutate(
    stratum = ordered(stratum, levels = c(
      top_fire_late_levels,
      unique(sorted_top_grid_states[early_late == "Late"]$grid_state),
      "Other US",
      "Other US (30%)",
      "Canada (8%)",
      "Mexico (3%)"
    )),
    early_late = "Late\n(2016-2020)"
  )

YMAX_state_transport <- top_state_transport_dt_late_long %>%
  filter(early_late == "Late\n(2016-2020)", x == "top_fire_state_name") %>%
  .$total_asthma_ed_visits %>%
  sum() + 10

label_out_rect_right <- c(10:12)
label_out_rect_left <- c(1, 4, 5)

p_alluvial_early <- ggplot(
  top_state_transport_dt_early_long,
  aes(y = total_asthma_ed_visits, stratum = stratum, alluvium = flow, x = x, label = stratum)
) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_markdown(),
    legend.position = "none",
    plot.margin = margin(0, 1, 0, 0, "cm"),
  ) +
  geom_flow(aes(fill = within_state, alpha = within_state)) +
  geom_stratum() +
  geom_text(
    stat = "stratum", aes(label = ifelse(after_stat(deposit) %not_in% c(label_out_rect_left, label_out_rect_right),
      as.character(after_stat(stratum)), ""
    )),
    size = 3.5
  ) +
  ggrepel::geom_text_repel(
    stat = "stratum",
    aes(label = ifelse(after_stat(deposit) %in% label_out_rect_right,
      as.character(after_stat(stratum)), ""
    )),
    nudge_x = 1.5,
    direction = "y",
    hjust = 0.5,
    segment.curvature = 1e-20,
    segment.color = "gray80",
    size = 3.5
  ) +
  ggrepel::geom_text_repel(
    stat = "stratum",
    aes(label = ifelse(after_stat(deposit) %in% label_out_rect_left,
      as.character(after_stat(stratum)), ""
    )),
    nudge_x = -.5,
    direction = "y",
    hjust = 0.5,
    segment.curvature = 1e-20,
    segment.color = "gray80",
    size = 3.5
  ) +
  scale_x_discrete(
    limits = c("top_fire_state_name", "top_grid_state"), expand = expansion(add = c(0.49, 0.3)),
    labels = c("top_fire_state_name" = "Source", "top_grid_state" = "Receptor state")
  ) +
  scale_alpha_manual(values = c("Outside state" = 0.75, "Within state" = 0.9, "Outside country" = 0.5), guide = "none") +
  scale_fill_manual(values = c(
    "Within state" = "#9ECAE1",
    "Outside state" = "#DEEBF7",
    "Outside country" = "#9ee1c5"
  )) +
  labs(fill = "Smoke source", y = "Total excess asthma emergency department visits", x = "") +
  scale_y_continuous(labels = scales::label_comma(), limits = c(0, YMAX_state_transport), expand = c(0, 0)) +
  facet_wrap(early_late ~ .)

p_alluvial_late <- ggplot(
  top_state_transport_dt_late_long,
  aes(y = total_asthma_ed_visits, stratum = stratum, alluvium = flow, x = x, label = stratum)
) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "pt")
  ) +
  geom_flow(aes(fill = within_state, alpha = within_state)) +
  geom_stratum() +
  geom_text(stat = "stratum", size = 3.5) +
  scale_x_discrete(
    limits = c("top_fire_state_name", "top_grid_state"), expand = c(.05, .05),
    labels = c("top_fire_state_name" = "Source", "top_grid_state" = "Receptor state")
  ) +
  scale_alpha_manual(values = c("Outside state" = 0.75, "Within state" = 0.9, "Outside country" = 0.5), guide = "none") +
  scale_fill_manual(values = c(
    "Within state" = "#9ECAE1",
    "Outside state" = "#DEEBF7",
    "Outside country" = "#9ee1c5"
  )) +
  labs(fill = "Smoke source", y = "Total excess asthma emergency department visits", x = "") +
  scale_y_continuous(labels = scales::label_comma(), limits = c(0, YMAX_state_transport), expand = c(0, 0)) +
  facet_wrap(early_late ~ .)

p_alluvial_flow <- p_alluvial_early +
  p_alluvial_late +
  plot_layout(guides = "collect") & theme(legend.position = "bottom", legend.box.spacing = unit(-5, "pt"))

## % of smokepm from outside of state (abstract 60% source outside state statistic)
state_transport_smokepm_dt[, `:=`(source_smokepm = fifelse(grid_state == fire_state, "Source within state", "Source outside state"))][, `:=`(
  sum_smokePM = sum(total_contrib_smokePM, na.rm = T),
  sum_pop_smokepm = sum(total_pop_smokePM, na.rm = T)
)][, .(
  perc_total_contrib_smokePM = sum(total_contrib_smokePM, na.rm = T) / sum_smokePM,
  perc_total_pop_smokePM = sum(total_pop_smokePM, na.rm = T) / sum_pop_smokepm
),
by = c("source_smokepm")
] %>% unique()

## for receiving states the % of smokepm from outside of state
state_transport_smokepm_dt[, `:=`(source_smokepm = fifelse(grid_state == fire_state, "Source within state", "Source outside state"))][, `:=`(
  sum_smokePM = sum(total_contrib_smokePM, na.rm = T),
  sum_pop_smokepm = sum(total_pop_smokePM, na.rm = T)
),
by = c("grid_state_name")
][, .(
  perc_total_contrib_smokePM = sum(total_contrib_smokePM, na.rm = T) / sum_smokePM,
  perc_total_pop_smokePM = sum(total_pop_smokePM, na.rm = T) / sum_pop_smokepm
),
by = c("source_smokepm", "grid_state_name")
][order(grid_state_name)] %>% unique()


### county transport monthly ----
counties <- tigris::counties(cb = TRUE)

counties_transform_df <- counties %>%
  st_transform(crs = st_crs("epsg:5070")) %>%
  st_make_valid()

fire_county_dt <- fire_hysplit_init_match_dt[globfire_matched_hysplit_init_df,
  on = "Id",
  `:=`(lon = i.lon, lat = i.lat)
][, `:=`(
  centroid_lon = fifelse(is.na(lon), hysplit_init_x, lon),
  centroid_lat = fifelse(is.na(lat), hysplit_init_y, lat)
)] %>%
  dplyr::select(Id, IDate, FDate, Type, centroid_lon, centroid_lat) %>%
  unique() %>%
  st_as_sf(coords = c("centroid_lon", "centroid_lat"), crs = "epsg:5070", agr = "constant") %>%
  st_join(counties_transform_df %>%
    dplyr::select(STATEFP, COUNTYFP, COUNTYNS, NAME)) %>%
  st_drop_geometry() %>%
  rename(
    fire_state_fips = STATEFP,
    fire_county_fips = COUNTYFP,
    fire_county_ns = COUNTYNS,
    fire_county_name = NAME
  ) %>%
  as.data.table()

grid_county_dt <- grid_transform %>%
  dplyr::select(ID) %>%
  st_centroid() %>%
  st_join(counties_transform_df %>%
    dplyr::select(STATEFP, COUNTYFP, COUNTYNS, NAME)) %>%
  st_drop_geometry() %>%
  rename(
    grid_state_fips = STATEFP,
    grid_county_fips = COUNTYFP,
    grid_county_ns = COUNTYNS,
    grid_county_name = NAME
  ) %>%
  as.data.table()

#### county source (county level) ----
## aggregate county smoke transport to the county level for each month and year
SAVE_PATH_COUNTY <- "data/clean/county_transport/"
MONTH_YEAR_LIST <- unique(zoo::as.yearmon(seq.Date(from = ymd("2006-04-19"), to = ymd("2020-12-31"), by = "days")))

month_county_transport_smokepm_dt <- pbmclapply(MONTH_YEAR_LIST, function(MONTH_YEAR) {
  year_ <- year(MONTH_YEAR)
  month_ <- str_pad(month(MONTH_YEAR), 2, pad = "0")

  temp_fire_smokepm_dt <- read_fst(str_glue("{SAVE_PATH_COUNTY}/{year_}-{month_}_county_transport_dt.fst"), as.data.table = T)

  temp_transport_smokepm_dt <- temp_fire_smokepm_dt[, `:=`(source_smokepm = fifelse(grid_county_ns == fire_county_ns, "Source within county", "Source outside county"))][, .(
    total_contrib_smokePM = sum(total_contrib_smokePM, na.rm = T),
    total_pop_smokePM = sum(total_pop_smokePM, na.rm = T)
  ),
  by = c(
    "month", "year", "grid_state_fips", "grid_county_fips", "grid_county_ns",
    "grid_county_name", "source_smokepm"
  )
  ]

  return(temp_transport_smokepm_dt)
}) %>% bind_rows()

#### county total PM2.5 over month year (for county transport plot)----
year_list <- 2006:2020
month_county_pm25_dt <- pbmclapply(year_list, function(year_) {
  temp_year_pm25_western_states <- read_fst(path = str_glue("data/pm25/extracted_pm25/{year_}_western_states_pm25_dt.fst"), as.data.table = T)

  ## smoke data starts 2006-04-19
  if (year_ == 2006) {
    temp_year_pm25_western_states <- temp_year_pm25_western_states[date >= "2006-04-19"]
  }

  temp_month_county_pm25_dt <- temp_year_pm25_western_states[, `:=`(
    month = month(date),
    year = year(date)
  )][grid_county_dt, `:=`(
    grid_state_fips = i.grid_state_fips,
    grid_county_fips = i.grid_county_fips,
    grid_county_ns = i.grid_county_ns,
    grid_county_name = i.grid_county_name
  ),
  on = c(ID = "ID")
  ][, .(total_pm25 = sum(pm25, na.rm = T)),
    by = c(
      "month", "year", "grid_state_fips", "grid_county_fips",
      "grid_county_ns", "grid_county_name"
    )
  ][!is.na(grid_county_ns)]

  return(temp_month_county_pm25_dt)
}) %>% bind_rows()

#### western states county transport ratio maps ----
## filter to western states for county smokePM from within state
#### early period
county_pm25_early <- month_county_pm25_dt[year %in% c(2006:2010)][, .(total_pm25 = sum(total_pm25, na.rm = T)),
  by = c(
    "grid_state_fips", "grid_county_fips", "grid_county_ns",
    "grid_county_name"
  )
]

county_smokepm_pm_ratio_western_early <- month_county_transport_smokepm_dt[year %in% c(2006:2010)][, .(total_contrib_smokePM = sum(total_contrib_smokePM, na.rm = T)),
  by = c(
    "grid_state_fips", "grid_county_fips", "grid_county_ns",
    "grid_county_name", "source_smokepm"
  )
][, `:=`(gridcell_total_smokePM = sum(total_contrib_smokePM, na.rm = T)),
  by = c("grid_county_ns")
][county_pm25_early,
  `:=`(total_pm25 = i.total_pm25),
  on = c(
    "grid_state_fips", "grid_county_fips", "grid_county_ns",
    "grid_county_name"
  )
] %>%
  as.data.frame() %>%
  left_join(counties_transform_df, by = c("grid_county_ns" = "COUNTYNS")) %>%
  filter(STUSPS %in% western_states) %>%
  st_as_sf()

#### late period
county_pm25_late <- month_county_pm25_dt[year %in% c(2016:2020)][, .(total_pm25 = sum(total_pm25, na.rm = T)),
  by = c(
    "grid_state_fips", "grid_county_fips", "grid_county_ns",
    "grid_county_name"
  )
]

county_smokepm_pm_ratio_western_late <- month_county_transport_smokepm_dt[year %in% c(2016:2020)][, .(total_contrib_smokePM = sum(total_contrib_smokePM, na.rm = T)),
  by = c(
    "grid_state_fips", "grid_county_fips", "grid_county_ns",
    "grid_county_name", "source_smokepm"
  )
][, `:=`(gridcell_total_smokePM = sum(total_contrib_smokePM, na.rm = T)),
  by = c("grid_county_ns")
][county_pm25_late,
  `:=`(total_pm25 = i.total_pm25),
  on = c(
    "grid_state_fips", "grid_county_fips", "grid_county_ns",
    "grid_county_name"
  )
] %>%
  as.data.frame() %>%
  left_join(counties_transform_df, by = c("grid_county_ns" = "COUNTYNS")) %>%
  filter(STUSPS %in% western_states) %>%
  st_as_sf()

## needed for facets
county_smokepm_pm_ratio_western_early$period <- "Early\n(2006-2010)"
county_smokepm_pm_ratio_western_late$period <- "Late\n(2016-2020)"

county_smokepm_pm_ratio_western <- bind_rows(
  county_smokepm_pm_ratio_western_early,
  county_smokepm_pm_ratio_western_late
)

##### early/late smokepm-pm ratio county facets ----
p_county_transport_ratio <- ggplot(data = county_smokepm_pm_ratio_western %>%
  filter(source_smokepm == "Source outside county")) +
  map_theme +
  theme(
    strip.text.x = element_text(size = 9),
    legend.title = element_markdown(size = 10),
    legend.text = element_text(size = 8),
    legend.position = "bottom",
    panel.spacing.x = unit(8, "lines"),
  ) +
  geom_sf(data = western_shape, fill = NA, lwd = 0.1) +
  geom_sf(aes(fill = total_contrib_smokePM / total_pm25), color = NA) +
  facet_wrap(period ~ ., nrow = 1) +
  scale_fill_viridis_c(
    name = "% of total PM<sub>2.5</sub><br>from outside<br>county fire sources",
    option = "rocket", direction = -1, label = scales::label_percent(),
    breaks = c(0, .15, .30, .45)
  )


## avg county smoke pm proportion in early vs late periods
county_smokepm_pm_ratio_western %>%
  filter(source_smokepm == "Source outside county") %>%
  mutate(smokepm_prop = total_contrib_smokePM / total_pm25) %>%
  group_by(period) %>%
  summarize(avg_prop = mean(smokepm_prop, na.rm = T))

## top 5 counties by outside county smoke experienced
county_smokepm_pm_ratio_western %>%
  filter(source_smokepm == "Source outside county") %>%
  mutate(smokepm_prop = total_contrib_smokePM / total_pm25) %>%
  group_by(period) %>%
  arrange(desc(smokepm_prop), .by_group = TRUE) %>%
  top_n(5) %>%
  dplyr::select(
    grid_county_name, total_contrib_smokePM, total_pm25,
    NAMELSAD, STUSPS, STATE_NAME, period, smokepm_prop
  )

## wider version
county_smokepm_change_df <- county_smokepm_pm_ratio_western %>%
  filter(source_smokepm == "Source outside county") %>%
  mutate(smokepm_prop = total_contrib_smokePM / total_pm25) %>%
  dplyr::select(grid_county_name, NAMELSAD, STUSPS, STATE_NAME, period, smokepm_prop) %>%
  tidyr::pivot_wider(names_from = "period", values_from = "smokepm_prop") %>%
  arrange(desc(`Early\n(2006-2010)`)) %>%
  mutate(perc_change = (`Late\n(2016-2020)` - `Early\n(2006-2010)`) / `Early\n(2006-2010)`)

## % of smokepm from outside county (abstract source outside county statistic)
## also % of experienced smoke exposure from trans-county sources in results
month_county_transport_smokepm_dt[, `:=`(
  sum_smokePM = sum(total_contrib_smokePM, na.rm = T),
  sum_pop_smokepm = sum(total_pop_smokePM, na.rm = T)
)][, .(
  perc_total_contrib_smokePM = sum(total_contrib_smokePM, na.rm = T) / sum_smokePM,
  perc_total_pop_smokePM = sum(total_pop_smokePM, na.rm = T) / sum_pop_smokepm
),
by = c("source_smokepm")
] %>% unique()

## number of contiguous US Western state counties with increase in smokePM from out of county fires
sum(county_smokepm_change_df$perc_change > 0)

## number of counties with over 25% of total smokepm from out-of-county smoke
county_smokepm_pm_ratio_western %>%
  st_drop_geometry() %>%
  filter(source_smokepm == "Source outside county") %>%
  mutate(smokepm_totalpm_ratio = total_contrib_smokePM / total_pm25) %>%
  dplyr::select(grid_state_fips, grid_county_fips, grid_county_ns, grid_county_name, period, source_smokepm, smokepm_totalpm_ratio) %>%
  tidyr::pivot_wider(names_from = period, values_from = smokepm_totalpm_ratio) %>%
  filter(`Late\n(2016-2020)` > .25)

#### combine plots ----
p5_combined <- plot_grid(p_alluvial_flow,
  p_county_transport_ratio,
  ncol = 1,
  rel_heights = c(1.5, 1),
  align = "v", axis = "l", labels = "AUTO"
)

save_plot("output/fig5/fig5.pdf", plot = p5_combined, base_width = 7, base_height = 8, scale = 1.7, device = cairo_pdf)
save_plot("output/fig5/fig5.png", plot = p5_combined, base_width = 7, base_height = 8, scale = 1.7, bg = "white")




# Supplemental information figures and tables ----
## Supp Figure 2 + 3 ----
## clean fire sat, traj, smokepm, contrib smokepm
## American Fire for supp fig 2 and Camp Fire for supp fig 3

FIRE_NAME <- "American Fire"
# FIRE_NAME <- "Camp Fire"
FIRE_DT <- data.table(
  "fire_id" = c(23410594, 19662143, 16097817, 16215337, 22343661),
  "fire_name" = c(
    "Kincade Fire", "Soberanes Fire",
    "Mile Marker 28 Fire", "American Fire",
    "Camp Fire"
  ),
  "date" = c("2019-10-24", "2016-07-24", "2013-07-24", "2013-08-12", "2018-11-08"),
  "bbox" = list(
    c(xmin = -123.5539, ymin = 36.8525, xmax = -121.2473, ymax = 39.1591),
    c(xmin = -122.1235, ymin = 35.1994, xmax = -119.6351, ymax = 37.6878),
    c(xmin = -120.9163, ymin = 44.3780, xmax = -117.9919, ymax = 47.3025),
    c(xmin = -121.7304, ymin = 38.1391, xmax = -119.4216, ymax = 40.4479),
    c(xmin = -124.3025, ymin = 37.0677, xmax = -121.3244, ymax = 40.0458)
  )
)

FIRE_BBOX <- sf::st_bbox(FIRE_DT[fire_name == FIRE_NAME, bbox][[1]], crs = st_crs(4326)) %>%
  st_as_sfc() %>%
  st_transform("epsg:5070")

### sat image ----
sat_img_file <- stringr::str_glue("data/sat_img/{FIRE_NAME}_{FIRE_DT[fire_name==FIRE_NAME,date]}.jpg")

sat_raster <- raster::brick(sat_img_file)
crs(sat_raster) <- crs("EPSG:4326")
extent(sat_raster) <- c(
  FIRE_DT[fire_name == FIRE_NAME, bbox][[1]][["xmin"]],
  FIRE_DT[fire_name == FIRE_NAME, bbox][[1]][["xmax"]],
  FIRE_DT[fire_name == FIRE_NAME, bbox][[1]][["ymin"]],
  FIRE_DT[fire_name == FIRE_NAME, bbox][[1]][["ymax"]]
)
sat_raster <- projectRaster(sat_raster, crs = "EPSG:5070")

# crop and mask to only show cali shape
sat_raster <- mask(sat_raster, ca_shape)
sat_raster <- crop(sat_raster, extent(ca_shape))

### smoke pm ----
plot_date_str <- FIRE_DT[fire_name == FIRE_NAME, date]
formatted_date_str <- format.Date(plot_date_str, "%Y%m%d")
smokepm_df <- readRDS(sprintf("data/10km_smoke_days/smokePM_predictions_10km_%s.rds", formatted_date_str))

grid_smokepm_df <- grid_transform %>%
  left_join(smokepm_df, by = c("ID" = "grid_id_10km")) %>%
  st_crop(ca_shape) %>%
  mutate(
    smokePM_pred_coded = ifelse(smokePM_pred < 0, 0, smokePM_pred),
    smokePM_pred_capped = ifelse(smokePM_pred_coded >= 100, 100, smokePM_pred_coded)
  )

### hysplit trajectories ----
hysplit_plot_date_list <- seq.Date(from = ymd(plot_date_str) - 6, to = ymd(plot_date_str), by = "day") # need traj points from before first fire day of interest

hysplit_traj_files_list <- list.files(path = "data/traj_example", pattern = ".rds", recursive = T, full.names = T)
hysplit_df <- pbmclapply(hysplit_traj_files_list, function(x) {
  if (as.Date(str_split(str_split(x, "_")[[1]][4], ".rds")[[1]][1]) %in% hysplit_plot_date_list) {
    temp_df <- readRDS(x) %>%
      as.data.table()

    # filter and remove rained out or grounded traj points and get distance to successive points
    temp_df <- temp_df[, `:=`(traj_id = paste(lon_i, lat_i, traj_dt_i, sep = "_"))][order(traj_id, height_i, hour_along)][, `:=`(
      cumsum_precip = cumsum(rainfall),
      cummin_height = cummin(height)
    ),
    by = c("traj_id", "height_i")
    ][(cumsum_precip == 0) & (cummin_height > 0)] %>%
      as.data.frame() %>%
      dplyr::select(traj_dt, hour_along, lon, lat, height, lon_i, lat_i, traj_dt_i, height_i, traj_id) # select conflicts with raster so call from dplyr

    return(temp_df)
  }
}) %>%
  bind_rows() %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant") %>%
  mutate(date_i = as.Date(traj_dt_i)) %>%
  st_transform(st_crs("epsg:5070"))

## filter to just traj points related to specific fire
hysplit_filtered_df <- hysplit_df %>%
  left_join(st_drop_geometry(fire_hysplit_init_match_dt[, c("Id", "traj_id")]), by = "traj_id") %>%
  filter(Id %in% FIRE_DT[, fire_id]) %>%
  mutate(
    fire_name = case_when(
      Id == 23410594 ~ "Kincade Fire",
      Id == 19662143 ~ "Soberanes Fire",
      Id == 16097817 ~ "Mile Marker 28 Fire",
      Id == 16215337 ~ "American Fire",
      Id == 22343661 ~ "Camp Fire",
    ),
    fire_name = ordered(fire_name, levels = c(
      "Kincade Fire", "Soberanes Fire",
      "Mile Marker 28 Fire", "American Fire",
      "Camp Fire"
    ))
  )

hysplit_filtered_df$x <- st_coordinates(hysplit_filtered_df)[, 1]
hysplit_filtered_df$y <- st_coordinates(hysplit_filtered_df)[, 2]

## set up globfire polygon with correct name
globfire_plot_df <- globfire_df %>%
  filter(Id %in% FIRE_DT[, fire_id]) %>%
  mutate(
    fire_name = case_when(
      Id == 23410594 ~ "Kincade Fire",
      Id == 19662143 ~ "Soberanes Fire",
      Id == 16097817 ~ "Mile Marker 28 Fire",
      Id == 16215337 ~ "American Fire",
      Id == 22343661 ~ "Camp Fire"
    ),
    fire_name = ordered(fire_name, levels = c(
      "Kincade Fire", "Soberanes Fire",
      "Mile Marker 28 Fire", "American Fire",
      "Camp Fire"
    ))
  ) %>%
  st_transform(crs = st_crs("epsg:5070"))

fire_date_labeller <- function(x) {
  stringr::str_glue("{x} ({FIRE_DT[fire_name==x,date]})")
}


### contributed smokepm ----
SAVE_PATH <- "data/clean/fire_smokepm/"
WINDOW_SIZE <- 3
year_ <- year(plot_date_str)
month_ <- str_pad(month(plot_date_str), 2, pad = "0")
temp_fire_smokepm_dt <- read_fst(str_glue("{SAVE_PATH}{WINDOW_SIZE}0km/{year_}-{month_}_{WINDOW_SIZE}0km_focal_grid_fire_smokepm_agg.fst"), as.data.table = T)

specific_fire_smokepm_dt <- temp_fire_smokepm_dt[fire_id %in% FIRE_DT[, fire_id] & !is.na(contrib_smokePM), ] %>%
  as.data.frame() %>%
  left_join(grid_transform, by = c("ID")) %>%
  mutate(contrib_smokePM_capped = ifelse(contrib_smokePM >= 100, 100, contrib_smokePM)) %>%
  st_as_sf() %>%
  mutate(
    fire_name = case_when(
      fire_id == 23410594 ~ "Kincade Fire",
      fire_id == 19662143 ~ "Soberanes Fire",
      fire_id == 16097817 ~ "Mile Marker 28 Fire",
      fire_id == 16215337 ~ "American Fire",
      fire_id == 22343661 ~ "Camp Fire"
    ),
    fire_name = ordered(fire_name, levels = c(
      "Kincade Fire", "Soberanes Fire",
      "Mile Marker 28 Fire", "American Fire",
      "Camp Fire"
    ))
  )

### smokePM in concentric buffers around clean fires ----
globfire_plot_centroid_dt <- globfire_plot_df %>%
  st_centroid() %>%
  as.data.table()
globfire_plot_centroid_dt[, `:=`(
  buffer_50km = st_buffer(geometry, 50000),
  buffer_100km = st_buffer(geometry, 100000),
  buffer_200km = st_buffer(geometry, 200000)
)]

centroid_buffer_df <- lapply(c("buffer_50km", "buffer_100km", "buffer_200km"), function(CENTROID_BUFFER) {
  ## smokePM on plot date
  total_smokepm_df <- globfire_plot_centroid_dt[
    fire_name == FIRE_NAME,
    c("Id", "IDate", "FDate", "Type", "fire_name", ..CENTROID_BUFFER)
  ] %>%
    st_as_sf() %>%
    st_join(grid_smokepm_df %>% filter(date == plot_date_str)) %>%
    group_by(fire_name, date) %>%
    summarize(total_smokePM_pred_coded = sum(smokePM_pred_coded, na.rm = T)) %>%
    st_drop_geometry()

  ## contributed smokePM on plot date
  total_contrib_smokepm_df <- globfire_plot_centroid_dt[
    fire_name == FIRE_NAME,
    c("Id", "IDate", "FDate", "Type", "fire_name", ..CENTROID_BUFFER)
  ] %>%
    st_as_sf() %>%
    st_join(specific_fire_smokepm_dt %>%
      filter(date == plot_date_str) %>%
      dplyr::select(ID, date, contrib_smokePM)) %>%
    group_by(fire_name, date) %>%
    summarize(total_contrib_smokePM = sum(contrib_smokePM, na.rm = T)) %>%
    st_drop_geometry()

  ## merge data together
  result_df <- total_smokepm_df %>%
    left_join(total_contrib_smokepm_df, by = c("fire_name", "date")) %>%
    mutate(
      smokepm_coverage_ratio = total_contrib_smokePM / total_smokePM_pred_coded,
      buffer = ordered(str_split(CENTROID_BUFFER, "_")[[1]][2],
        levels = c("50km", "100km", "200km")
      )
    )

  return(result_df)
}) %>% bind_rows()

### generate plots ----
## get bounding box for map plots
bbox <- globfire_plot_df %>%
  filter(fire_name == FIRE_NAME) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_buffer(200000) %>%
  st_bbox()

color_pal <- RColorBrewer::brewer.pal(n = 6, "Blues") # get more colors than needed cause we want darker blues

p_smokepm_centroid_buffer <- ggplot() +
  map_theme +
  geom_sf(data = ca_shape, fill = NA, lwd = 0.1) +
  geom_sf(data = grid_smokepm_df %>% filter(date == plot_date_str), aes(fill = smokePM_pred_capped), color = NA) +
  scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0, 100), breaks = seq(0, 100, by = 25), labels = c(seq(0, 75, by = 25), "100+")) +
  geom_sf(data = globfire_plot_centroid_dt[fire_name == FIRE_NAME, "buffer_50km"] %>% st_as_sf(), aes(color = "50km"), fill = NA, lwd = 0.2) +
  geom_sf(data = globfire_plot_centroid_dt[fire_name == FIRE_NAME, "buffer_100km"] %>% st_as_sf(), aes(color = "100km"), fill = NA, lwd = 0.2) +
  geom_sf(data = globfire_plot_centroid_dt[fire_name == FIRE_NAME, "buffer_200km"] %>% st_as_sf(), aes(color = "200km"), fill = NA, lwd = 0.2) +
  scale_color_manual(name = "Centroid buffer", values = c(
    "50km" = color_pal[2],
    "100km" = color_pal[4],
    "200km" = color_pal[6]
  )) +
  labs(fill = "Smoke PM<sub>2.5</sub><br>(&mu;g/m<sup>3</sup>)", subtitle = str_glue("Smoke PM<sub>2.5</sub>")) +
  coord_sf(
    xlim = c(bbox$xmin, bbox$xmax),
    ylim = c(bbox$ymin, bbox$ymax)
  )

p_traj <- ggplot() +
  map_theme +
  geom_sf(data = ca_shape, fill = NA, lwd = 0.1) +
  geom_path(
    data = hysplit_filtered_df %>% filter(
      fire_name == FIRE_NAME,
      traj_dt >= ymd(plot_date_str),
      traj_dt < ymd(plot_date_str) + 1,
      date_i <= ymd(plot_date_str),
      height_i %in% c(500, 1500, 2500)
    ),
    aes(x = x, y = y, group = interaction(traj_id, height_i)), color = "black", size = 0.075, alpha = 0.5
  ) +
  facet_wrap(~fire_name,
    strip.position = "left",
    labeller = as_labeller(fire_date_labeller)
  ) +
  geom_sf(
    data = globfire_plot_df %>% filter(fire_name == FIRE_NAME),
    fill = "red", color = NA, lwd = 0.1
  ) +
  labs(subtitle = "HYSPLIT trajectories") +
  coord_sf(
    xlim = c(bbox$xmin, bbox$xmax),
    ylim = c(bbox$ymin, bbox$ymax)
  )

p_sat <- RStoolbox::ggRGB(sat_raster, r = 1, g = 2, b = 3) +
  map_theme +
  labs(subtitle = "Satellite imagery") +
  coord_sf(
    xlim = c(bbox$xmin, bbox$xmax),
    ylim = c(bbox$ymin, bbox$ymax)
  )


p_contrib_smokepm_centroid_buffer <- ggplot() +
  map_theme +
  theme(legend.position = "right") +
  geom_sf(data = ca_shape, fill = NA, lwd = 0.1) +
  geom_sf(data = specific_fire_smokepm_dt %>% filter(date == plot_date_str), aes(fill = contrib_smokePM_capped), color = NA) +
  scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0, 100), breaks = seq(0, 100, by = 25), labels = c(seq(0, 75, by = 25), "100+")) +
  geom_sf(data = globfire_plot_centroid_dt[fire_name == FIRE_NAME, "buffer_50km"] %>% st_as_sf(), aes(color = "50km"), fill = NA, lwd = 0.2) +
  geom_sf(data = globfire_plot_centroid_dt[fire_name == FIRE_NAME, "buffer_100km"] %>% st_as_sf(), aes(color = "100km"), fill = NA, lwd = 0.2) +
  geom_sf(data = globfire_plot_centroid_dt[fire_name == FIRE_NAME, "buffer_200km"] %>% st_as_sf(), aes(color = "200km"), fill = NA, lwd = 0.2) +
  scale_color_manual(name = "Distance buffer", values = c(
    "50km" = color_pal[2],
    "100km" = color_pal[4],
    "200km" = color_pal[6]
  )) +
  labs(fill = "Smoke PM<sub>2.5</sub><br>(&mu;g/m<sup>3</sup>)", subtitle = str_glue("Contributed smoke PM<sub>2.5</sub>")) +
  coord_sf(
    xlim = c(bbox$xmin, bbox$xmax),
    ylim = c(bbox$ymin, bbox$ymax)
  )

p_coverage_ratio_bar <- ggplot(data = centroid_buffer_df, aes(x = buffer, y = smokepm_coverage_ratio)) +
  base_theme +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 7),
    plot.subtitle = element_markdown(size = 8),
    axis.text = element_text(size = 7)
  ) +
  geom_bar(aes(fill = buffer), stat = "identity") +
  geom_text(aes(label = scales::percent(smokepm_coverage_ratio)), vjust = -1, color = "black", size = 3) +
  labs(x = "Centroid buffer", y = "", subtitle = "Contributed smoke PM<sub>2.5</sub> / smoke PM<sub>2.5</sub> within buffer") +
  scale_fill_manual(values = c(
    "50km" = color_pal[2],
    "100km" = color_pal[4],
    "200km" = color_pal[6]
  )) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.1))

p_coverage <- (p_traj + p_sat) / (p_smokepm_centroid_buffer + p_contrib_smokepm_centroid_buffer) / p_coverage_ratio_bar +
  plot_layout(heights = c(2, 2, 1))

save_plot(stringr::str_glue('output/figsupp/{str_replace(tolower(FIRE_NAME)," ", "_")}_coverage_ratio.png'), plot = p_coverage, base_width = 7, base_height = 8)





## Supp Figure 5 ----
## sparks monitor timeseries (sparks epa monitor id: 320311005; gridcell id: 158677)
GRIDCELL_ID <- 158677
MONITOR_ID <- 320311005
START_DATE <- "2013-08-04"
END_DATE <- "2013-09-07"

## monitoring station smoke PM for the lat lon...
SAVE_PATH <- "data/smokepm_training/"
smokepm_training_df <- readRDS(str_glue("{SAVE_PATH}smokePM_training.rds")) %>%
  dplyr::select(id, date, grid_id_10km, lat, lon, smokePM) %>%
  left_join(grid_state_dt, by = c("grid_id_10km" = "ID")) %>%
  mutate(monitor_id_str = stringr::str_pad(id, 9, pad = "0")) %>%
  filter(
    monitor_id_str == MONITOR_ID,
    date >= START_DATE,
    date <= END_DATE
  )

## epa monitoring station data (both total PM and smoke PM)
epa_monitor_pm_df <- readRDS("data/pm25/station_smokePM.rds") %>%
  mutate(monitor_id_str = stringr::str_pad(id, 9, pad = "0")) %>%
  filter(
    monitor_id_str == MONITOR_ID,
    date >= START_DATE,
    date <= END_DATE
  )

## fire smoke pm
SAVE_PATH <- "data/clean/fire_smokepm/"
WINDOW_SIZE <- 3
MONTH_YEAR_LIST <- unique(zoo::as.yearmon(seq.Date(from = ymd(START_DATE), to = ymd(END_DATE), by = "days")))

fire_smokepm_dt <- pbmclapply(MONTH_YEAR_LIST, function(MONTH_YEAR) {
  year_ <- year(MONTH_YEAR)
  month_ <- str_pad(month(MONTH_YEAR), 2, pad = "0")

  temp_fire_smokepm_dt <- read_fst(str_glue("{SAVE_PATH}{WINDOW_SIZE}0km/{year_}-{month_}_{WINDOW_SIZE}0km_focal_grid_fire_smokepm_agg.fst"), as.data.table = T)

  ## filter for specific monitor and matched fire_id
  temp_fire_smokepm_dt <- temp_fire_smokepm_dt[ID == GRIDCELL_ID &
    !is.na(fire_id) &
    date >= START_DATE &
    date <= END_DATE]

  return(temp_fire_smokepm_dt)
}) %>% bind_rows()

## find all fire_ids for globfire fires (search for fire name in MTBS matched globfire dataset)
fire_smokepm_dt[, fire_id][str_length(fire_smokepm_dt[, fire_id]) < 10] %>% unique()

## get major fire name and fill in 0 for no obs dates
fire_specific_contributed_smokePM_filled_df <- fire_smokepm_dt[, `:=`(fire_name = fcase(
  fire_id == "16215337", "American Fire",
  fire_id == "16215347", "Rim Fire",
  rep_len(TRUE, length(fire_id)), "Other fires"
))][, .(total_contrib_smokePM = sum(contrib_smokePM, na.rm = T)),
  by = c("fire_name", "ID", "date")
] %>%
  tidyr::complete(fire_name, date, fill = list(total_contrib_smokePM = 0))

## generate plots
color_pal <- RColorBrewer::brewer.pal(6, name = "Dark2")

p_timeseries <- ggplot() +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.line = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_markdown(hjust = 0.5, vjust = 1),
    legend.text = element_markdown(),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, 0, 0, 0),
  ) +
  geom_curve(aes(xend = as.Date("2013-08-08"), yend = 30, x = as.Date("2013-08-11"), y = 10.5),
    angle = 90, size = 0.3, color = "gray70", curvature = -0.25
  ) +
  geom_richtext(aes(x = as.Date("2013-08-08"), y = 30),
    label = "Sparks EPA station<br>calculated smoke PM<sub>2.5</sub>", size = 3, fill = "white", label.color = NA
  ) +
  geom_curve(aes(xend = as.Date("2013-08-19"), yend = 45, x = as.Date("2013-08-17"), y = 34),
    angle = 90, size = 0.3, color = "gray70", curvature = -0.1
  ) +
  geom_richtext(aes(x = as.Date("2013-08-19"), y = 45),
    label = "Sparks EPA station<br>total PM<sub>2.5</sub>", size = 3, fill = "white", label.color = NA
  ) +
  geom_vline(aes(xintercept = as.Date("2013-08-15")), linetype = 1, color = "gray90") +
  geom_text(aes(x = as.Date("2013-08-15"), y = 85), label = "Aug. 15, 2013", size = 3, hjust = 1, nudge_x = -.25) +
  geom_vline(aes(xintercept = as.Date("2013-08-23")), linetype = 1, color = "gray90") +
  geom_text(aes(x = as.Date("2013-08-23"), y = 85), label = "Aug. 23, 2013", size = 3, hjust = 1, nudge_x = -.25) +
  geom_area(data = fire_specific_contributed_smokePM_filled_df, aes(x = date, y = total_contrib_smokePM, fill = fire_name)) +
  geom_line(data = smokepm_training_df, aes(x = date, y = smokePM), linetype = 3, color = "black") +
  geom_line(data = epa_monitor_pm_df, aes(x = date, y = pm25), linetype = 2, color = "black") +
  labs(x = "", y = "PM<sub>2.5</sub> (&mu;g/m<sup>3</sup>)", fill = "Smoke source") +
  scale_x_date(labels = scales::date_format("%b %d %Y"), date_breaks = "3 days") +
  scale_fill_brewer(palette = "Dark2")

## plot map of nearby fires
ca_pnw_shape <- states %>%
  filter(STUSPS %in% c("CA", "OR", "NV")) %>%
  st_transform(crs = st_crs("epsg:5070"))

STATES_BBOX <- st_bbox(ca_pnw_shape)

sparks_fire_polygons <- globfire_final_df %>%
  mutate(Id = as.character(Id)) %>%
  filter(Id %in% fire_smokepm_dt$fire_id[str_length(fire_smokepm_dt$fire_id) < 10]) %>% # filter for fires with globfire ID
  left_join(fire_smokepm_dt[, c("fire_id", "fire_name")], by = c("Id" = "fire_id"))

sparks_hysplit_init <- hysplit_init_distinct %>%
  filter(traj_id %in% fire_smokepm_dt$fire_id[str_length(fire_smokepm_dt$fire_id) > 10]) # filter for fires with hysplit init traj ID

sparks_monitor <- smokepm_training_df %>%
  dplyr::select(monitor_id_str, lat, lon) %>%
  unique() %>%
  st_as_sf(coords = c("lon", "lat"), crs = "epsg:4326") %>%
  st_transform("epsg:5070")

p_local_fires <- ggplot() +
  map_theme +
  geom_sf(data = ca_pnw_shape, fill = NA, lwd = 0.1) +
  geom_sf(data = sparks_fire_polygons, aes(fill = fire_name), color = NA) +
  geom_sf(data = sparks_hysplit_init, color = color_pal[[2]], size = .1) +
  geom_sf(data = sparks_monitor, fill = "black", color = "black", size = 2, shape = 23) +
  geom_segment(
    aes(
      xend = st_bbox(sparks_monitor)$xmax + 5000, yend = st_bbox(sparks_monitor)$ymax + 5000,
      x = st_bbox(sparks_monitor)$xmax + 100000, y = st_bbox(sparks_monitor)$ymax + 100000
    ),
    size = 0.3, color = "gray70"
  ) +
  geom_richtext(aes(x = st_bbox(sparks_monitor)$xmax + 125000, y = st_bbox(sparks_monitor)$ymax + 100000),
    label = "Sparks EPA<br>monitoring station", size = 3, fill = "white", label.color = NA
  ) +
  scale_fill_brewer(palette = "Dark2") +
  coord_sf(
    xlim = c(STATES_BBOX$xmin, STATES_BBOX$xmax),
    ylim = c(STATES_BBOX$ymin, STATES_BBOX$ymax)
  )


## sat image 1
sat_img_file <- "data/sat_img/snapshot-2013-08-15T00_00_00Z.tiff"

sat_raster <- terra::rast(sat_img_file)
sat_raster <- terra::project(sat_raster, "EPSG:5070")
sat_raster <- mask(sat_raster, ca_pnw_shape)

p_sat_1 <- RStoolbox::ggRGB(sat_raster, r = 1, g = 2, b = 3) +
  map_theme +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_sf(data = ca_pnw_shape, fill = NA, lwd = 0.1) +
  geom_sf(data = sparks_fire_polygons, aes(fill = fire_name), color = NA) +
  geom_sf(data = sparks_hysplit_init, color = color_pal[[2]], size = .1) +
  geom_sf(data = sparks_monitor, fill = "black", color = "black", size = 2, shape = 23) +
  scale_fill_manual("",
    labels = c("american" = "American Fire", "rim" = "Rim Fire", "Other fires" = "Other fires"),
    values = c("american" = color_pal[[1]], "rim" = color_pal[[2]], "Other fires" = color_pal[[3]])
  ) +
  labs(title = "Aug. 15, 2013") +
  scale_fill_brewer(palette = "Dark2") +
  coord_sf(
    xlim = c(STATES_BBOX$xmin, STATES_BBOX$xmax),
    ylim = c(STATES_BBOX$ymin, STATES_BBOX$ymax)
  )

## sat image 2
sat_img_file <- "data/sat_img/snapshot-2013-08-23T00_00_00Z.tiff"

sat_raster <- terra::rast(sat_img_file)
sat_raster <- terra::project(sat_raster, "EPSG:5070")
sat_raster <- mask(sat_raster, ca_pnw_shape)

p_sat_2 <- RStoolbox::ggRGB(sat_raster, r = 1, g = 2, b = 3) +
  map_theme +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_sf(data = ca_pnw_shape, fill = NA, lwd = 0.1) +
  geom_sf(data = sparks_fire_polygons, aes(fill = fire_name), color = NA) +
  geom_sf(data = sparks_hysplit_init, color = color_pal[[2]], size = .1) +
  geom_sf(data = sparks_monitor, fill = "black", color = "black", size = 2, shape = 23) +
  scale_fill_manual("",
    labels = c("american" = "American Fire", "rim" = "Rim Fire", "Other fires" = "Other fires"),
    values = c("american" = color_pal[[1]], "rim" = color_pal[[2]], "Other fires" = color_pal[[3]])
  ) +
  labs(title = "Aug. 23, 2013") +
  scale_fill_brewer(palette = "Dark2") +
  coord_sf(
    xlim = c(STATES_BBOX$xmin, STATES_BBOX$xmax),
    ylim = c(STATES_BBOX$ymin, STATES_BBOX$ymax)
  )

p_top_row <- plot_grid(p_timeseries, labels = c("A"))
p_bottom_row <- plot_grid(p_local_fires, p_sat_1, p_sat_2, nrow = 1, labels = c("B", "C", "D"))
p_combined <- plot_grid(p_top_row, p_bottom_row, ncol = 1, rel_heights = c(1.75, 3))

save_plot("output/figsupp/sparks_timeseries_map.pdf", plot = p_combined, base_width = 7, base_height = 7, scale = 1.7, device = cairo_pdf)
save_plot("output/figsupp/sparks_timeseries_map.png", plot = p_combined, base_width = 7, base_height = 7, scale = 1.7, bg = "white")





## Supp Figure 7 ----
## state source-receptor matrix plot
region_dt <- readr::read_csv("data/etc/census_region.csv") %>%
  as.data.table()

state_transport_smokepm_dt <- readRDS("data/clean/state_transport_popsmokepm_dt.rds")
plot_state_source_receptor_dt <- state_transport_smokepm_dt[, .(total_contrib_smokePM = sum(total_contrib_smokePM, na.rm = T)),
  by = c(
    "fire_state", "fire_state_fips", "fire_state_name",
    "grid_state", "grid_state_fips", "grid_state_name"
  )
][, `:=`(total_grid_smoke = sum(total_contrib_smokePM, na.rm = T)),
  by = c("grid_state", "grid_state_fips", "grid_state_name")
][, `:=`(prop = total_contrib_smokePM / total_grid_smoke)] %>%
  filter(fire_state %not_in% c("MEX", "CAN", "BHS", "CUB", "HTI", "JAM", "DOM", "PRI")) %>%
  tidyr::complete(fire_state_name, grid_state,
    fill = list(prop = 0)
  ) %>%
  as.data.table()

plot_state_source_receptor_dt <- plot_state_source_receptor_dt[region_dt, on = c("grid_state" = "state"), `:=`(grid_region = i.region)][region_dt, on = c("fire_state_name" = "state_name"), `:=`(fire_region = i.region)]

p_state_source_receptor_mat <- ggplot(data = plot_state_source_receptor_dt) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_tile(aes(x = grid_state, y = forcats::fct_rev(fire_state_name), fill = prop)) +
  facet_grid(fire_region ~ grid_region, scales = "free", space = "free", switch = "y") +
  colorspace::scale_fill_continuous_sequential(palette = "Reds 3", limits = c(0, 1)) +
  labs(x = "Receptor state", y = "Source state", fill = "Proportion of receptor\nsmoke from source") +
  scale_x_discrete(position = "top")

save_plot("output/figsupp/state_source_receptor_matrix.png",
  plot = p_state_source_receptor_mat, base_width = 8, base_height = 7, scale = 1.5, bg = "white"
)

save_plot("output/figsupp/state_source_receptor_matrix.pdf",
  plot = p_state_source_receptor_mat, base_width = 8, base_height = 7, scale = 1.5, bg = "white"
)





## Supp Figure 9 ----
## MTBS + Globfire polygon with Hysplit init points
bbox <- globfire_plot_df %>%
  filter(fire_name == "Camp Fire") %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_buffer(10000) %>%
  st_bbox()

p_camp_globfire_hysplit <- ggplot() +
  map_theme +
  theme(legend.position = "bottom") +
  geom_sf(data = globfire_plot_df %>% filter(fire_name == "Camp Fire") %>% st_buffer(2000), aes(fill = "2km buffer"), color = NA, lwd = 0.1) +
  geom_sf(data = globfire_plot_df %>% filter(fire_name == "Camp Fire"), aes(fill = "Fire polygon"), color = NA, lwd = 0.1) +
  geom_sf(
    data = hysplit_init_distinct %>% filter(ymd >= "2018-11-08" & ymd <= "2018-11-09"),
    aes(color = "HYSPLIT initialization point\n(November 8-9th, 2018)")
  ) +
  scale_fill_manual(name = "", values = c("2km buffer" = "gray90", "Fire polygon" = "#c49999")) +
  scale_color_manual(name = "", values = c("HYSPLIT initialization point\n(November 8-9th, 2018)" = "#006d6d")) +
  coord_sf(
    xlim = c(bbox$xmin, bbox$xmax),
    ylim = c(bbox$ymin, bbox$ymax)
  )

save_plot(stringr::str_glue("output/figsupp/camp_fire_hysplit_overlap.png"),
  plot = p_camp_globfire_hysplit, base_width = 6, base_height = 5, bg = "white"
)





## Supp Figure 10 ----
## yearly hysplit globfire matched plot
## merge together the fire perimeters and distinct hysplit init fire points
## filter to the relevant dates
buffer_init_match_df <- globfire_final_df %>%
  st_buffer(dist = 2000) %>% # add in buffer to account for resolution
  st_join(hysplit_init_distinct) %>%
  filter((ymd >= IDate & ymd <= FDate)) %>%
  st_drop_geometry() %>%
  mutate(buffer_dist = 2000)

## count number of unique matched hysplit points
buffer_init_match_df %>%
  group_by(buffer_dist) %>%
  summarize(unique_hysplit_points = length(unique(traj_id)))

buffer_matched_traj_ids <- buffer_init_match_df %>%
  filter(buffer_dist == 2000) %>%
  pull(traj_id)

# filter hysplit init points for relevant hysplit points
hysplit_match_df <- hysplit_init_distinct %>%
  mutate(
    buffer_dist = 2000,
    matched = ifelse(traj_id %in% buffer_matched_traj_ids, "Match", "No match")
  ) %>%
  st_drop_geometry() %>%
  as.data.frame()

p_hysplit_globfire_match <- ggplot(hysplit_match_df %>%
  mutate(
    matched = ordered(matched, levels = c("No match", "Match")),
    buffer_dist = "2km buffer"
  )) +
  base_theme +
  geom_histogram(aes(x = year, fill = matched), binwidth = 1) +
  facet_wrap(buffer_dist ~ ., ncol = 1) +
  labs(x = "Year", y = "Count", fill = "") +
  scale_fill_brewer(palette = "Greens") +
  scale_y_continuous(labels = scales::label_number())

save_plot("output/figsupp/hysplit_globfire_match.png",
  plot = p_hysplit_globfire_match,
  base_width = 5.5, base_height = 2, scale = 1.5, bg = "white"
)





## Supp Figure 11 ----
## calculate % of smokepm gridcells with and without trajectory points
## 1. for the set of gridcells without traj points, get unique dates
SAVE_PATH <- "data/clean/fire_smokepm/"

## get file names
fname_list <- list.files(path = "data/clean/fire_smokepm/30km/", pattern = ".fst", recursive = T)

year_perc_traj_dt <- pbmclapply(fname_list, mc.cores = max(parallel::detectCores() - 2, 1), function(fname) {
  year_ <- str_sub(fname, start = 1, end = 4)
  month_ <- str_sub(fname, start = 6, end = 7)

  month_focal_summary_dt <- lapply(c(1, 3, 5), function(WINDOW_SIZE) {
    ## read in gridcell smokePM data
    fire_smokepm_dt <- read_fst(str_glue("{SAVE_PATH}{WINDOW_SIZE}0km/{year_}-{month_}_{WINDOW_SIZE}0km_focal_grid_fire_smokepm_agg.fst"), as.data.table = T)

    ## first, filter out obs for 30km and 50km files where for the same gridcell there are obs with <NA> fire_id
    ## this happens because neighbors have <NA> fire_id obs with no traj points
    fire_smokepm_dt <- fire_smokepm_dt[, `:=`(group_n = .N), by = c("ID", "date")][!(group_n > 1 & num_traj_points == 0 & is.na(fire_id))][, `:=`(
      fire_bin = fifelse(!is.na(fire_id), "Matched fire", "NA fire"),
      smokePM_bin = (smokePM_pred_coded > 0) & !is.na(smokePM_pred_coded),
      traj_bin = fifelse(num_traj_points > 0, "With traj", "Without traj")
    )]

    result_dt <- fire_smokepm_dt[(smokePM_bin),
      .(smokePM_pred_coded = mean(smokePM_pred_coded, na.rm = T)), ## do this because data has multiple fire_ids for each cell...
      by = c("ID", "date", "traj_bin")
    ][, .(
      year = year_,
      month = month_,
      window_size = str_glue("{WINDOW_SIZE}0km"),
      num_cells = length(unique(ID)),
      median_smokepm = quantile(smokePM_pred_coded, probs = 0.5, na.rm = T),
      smokepm = sum(smokePM_pred_coded, na.rm = T)
    ),
    by = c("date", "traj_bin")
    ][, `:=`(
      total_cells = sum(num_cells),
      total_smokepm = sum(smokepm)
    ),
    by = c("date")
    ][, `:=`(
      perc_cells = num_cells / total_cells,
      perc_smokepm = smokepm / total_smokepm
    )]

    return(result_dt)
  }) %>% rbindlist()

  return(month_focal_summary_dt)
}) %>% rbindlist()

plot_year_smokepm_dt <- year_perc_traj_dt[, .(
  med_smokepm = median(median_smokepm),
  med_perc_cells = median(perc_cells),
  med_perc_smokepm = median(perc_smokepm)
),
by = c("year", "traj_bin", "window_size")
]

p_perc_smokepm_no_traj <- ggplot(data = plot_year_smokepm_dt[traj_bin == "Without traj"]) +
  base_theme +
  theme(axis.title.y = element_markdown()) +
  geom_line(aes(x = year, y = med_perc_smokepm, group = window_size, color = window_size)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Year", y = "% smoke PM<sub>2.5</sub> in gridcells without trajectory points", color = "Window size")

save_plot("output/figsupp/percent_smokepm_no_traj_gridcells.png", plot = p_perc_smokepm_no_traj, base_width = 6, base_height = 3.5, scale = 1.7, bg = "white")




## Supp Table 1 ----
## requires first running code in Figure 3 to get `top_fires_dt` and asthma ed visit coefficients
## also requires `fire_state_dt` in Figure 5
## top 20 table
RANKING <- 1:20
globfire_plot_df <- globfire_df %>%
  filter(Id %in% top_fires_dt[RANKING, fire_id]) %>%
  left_join(top_fires_dt[, c("rank", "fire_id", "fire_name")], by = c("Id" = "fire_id")) %>%
  mutate(fire_name = ordered(fire_name, levels = top_fires_dt[RANKING, fire_name])) %>%
  st_transform(crs = st_crs("epsg:5070"))

# get months that we need to cover for calculating isolation score
MONTH_YEAR_LIST <- globfire_plot_df %>%
  rowwise() %>%
  mutate(date_list = list(seq.Date(from = IDate, to = FDate + 6, by = "days")))
MONTH_YEAR_LIST <- unique(zoo::as.yearmon(do.call(c, MONTH_YEAR_LIST$date_list)))

fire_smokepm_dt <- pbmclapply(MONTH_YEAR_LIST, function(MONTH_YEAR) {
  year_ <- year(MONTH_YEAR)
  month_ <- str_pad(month(MONTH_YEAR), 2, pad = "0")

  temp_fire_smokepm_dt <- read_fst(str_glue("{SAVE_PATH}{WINDOW_SIZE}0km/{year_}-{month_}_{WINDOW_SIZE}0km_focal_grid_fire_smokepm_agg.fst"), as.data.table = T)

  # filter for specific fires
  temp_fire_smokepm_dt <- temp_fire_smokepm_dt[fire_id %in% top_fires_dt[RANKING, fire_id] & !is.na(contrib_smokePM), ]

  return(temp_fire_smokepm_dt)
}) %>% bind_rows()

### pop smokePM and asthma ed visits for table
fire_smokepm_dt[, `:=`(fire_id = as.numeric(fire_id))] ## need to convert string to integer

plot_pop_smokepm_df <- fire_smokepm_dt[top_fires_dt, `:=`(fire_name = i.fire_name), on = c("fire_id")][, `:=`(year = year(date))][, `:=`(fire_name = ordered(fire_name, levels = top_fires_dt[RANKING, fire_name]))][grid_pop_dt, `:=`(pop = i.pop), on = c("ID", "year")][, `:=`(
  pop_smokepm = pop * contrib_smokePM,
  asthma_ed_visits = pop * contrib_smokePM * ASTHMA_ED_COEFF,
  asthma_ed_visits_lb = pop * contrib_smokePM * (ASTHMA_ED_COEFF - qnorm(.95) * ASTHMA_ED_SE),
  asthma_ed_visits_ub = pop * contrib_smokePM * (ASTHMA_ED_COEFF + qnorm(.95) * ASTHMA_ED_SE)
)] %>%
  as.data.frame() %>%
  left_join(grid_transform, by = c("ID")) %>%
  st_as_sf()

### fire isolation (weight share by contrib smoke PM)
fire_isolation_score_dt <- fire_smokepm_dt[, .(isolation = weighted.mean(share, coalesce(pop_smokepm, 0), na.rm = T)),
  by = c("fire_id")
]

### elapsed time plots
globfire_date_df <- globfire_plot_df %>%
  st_drop_geometry() %>%
  dplyr::select(IDate, FDate, fire_name, rank) %>%
  arrange(fire_name)

pop_fire_smokepm_dt <- plot_pop_smokepm_df %>%
  st_drop_geometry() %>%
  as.data.table()

## text with info of fire
top_table_dt <- pop_fire_smokepm_dt[globfire_date_df,
  `:=`(IDate = i.IDate, FDate = i.FDate, rank = i.rank),
  on = "fire_name"
][, `:=`(days_elapsed = date - IDate)][fire_isolation_score_dt[, `:=`(fire_id = as.numeric(fire_id))],
  `:=`(isolation = i.isolation),
  on = "fire_id"
][, .(
  rank = median(rank),
  year = median(year),
  isolation = median(isolation),
  total_days_elapsed = max(days_elapsed, na.rm = T),
  total_affected_pop = sum(pop, na.rm = T),
  total_pop_smokepm = sum(pop_smokepm, na.rm = T),
  total_contrib_smokePM = sum(contrib_smokePM, na.rm = T),
  total_asthma_ed_visits = sum(asthma_ed_visits, na.rm = T),
  total_asthma_ed_visits_lb = sum(asthma_ed_visits_lb, na.rm = T),
  total_asthma_ed_visits_ub = sum(asthma_ed_visits_ub, na.rm = T),
  avg_affected_pop = mean(pop, na.rm = T),
  avg_pop_smokepm = mean(pop_smokepm, na.rm = T),
  avg_contrib_smokePM = mean(contrib_smokePM, na.rm = T)
),
by = c("fire_name", "fire_id")
][order(fire_name)][fire_state_dt[Type == "FinalArea" & Id %in% as.character(top_fires_dt[RANKING, fire_id])][, `:=`(Id = as.numeric(Id))], # only globfire polygons are largest fires
  `:=`(state = i.fire_state),
  on = c("fire_id" = "Id")
][, .(
  `Fire name` = fire_name,
  Year = year,
  State = state,
  `Isolation score` = round(isolation, 3),
  `Days elapsed` = total_days_elapsed,
  `Population smokePM (billion person ug/m^3)` = round(total_pop_smokepm / 1e9, digits = 2),
  `Excess asthma ED visits (95% CI)` = str_glue("{round(total_asthma_ed_visits,0)} ({round(total_asthma_ed_visits_lb,0)}, {round(total_asthma_ed_visits_ub,0)})", .envir = .SD),
  `Average smokePM (ug/m^3)` = round(avg_contrib_smokePM, digits = 4)
)]

top_table_dt[
  `Fire name` == "Bugaboo Fire",
  `:=`(State = "GA & FL")
]

## generate and format latex
xtable::xtable(top_table_dt, display = c("d", "s", "d", "s", "f", "d", "f", "d", "f"))
