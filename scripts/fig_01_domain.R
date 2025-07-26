#Setup----
#Updated March 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Produce study area figure with red box within conus extent

# # Load Libraries & Set Project Root ----
# library(here)
# here::i_am("scripts/fig_01_domain.R")  # Adjust this file path as needed
# source(here::here("scripts", "01_library.R"))
# 
# # Define the output directory using the here package (this will create an "output" folder in your project root)
# output_dir <- here("figures")
# if (!dir.exists(output_dir)) {
#   dir.create(output_dir, recursive = TRUE)
# }
# 
# # Load US states data and convert to an sf object
# # us.states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
# globe <- st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))
# us.states <- globe
# 
# # Create the plot with a thick red box over the CONUS region
# p <- ggplot() +
#   geom_sf(data = us.states, fill = "white", color = "black") +
#   geom_rect(aes(xmin = -95, xmax = -75, ymin = 24, ymax = 40),
#             fill = NA, color = "red", size = 1) +
#   # coord_sf(xlim = c(-125, -67), ylim = c(25, 50)) +
#   theme_bw()
# 
# p
# 
# # Define file paths using here
# png_path <- here("figures","01_domain.png")
# svg_path <- here("figures","01_domain.svg")
# 
# # Save the plot as a PNG file
# ggsave(filename = png_path, plot = p, width = 8, height = 6, dpi = 300)
# ggsave(filename = svg_path, plot = p, width = 8, height = 6, device = "svg")
# 
#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# fig_01_ecoregions_bbox.R
# Updated June 2025
# Author: Hunter Quintal
# Purpose: Read EPA‐derived “Aggr_Ecoregions_2015.shp”, crop to bbox (–95,–75,24,40),
#          and plot all clipped ecoregions with a legend.
#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

# 1) Libraries & project root setup ------------------------------------------
library(here)
here::i_am("scripts/fig_01_domain.R")

# #––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# # fig_01_ecoregions_bbox.R
# # Updated June 2025
# # Author: Hunter Quintal
# # Purpose: Read EPA‐derived “Aggr_Ecoregions_2015.shp”, fix invalid geometries,
# #          crop to bbox (–95, –75, 24, 40), and plot all clipped ecoregions.
# #––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# 
# library(sf)        # for vector data handling
# library(ggplot2)   # for plotting
# library(dplyr)     # for data manipulation
# 
# # 2) Define output directory -------------------------------------------------
# output_dir <- here("figures")
# if (!dir.exists(output_dir)) {
#   dir.create(output_dir, recursive = TRUE)
# }
# 
# # 3) Define bounding box (lon/lat) -------------------------------------------
# bbox_extent <- st_bbox(
#   c(xmin = -95, ymin =  24, xmax = -75, ymax =  40),
#   crs  = st_crs(4326)
# )
# 
# # 4) Read and fix ecoregions shapefile ---------------------------------------
# shp_path <- "V:/users/hquintal/phd1_cluster_southeast/data/input/regional_aggregation/ecoregion/Aggr_Ecoregions_2015/Aggr_Ecoregions_2015.shp"
# 
# ecoregions_all <- st_read(shp_path, quiet = TRUE) %>%
#   st_transform(crs = 4326) %>%        # ensure lon/lat CRS
#   st_make_valid()                     # fix any invalid geometries
# 
# # 5) Crop to bounding box -----------------------------------------------------
# ecoregions_clip <- st_crop(ecoregions_all, bbox_extent)
# 
# # 6) Plot clipped ecoregions -------------------------------------------------
# p_ecoregions <- ggplot(ecoregions_clip) +
#   geom_sf(
#     aes(fill = WSA9_NAME),
#     color = "grey30",
#     size  = 0.2
#   ) +
#   coord_sf(
#     xlim   = c(bbox_extent["xmin"], bbox_extent["xmax"]),
#     ylim   = c(bbox_extent["ymin"], bbox_extent["ymax"]),
#     expand = FALSE
#   ) +
#   scale_fill_viridis_d(
#     option = "magma",
#     name   = "Ecoregion"
#   ) +
#   theme_bw(base_size = 14) +
#   theme(
#     panel.grid.major = element_line(color = "grey90"),
#     panel.background = element_rect(fill = "white"),
#     axis.title       = element_blank(),
#     legend.position  = "right",
#     legend.title     = element_text(size = 12),
#     legend.text      = element_text(size = 10)
#   )
# 
# # 7) Save to disk -------------------------------------------------------------
# png_path <- here(output_dir, "01_ecoregions_bbox.png")
# svg_path <- here(output_dir, "01_ecoregions_bbox.svg")
# 
# ggsave(
#   filename = png_path,
#   plot     = p_ecoregions,
#   width    = 8,
#   height   = 6,
#   dpi      = 300
# )
# 
# ggsave(
#   filename = svg_path,
#   plot     = p_ecoregions,
#   width    = 8,
#   height   = 6,
#   device   = "svg"
# )

#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# fig_01_ecoregions_bbox_with_inset.R
# Updated June 2025
# Author: Hunter Quintal
# Purpose: Read “Aggr_Ecoregions_2015.shp”, fix invalid geometries,
#          crop to bbox (–95, –75, 24, 40), plot clipped ecoregions colored by WSA9_NAME
#          with legend inside bottom‐left and a full‐CONUS inset at top‐right.
#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

# # 1) Libraries & project root setup ------------------------------------------
# # library(here)
# # here::i_am("scripts/fig_01_ecoregions_bbox_with_inset.R")
# 
# library(sf)        # vector data handling
# library(ggplot2)   # plotting
# library(dplyr)     # data manipulation
# library(cowplot)   # combining main + inset
# 
# # 2) Define output directory -------------------------------------------------
# output_dir <- here("figures")
# if (!dir.exists(output_dir)) {
#   dir.create(output_dir, recursive = TRUE)
# }
# 
# # 3) Define bounding box (lon/lat) -------------------------------------------
# bbox_extent <- st_bbox(
#   c(xmin = -95, ymin =  24, xmax = -75, ymax =  40),
#   crs  = st_crs(4326)
# )
# 
# # 4) Read and fix ecoregions shapefile ---------------------------------------
# shp_path <- "V:/users/hquintal/phd1_cluster_southeast/data/input/regional_aggregation/ecoregion/Aggr_Ecoregions_2015/Aggr_Ecoregions_2015.shp"
# 
# ecoregions_all <- st_read(shp_path, quiet = TRUE) %>%
#   st_transform(crs = 4326) %>%   # ensure lon/lat CRS
#   st_make_valid()                # fix invalid geometries
# 
# # 5) Crop to bounding box -----------------------------------------------------
# ecoregions_clip <- st_crop(ecoregions_all, bbox_extent)
# 
# # 6) Build full‐CONUS inset (red box) ----------------------------------------
# #    We can reuse the union of all ecoregions as a boundary for the inset.
# conus_boundary <- st_union(ecoregions_all)
# 
# p_inset <- ggplot() +
#   geom_sf(
#     data  = conus_boundary,
#     fill  = "white",
#     color = "black",
#     size  = 0.2
#   ) +
#   geom_rect(
#     aes(
#       xmin = bbox_extent["xmin"],
#       xmax = bbox_extent["xmax"],
#       ymin = bbox_extent["ymin"],
#       ymax = bbox_extent["ymax"]
#     ),
#     fill  = NA,
#     color = "red",
#     size  = 0.8
#   ) +
#   theme_void() +
#   theme(
#     panel.background = element_rect(fill = "white", color = NA)
#   )
# 
# # 7) Plot clipped ecoregions colored by WSA9_NAME -----------------------------
# p_ecoregions <- ggplot(ecoregions_clip) +
#   geom_sf(
#     aes(fill = WSA9_NAME),
#     color = "grey30",
#     size  = 0.2
#   ) +
#   coord_sf(
#     xlim   = c(bbox_extent["xmin"], bbox_extent["xmax"]),
#     ylim   = c(bbox_extent["ymin"], bbox_extent["ymax"]),
#     expand = FALSE
#   ) +
#   scale_fill_viridis_d(
#     option = "magma",
#     name   = "Ecoregion"
#   ) +
#   theme_bw(base_size = 14) +
#   theme(
#     panel.grid.major   = element_line(color = "grey90"),
#     panel.background   = element_rect(fill = "white"),
#     axis.title         = element_blank(),
#     # Place legend inside bottom‐left (a little inset from edges)
#     legend.position    = c(0.02, 0.02),
#     legend.justification = c(0, 0),
#     legend.background  = element_rect(fill = "white", color = "black", size = 0.2, alpha = 0.7),
#     legend.title       = element_text(size = 12),
#     legend.text        = element_text(size = 10)
#   )
# 
# # 8) Combine main + inset via cowplot -----------------------------------------
# combined <- ggdraw() +
#   draw_plot(p_ecoregions) +
#   draw_plot(
#     p_inset,
#     x      = 0.60,     # shift right
#     y      = 0.60,     # shift up
#     width  = 0.35,     # 35% of panel width
#     height = 0.35      # 35% of panel height
#   )
# 
# # 9) Save PNG & SVG ----------------------------------------------------------
# png_path <- here(output_dir, "01_ecoregions_bbox_with_inset.png")
# svg_path <- here(output_dir, "01_ecoregions_bbox_with_inset.svg")
# 
# ggsave(
#   filename = png_path,
#   plot     = combined,
#   width    = 8,
#   height   = 6,
#   dpi      = 300
# )
# 
# ggsave(
#   filename = svg_path,
#   plot     = combined,
#   width    = 8,
#   height   = 6,
#   device   = "svg"
# )

#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# fig_01_ecoregions_bbox_with_inset.R
# Updated June 2025
# Author: Hunter Quintal
# Purpose: Read “Aggr_Ecoregions_2015.shp”, fix invalid geometries,
#          crop to bbox (–95, –75, 24, 40), plot clipped ecoregions colored by WSA9_NAME
#          with legend inside bottom‐left and a full‐CONUS inset at top‐right.
#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

# # 1) Libraries & project root setup ------------------------------------------
# # library(here)
# # here::i_am("scripts/fig_01_ecoregions_bbox_with_inset.R")
# 
# library(sf)        # vector data handling
# library(ggplot2)   # plotting
# library(dplyr)     # data manipulation
# library(cowplot)   # combining main + inset
# library(scales)    # for alpha()
# 
# # 2) Define output directory -------------------------------------------------
# output_dir <- here("figures")
# if (!dir.exists(output_dir)) {
#   dir.create(output_dir, recursive = TRUE)
# }
# 
# # 3) Define bounding box (lon/lat) -------------------------------------------
# bbox_extent <- st_bbox(
#   c(xmin = -95, ymin =  24, xmax = -75, ymax =  40),
#   crs  = st_crs(4326)
# )
# 
# # 4) Read and fix ecoregions shapefile ---------------------------------------
# shp_path <- "V:/users/hquintal/phd1_cluster_southeast/data/input/regional_aggregation/ecoregion/Aggr_Ecoregions_2015/Aggr_Ecoregions_2015.shp"
# 
# ecoregions_all <- st_read(shp_path, quiet = TRUE) %>%
#   st_transform(crs = 4326) %>%   # ensure lon/lat CRS
#   st_make_valid()                # fix invalid geometries
# 
# # 5) Crop to bounding box -----------------------------------------------------
# ecoregions_clip <- st_crop(ecoregions_all, bbox_extent)
# 
# # 6) Build full‐CONUS inset (red box) ----------------------------------------
# conus_boundary <- st_union(ecoregions_all)
# 
# p_inset <- ggplot() +
#   geom_sf(
#     data  = conus_boundary,
#     fill  = "white",
#     color = "black",
#     size  = 0.2
#   ) +
#   geom_rect(
#     aes(
#       xmin = bbox_extent["xmin"],
#       xmax = bbox_extent["xmax"],
#       ymin = bbox_extent["ymin"],
#       ymax = bbox_extent["ymax"]
#     ),
#     fill  = NA,
#     color = "red",
#     size  = 0.8
#   ) +
#   theme_void() +
#   theme(
#     panel.background = element_rect(fill = "white", color = NA)
#   )
# 
# # 7) Plot clipped ecoregions colored by WSA9_NAME -----------------------------
# p_ecoregions <- ggplot(ecoregions_clip) +
#   geom_sf(
#     aes(fill = WSA9_NAME),
#     color = "grey30",
#     size  = 0.2
#   ) +
#   coord_sf(
#     xlim   = c(bbox_extent["xmin"], bbox_extent["xmax"]),
#     ylim   = c(bbox_extent["ymin"], bbox_extent["ymax"]),
#     expand = FALSE
#   ) +
#   scale_fill_viridis_d(
#     option = "magma",
#     name   = "Ecoregion"
#   ) +
#   theme_bw(base_size = 14) +
#   theme(
#     panel.grid.major    = element_line(color = "grey90"),
#     panel.background    = element_rect(fill = "white"),
#     axis.title          = element_blank(),
#     # Legend inside bottom‐left, with semi-transparent background
#     legend.position     = c(0.02, 0.02),
#     legend.justification = c(0, 0),
#     legend.background   = element_rect(
#       fill  = alpha("white", 0.7),
#       color = "black",
#       size  = 0.2
#     ),
#     legend.title        = element_text(size = 12),
#     legend.text         = element_text(size = 10)
#   )
# 
# # 8) Combine main + inset via cowplot -----------------------------------------
# combined <- ggdraw() +
#   draw_plot(p_ecoregions) +
#   draw_plot(
#     p_inset,
#     x      = 0.60,     # shift right
#     y      = 0.60,     # shift up
#     width  = 0.35,     # 35% of panel width
#     height = 0.35      # 35% of panel height
#   )
# 
# # 9) Save PNG & SVG ----------------------------------------------------------
# png_path <- here(output_dir, "01_ecoregions_bbox_with_inset.png")
# svg_path <- here(output_dir, "01_ecoregions_bbox_with_inset.svg")
# 
# ggsave(
#   filename = png_path,
#   plot     = combined,
#   width    = 8,
#   height   = 6,
#   dpi      = 300
# )
# 
# ggsave(
#   filename = svg_path,
#   plot     = combined,
#   width    = 8,
#   height   = 6,
#   device   = "svg"
# )

#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# fig_01_ecoregions_bbox_with_inset_and_legend.R
# Updated June 2025
# Author: Hunter Quintal
# Purpose: Read “Aggr_Ecoregions_2015.shp”, fix invalid geometries,
#          crop to bbox (–95, –75, 24, 40), plot clipped ecoregions colored by WSA9_NAME
#          with legend below the figure (horizontal), and a full-CONUS inset
#          (with state outlines) placed inside bottom-left where the legend used to be.
#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

# 1) Libraries & project root setup ------------------------------------------
# library(here)
# here::i_am("scripts/fig_01_ecoregions_bbox_with_inset_and_legend.R")

library(sf)        # for vector data handling
library(ggplot2)   # for plotting
library(dplyr)     # for data manipulation
library(cowplot)   # for combining main + inset
library(maps)      # for state outlines
library(scales)    # for alpha()

# 2) Define output directory -------------------------------------------------
output_dir <- here("figures")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# 3) Define bounding box (lon/lat) -------------------------------------------
bbox_extent <- st_bbox(
  c(xmin = -95, ymin =  24, xmax = -75, ymax =  40),
  crs  = st_crs(4326)
)

# 4) Read and fix ecoregions shapefile ---------------------------------------
shp_path <- "V:/users/hquintal/phd1_cluster_southeast/data/input/regional_aggregation/ecoregion/Aggr_Ecoregions_2015/Aggr_Ecoregions_2015.shp"

ecoregions_all <- st_read(shp_path, quiet = TRUE) %>%
  st_transform(crs = 4326) %>%   # ensure lon/lat CRS
  st_make_valid()                # fix invalid geometries

# 5) Crop to bounding box -----------------------------------------------------
ecoregions_clip <- st_crop(ecoregions_all, bbox_extent)

# 6) Prepare state outlines for inset -----------------------------------------
states_sf <- st_as_sf(
  maps::map("state", plot = FALSE, fill = TRUE)
) %>%
  st_transform(crs = 4326)

# 7) Build full-CONUS inset (state outlines + red bbox) -----------------------
p_inset <- ggplot() +
  geom_sf(
    data     = states_sf,
    fill     = "white",
    color    = "grey50",
    size     = 0.3
  ) +
  geom_rect(
    aes(
      xmin = bbox_extent["xmin"],
      xmax = bbox_extent["xmax"],
      ymin = bbox_extent["ymin"],
      ymax = bbox_extent["ymax"]
    ),
    fill  = NA,
    color = "red",
    size  = 0.8
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA)
  )

# 8) Plot clipped ecoregions colored by WSA9_NAME, legend below -------------
# p_ecoregions <- ggplot(ecoregions_clip) +
#   geom_sf(
#     aes(fill = WSA9_NAME),
#     color = "grey30",
#     size  = 0.2
#   ) +
#   coord_sf(
#     xlim   = c(bbox_extent["xmin"], bbox_extent["xmax"]),
#     ylim   = c(bbox_extent["ymin"], bbox_extent["ymax"]),
#     expand = FALSE
#   ) +
#   scale_fill_viridis_d(
#     option = "magma",
#     name   = "Ecoregion"
#   ) +
#   theme_bw(base_size = 14) +
#   theme(
#     panel.grid.major   = element_line(color = "grey90"),
#     panel.background   = element_rect(fill = "white"),
#     axis.title         = element_blank(),
#     # Move legend below (horizontal)
#     legend.position    = "bottom",
#     legend.direction   = "horizontal",
#     legend.title       = element_text(size = 10),
#     legend.text        = element_text(size = 8),
#     # Add a semi-transparent white background behind legend keys
#     legend.background  = element_rect(fill = alpha("white", 0.7), color = "black", size = 0.2)
#   )
p_ecoregions <- ggplot(ecoregions_clip) +
  geom_sf(
    aes(fill = WSA9_NAME),
    color = "grey30",
    size  = 0.2
  ) +
  coord_sf(
    xlim   = c(bbox_extent["xmin"], bbox_extent["xmax"]),
    ylim   = c(bbox_extent["ymin"], bbox_extent["ymax"]),
    expand = FALSE
  ) +
  scale_fill_viridis_d(
    option = "magma",
    name   = "Ecoregion"
  ) +
  guides(
    fill = guide_legend(
      nrow   = 2,
      byrow  = TRUE
    )
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major   = element_line(color = "grey90"),
    panel.background   = element_rect(fill = "white"),
    axis.title         = element_blank(),
    # Move legend below (horizontal), now wrapped in 2 rows
    legend.position    = "bottom",
    legend.direction   = "horizontal",
    legend.title       = element_text(size = 10),
    legend.text        = element_text(size = 8),
    # Semi‐transparent white background behind legend keys
    legend.background  = element_rect(
      fill  = alpha("white", 0.7),
      color = "black",
      size  = 0.2
    )
  )

# 9) Combine main + inset via cowplot -----------------------------------------
#    Place the inset inside bottom-left (where the legend used to be)
combined <- ggdraw() +
  draw_plot(p_ecoregions) +
  draw_plot(
    p_inset,
    x      = 0.23,     # 2% from left
    y      = 0.19,     # 2% from bottom
    width  = 0.3,     # 35% of panel width
    height = 0.3      # 35% of panel height
  )

# 10) Save PNG & SVG ----------------------------------------------------------
png_path <- here(output_dir, "01_ecoregions_bbox_with_inset_and_legend.png")
svg_path <- here(output_dir, "01_ecoregions_bbox_with_inset_and_legend.svg")

ggsave(
  filename = png_path,
  plot     = combined,
  width    = 8,
  height   = 6,
  dpi      = 300
)

ggsave(
  filename = svg_path,
  plot     = combined,
  width    = 8,
  height   = 6,
  device   = "svg"
)

## 2025-06-10 ----

library(sf)         # for vector data handling
library(ggplot2)    # for plotting
library(dplyr)      # for data manipulation
library(cowplot)    # for combining main + inset
library(maps)       # for state outlines
library(scales)     # for alpha()
library(here)       # for robust paths
library(viridis)    # for scale_fill_viridis_d()

# 1) Define output directory -------------------------------------------------
output_dir <- here("figures")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# 2) Define bounding box (lon/lat) -------------------------------------------
bbox_extent <- st_bbox(
  c(xmin = -95, ymin = 24, xmax = -75, ymax = 40),
  crs  = st_crs(4326)
)
bbox_poly <- st_as_sfc(bbox_extent)

# 3) Read & fix ecoregions shapefile -----------------------------------------
shp_path <- "V:/users/hquintal/phd1_cluster_southeast/data/input/regional_aggregation/ecoregion/Aggr_Ecoregions_2015/Aggr_Ecoregions_2015.shp"
stopifnot(file.exists(shp_path))

ecoregions_clip <- st_read(shp_path, quiet = TRUE) %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  st_crop(bbox_extent)

# 4) Prepare CONUS state outlines & labels for main map ----------------------
# turn off S2 so GEOS buffer/intersection can clean tiny loops
sf::sf_use_s2(FALSE)

states_sf <- maps::map("state", fill = TRUE, plot = FALSE) %>%
  st_as_sf() %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  st_buffer(0) %>%                    # clean residual topology issues
  mutate(abb = state.abb[match(ID, tolower(state.name))])

# clip to bbox and compute centroids for labels
states_clip    <- st_intersection(states_sf, bbox_poly)
state_centroids <- st_centroid(states_clip)

# re-enable S2
sf::sf_use_s2(TRUE)

# 5) Build full-CONUS inset (state outlines + red bbox) ----------------------
p_inset <- ggplot() +
  geom_sf(
    data  = states_sf,
    fill  = "white",
    color = "grey50",
    size  = 0.3
  ) +
  geom_rect(
    aes(
      xmin = bbox_extent["xmin"],
      xmax = bbox_extent["xmax"],
      ymin = bbox_extent["ymin"],
      ymax = bbox_extent["ymax"]
    ),
    fill  = NA,
    color = "red",
    size  = 0.8
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", colour = "black", size = 1)
  )

# 6) Main map: clipped ecoregions + states + labels --------------------------
p_ecoregions <- ggplot() +
  # A) state outlines underneath
  geom_sf(
    data  = states_clip,
    fill  = 'white',
    color = "grey70",
    size  = 0.3
  ) +
  # A) state outlines underneath
  geom_sf(
    data  = states_clip,
    fill  = NA,
    color = "grey70",
    size  = 0.3
  ) +
  # B) ecoregion polygons with semi-transparent fill
  geom_sf(
    data  = ecoregions_clip,
    aes(fill = WSA9_NAME),
    color = "grey30",
    size  = 0.2,
    alpha = 0.4              # <-- makes them see-through
  ) +
  # C) state abbreviations
  geom_sf_text(
    data  = state_centroids,
    aes(label = abb),
    size   = 3,
    colour = "grey20"
  ) +
  coord_sf(
    xlim   = c(bbox_extent["xmin"], bbox_extent["xmax"]),
    ylim   = c(bbox_extent["ymin"], bbox_extent["ymax"]),
    expand = FALSE
  ) +
  scale_fill_viridis_d(
    option = "plasma",
    name   = "Ecoregion"
  ) +
  guides(
    fill = guide_legend(
      nrow = 2, byrow = TRUE,
      override.aes = list(alpha = 0.6)  # <-- legend keys match map transparency
    )
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major   = element_line(color = "grey90"),
    panel.background   = element_rect(fill = "white"),
    axis.title         = element_blank(),
    legend.position    = "bottom",
    legend.direction   = "horizontal",
    legend.title       = element_text(size = 10),
    legend.text        = element_text(size = 8),
    legend.background  = element_rect(
      fill  = alpha("white", 0.4),
      color = "black",
      size  = 0.2
    )
  )

# 7) Combine main + inset via cowplot -----------------------------------------
combined <- ggdraw() +
  draw_plot(p_ecoregions) +
  draw_plot(
    p_inset,
    x      = 0.23,   # moved inset a bit to the right
    y      = 0.19,
    width  = 0.3,
    height = 0.3
  )

# 8) Save PNG & SVG ----------------------------------------------------------
ggsave(
  here(output_dir, "ecoregions_alpha_states.png"),
  combined, width = 8, height = 6, dpi = 300
)
ggsave(
  here(output_dir, "ecoregions_alpha_states.svg"),
  combined, width = 8, height = 6, device = "svg"
)
