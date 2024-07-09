# === === === === === === === === === === === === === === === === === === === ===

# create sample maps, version 2

# === === === === === === === === === === === === === === === === === === === ===

# dependencies ####

## packages
devtools::load_all()
library(dplyr)
library(ggplot2)
library(tigris)

## map breaks function
map_breaks <- function(.data, var, newvar, classes, style, breaks, clean_labels = TRUE, sig.digits = 2){

  # calculate breaks and categories
  if (missing(breaks) == TRUE){
    breaks <- classInt::classIntervals(.data[[var]], n = classes, style = style)
  }

  # clean labels
  if (clean_labels == TRUE){

    ## create sample breaks and clean
    sample <- cut(.data[[var]], breaks = breaks$brks, include.lowest = TRUE,
                  dig.lab = 10)

    sample <- levels(sample)

    sample <- gsub(",", " - ", sample)
    sample <- gsub("[^0-9.-]", "", sample)
    sample <- gsub("-", " ", sample)

    ## format upper and lower bounds
    bounds <- data.frame(
      range = sample
    )

    ## calculate offset for upper bound label
    if (sig.digits == 0){
      value <- 1
    } else if (sig.digits > 0){
      x <- sig.digits-1
      value <- as.numeric(paste0(".", paste0(as.character(rep(0, times = x)), collapse = ""), "1"))
    }

    ## calculate and format upper and lower bounds
    bounds$lower <- as.numeric(stringr::word(bounds$range, start = 1))
    bounds$upper <- as.numeric(stringr::word(bounds$range, start = 2))-value
    bounds$upper <- ifelse(bounds$upper == 100-value, 100, bounds$upper)

    bounds$lower <- sprintf(paste0("%0.", sig.digits,"f"), bounds$lower)
    bounds$upper <- sprintf(paste0("%0.", sig.digits,"f"), bounds$upper)

    ## create labels vector
    bounds$range <- paste0(bounds$lower, " - ", bounds$upper)
    bounds <- bounds$range

    ## cut based on breaks, add cleaned labels
    .data[[newvar]] <- cut(.data[[var]], breaks = breaks$brks, labels = bounds,
                           include.lowest = TRUE)

  } else if (clean_labels == FALSE){

    ## cut based on breaks
    .data[[newvar]] <- cut(.data[[var]], breaks = c(breaks$brks),
                           include.lowest = TRUE)

  }

  # move geometry column
  varlist <- c(names(.data)[names(.data) != "geometry"])
  varlist <- c(varlist, "geometry")

  ## subset
  .data <- subset(.data, select = varlist)

  # return result
  return(.data)

}

## define theme
map_theme <- function(base_size = 28, base_family = "sans", legend_size = 1.5){

  (ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
     theme(
       line = element_line(colour = "black"),
       rect = element_rect(fill = NA, linetype = 1, colour = '#898989'),
       text = element_text(colour = '#3C3C3C'),
       axis.line = element_blank(),
       axis.text = element_blank(),
       axis.ticks = element_blank(),
       axis.title = element_blank(),
       panel.background = element_rect(fill = '#FFFFFF', color = NA),
       panel.border = element_blank(),
       panel.grid = element_blank(),
       panel.spacing = unit(0, "lines"),
       plot.background = element_rect(fill = '#FFFFFF', color = NA),
       legend.justification = c(0, 0),
       legend.background = element_rect(fill = '#FFFFFF'),
       legend.key = element_rect(fill = '#FFFFFF'),
       legend.key.size = unit(legend_size, units="cm"),
       legend.position = "right",
       legend.direction = "vertical",
       legend.box = "vertical",
       plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
       plot.caption = element_text(hjust = "0")))

}

# === === === === === === === === === === === === === === === === === === === ===

# load state boundaries ####

## download and subset
states <- states(cb=TRUE) %>%
  filter(STUSPS %in% c("AS", "GU", "MP", "PR", "VI") == FALSE)

## shift position
states <- shift_geometry(states, position = "below")

# === === === === === === === === === === === === === === === === === === === ===

# create deprivation scores ####

## initial scoring
dep <- dep_get_index(geography = "county", index = c("svi20", "adi"), year = 2021,
                     return_percentiles = TRUE, output = "sf", shift_geo = TRUE)

## create quartiles
dep <- dep %>%
  map_breaks(var = "SVI", newvar = "map_svi", style = "quantile", classes = 4, sig.digits = 2) %>%
  map_breaks(var = "ADI", newvar = "map_adi", style = "quantile", classes = 4)

# === === === === === === === === === === === === === === === === === === === ===

# maps ####

## create SVI map
p <- ggplot() +
  geom_sf(data = states, fill = "#e0e0e0", color = NA) +
  geom_sf(data = dep, mapping = aes(fill = map_svi), size = .2) +
  geom_sf(data = states, fill = NA, color = "black", size = 1.2) +
  scale_fill_brewer(palette = "Blues", name = "SVI", na.value = "#e0e0e0") +
  labs(
    title = "SVI by County (2021)"
  ) +
  map_theme(base_size = 18)

## save SVI map
ggsave(filename = "inst/extdata/svi_2021.png", plot = p,
                width = 1024 * 0.352778,
                height = 768 * 0.352778,
                units = "mm", dpi = 300)

## create ADI map
p <- ggplot() +
  geom_sf(data = states, fill = "#e0e0e0", color = NA) +
  geom_sf(data = dep, mapping = aes(fill = map_adi), size = .2) +
  geom_sf(data = states, fill = NA, color = "black", size = 1.2) +
  scale_fill_brewer(palette = "Purples", name = "ADI", na.value = "#e0e0e0") +
  labs(
    title = "ADI by County (2021)"
  ) +
  map_theme(base_size = 18)

## save ADI map
ggsave(filename = "inst/extdata/adi_2021.png", plot = p,
       width = 1024 * 0.352778,
       height = 768 * 0.352778,
       units = "mm", dpi = 300)
