map_breaks <- function(.data, var, newvar, classes, style, breaks, clean_labels = TRUE, dig_lab = 10){

  # save parameters to list
  paramList <- as.list(match.call())

  # quote input variables
  if (!is.character(paramList$var)) {
    ref <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    ref <- rlang::quo(!! rlang::sym(var))
  }

  refQ <- rlang::quo_name(rlang::enquo(ref))

  if (!is.character(paramList$newvar)) {
    new <- rlang::enquo(newvar)
  } else if (is.character(paramList$newvar)) {
    new <- rlang::quo(!! rlang::sym(newvar))
  }

  newQ <- rlang::quo_name(rlang::enquo(new))

  # calculate breaks and categories
  if (missing(breaks) == TRUE){
    breaks <- classInt::classIntervals(.data[[refQ]], n = classes, style = style)
  }

  categories <- cut(.data[[refQ]], breaks = c(breaks$brks), include.lowest = TRUE, dig.lab = dig_lab)

  # create new variable
  .data <- dplyr::mutate(.data, !!newQ := categories)

  # clean labels
  if (clean_labels == TRUE){

    .data[[newQ]] %>%
      forcats::fct_relabel(~ gsub(",", " - ", .x)) %>%
      forcats::fct_relabel(~ gsub("\\(", "", .x)) %>%
      forcats::fct_relabel(~ gsub("\\[", "", .x)) %>%
      forcats::fct_relabel(~ gsub("\\]", "", .x)) -> .data[[newQ]]

  }

  # return result
  return(.data)

}

# ggplot2 Theme for Sequoia Keynote Template

sequoia_theme <-function(base_size = 28, base_family = "sans", background = c("transparent", "white", "gray"), legend_size = 1.5, map = FALSE) {

  if (map == FALSE) {

    if (background == "gray"){

      (ggthemes::theme_foundation(base_size = base_size, base_family = base_family)
       + theme(
         line = element_line(colour = "black"),
         rect = element_rect(fill = '#F0F0F0', linetype = 0, colour = NA),
         text = element_text(colour = '#3C3C3C'),
         axis.title = element_text(),
         axis.text = element_text(),
         axis.ticks = element_blank(),
         axis.line = element_blank(),
         legend.background = element_rect(fill = '#EBEBEB'),
         legend.key = element_rect(fill = '#EBEBEB'),
         legend.key.size = unit(legend_size, units="cm"),
         legend.position = "right",
         legend.direction = "vertical",
         legend.box = "vertical",
         panel.grid = element_line(colour = NULL),
         panel.grid.major =
           element_line(colour = '#D2D2D2'),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = '#EBEBEB'),
         plot.background = element_rect(fill = '#EBEBEB'),
         plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
         plot.margin = unit(c(1, 1, 1, 1), "lines"),
         plot.caption = element_text(hjust = "0"),
         strip.background = element_rect()))


    } else if (background == "white"){

      (ggthemes::theme_foundation(base_size = base_size, base_family = base_family)
       + theme(
         line = element_line(colour = "black"),
         rect = element_rect(fill = '#F0F0F0', linetype = 0, colour = NA),
         text = element_text(colour = '#3C3C3C'),
         axis.title = element_text(),
         axis.text = element_text(),
         axis.ticks = element_blank(),
         axis.line = element_blank(),
         legend.background = element_rect(fill = '#FFFFFF'),
         legend.key = element_rect(fill = '#FFFFFF'),
         legend.key.size = unit(legend_size, units="cm"),
         legend.position = "right",
         legend.direction = "vertical",
         legend.box = "vertical",
         panel.grid = element_line(colour = NULL),
         panel.grid.major =
           element_line(colour = '#D2D2D2'),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = '#FFFFFF'),
         plot.background = element_rect(fill = '#FFFFFF'),
         plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
         plot.margin = unit(c(1, 1, 1, 1), "lines"),
         plot.caption = element_text(hjust = "0"),
         strip.background = element_rect()))

    } else if (background == "transparent"){

      (ggthemes::theme_foundation(base_size = base_size, base_family = base_family)
       + theme(
         line = element_line(colour = "black"),
         rect = element_rect(fill = '#F0F0F0', linetype = 0, colour = NA),
         text = element_text(colour = '#3C3C3C'),
         axis.title = element_text(),
         axis.text = element_text(),
         axis.ticks = element_blank(),
         axis.line = element_blank(),
         legend.background = element_rect(colour = NA, fill = NA),
         legend.key = element_blank(),
         legend.key.size = unit(legend_size, units="cm"),
         legend.position = "right",
         legend.direction = "vertical",
         legend.box = "vertical",
         panel.grid = element_line(colour = NULL),
         panel.grid.major =
           element_line(colour = '#D2D2D2'),
         panel.grid.minor = element_blank(),
         panel.background = element_blank(),
         plot.background = element_blank(),
         plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
         plot.margin = unit(c(1, 1, 1, 1), "lines"),
         plot.caption = element_text(hjust = "0"),
         strip.background = element_rect()))

    }

  } else if (map == TRUE){

    if (background == "transparent"){

      (ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
         theme(
           line = element_line(colour = "black"),
           rect = element_rect(fill = NA, linetype = 1, colour = '#898989'),
           text = element_text(colour = '#3C3C3C'),
           axis.line = element_blank(),
           axis.text = element_blank(),
           axis.ticks = element_blank(),
           axis.title = element_blank(),
           panel.background = element_blank(),
           panel.border = element_blank(),
           panel.grid = element_blank(),
           panel.spacing = unit(0, "lines"),
           plot.background = element_blank(),
           legend.justification = c(0, 0),
           legend.background = element_rect(colour = NA, fill = NA),
           legend.key.size = unit(legend_size, units="cm"),
           legend.position = "right",
           legend.direction = "vertical",
           legend.box = "vertical",
           plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
           plot.caption = element_text(hjust = "0")))

    } else if (background == "white"){

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
  }
}

# save plots
save_plots <- function(filename, plot, device = "png", preset = c("sm", "med", "lg"), dpi = 300){

  # identify plot object
  if (missing(plot)) {
    plotDef <- ggplot2::last_plot()
  } else {
    plotDef <- plot
  }

  # set small as default preset
  if (missing(preset)){
    preset <- "sm"
  }

  # save plot
  if (preset == "sm"){

    ggplot2::ggsave(filename, plotDef, device = device,
                    width = 960 * 0.352778,
                    height = 540 * 0.352778,
                    units = "mm", dpi = dpi)

  } else if (preset == "med"){

    ggplot2::ggsave(filename, plotDef, device = device,
                    width = 960 * 0.352778,
                    height = 630 * 0.352778,
                    units = "mm", dpi = dpi)

  } else if (preset == "lg"){

    ggplot2::ggsave(filename, plotDef, device = device,
                    width = 1024 * 0.352778,
                    height = 768 * 0.352778,
                    units = "mm", dpi = dpi)

  }
}
