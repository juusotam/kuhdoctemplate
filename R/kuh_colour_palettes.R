# KUH Brand color palette
#
# To be used with ggplot2
#
# example:
#
# ggplot(aes(x = date, y = patients, fill = erva), data = data) + geom_col() +
#   scale_fill_kuh(name = "ERVA")
#
#

kuh_colors <- c(
  `red`        = "#FF4046",
  `red80`      = "#ff6b6c",
  `red50`      = "#ffa4a4",
  `yellow`     = "#FEE840",
  `yellow80`   = "#ffee78",
  `yellow50`   = "#fff5ad",
  `blue`       = "#3FA9F5",
  `blue80`     = "#6dbaf7",
  `blue50`     = "#a5d3fb",
  `black`      = "#3E3E3E",
  `black80`    = "#686868",
  `black50`    = "#a0a0a0",
  `green`      = "#006938",
  `green80`    = "#468761",
  `green50`    = "#76a589",
  `orange`     = "#E85924",
  `orange80`   = "#ee7b55",
  `orange50`   = "#f39d81",
  `violet`     = "#94268F",
  `violet80`   = "#ab54a6",
  `violet50`   = "#c180bc")

kuh_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (kuh_colors)

  kuh_colors[cols]
}

kuh_palettes <- list(
  `main`  = kuh_cols("blue", "red", "yellow", "green", "violet"),
  `main.short` = kuh_cols("blue", "yellow", "red"),
  `main80`= kuh_cols("blue80", "red80", "yellow80", "green80", "violet80"),
  `main80.short` = kuh_cols("blue80", "yellow80", "red80"),
  `main50`= kuh_cols("blue50", "red50", "yellow50", "green50", "violet50"),
  `main50.short` = kuh_cols("blue50", "yellow50", "red50"),

  `highlight` = kuh_cols("blue50", "red"),

  `cool`  = kuh_cols("blue", "green", "violet", "black"),

  `hot`   = kuh_cols("yellow", "orange", "red"),

  `mixed` = kuh_cols("blue", "green", "yellow", "orange", "red")
)

kuh_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- kuh_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

scale_color_kuh <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- kuh_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("kuh_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_kuh <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- kuh_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("kuh_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
