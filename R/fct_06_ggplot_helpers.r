# Wrapping long text by adding \n
#  "Mitotic DNA damage checkpoint"  --> "Mitotic DNA damage\ncheckpoint"
# https://stackoverflow.com/questions/7367138/text-wrap-for-plot-titles

#' wrap_strings
#'
#' @description Inserts line breaks into long strings so they wrap onto
#'   multiple lines (e.g. pathway names used as axis labels or plot titles)
#'   instead of running off the edge of the plot.
#'
#' @param vector_of_strings Character vector of strings to wrap.
#' @param width Maximum number of characters per line before wrapping.
#'
#' @return A character vector the same length as `vector_of_strings`, with
#'   `\n` inserted at word boundaries near every `width` characters.
#'
#' @noRd
wrap_strings <- function(vector_of_strings, width = 30) {
  as.character(sapply(vector_of_strings, FUN = function(x) {
    paste(strwrap(x, width = width), collapse = "\n")
  }))
}

# function to increase vertical spacing between legend keys
# @clauswilke https://stackoverflow.com/questions/11366964/is-there-a-way-to-change-the-spacing-between-legend-items-in-ggplot2

#' draw_key_polygon3
#'
#' @description Custom ggplot2 legend key-drawing function that increases
#'   the vertical spacing between legend keys (the default `draw_key_polygon`
#'   packs keys too tightly). Assigned to `GeomBar$draw_key` immediately
#'   below, so it takes effect for every bar/column plot for the rest of the
#'   R session.
#'
#' @param data,params,size Standard `draw_key_*` arguments supplied by
#'   ggplot2's legend-drawing machinery (see `ggplot2::draw_key`); not meant
#'   to be called directly.
#'
#' @return A `grid::rectGrob()` representing one legend key.
#'
#' @noRd
draw_key_polygon3 <- function(data, params, size) {
  lwd <- min(data$size, min(size) / 4)

  grid::rectGrob(
    width = grid::unit(0.6, "npc"),
    height = grid::unit(0.6, "npc"),
    gp = grid::gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lty = data$linetype,
      lwd = lwd * .pt,
      linejoin = "mitre"
    )
  )
}
# register new key drawing function,
# the effect is global & persistent throughout the R session
GeomBar$draw_key <- draw_key_polygon3

# find peak values in density plots
# for adding annotation texts
# http://ianmadd.github.io/pages/PeakDensityDistribution.html

#' densMode
#'
#' @description Finds the peak (mode) of a kernel density estimate, used to
#'   position an annotation label (e.g. a significance marker) at the
#'   tallest point of a density plot.
#'
#' @param x Numeric vector to compute the density of.
#'
#' @return A list with `x` and `y`, the coordinates of the density curve's
#'   highest point.
#'
#' @noRd
densMode <- function(x) {
  td <- density(x, na.rm = TRUE)
  maxDens <- which.max(td$y)
  list(x = td$x[maxDens], y = td$y[maxDens])
}

#' refine_ggplot2
#'
#' @description Applies the user-selected ggplot2 theme to a plot, and
#'   optionally strips the panel gridlines.
#'
#' @param p A ggplot2 plot object.
#' @param gridline `TRUE` to keep panel gridlines, `FALSE` to remove them.
#' @param ggplot2_theme Theme name selected by the user: one of
#'   `"linedraw"`, `"classic"`, `"gray"`, `"light"`, `"dark"`, `"bw"` to
#'   apply that `ggplot2::theme_*()`, the sentinel `"Add grid"` to leave
#'   gridlines untouched regardless of `gridline`, or any other value for no
#'   theme change.
#'
#' @return `p` with the theme applied and gridlines removed if requested.
#'
#' @noRd
refine_ggplot2 <- function(p, gridline, ggplot2_theme = "light") {

  # apply theme based on selection
  p <- switch(ggplot2_theme,
    "linedraw" = p + ggplot2::theme_linedraw(),
    "classic" = p + ggplot2::theme_classic(),
    "gray" = p + ggplot2::theme_gray(),
    "light" = p + ggplot2::theme_light(),
    "dark" = p + ggplot2::theme_dark(),
    "bw" = p + ggplot2::theme_bw(),
    p # default, no change
  )

  if (ggplot2_theme != "Add grid") { # keep grid
    if (!gridline) { # by default it has gridlines
      p <- p +
        ggplot2::theme(panel.grid = ggplot2::element_blank())
    }
  }

  return(p)
}

# generates a fake ggplot2, with some message like: "Not available."

#' fake_plot
#'
#' @description Builds a blank placeholder ggplot2 plot displaying a
#'   centered message, for use in place of a real plot when there's no data
#'   available to draw one.
#'
#' @param some_text Message to display in the middle of the placeholder
#'   plot.
#'
#' @return A ggplot2 plot object with no axes, legend, or gridlines — just
#'   the message text.
#'
#' @noRd
fake_plot <- function(some_text) {
  p <- ggplot2::ggplot() +
    geom_point() +
    xlim(-10, 10) +
    ylim(-10, 10) +
    annotate("text",
      x = 0,
      y = 0,
      label = some_text
    ) +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
  return(p)
}
