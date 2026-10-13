#' Add the D2SB figure signature to a plot
#'
#' Places the D2SB logo in the bottom-right corner of a ggplot, in a strip
#' reserved below the plot so it never collides with axis labels. Size and
#' inset are given in inches rather than as fractions of the figure, so the
#' mark is physically identical on every figure whatever its dimensions. This
#' is the group's visual signature for published figures; use it on every
#' figure, at the defaults, so they read as one family.
#'
#' Because placement is computed for a particular figure size, `width` and
#' `height` must match the size the figure is saved or rendered at (e.g. the
#' `width`/`height` passed to [ggplot2::ggsave()], or a Quarto chunk's
#' `fig-width`/`fig-height`). The logo is black and gray, so it does not turn
#' a grayscale figure into a colour one.
#'
#' @param p A ggplot object.
#' @param width,height Size of the saved figure, in inches.
#' @param logo_width Width of the logo, in inches.
#' @param inset Gap between the logo and the figure edges, in inches.
#' @param opacity Logo opacity, from 0 (invisible) to 1. Below about 0.6 the
#'   white lettering on the gray bar becomes hard to read.
#'
#' @return A ggplot object (drawn with [cowplot::ggdraw()]).
#'
#' @examples
#' \dontrun{
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point()
#' signed <- add_signature(p, width = 7, height = 5)
#' ggplot2::ggsave("fig1.tif", signed, width = 7, height = 5, dpi = 1200)
#' }
#'
#' @export
add_signature <- function(p, width, height, logo_width = 0.75, inset = 0.06,
                          opacity = 1) {
  rlang::check_installed(c("ggplot2", "cowplot", "magick"))

  if (!inherits(p, "ggplot")) {
    rlang::abort("`p` must be a ggplot object.")
  }
  if (!is.numeric(opacity) || length(opacity) != 1 ||
      opacity < 0 || opacity > 1) {
    rlang::abort("`opacity` must be a single number between 0 and 1.")
  }
  if (logo_width + 2 * inset > width) {
    rlang::abort(glue::glue(
      "A {logo_width} in logo with {inset} in insets does not fit a ",
      "{width} in wide figure."
    ))
  }

  logo <- magick::image_read(
    system.file("logo", "logo-small.png", package = "D2SBtools")
  )
  if (opacity < 1) {
    logo <- magick::image_fx(logo, expression = paste0(opacity, "*a"),
                             channel = "alpha")
  }
  info <- magick::image_info(logo)
  logo_height <- logo_width * info$height / info$width
  strip <- logo_height + 2 * inset

  if (strip >= height) {
    rlang::abort(glue::glue(
      "The signature strip ({round(strip, 2)} in) leaves no room in a ",
      "{height} in tall figure."
    ))
  }

  # Reserve a strip below the plot for the mark. ggplot2 cannot mix units in
  # one margin, so the strip is converted to points.
  p <- p +
    ggplot2::theme(
      plot.margin = ggplot2::margin(5.5, 5.5, strip * 72, 5.5, unit = "pt")
    )

  cowplot::ggdraw(p) +
    cowplot::draw_image(
      logo,
      x = 1 - (logo_width + inset) / width, y = inset / height,
      width = logo_width / width, height = logo_height / height
    )
}
