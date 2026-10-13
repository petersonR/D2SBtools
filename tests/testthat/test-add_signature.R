skip_if_not_installed("ggplot2")
skip_if_not_installed("cowplot")
skip_if_not_installed("magick")

blank_plot <- function() ggplot2::ggplot() + ggplot2::theme_void()

# Render a plot at `dpi` and return the bounding box of its non-white pixels,
# in inches from the bottom-right corner.
ink_box <- function(p, width, height, dpi = 100) {
  f <- withr::local_tempfile(fileext = ".png")
  ggplot2::ggsave(f, p, width = width, height = height, dpi = dpi,
                  bg = "white")
  px <- as.integer(magick::image_data(magick::image_read(f), "gray"))[, , 1]
  ink <- which(px < 250, arr.ind = TRUE)
  if (nrow(ink) == 0) return(NULL)
  list(
    width = (max(ink[, "col"]) - min(ink[, "col"]) + 1) / dpi,
    right_gap = (ncol(px) - max(ink[, "col"])) / dpi,
    bottom_gap = (nrow(px) - max(ink[, "row"])) / dpi
  )
}

test_that("returns a ggplot", {
  expect_s3_class(add_signature(blank_plot(), 7, 5), "ggplot")
})

test_that("the logo has the same physical size and position at any figure size", {
  small <- ink_box(add_signature(blank_plot(), 3.5, 3), 3.5, 3)
  large <- ink_box(add_signature(blank_plot(), 7, 7), 7, 7)

  for (box in list(small, large)) {
    expect_equal(box$width, 0.75, tolerance = 0.03)
    expect_equal(box$right_gap, 0.06, tolerance = 0.03)
    expect_equal(box$bottom_gap, 0.06, tolerance = 0.03)
  }
})

test_that("the logo sits in a strip below the plot, not on top of it", {
  # A plot whose panel is solid black (the panel excludes the margins, unlike
  # plot.background): if the strip were not reserved, the panel would reach
  # the bottom of the figure and those rows would be black.
  p <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "black", colour = NA)
    )
  f <- withr::local_tempfile(fileext = ".png")
  ggplot2::ggsave(f, add_signature(p, 7, 5), width = 7, height = 5, dpi = 100,
                  bg = "white")
  px <- as.integer(magick::image_data(magick::image_read(f), "gray"))[, , 1]

  # The left half of the bottom 0.2 in is outside the logo, so it must be
  # white: the strip was reserved.
  bottom_left <- px[(nrow(px) - 19):nrow(px), 1:(ncol(px) / 2)]
  expect_true(all(bottom_left > 250))
})

test_that("opacity below 1 lightens the logo", {
  mean_ink <- function(opacity) {
    f <- withr::local_tempfile(fileext = ".png")
    ggplot2::ggsave(f, add_signature(blank_plot(), 7, 5, opacity = opacity),
                    width = 7, height = 5, dpi = 100, bg = "white")
    mean(as.integer(magick::image_data(magick::image_read(f), "gray")))
  }
  expect_gt(mean_ink(0.3), mean_ink(1))
})

test_that("bad input is rejected", {
  expect_error(add_signature(mtcars, 7, 5), "ggplot")
  expect_error(add_signature(blank_plot(), 7, 5, opacity = 2), "opacity")
  expect_error(add_signature(blank_plot(), 0.5, 5), "does not fit")
  expect_error(add_signature(blank_plot(), 7, 0.2), "no room")
})
