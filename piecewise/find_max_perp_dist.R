
#' Find the maxium perpendicular distance.
#'
#' Find the maximum perpendicular distance from the line joining start and
#' end points to points in between the start and end points.
#'
#' @param df \code{data.frame} containing points. Must contain columns "x" and
#'        "y".
#'
#' @examples
#' coefs <- data.frame(x = 1:6, y = c(-2.44, -2.37, 0.37, 0, 0.95, 0.92))
#'
#' find_max_perp_dist(coefs)
#'
#' @export
find_max_perp_dist <- function(df) {

  #---------------------------------------------------------------------------#
  # Function Layout:
  # Section 1. Fit straight line through the start and end points
  # Section 2. Get maximum perpendicular distance
  #---------------------------------------------------------------------------#

  #---------------------------------------------------------------------------#
  # Section 1. Fit straight line through the start and end points ----
  #---------------------------------------------------------------------------#

  n <- nrow(df)

  straight_line <- lm(y ~ x, data = df[c(1, n), ])

  # get line predictions for all points in df
  line_points <- predict(straight_line, df)

  x_delta <- df[n, 'x'] - df[1, 'x']

  y_delta <- df[n, 'y'] - df[1, 'y']

  # calculate the angle of the line from the horizontal
  line_angle_form_horiz <- atan(y_delta / x_delta)

  #---------------------------------------------------------------------------#
  # Section 2. Get maximum perpendicular distance ----
  #---------------------------------------------------------------------------#

  vertical_distances <- abs(df$y - line_points)

  perpendicular_distances <- vertical_distances * cos(line_angle_form_horiz)

  return(max(perpendicular_distances))

}
