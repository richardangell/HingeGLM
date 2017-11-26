
#' Reduce the number of points (betas) using Ramer Douglas Peucker algorithm.
#'
#' This function is intended to be used to find split points for numeric
#' variables in regression models. Once a model has been fitted on bucketed
#' numeric variables this function can be run to reduce the number of betas.
#' Then take these betas as the split points to create piecewise segments
#' to fit in another model.
#'
#' @param df \code{data.frame} containing points (betas) to reduce. Must
#'        contain columns 'x' and 'y'.
#' @param eta tolerance for choosing when to keep a point or not.
#'
#' @details Ramer Douglas Peucker algorithm fits a straight line between the
#'          input start and end points, it then adds a buffer around the line
#'          a perpeendicular distance of eta away. If any points fall outside
#'          the buffer then the furthest point away is selected and the
#'          algorithm recursively calls itself either side of the point. If
#'          no points fall outside the eta buffer or there are only two points
#'          then the lowest x point in current line is returned.
#'
#' @references \url{https://en.wikipedia.org/wiki/Ramer-Douglas-Peucker_algorithm}
#'
#' @examples
#' coefs <- data.frame(x = 1:6, y = c(-2.44, -2.37, 0.37, 0, 0.95, 0.92))
#'
#' ramer_douglas_peucker_betas(coefs, 0.6)
#'
#' @export
ramer_douglas_peucker_betas_LIST <- function(df, eta = 0.01) {

  #---------------------------------------------------------------------------#
  # Function Layout:
  # Section 0. If there are only two points then do not try to split
  # Section 1. Fit straight line through the start and end points
  # Section 2. Construct eta buffer around line
  # Section 3. Check if any points are outside the eta buffer
  # Section 4. Determine the point furthest away from eta buffer (above)
  # Section 5. Determine the point furthest away from eta buffer (below)
  # Section 6. If no split point return or recursive function call
  #---------------------------------------------------------------------------#

  #---------------------------------------------------------------------------#
  # Section 1. Fit straight line through the start and end points ----
  #---------------------------------------------------------------------------#

  n <- nrow(df)

  straight_line <- lm(y ~ x, data = df[c(1, n), ])

  # get line predictions for all points in df
  line_points <- predict(straight_line, df)

  #---------------------------------------------------------------------------#
  # Section 2. Construct eta buffer around line ----
  #---------------------------------------------------------------------------#

  x_delta <- df[n, 'x'] - df[1, 'x']

  y_delta <- df[n, 'y'] - df[1, 'y']

  # calculate the angle of the line from the horizontal
  line_angle_form_horiz <- atan(y_delta / x_delta)

  # calculate the vertical offset of the line which is a perpendicular
  # distance of eta away
  eta_vertical_offset <- eta * cos(line_angle_form_horiz)

  upper_eta_line <- line_points + eta_vertical_offset

  lower_eta_line <- line_points - eta_vertical_offset

  #---------------------------------------------------------------------------#
  # Section 0. If there are only two points then do not try to split ----
  #---------------------------------------------------------------------------#

  if (n < 3) {

    return_list <- list(list(split_point = df$x[1],
                             straight_line = straight_line,
                             line_points = line_points,
                             upper_eta_line = upper_eta_line,
                             lower_eta_line = lower_eta_line))

    names(return_list) <- paste0('split_', df$x[1], '_', df$x[n])

    return(return_list)

  }

  #---------------------------------------------------------------------------#
  # Section 3. Check if any points are outside the eta buffer ----
  #---------------------------------------------------------------------------#

  below_lower_eta <- which(df$y < lower_eta_line)

  above_upper_eta <- which(df$y > upper_eta_line)

  #plot(df$x, df$y)

  #lines(df$x,line_points, type = 'l')

  #lines(df$x,upper_eta_line, type = 'l', col = 2)

  #lines(df$x,lower_eta_line, type = 'l', col = 2)

  #---------------------------------------------------------------------------#
  # Section 4. Determine the point furthest away from eta buffer (above) ----
  #---------------------------------------------------------------------------#

  if (any(above_upper_eta)) {

    above_perp_dist <- (df$y[above_upper_eta] - upper_eta_line[above_upper_eta]) * cos(line_angle_form_horiz)

    max_perp_dist_above <- max(above_perp_dist)

  }

  #---------------------------------------------------------------------------#
  # Section 5. Determine the point furthest away from eta buffer (below) ----
  #---------------------------------------------------------------------------#

  if (any(below_lower_eta)) {

    below_perp_dist <- (lower_eta_line[below_lower_eta] - df$y[below_lower_eta]) * cos(line_angle_form_horiz)

    max_perp_dist_below <- max(below_perp_dist)

  }

  #---------------------------------------------------------------------------#
  # Section 6. If no split point return or recursive function call ----
  #---------------------------------------------------------------------------#

  # if there is no point outside the eta butter return start point
  if (!any(above_upper_eta) & !any(below_lower_eta)) {

    return_list <- list(list(split_point = df$x[1],
                             straight_line = straight_line,
                             line_points = line_points,
                             upper_eta_line = upper_eta_line,
                             lower_eta_line = lower_eta_line))

    names(return_list) <- paste0('split_', df$x[1], '_', df$x[n])

    return(return_list)

    # otherwise if there was a point outside the eta buffer determine the x value
    # then call ramer_douglas_peucker again on sections above and below the split
    # point
  } else {

    if (any(above_upper_eta) & any(below_lower_eta)) {

      if (max_perp_dist_above > max_perp_dist_below) {

        split_point <- above_upper_eta[which(above_perp_dist == max_perp_dist_above)[1]]

      } else {

        split_point <- below_lower_eta[which(below_perp_dist == max_perp_dist_below)[1]]

      }

    } else if (any(above_upper_eta)) {

      split_point <- above_upper_eta[which(above_perp_dist == max_perp_dist_above)[1]]

    } else if (any(below_lower_eta)) {

      split_point <- below_lower_eta[which(below_perp_dist == max_perp_dist_below)[1]]

    }

    left_split_call <- ramer_douglas_peucker_betas(df = df[1:split_point, ], eta = eta)

    right_split_call <- ramer_douglas_peucker_betas(df = df[split_point:n, ], eta = eta)

    return(c(left_split_call, right_split_call))

  }

}

