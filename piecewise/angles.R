




#' Calculate the angle between the origin and two points.
#'
#' @param x1 x coord of point 1.
#' @param y1 y coord of point 1.
#' @param x2 x coord of point 2.
#' @param y2 y coord of point 2.
#'
#' @examples
#' angle_between_points(4, 4, -4, 4)
#'
#' @export
angle_between_points <- function(x1, y1, x2, y2) {

  # calculate angle from horizontal for point 1
  horiz_phi1 <- atan2(y1, x1)

  #print(horiz_phi1)

  # calculate angle from horizontal for point 2
  horiz_phi2 <- atan2(y2, x2)

  #print(horiz_phi2)

  # if both angles from horizontal have the same sign
  if (sign(horiz_phi2) == sign(horiz_phi1)) {

    delta_phi <- abs(horiz_phi2 - horiz_phi1)

  } else {

    delta_phi <- abs(horiz_phi2) + abs(horiz_phi1)

  }

  return(delta_phi)

}




angles(coefs, 10)

angles <- function(df, angle_tol) {

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
  # Section 0. If there are only two points then do not try to split ----
  #---------------------------------------------------------------------------#

  print('-----------')

  print(df)

  n <- nrow(df)

  if (n < 3) {

    print('a')

    return(df$x[1])

  }

  #---------------------------------------------------------------------------#
  # Section 1. Offset all points so that the first point is (0, 0) ----
  #---------------------------------------------------------------------------#

  df_origin <- df

  df_origin$x <- df$x - 1

  df_origin$y <- df$y - df[1, 'y']

  plot(df_origin)

  angle_change <- sapply(3:n, function(a) angle_between_points(x1 = df_origin[2, 'x'],
                                                               y1 = df_origin[2, 'y'],
                                                               x2 = df_origin[a, 'x'],
                                                               y2 = df_origin[a, 'y']))

  angle_change_deg <- angle_change * 180 / pi

  print(angle_change_deg)

  if (any(angle_change_deg > angle_tol)) {

    print('b')

    first_split_point <- which(angle_change_deg > angle_tol)[1] + 1

    right_side_split <- angles(df = df[first_split_point:n, ], angle_tol = angle_tol)

    return(c(df$x[1], right_side_split))


  } else {

    print('c')

    return(df$x[1])

  }

}





