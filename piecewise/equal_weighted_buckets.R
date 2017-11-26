
#' Cut numeric vectors in equal weighted buckets.
#'
#' @param x numeric vector to cut into equal weight buckets.
#' @param w numeric vector of weights for each value in x. Default value of
#'        \code{NULL} uses equal weights.
#' @param probs numeric vector of weighted probabilities with values in [0,1],
#'        to cut x at.
#' @param ... further arguments to pass to \code{cut}.
#'
#' @examples
#' set.seed(1)
#' a <- rnorm(30)
#' b <- abs(rnorm(30))
#' equal_weighted_buckets(a, b, c(0.1,0.5,0.7), include.lowest = TRUE)
#'
#' @export
equal_weighted_buckets <- function(x, w = NULL, probs = seq(0, 1, 0.1), ...) {

  #---------------------------------------------------------------------------#
  # Function Layout:
  # Section 0. Input checking
  # Section 1. Calculate weighted quantiles
  # Section 2. Cut x by weighted quantile values
  #---------------------------------------------------------------------------#

  #---------------------------------------------------------------------------#
  # Section 0. Input checking
  #---------------------------------------------------------------------------#

  if (is.null(w)) {

    w <- rep(1, length(x))

  }

  #---------------------------------------------------------------------------#
  # Section 1. Calculate weighted quantiles
  #---------------------------------------------------------------------------#

  # # ensure the quantiles to calculate include min and max values
  weighted_quantiles <- weighted_quantile(x = x,
                                          w = w,
                                          probs = unique(c(0, probs, 1)))

  #---------------------------------------------------------------------------#
  # Section 2. Cut x by weighted quantile values
  #---------------------------------------------------------------------------#

  x_weighted_buckets <- cut(x = x, breaks = unique(weighted_quantiles), ...)

  return(list(bucketed_x = x_weighted_buckets,
              break_points = unique(weighted_quantiles)))

}
