
#' Calculate weighted quantiles.
#'
#' @param x numeric vector whose sample quantiles are wanted.
#' @param w numeric vector of weights for each value in x.
#' @param probs numeric vector of probabilities with values in [0,1].
#'
#' @examples
#' set.seed(1)
#' a <- rnorm(30)
#' b <- abs(rnorm(30))
#' weighted_quantile(a, b, c(0.1,0.5,0.7))
#'
#' @export
weighted_quantile <- function(x, w, probs) {

  #---------------------------------------------------------------------------#
  # Function Layout:
  # Section 0. Input checking
  # Section 1. Get weighted quantiles
  #---------------------------------------------------------------------------#

  #---------------------------------------------------------------------------#
  # Section 0. Input checking ----
  #---------------------------------------------------------------------------#

  if (!is.numeric(x)) {

    stop("x must be numeric.")

  }

  if (!is.numeric(w)) {

    stop("w must be numeric.")

  }

  if (!is.numeric(probs)) {

    stop("probs must be numeric.")

  }

  if (any(probs > 1) | any(probs < 0)) {

    stop("probs nust be between 0 and 1.")

  }

  #---------------------------------------------------------------------------#
  # Section 1. Get weighted quantiles ----
  #---------------------------------------------------------------------------#

  order_x <- order(x)

  x <- x[order_x]

  w <- w[order_x]

  total_w <- sum(w)

  cum_w <- cumsum(w)

  cum_w_propn <- cum_w / total_w

  # get the index of the first cum_w_propn value which is greater than or equal
  # to each value in prob
  cum_w_q <- sapply(probs, function(x) which(cum_w_propn >= x)[1])

  # get the x value for each index above, i.e. weighted quantile values
  x_w_q <- x[cum_w_q]

  return(x_w_q)

}
