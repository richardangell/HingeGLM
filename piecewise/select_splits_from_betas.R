

select_splits_from_betas <- function(betas, eta = 0.5) {

  #---------------------------------------------------------------------------#
  # Function Layout:
  # Section 0. Input checking
  # Section 1. Reduce betas with Ramer Douglas Peucker algorithm
  # Section 2. Add last point back onto selected betas
  # Section 3. Return selected betas
  #---------------------------------------------------------------------------#

  #---------------------------------------------------------------------------#
  # Section 0. Input checking ----
  #---------------------------------------------------------------------------#

  if (!is.numeric(betas)) {

    stop('betas should be numeric.')

  }

  if (!is.vector(betas)) {

    stop('betas should be a vector.')

  }

  n <- length(betas)

  if (n < 1) {

    stop('no betas supplied.')

  } else if (n == 1) {

    warning('only one beta supplied, no reduced in betas possible.')

    return(NULL)

  }

  #---------------------------------------------------------------------------#
  # Section 1. Reduce betas with Ramer Douglas Peucker algorithm ----
  #---------------------------------------------------------------------------#

  betas_df <- data.frame(x = 1:n, y = betas)

  betas_x_splits <- ramer_douglas_peucker_betas(df = betas_df, eta = eta)

  #---------------------------------------------------------------------------#
  # Section 2. Add last point back onto selected betas ----
  #---------------------------------------------------------------------------#

  betas_x_splits <- c(betas_x_splits, n)

  #---------------------------------------------------------------------------#
  # Section 3. Return selected betas ----
  #---------------------------------------------------------------------------#

  selected_betas <- betas_df[betas_x_splits, ]

  return(selected_betas)

}


