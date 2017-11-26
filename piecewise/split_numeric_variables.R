
split_numeric_variables <- function(numeric_betas_list, eta = 0.5) {

  #---------------------------------------------------------------------------#
  # Function Layout:
  # Section 0. Input checking
  # Section 1. Reduce betas with Ramer Douglas Peucker algorithm
  # Section 2. Add last point back onto selected betas
  # Section 3. Check if any points are outside the eta buffer
  # Section 4. Determine the point furthest away from eta buffer (above)
  # Section 5. Determine the point furthest away from eta buffer (below)
  # Section 6. If no split point return or recursive function call
  #---------------------------------------------------------------------------#

  #---------------------------------------------------------------------------#
  # Section 0. Input checking ----
  #---------------------------------------------------------------------------#

  if (!is.list(numeric_betas_list)) {

    stop('numeric_betas_list should be a list.')

  }

  if (any(!sapply(numeric_betas_list, is.numeric))) {

    stop('numeric_betas_list should be a list containing numeric beta vectors.
         Some list items that are not numeric.')

  }

  if (any(!sapply(numeric_betas_list, is.vector))) {

    stop('numeric_betas_list should be a list containing numeric beta vectors.
         Some list items that are not vectors.')

  }

  #---------------------------------------------------------------------------#
  # Section 1. Loop through all betas and get split points ----
  #---------------------------------------------------------------------------#

  x <- lapply(numeric_betas_list, select_splits_from_betas, eta = eta)

  return(x)

}

aa <- split_numeric_variables(h2o_glm_coefs, 1)

hist(unlist(sapply(aa, nrow)))

hist(unlist(lapply(h2o_glm_coefs, length)))









