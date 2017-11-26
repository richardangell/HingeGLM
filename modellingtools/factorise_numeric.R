
factorise_numeric <- function(x, weights = NULL, levels = NULL, weight_per_level = NULL,
                              round_levels = FALSE, ordered_result = FALSE) {
  
  #---------------------------------------------------------------------------#
  # Section 0. Input checking
  #---------------------------------------------------------------------------#
  
  if (!is.numeric(x)) {
    
    stop('x should be numeric')
    
  }
  
  if (!is.null(weights)) {
    
    if (!is.numeric(weights)) {
      
      stop('weights should be numeric')
      
    }
    
  }
  
  if (is.null(levels) & is.null(weight_per_level)) {
    
    stop('both levels and weight_per_level are NULL, either levels or weight_per_level should be specified.')
    
  } else if (!is.null(levels) & !is.null(weight_per_level)) {
    
    stop('both levels and weight_per_level are non NULL, either levels or weight_per_level should be specified.')
    
  }
  
  #---------------------------------------------------------------------------#
  # Section 1. Create equal width factor buckets
  #---------------------------------------------------------------------------#
  
  if (!is.null(levels)) {
  
    factor_x <- cut(x, breaks = levels, include.lowest = TRUE, ordered_result = ordered_result)
    
  #---------------------------------------------------------------------------#
  # Section 2. Create equal weight factor buckets
  #---------------------------------------------------------------------------#
  
  } else if (!is.null(weight_per_level)) {
    
    if (is.null(weights)) {
      
      weights <- rep(1, length(x))
      
    }
    
    total_weight <- sum(weights)
    
    x_df <- data.frame(x = x, w = weights)
    
    x_df <- x_df[order(x_df$x, decreasing = FALSE), ]
    
    
    
    factor_x <- 2
    
  } 
  
  return(factor_x)
  
}
