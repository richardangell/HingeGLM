
#' Build regression models using hinges from ordered variables.
#'
#' Build regression models using hinges from ordered variables to capture non
#' linear trends in the explanatory variables..
#'
#' @param data \code{data.frame} containing
#' @param ordered_variables
#' @param non_ordered_variables
#'
#' @examples
#'
#' @export
hinge_glm <- function(data, weights = NULL, ordered_variables, non_ordered_variables, ...) {

  #---------------------------------------------------------------------------#
  # Function Layout:
  # Section 0. Input checking
  # Section 1. Bucket the ordered variables
  # Section 2. Build model on bucketed variables
  # Section 3. Group the coefficients from the bucketed glm
  # Section 4. Add randomised versions of bucketed ordered columns
  # Section 5. Build model on the randomised ordered columns
  # Section 6. Find max perpendicular distance for the random col coefs
  # Section 7. Run ramer douglas peucker to choose hinge points
  # Section 8. Add hinge columns to data
  # Section 9. Build final models and hinged data
  #---------------------------------------------------------------------------#

  #---------------------------------------------------------------------------#
  # Section 0. Input checking ----
  #---------------------------------------------------------------------------#

  if (length(ordered_variables) == 0) {

    stop("no variables supplied in ordered_variables.")

  }

  if (any(!ordered_variables %in% colnames(data))) {

    missing <- ordered_variables[which(!ordered_variables %in% colnames(data))]

    stop("the following variables in ordered_variables are not in data ",
         paste(missing, sep = " "),
         ".")

  }

  #---------------------------------------------------------------------------#
  # Section 1. Bucket the ordered variables ----
  #---------------------------------------------------------------------------#

  bucket_split_points <- list()

  for (col in ordered_variables) {

    buckets <- equal_weighted_buckets(x = data[[col]],
                                      w = weights,
                                      include.lowest = TRUE)

    bucket_split_points[[col]] <- buckets$break_points

    data[[paste0(col, "_bucketed")]] <- buckets$bucketed_x

  }

  #---------------------------------------------------------------------------#
  # Section 2. Add randomised versions of bucketed ordered columns ----
  #---------------------------------------------------------------------------#

  for (col in ordered_variables) {

    data[[paste0(col, "_bucket_rand")]] <- sample(data[[paste0(col, "_bucketed")]],
                                                  size = nrow(data),
                                                  replace = TRUE)

  }

  #---------------------------------------------------------------------------#
  # Section 3. Send data to h2o ----
  #---------------------------------------------------------------------------#

  data_h2o <- as.h2o(data)

  #---------------------------------------------------------------------------#
  # Section 4. Build model on bucketed variables ----
  #---------------------------------------------------------------------------#

  glm_bucketed <- h2o.glm(x = c(non_ordered_variables,
                                paste0(ordered_variables, "_bucketed")),
                          training_frame = data_h2o,
                          ...)

  print(glm_bucketed)

  #---------------------------------------------------------------------------#
  # Section 5. Group the coefficients from the bucketed glm ----
  #---------------------------------------------------------------------------#

  glm_bucketed_coefs <- group_betas(glm_bucketed@model$coefficients)

  print(glm_bucketed_coefs)

  #---------------------------------------------------------------------------#
  # Section 6. Build model on the randomised bucketed columns ----
  #---------------------------------------------------------------------------#

  glm_random <- h2o.glm(x = c(non_ordered_variables,
                              paste0(ordered_variables, "_bucket_rand")),
                        training_frame = data_h2o,
                        family = 'gamma',
                        link = 'log',
                        alpha = 0,
                        lambda_search = FALSE,
                        y = 'cnt')

  print(glm_random)

  #---------------------------------------------------------------------------#
  # Section 7. Group the coefficients from the randomised bucketed glm ----
  #---------------------------------------------------------------------------#

  glm_random_coefs <- group_betas(glm_random@model$coefficients)

  print(glm_random_coefs)

  #---------------------------------------------------------------------------#
  # Section 8. Find max perpendicular distance for the random col coefs ----
  #---------------------------------------------------------------------------#

  max_perp_dists <- rep(0, length(glm_random_coefs$ordered_bucketed_vars))

  for (i in 1:length(glm_random_coefs$ordered_bucketed_vars)) {

    if (glm_random_coefs$ordered_bucketed_vars[i] == 1) {

      coef_df <- data.frame(x = 1:length(glm_random_coefs$coefficients[[i]]),
                            y = glm_random_coefs$coefficients[[i]])

      max_perp_dists[i] <- find_max_perp_dist(coef_df)

    }

  }

  print(max_perp_dists)

  #---------------------------------------------------------------------------#
  # Section 9. Run ramer douglas peucker to choose hinge points ----
  #---------------------------------------------------------------------------#

  split_points <- list()

  for (i in 1:length(glm_random_coefs$ordered_bucketed_vars)) {

    if (glm_random_coefs$ordered_bucketed_vars[i] == 1) {

      coef_df <- data.frame(x = 1:length(glm_bucketed_coefs$coefficients[[i]]),
                            y = glm_bucketed_coefs$coefficients[[i]])

      split_points[[i]] <- ramer_douglas_peucker_betas(coef_df, max_perp_dists[i])

    } else {

      split_points[[i]] <- NA

    }

  }

  print(split_points)

  #---------------------------------------------------------------------------#
  # Section 8. Add hinge columns to data ----
  #---------------------------------------------------------------------------#

  ncols <- ncol(data)

  for (i in 1:length(glm_bucketed_coefs$ordered_bucketed_vars)) {

    if (glm_bucketed_coefs$ordered_bucketed_vars[i] == 1) {

      var_name <- names(glm_bucketed_coefs$coefficients)[i]

      var_name <- gsub("_bucketed", "", var_name)

      data <- add_hinges_to_data(data = data,
                                 var = var_name,
                                 split_points = split_points[[i]],
                                 split_values = bucket_split_points[[var_name]])

    }

  }

  print(data)

  #---------------------------------------------------------------------------#
  # Section 9. Build final models and hinged data ----
  #---------------------------------------------------------------------------#

  hinge_cols <- colnames(data)[ncols:ncol(data)]

  print(hinge_cols)

  data_h2o <- as.h2o(data)

  glm_hinge <- h2o.glm(x = c(non_ordered_variables, ordered_variables, hinge_cols),
                       training_frame = data_h2o,
                       ...)

  print(glm_hinge)

  return(glm_hinge)

}



a <- hinge_glm(data = train,
          ordered_variables = c('temp', 'atemp', 'hum', 'windspeed'),
          non_ordered_variables = c('season', 'yr', 'mnth', 'hr', 'holiday', 'weekday', 'workingday', 'weathersit'),
          y = 'cnt',
          nfolds = 5,
          seed = 1,
          family = 'gamma',
          link = 'log',
          alpha = 1,
          lambda_search = TRUE,
          nlambdas = 100)



