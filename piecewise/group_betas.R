
#' Group coefficients by underlying variable.
#'
#' For use when continuous variables have been bucketed for use in regression
#' models, to group the, back together.
#'
#' @param betas vector of betas / coefficients.
#' @param beta_names character vector of names for each beta. Default is
#'        \code{NULL} which uses the names from \code{betas}.
#'
#' @examples
#' coefs <- c(-2.44, -2.37, 0.37, 0, 0.95, 0.92)
#' coef_names <- c('a.[0,5]', 'a.(5, 10]', 'a.(10,20]',
#'                 'b.level1', 'b.level2', 'c')
#'
#' group_betas(coefs, coef_names)
#'
#' @export
group_betas <- function(betas, beta_names = NULL) {

  #---------------------------------------------------------------------------#
  # Function Layout:
  # Section 0. Input checking
  # Section 1. Separate beta names by "."
  # Section 2. Identify lb and ub from "(lb, ub]" or "[lb, ub]" examples
  # Section 3. Group betas by base variable
  #---------------------------------------------------------------------------#

  warning('this function assumes that coefficient names for bucketed ordinal ',
          'variables follows this naming convention "var_name.[lb, ub]", ',
          '"var_name.(lb, ub]", etc..')

  #---------------------------------------------------------------------------#
  # Section 0. Input checking  ----
  #---------------------------------------------------------------------------#

  if (is.null(beta_names)) {

    beta_names <- names(betas)

  } else {

    names(betas) <- beta_names

  }

  if (length(beta_names) != length(betas)) {

    stop('beta and beta names are of different lengths')

  }

  #---------------------------------------------------------------------------#
  # Section 1. Separate beta names by "."  ----
  #---------------------------------------------------------------------------#

  # get location of first . in beta names
  beta_names_dot <- regexpr("\\.", beta_names)

  # first part of beta names split by first "."
  beta_names_dot_1 <- mapply(function(x, y) if (y == - 1) {
                                              x
                                            } else {
                                              substr(x, 1, y - 1)
                                            },
                             beta_names,
                             beta_names_dot)

  # second part of beta names split by first "."
  beta_names_dot_2 <- mapply(function(x, y) if (y == - 1) {
                                              NA
                                            } else {
                                              substr(x, y + 1, nchar(x))
                                            },
                             beta_names,
                             beta_names_dot)

  #---------------------------------------------------------------------------#
  # Section 2. Identify lb and ub from "(lb, ub]" or "[lb, ub]" examples ----
  #---------------------------------------------------------------------------#

  # remove "(" "[" "]" which are assumed to be part of the bucketed variable names
  # this will take "(a, b]" or "[a, b]" to "a, b"
  beta_names_dot_2_clean <- gsub("\\(", "", beta_names_dot_2)

  beta_names_dot_2_clean <- gsub("\\[", "", beta_names_dot_2_clean)

  beta_names_dot_2_clean <- gsub("\\]", "", beta_names_dot_2_clean)

  # split the cleaned name by ,
  # this will take "a, b" to "a", "b"
  beta_names_dot_2_split <- strsplit(beta_names_dot_2_clean, "\\,")

  # extract the first and second part of the word when split by ","
  beta_names_dot_2_split_1 <- sapply(beta_names_dot_2_split, "[", 1)

  beta_names_dot_2_split_2 <- sapply(beta_names_dot_2_split, "[", 2)

  # any words which did not have only 1 "," are removed as they do not follow
  # the convention of "(a, b]" or "[a, b]" which is what we are looking for
  beta_names_dot_2_split_1[which(sapply(beta_names_dot_2_split,
                                        length) != 2)] <- NA

  beta_names_dot_2_split_2[which(sapply(beta_names_dot_2_split,
                                        length) != 2)] <- NA

  # convert to numeric
  # do not want characters so conversion to NA is acceptable hence the
  # supressed warnings
  suppressWarnings(beta_names_dot_2_split_1 <- as.numeric(beta_names_dot_2_split_1))

  suppressWarnings(beta_names_dot_2_split_2 <- as.numeric(beta_names_dot_2_split_2))

  #---------------------------------------------------------------------------#
  # Section 3. Group betas by base variable  ----
  #---------------------------------------------------------------------------#

  base_vars_unique <- unique(beta_names_dot_1)

  beta_group_info <- list()

  # put base variable, coefficients, extract lower and upper bounds into nested
  # list grouped by the base variable
  for (i in base_vars_unique) {

    beta_group_info[[i]]$base_variable <- beta_names_dot_1[which(beta_names_dot_1 == i)]

    beta_group_info[[i]]$coef <- betas[which(beta_names_dot_1 == i)]

    beta_group_info[[i]]$lower_bound <- beta_names_dot_2_split_1[which(beta_names_dot_1 == i)]

    beta_group_info[[i]]$upper_bound <- beta_names_dot_2_split_2[which(beta_names_dot_1 == i)]

  }

  # identify which variables are buckted ordinal variables
  ordered_bucketed_vars <- sapply(beta_group_info, ordered_buckets_check)

  for (i in 1:length(beta_group_info)) {

    # reorder the coefficients for buckted ordinal variables
    # often the lower most bucket will end up at the top as it starts with "["
    if (ordered_bucketed_vars[i] == 1) {

      beta_group_info[[i]]$coef <- beta_group_info[[i]]$coef[order(beta_group_info[[i]]$lower_bound)]

    }

  }

  # select just the grouped betas to return
  grouped_betas <- sapply(beta_group_info, '[', 2)

  names(grouped_betas) <- names(beta_group_info)

  return(list(coefficients = grouped_betas,
              ordered_bucketed_vars = ordered_bucketed_vars))

}




#' Check for bucketed ordinal variable.
#'
#' To be used within group_betas function. This function checks whether a
#' bucket label is from an ordinal variable that has been bucketed, from
#' the lower and upper bounds of the bucket extracted in group_betas. There
ordered_buckets_check <- function(x) {

  n_lb <- length(x$lower_bound)

  n_ub <- length(x$upper_bound)

  n_lb_na <- sum(is.na(x$lower_bound))

  n_ub_na <- sum(is.na(x$upper_bound))

  # if lb and ub have different lengths
  if (n_lb != n_ub) {

    return(0)

  }

  # either lb or ub vectors are not numeric
  if (!is.numeric(x$lower_bound) | !is.numeric(x$lower_bound)) {

    return(0)

  }

  # if the number if NAs in either is greater than 1
  if (n_lb_na > 1 | n_ub_na > 1) {

    return(0)

  }

  # if there is only 1 element which is NA
  if (n_lb == 1 & n_lb_na == 1) {

    return(0)

  }

  # if there is only 1 element which is NA
  if (n_ub == 1 & n_ub_na == 1) {

    return(0)

  }

  return(1)

}
