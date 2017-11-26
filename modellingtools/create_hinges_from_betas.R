

group_betas <- function(betas, beta_names = NULL) {

  warning('this function assumes that coefficient names follow this naming convention "variable_name.[lower_bound, upper_bound]", "variable_name.(lower_bound, upper_bound]", etc..,')

  #---------------------------------------------------------------------------#
  # Section 0. Input checking
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
  # Section 1. Group betas by base variable
  #---------------------------------------------------------------------------#

  # first split the beta names by '.('
  split <- strsplit(beta_names, split = '\\.\\(')

  splits <- sapply(split, length)

  # then split beta names by '.['
  split[which(splits == 1)] <- strsplit(beta_names[which(splits == 1)], split = '\\.\\[')

  # get the first word i.e. base variable
  variable_name <- sapply(split, '[', 1)

  # get the second word i.e. var level
  variable_level <- sapply(split, '[', 2)

  # split the variable level by comma
  lower_bound <- strsplit(variable_level, ',')

  # get the lower bound of the range
  lower_bound <- as.numeric(sapply(lower_bound, '[', 1))

  # add back on the '(' which is removed in strsplit
  variable_level[which(splits != 1)] <- paste0('(', variable_level[which(splits != 1)])

  # add back on the '[' which is removed in strsplit
  variable_level[which(splits == 1)] <- paste0('[', variable_level[which(splits == 1)])

  variables <- unique(variable_name)

  n_variables <- length(variables)

  betas_grouped <- list()

  # put betas into list structure
  for (var in variables) {

    var_indices <- which(variable_name == var)

    betas_grouped[[var]] <- betas[var_indices][order(lower_bound[var_indices])]

  }

  #---------------------------------------------------------------------------#
  # Section 2. Return list of grouped betas
  #---------------------------------------------------------------------------#

  return(betas_grouped)

}







get_cuts_for_betas <- function(betas_list, minimum_improvement = 0.01) {

  split_points <- lapply(betas_list, function(x) {

    # recursive find splits
    all_split_decision <- choose_beta_split_r(betas = x, minimum_improvement = minimum_improvement)

    # remove no split decisions (0s)
    split_points <- all_split_decision[which(all_split_decision > 0)]

    return(split_points)

  })

  return(split_points)

}


choose_beta_split_r <- function(betas, minimum_improvement) {

  #cat('running choose_beta_split_r on...\n')
  #cat(betas, '\n')

  s <- choose_beta_split(betas = betas, minimum_improvement = minimum_improvement)

  cat('s' , s, '\n')

  if (!is.null(s)) {

    l <- choose_beta_split_r(betas = betas[1:s],
                             minimum_improvement = minimum_improvement)

    cat('l', l, '\n')

    u <- choose_beta_split_r(betas = betas[(s+1):length(betas)],
                             minimum_improvement = minimum_improvement)

    cat('u', u, '\n')

    if (is.null(l) & is.null(u)) {

      return(list(s))

    } else if (!is.null(l) & is.null(u)) {

      return(list(l, s))

    } else if (is.null(l) & !is.null(u)) {

      return(list(s, u))

    }

  } else {

    #return(list(s))

  }

}


choose_beta_split <- function(betas, minimum_improvement) {

  cat('running choose_beta_split on...\n')
  cat(betas, '\n')

  # if there are more than 2 points then find the best split point
  if (length(betas) > 2) {

    lines <- fit_lines_for_betas(betas = betas)

    # determine the fitted line(s) with the smallest residual
    smallest_residual <- which(lines$total_residuals == min(lines$total_residuals))[1]

    # if the first residual is the smallest then there is no split
    if (smallest_residual == 1) {

      #return(0)

    # otherwise if the improvement
    } else {

      if (lines$total_residuals[smallest_residual] < ((1 - minimum_improvement) * lines$total_residuals[1])) {

        return(smallest_residual - 1)

      } else {

        #return(0)

      }

    }

  # do not split if there are only two points
  } else {

    #return(0)

  }

}








fit_lines_for_betas <- function(betas) {

  # find 1 split for beta section

  n <- length(betas)

  betas_df <- data.frame(x = 1:n, betas = betas)

  single_line <- lm(betas ~ x, data = betas_df)

  line_1_list <- lapply(1:(n-1), function(i) lm(betas ~ x, data = betas_df[1:i, ]))

  # get predictions for the one point above the left hand line
  line_1_overlap_pred <- mapply(function(x, y) predict(x, newdata = betas_df[y, ]),
                               line_1_list,
                               2:n)

  # calculate total residuals for the left hand lines
  line_1_residuals <- sapply(line_1_list, function(x) sum(abs(x$residuals)))

  # calculate residuals for the one overlap points for the left hand lines
  lines_1_overlap_resids <- 1 / abs(line_1_overlap_pred - betas_df[(2:n), 'betas'])

  # fit lines on the right hand line
  line_2_list <- lapply(1:(n-1), function(i) lm(betas ~ x, data = betas_df[(i+1):n, ]))

  line_2_overlap_pred <- mapply(function(x, y) predict(x, newdata = betas_df[y, ]),
                                line_2_list,
                                1:(n-1))

  line_2_residuals <- sapply(line_2_list, function(x) sum(abs(x$residuals)))

  lines_2_overlap_resids <- 1 / abs(line_2_overlap_pred - betas_df[1:(n-1), 'betas'])

  total_residuals <- mapply(sum, line_1_residuals, line_2_residuals, lines_1_overlap_resids, lines_2_overlap_resids)

  return(list(single_line = single_line,
              line_1_list = line_1_list,
              line_1_overlap_pred = line_1_overlap_pred,
              lines_1_overlap_resids = lines_1_overlap_resids,
              line_2_list = line_2_list,
              line_2_overlap_pred = line_2_overlap_pred,
              lines_2_overlap_resids = lines_2_overlap_resids,
              total_residuals = c(sum(abs(single_line$residuals)), total_residuals)))

}





plot_lines_for_betas <- function(beta_cuts) {

  obsered_values <- beta_cuts$single_line$fitted.values + beta_cuts$single_line$residuals

  ylim_range <- c(min(obsered_values,
                      beta_cuts$line_1_overlap_pred,
                      beta_cuts$line_2_overlap_pred),
                  max(obsered_values,
                      beta_cuts$line_1_overlap_pred,
                      beta_cuts$line_2_overlap_pred))

  plot(obsered_values,
       main = paste('single line (abs residuals;', beta_cuts$total_residuals[1], ')'),
       xlab = 'coefficient',
       ylim = ylim_range)

  lines(beta_cuts$single_line$fitted.values, type = 'l')

  for (i in 1:length(beta_cuts$line_1_list)) {

    plot(obsered_values,
         main = paste('split lines', i, ' (abs residuals;', beta_cuts$total_residuals[i+1], ')'),
         xlab = 'coefficient',
         ylim = ylim_range)

    lines(beta_cuts$line_1_list[[i]]$fitted.values,
          type = 'l')

    lines(c(rep(NA, (i-1)),
            beta_cuts$line_1_list[[i]]$fitted.values[i],
            beta_cuts$line_1_overlap_pred[i]),
          type = 'b',
          pch = 4,
          col = 'red')

    lines(c(rep(NA, i), beta_cuts$line_2_list[[i]]$fitted.values),
          type = 'l')

    lines(c(rep(NA, (i-1)),
            beta_cuts$line_2_overlap_pred[i],
            beta_cuts$line_2_list[[i]]$fitted.values[1]),
          type = 'b',
          pch = 4,
          col = 'red')

    abline(v = i + 0.5)

  }

}





