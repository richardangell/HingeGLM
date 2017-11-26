
#' Get summary of data value counts.
#'
#' For each requested variable return results from table() in a single
#' \code{data.frame}. If a given variable has more unique data values than
#' the cut off (\code{max_levels}), then the first, middle and last
#' \code{print_levels} will be returned instead. Results are returned in a
#' single \code{data.frame}.
#'
#' @param data \code{data.frame} containing columns to summarise.
#' @param cols name of columns in data to summarise, default value of
#'        \code{NULL} summarises all columns.
#' @param max_levels maximum number of data values to report per variable.
#' @param print_levels for variables with more levels than max_levels how many
#'        levels should be reported instead? Default value = 5 means than the
#'        first, middle and last 5 data values are output for these variables.
#' @param csv_out csv file to output results table to, default value of
#'        \code{NULL} means no table is output.
#'
#' @return \code{data.frame} containing variable summaries and optionally csv
#'         version too.
#'
#' @examples
#' set.seed(1)
#'
#' df <- data.frame(a = rnorm(1000),
#'                  b = runif(1000),
#'                  c = sample(5),
#'                  d = as.factor(sample(5)))
#'
#' data_levels_summary(data = df,
#'                     cols = NULL,
#'                     max_levels = 20,
#'                     print_levels = 5,
#'                     csv_out = NULL)
#' @export
data_levels_summary <- function(data, cols = NULL, max_levels = 100, print_levels = 5, csv_out = NULL) {

  #---------------------------------------------------------------------------#
  # Section 0. Input checking ----
  #---------------------------------------------------------------------------#

  accepted_classes <- c('data.frame')

  if (!any(class(data) %in% accepted_classes)) {

    stop('data is not in expected classes; ', accepted_classes)

  }

  if (!is.null(cols)) {

    if (any(!cols %in% colnames(data))) {

      stop('the following columns are not in data; ',
           cols[which(!cols %in% colnames(data))])

    }

  } else {

    cols <- colnames(data)

  }

  #---------------------------------------------------------------------------#
  # Section 1. Get value counts for each column ----
  #---------------------------------------------------------------------------#

  cols_table <- lapply(data, function(x) data.frame(table(x, useNA = 'ifany')))

  #---------------------------------------------------------------------------#
  # Section 2. Ammend each table to be of equal length ----
  #---------------------------------------------------------------------------#

  na_row <- data.frame(x = NA, Freq = NA)

  for (column in cols) {

    n_levels <- nrow(cols_table[[column]])

    if (n_levels > max_levels) {

      bottom_indices <- 1:print_levels

      middle_start_index <- ceiling(n_levels / 2) - ceiling(print_levels / 2)

      middle_inidices <- middle_start_index:(middle_start_index + print_levels - 1)

      top_indices <- (n_levels - print_levels + 1):n_levels

      extra_rows <- max_levels - ((3 * print_levels) + 2)

      cols_table[[column]] <- rbind(cols_table[[column]][bottom_indices, ],
                                    na_row,
                                    cols_table[[column]][middle_inidices, ],
                                    na_row,
                                    cols_table[[column]][top_indices, ],
                                    na_row[rep(1, extra_rows), ])

    } else {

      extra_rows <- max_levels - n_levels

      cols_table[[column]] <- rbind(cols_table[[column]],
                                    na_row[rep(1, extra_rows), ])

    }

  }

  single_table <- do.call(cbind, cols_table)

  rownames(single_table) <- NULL

  colnames(single_table) <- as.vector(sapply(cols, function(x) paste0(x, c('.level', '.freq'))))

  #---------------------------------------------------------------------------#
  # Section 3. Optionally output to csv ----
  #---------------------------------------------------------------------------#

  if (!is.null(csv_out)) {

    write.table(x = single_table,
                file = csv_out,
                sep = ',',
                row.names = FALSE)

  }

  #---------------------------------------------------------------------------#
  # Section 4. Return counts table from function ----
  #---------------------------------------------------------------------------#

  return(single_table)

}

