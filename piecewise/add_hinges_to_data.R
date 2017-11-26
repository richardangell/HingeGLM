
add_hinges_to_data <- function(data, var, split_points, split_values) {

  if (length(split_points) > 1) {

    for (i in 2:length(split_points)) {

      data[[paste0(var, "_hinge_", i - 1)]] <- pmax(0, (data[[var]] - split_values[i]))

    }

    return(data)

  } else {

    return(data)

  }

}
