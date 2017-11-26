




max_d_from_digitisation <- function(df) {

  n <- nrow(df)

  s_2 <- (df[n, 'x'] - df[1, 'x'])**2 + (df[n, 'y'] - df[1, 'y'])**2

  s <- sqrt(s_2)

  m <- (df[n, 'x'] - df[1, 'x']) / (df[n, 'x'] - df[1, 'x'])

  phi <- atan(m)

  t_max <- (cos(phi) + sin(phi)) / s

  delta_phi_1 <- atan((1/s) * abs(sin(phi) + cos(phi)) * (1 - t_max + t_max**2))

  delta_phi_2 <- atan((1/s) * abs(sin(phi) - cos(phi)) * (1 - t_max + t_max**2))

  delta_phi_max <- max(delta_phi_1, delta_phi_2)

  return(list(delta_phi_1, delta_phi_2, delta_phi_max))

}




max_d_from_digitisation(coefs)
