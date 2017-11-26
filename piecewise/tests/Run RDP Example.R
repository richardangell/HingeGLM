
.libPaths('/Users/richardangell/Projects/h2o/R_Install')

library(h2o, lib = '/Users/richardangell/Projects/h2o/R_Install/3.10.3.4')

library(data.table)

source('/Users/richardangell/Projects/piecewise/R/select_splits_from_betas.R')

source('/Users/richardangell/Projects/piecewise/R/ramer_douglas_peucker_betas.R')

source('/Users/richardangell/Projects/piecewise/R/group_betas.R')

source('/Users/richardangell/Projects/piecewise/R/find_max_perp_dist.R')

train <- fread('/Users/richardangell/Projects/UCI ML Repository/Bike Sharing/Data/hour.csv')

train[ , season := as.factor(season)]
train[ , yr := as.factor(yr)]
train[ , mnth := as.factor(mnth)]
train[ , hr := as.factor(hr)]
train[ , holiday := as.factor(holiday)]
train[ , weekday := as.factor(weekday)]
train[ , workingday := as.factor(workingday)]
train[ , weathersit := as.factor(weathersit)]

for (col in c('temp', 'atemp', 'hum', 'windspeed')) {

  train[[paste0(col, '_factor')]] <- cut(train[[col]], breaks = 10, include.lowest = TRUE)

  train[[paste0(col, '_factor_2')]] <- cut(train[[col]],
                                           breaks = unique(quantile(train[[col]],
                                                                    probs = seq(0, 1, 0.1))),
                                           include.lowest = TRUE)

}

h2o.init()

train_h2o <- as.h2o(train)

explanatory_cols <- c('season', 'yr', 'mnth', 'hr', 'holiday', 'weekday', 'workingday', 'weathersit',
                      #'temp', 'atemp', 'hum', 'windspeed')
                      #'temp_factor', 'atemp_factor', 'hum_factor', 'windspeed_factor')
                      'temp_factor_2', 'atemp_factor_2', 'hum_factor_2', 'windspeed_factor_2')

bike_h2o_glm <- h2o.glm(x = explanatory_cols,
                        y = 'cnt',
                        training_frame = train_h2o,
                        nfolds = 5,
                        seed = 1,
                        family = 'gamma',
                        link = 'log',
                        alpha = 1,
                        lambda_search = TRUE,
                        nlambdas = 100)


coefficients_grouped <- group_betas(bike_h2o_glm@model$coefficients)



select_splits_from_betas(exp(coefficients_grouped$windspeed_factor), 0.05)

select_splits_from_betas(coefficients_grouped$windspeed_factor, 0.05)


for (col in explanatory_cols) {

  train[[paste0(col, "_rand")]] <- sample(train[[col]], size = nrow(train), replace = TRUE)

}

train_h2o <- as.h2o(train)

bike_h2o_glm_rand <- h2o.glm(x = paste0(explanatory_cols, "_rand"),
                             y = 'cnt',
                             training_frame = train_h2o,
                             nfolds = 5,
                             seed = 1,
                             family = 'gamma',
                             link = 'log',
                             alpha = 0,
                             lambda_search = FALSE)

coefficients_grouped_rand <- group_betas(bike_h2o_glm_rand@model$coefficients)

plot(coefficients_grouped_rand$temp_factor_2_rand)

find_max_perp_dist(data.frame(x = 1:length(coefficients_grouped_rand$temp_factor_2_rand),
                              y = coefficients_grouped_rand$temp_factor_2_rand))


select_splits_from_betas(coefficients_grouped$temp_factor_2, 0.07436987)



train[ , temp_hinge_3 := ifelse(temp < 0.3, 0, temp)]
train[ , temp_hinge_5 := ifelse(temp < 0.42, 0, temp)]
train[ , temp_hinge_8 := ifelse(temp < 0.62, 0, temp)]


train[ , atemp_hinge_5 := ifelse(atemp < 0.424, 0, temp)]
train[ , atemp_hinge_7 := ifelse(atemp < 0.53, 0, temp)]
train[ , atemp_hinge_8 := ifelse(atemp < 0.606, 0, temp)]



train[ , hum_hinge_5 := ifelse(hum < 0.562, 0, temp)]
train[ , hum_hinge_6 := ifelse(hum < 0.63, 0, temp)]
train[ , hum_hinge_8 := ifelse(hum < 0.75, 0, temp)]

train[ , windspeed_hinge_6 := ifelse(windspeed < 0.254, 0, temp)]


train_h2o <- as.h2o(train)

explanatory_cols <- c('season', 'yr', 'mnth', 'hr', 'holiday', 'weekday', 'workingday', 'weathersit',
                      'temp', 'temp_hinge_3', 'temp_hinge_5', 'temp_hinge_8',
                      'atemp', 'atemp_hinge_5', 'atemp_hinge_7', 'atemp_hinge_8',
                      'hum', 'hum_hinge_5', 'hum_hinge_6', 'hum_hinge_8',
                      'windspeed', 'windspeed_hinge_6')

bike_h2o_glm_hinge <- h2o.glm(x = explanatory_cols,
                              y = 'cnt',
                              training_frame = train_h2o,
                              nfolds = 5,
                              seed = 1,
                              family = 'gamma',
                              link = 'log',
                              alpha = 1,
                              lambda_search = TRUE,
                              nlambdas = 100)

bike_h2o_glm_hinge@model$coefficients


train[ , atemp_x_beta := atemp * 0.772696273]
train[ , atemp_hinge_5_x_beta := atemp_hinge_5 * -0.187375302]
train[ , atemp_hinge_7_x_beta := atemp_hinge_7 * -0.010651381]
train[ , atemp_hinge_8_x_beta := atemp_hinge_8 * -0.053370378]

train[ , atemp_curve := atemp_x_beta + atemp_hinge_5_x_beta + atemp_hinge_7_x_beta + atemp_hinge_8_x_beta]

train[ , hist(atemp)]

train[ , hist(atemp_x_beta)]
train[ , hist(atemp_hinge_5_x_beta)]
train[ , hist(atemp_hinge_7_x_beta)]
train[ , hist(atemp_hinge_8_x_beta)]
train[ , hist(atemp_curve)]

train[ , mean(cnt), by = atemp_factor_2]
train[ , mean(atemp_curve), by = atemp_factor_2]



