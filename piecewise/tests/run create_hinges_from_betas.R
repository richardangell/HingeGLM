




library(data.table)

train <- fread('/Users/richardangell/Projects/Kaggle/Archive/Otto Product Classification/Data/train.csv')


source('/Users/richardangell/Projects/piecewise/R/create_hinges_from_betas.R')

train$feat_23_h1 <- ifelse(train$feat_23 < 2, train$feat_23, 0)

train$feat_23_h2 <- ifelse(train$feat_23 < 2, 0, train$feat_23)

set.seed(1)

train$random_1 <- rnorm(nrow(train))


for (col in colnames(train)[-c(1, which(colnames(train) == 'target'))]) {

  train[[col]] <- cut(train[[col]], breaks = 10, include.lowest = TRUE)

}

for (i in 1:9) {

  train[[paste0('resp_', i)]] <- ifelse(train$target == paste0('Class_', i), 1, 0)

}



.libPaths('/Users/richardangell/Projects/h2o/R_Install')

library(h2o, lib = '/Users/richardangell/Projects/h2o/R_Install/3.10.3.4')

h2o.init()

train_h2o <- as.h2o(train)

cols <- colnames(train)[-c(1, which(colnames(train) == 'target'), grep('resp_', colnames(train)), grep('feat_23_h', colnames(train)))]



h2o_glm <- h2o.glm(x = cols,
                   y = 'resp_1',
                   training_frame = train_h2o,
                   nfolds = 0,
                   seed = 1,
                   alpha = 1,
                   family = 'binomial',
                   link = 'logit',
                   lambda_search = TRUE,
                   nlambdas = 100)


h2o_glm_r <- h2o.glm(x = 'random_1',
                     y = 'resp_1',
                     training_frame = train_h2o,
                     nfolds = 0,
                     seed = 1,
                     alpha = 0,
                     family = 'binomial',
                     link = 'logit',
                     lambda_search = FALSE,
                     nlambdas = -1)


preds <- h2o.predict(h2o_glm, train_h2o)

# group betas into based variable groups
h2o_glm_coefs <- group_betas(h2o_glm@model$coefficients)
h2o_glm_coefs <- group_betas(h2o_glm_r@model$coefficients)

# fit lines to betas for feat_23
feat_23_coefs <- fit_lines_for_betas(h2o_glm_coefs$feat_23)


# plot lines fitted
plot_lines_for_betas(feat_23_coefs)



choose_beta_split(h2o_glm_coefs$feat_23, 0.01)

choose_beta_split_r(h2o_glm_coefs$feat_23, 0.01)




coefs <- data.frame(x = 1:6, y = h2o_glm_coefs$feat_23)
coefs <- data.frame(x = 1:10, y = h2o_glm_coefs$random_1)
coefs <- data.frame(x = 1:8, y = h2o_glm_coefs$feat_81)
coefs <- data.frame(x = 1:10, y = h2o_glm_coefs$feat_10)

ramer_douglas_peucker_betas(coefs, 2)

ramer_douglas_peucker_betas(coefs,0.1901774)



find_max_perp_dist(coefs)

eta_range <- seq(from = 0, to = find_max_perp_dist(coefs)+0.00001, length.out = 100)

select_splits_from_betas(betas = h2o_glm_coefs$feat_81, eta = eta_range[75])

select_splits_from_betas(betas = h2o_glm_coefs$feat_81, eta = 0)

select_splits_from_betas(betas = h2o_glm_coefs$feat_81, eta = 6.367463)

x <- lapply(eta_range, function(x) select_splits_from_betas(betas = h2o_glm_coefs$feat_23, eta = x))

xx <- lapply(x, nrow)

plot(eta_range, unlist(xx))









a <- ramer_douglas_peucker_betas_LIST(coefs, 6.367463)

plot(coefs$x,
     coefs$y)

lines(a$split_1_8$line_points, type = 'l')



# calculate the angle of the line from the horizontal
bb <- atan((coefs[8, 'y'] - coefs[1, 'y']) / (coefs[8, 'x'] - coefs[1, 'x']))

# calculate the vertical offset of the line which is a perpendicular
# distance of eta away
vertical_offset <- 6.167463 / cos(bb)

lines(a$split_1_8$line_points + vertical_offset, type = 'l', col =2)





