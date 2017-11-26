




library(data.table)

train <- fread('/Users/richardangell/Projects/Kaggle/Archive/Otto Product Classification/Data/train.csv')

source('/Users/richardangell/Projects/modellingtools/R/data_levels_summary.R')

source('/Users/richardangell/Projects/modellingtools/R/create_hinges_from_betas.R')

check_levels <- data_levels_summary(data = train,
                                    max_levels = 50,
                                    csv_out = '/Users/richardangell/Projects/test.csv')


for (col in colnames(train)[-c(1, ncol(train))]) {

  train[[col]] <- cut(train[[col]], breaks = 10, include.lowest = TRUE)

}

for (i in 1:9) {

  train[[paste0('resp_', i)]] <- ifelse(train$target == paste0('Class_', i), 1, 0)

}



.libPaths('/Users/richardangell/Projects/h2o/R_Install')

library(h2o, lib = '/Users/richardangell/Projects/h2o/R_Install/3.10.3.4')

h2o.init()

train_h2o <- as.h2o(train)


h2o_glm <- h2o.glm(x = colnames(train)[grep('feat', colnames(train))],
                   y = 'resp_1',
                   training_frame = train_h2o,
                   nfolds = 0,
                   seed = 1,
                   family = 'binomial',
                   link = 'logit',
                   lambda_search = TRUE,
                   nlambdas = 100)

# group betas into based variable groups
h2o_glm_coefs <- group_betas(h2o_glm@model$coefficients)


# fit lines to betas for feat_23
feat_23_coefs <- fit_lines_for_betas(h2o_glm_coefs$feat_23)


# plot lines fitted
plot_lines_for_betas(feat_23_coefs)



choose_beta_split(h2o_glm_coefs$feat_23, 0.01)

choose_beta_split_r(h2o_glm_coefs$feat_23, 0.01)

choose_beta_split(h2o_glm_coefs$feat_23[3:6], 0.99)

get_cuts_for_betas(h2o_glm_coefs)

all_cuts <- get_cuts_for_betas(h2o_glm_coefs)




feat_2_coefs <- fit_lines_for_betas(h2o_glm_coefs$feat_2)

plot_lines_for_betas(feat_2_coefs)

choose_beta_split(h2o_glm_coefs$feat_2, 0.01)

choose_beta_split_r(h2o_glm_coefs$feat_2, 0.1)





feat_7_coefs <- fit_lines_for_betas(h2o_glm_coefs$feat_7)

plot_lines_for_betas(feat_7_coefs)

choose_beta_split(h2o_glm_coefs$feat_7, 0.01)

feat_7_tree <- choose_beta_split_r(h2o_glm_coefs$feat_7, 0.1)

str(feat_7_tree)


length(feat_7_tree)

sapply(feat_7_tree, length)

str(feat_7_tree, max.level = 2)


?str


