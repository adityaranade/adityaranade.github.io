library(reshape2)
library(ggplot2)
library(dplyr)
# library(ggh4x)
# library(ggcorrplot)
# library(GGally) # for pairs plot using ggplot framework
# library(car) # to calculate the VIF values
# library(cmdstanr)
# library(bayesplot)
# library(rstanarm)
###############################################################
# Get starbucks data from github repo
# path <- "https://raw.githubusercontent.com//adityaranade//portfolio//refs//heads//main//starbucks//starbucks-menu-nutrition-drinks.csv"
path <- "./data/obesity_data.csv"
data0 <- read.csv(path, header = TRUE)
###############################################################
# Data processing
head(data0)
# change column names
# colnames(data0) <- c("name", "calories", "fat", 
#                      "carbs", "fiber","protein", 
#                      "sodium")

# Check the first 6 rows of the dataset
data0 |> head()

# Check the type of data
data0 |> str()

# Check the rows which do not have any entries
sum(is.na(data0)) # no NA values

# data (no need to exclude anything
data <- data0 |> dplyr::select(NObeyesdad,Age,Height,Weight)

# convert the response to factor
data$NObeyesdad <- data$NObeyesdad |> as.factor()

# split the data into training and testing data
seed <- 23
set.seed(seed)

ind <- sample(floor(0.8*nrow(data)),
              replace = FALSE)

# Training dataset
data_train <- data[ind,]
# Testing dataset
data_test <- data[-ind,]

###############################################################
# # Data for histogram
# melted_data <- melt(data)
# 
# # Plot the histogram of all the variables
# ggplot(melted_data,aes(value))+
#   geom_histogram(aes(),bins = 20)+
#   # geom_histogram(aes(y = after_stat(density)),bins = 20)+
#   facet_wrap(~variable, scales="free")+theme_bw()
###############################################################
library(MASS)
# Fit an ordinal logistic regression model
model <- MASS::polr(NObeyesdad ~ Age + Height + Weight, data = data_train)

# Check the summary of the model
model |> summary()

# Prediction on the testing dataset
y_pred <- predict(model, data_test)

table(data_test$NObeyesdad,y_pred)


# confusion matrix
caret::confusionMatrix(data=y_pred, reference = data_test$NObeyesdad)
# ###############################################################
# # # correlation plot of all the variables
# # corr <- round(cor(data[,-1]), 1)
# # p.mat <- cor_pmat(mtcars) # correlation p-value
# # # Barring the no significant coefficient
# # ggcorrplot(corr, hc.order = TRUE,
# #            type = "lower", p.mat = p.mat)
# # # All positive correlation
# ###############################################################
# corr <- round(cor(data), 2)
# ggcorrplot(corr)
# ###############################################################
# # Pairs plot which plots the paired 
# # scatterplots along with histogram
# library(GGally)
# ggpairs(data,lower = list(continuous = "smooth"))
# ###############################################################
# # Principal component analysis
# pc <- prcomp(data[,-(ncol(data))],
#              center = TRUE,
#              scale. = TRUE)
# attributes(pc)
# ###############################################################
# # Check the factor loadings of the principal components
# print(pc)
# # Check the summary of the principal components
# summary(pc)
# # first 7 PC's explain approximately 90 % of variation 
# ###############################################################
# # Check if the multicollinearity issue has been resolved
# ggpairs(pc$x,columns = 1:7,
#         lower = list(continuous = "smooth"))
# # it has been resolved
# ###############################################################
# library(ggbiplot)
# g <- ggbiplot(pc,
#               obs.scale = 1,
#               var.scale = 1,
#               # groups = training$Species,
#               ellipse = TRUE,
#               circle = TRUE,
#               ellipse.prob = 0.68)
# g <- g + scale_color_discrete(name = '')
# g <- g + theme(legend.direction = 'horizontal',
#                legend.position = 'top')
# print(g)
# ###############################################################
# # split the data into training and testing data
# seed <- 23
# set.seed(seed)
# 
# ind <- sample(floor(0.8*nrow(data)),
#               replace = FALSE)
# 
# # Training dataset
# data_train <- data[ind,-1]
# # Testing dataset
# data_test <- data[-ind,-1]
# ###############################################################
# # Multiple linear regression using raw data
# model <- lm(quality ~ ., data = data_train)
# summary(model)
# 
# # Prediction on the testing dataset
# y_pred <- predict(model, data_test)
# 
# # Create a observed vs. predicted plot
# ggplot(NULL,aes(y_pred,data_test$quality))+geom_point()+
#   labs(y = "Observed", x="Predicted")+theme_minimal()+geom_abline()
# 
# 
# # Calculate RMSE
# rmse <- (y_pred-data_test$quality)^2 |> sum() |> sqrt()
# rmse # 11.76
# 
# # Check the variance inflation factor
# vif_values <- vif(model)
# vif_values
# 
# # Check the assumptions of the regression model
# # par(mfrow = c(2, 2))
# # plot(model)
# ###############################################################
# data_pc <- cbind(data[,ncol(data)],pc$x) |> data.frame()
# colnames(data_pc) <- c("quality",colnames(pc$x))
# 
# # training data
# data_pc_train <- data_pc[ind,]
# 
# # testing data
# data_pc_test <- data_pc[-ind,]
# 
# # Multiple linear regression using PC
# model_pc <- lm(quality ~ PC1 + PC2 + PC3 + PC5 + PC7 + PC8 + PC9,
#                data = data_pc_train)
# summary(model_pc)
# 
# # Prediction on the testing dataset
# y_pred_pc <- predict(model_pc, data_pc_test)
# 
# # Create a observed vs. predicted plot
# ggplot(NULL,aes(y_pred_pc,data_test$quality))+geom_point()+
#   labs(y = "Observed", x="Predicted")+theme_minimal()+geom_abline()
# 
# # Calculate RMSE
# rmse <- (y_pred_pc-data_pc_test$quality)^2 |> sum() |> sqrt()
# rmse # 11.81201
# 
# # Check the variance inflation factor
# vif_values_pc <- vif(model_pc)
# vif_values_pc
# ###############################################################
# # Now we can Gaussian process models
# ###############################################################
# # STAN model
# # Read the STAN file
# file_stan <- "GP_1d.stan"
# 
# # Compile stan model
# model_stan <- cmdstan_model(stan_file = file_stan,
#                             cpp_options = list(stan_threads = TRUE))
# model_stan$check_syntax()
# ###############################################################
# # Use raw data predictions
# x1 <- data[ind,-ncol(data)]
# y1 <- data[ind,ncol(data)]
# x2 <- data[-ind,-ncol(data)]
# y2 <- data[-ind,ncol(data)]
# 
# standata <- list(K = ncol(x1),
#                  N1 = nrow(x1),
#                  X1 = x1,
#                  Y1 = y1,
#                  N2 = nrow(x2),
#                  X2 = x2,
#                  Y2 = y2)
# 
# # Start with optimized values (Penalized likelihood)
# fit_optim <- model_stan$optimize(data = standata,
#                                  seed = seed,
#                                  threads =  10)
# 
# # fit_optim$output()
# 
# fsum_optim <- as.data.frame(fit_optim$summary())
# 
# # The optimized parameter would be 
# par_ind <- 2:4
# opt_pars <- fsum_optim[par_ind,];opt_pars
# 
# start_parameters <- rep(list(list(lambda = opt_pars[1,2],
#                                   sigma = opt_pars[2,2],
#                                   tau = opt_pars[3,2])),4)
# ###############################################################
# # Run the MCMC with optimized values as the starting values
# # Run MCMC
# fit <- model_stan$sample(
#   data = standata,
#   init = start_parameters,
#   seed = seed,
#   iter_warmup = 10,
#   iter_sampling = 10,
#   chains = 4,
#   parallel_chains = 4,
#   refresh = 1,
#   threads =  8)
# 
# #Summary
# fit$summary()
# 
# #Check the diagnostic summary to confirm convergence
# fit$diagnostic_summary()
# fsum <- as.data.frame(fit$summary())
# 
# # Save the model
# fit$save_object(file = paste0("fit_model.rds"))
# 
# # Plot posterior distribution of parameters
# # bayesplot::color_scheme_set("gray")
# bayesplot::mcmc_dens(fit$draws(c("lambda","sigma","tau")))
# 
# #Trace plots
# bayesplot::color_scheme_set("brewer-Spectral")
# bayesplot::mcmc_trace(fit$draws(c("lambda","sigma","tau")))
# 
# # # Replications to check if posterior data is correct
# # y_reps <- fit$draws("y_rep", format = "matrix")
# # pp_check <- pp_check(y1,y_reps,ppc_dens_overlay)
# # pp_check
# 
# # Prediction
# y_observed <- y2 #observed
# y_predicted <-  fsum[max(par_ind)+(1:length(y2)),c(2)] #predicted
# 
# ovp_1d <- ggplot(NULL,aes(y_predicted,y_observed))+geom_point()+
#   labs(y = "Observed", x="Predicted")+theme_minimal()+geom_abline()
# 
# ovp_1d
# 
# rmse_log = sqrt(mean((y_observed-y_predicted)^2))
# rmse_log
# ###############################################################
# 
# ###############################################################
# 
# ###############################################################
# # Use principal components for predictions
# x1 <- pc$x[ind,1] |> as.matrix()
# y1 <- data[ind,2]
# x2 <- pc$x[-ind,1] |> as.matrix()
# y2 <- data[-ind,2]
# 
# standata <- list(K = ncol(x1),
#                     N1 = nrow(x1),
#                     X1 = x1,
#                     Y1 = y1,
#                     N2 = nrow(x2),
#                     X2 = x2,
#                     Y2 = y2)
# ###############################################################
# x <- rnorm(100,0,3)
# y <- sin(x) + rnorm(100,0,0.1)
# qplot(x,y)
# 
# 
# 
# xx <- x |> as.matrix()
# 
# ind <- sample(floor(0.75*length(y)),
#               replace = FALSE)
# 
# standata <- list(K = ncol(xx),
#                  N1 = length(xx[ind,]),
#                  X1 = as.matrix(xx[ind,]),
#                  Y1 = y[ind],
#                  N2 = length(xx[-ind,]),
#                  X2 = as.matrix(xx[-ind,]),
#                  Y2 = y[-ind])
# ###############################################################
# # Start with optimized values (Penalized likelihood)
# fit_optim <- model_stan$optimize(data = standata,
#                                  seed = seed,
#                                  threads =  10)
# 
# # fit_optim$output()
# 
# fsum_optim <- as.data.frame(fit_optim$summary())
# 
# # The optimized parameter would be 
# par_ind <- 2:4
# opt_pars <- fsum_optim[par_ind,];opt_pars
# 
# start_parameters <- rep(list(list(lambda = opt_pars[1,2],
#                               sigma = opt_pars[2,2],
#                               tau = opt_pars[3,2])),4)
# ###############################################################
# # Run the MCMC with optimized values as the starting values
# # Run MCMC
# fit <- model_stan$sample(
#   data = standata,
#   init = start_parameters,
#   seed = seed,
#   iter_warmup = 10,
#   iter_sampling = 10,
#   chains = 4,
#   parallel_chains = 4,
#   refresh = 1,
#   threads =  8)
# 
# #Summary
# fit$summary()
# 
# #Check the diagnostic summary to confirm convergence
# fit$diagnostic_summary()
# fsum <- as.data.frame(fit$summary())
# 
# # Save the model
# fit$save_object(file = paste0("fit_model.rds"))
# 
# # Plot posterior distribution of parameters
# # bayesplot::color_scheme_set("gray")
# bayesplot::mcmc_dens(fit$draws(c("lambda","sigma","tau")))
# 
# #Trace plots
# bayesplot::color_scheme_set("brewer-Spectral")
# bayesplot::mcmc_trace(fit$draws(c("lambda","sigma","tau")))
# 
# # # Replications to check if posterior data is correct
# # y_reps <- fit$draws("y_rep", format = "matrix")
# # pp_check <- pp_check(y1,y_reps,ppc_dens_overlay)
# # pp_check
# 
# # Prediction
# y_observed <- y2 #observed
# y_predicted <-  fsum[max(par_ind)+(1:length(y2)),c(2)] #predicted
# 
# ovp_1d <- ggplot(NULL,aes(y_predicted,y_observed))+geom_point()+
#   labs(y = "Observed", x="Predicted")+theme_minimal()+geom_abline()
# 
# ovp_1d
# 
# rmse_log = sqrt(mean((y_observed-y_predicted)^2))
# rmse_log
# # raw data rmse = 147.9828
# # All 5 PC data rmse = 54.94
# # First 4 PC data rmse = 52.61
# # First 3 PC data rmse = 67.02
# # First 2 PC data rmse = 73.77
# # First PC data rmse = 55.36
# 
# # Check the assumptions of the regression model
# # par(mfrow = c(2, 2))
# # plot(model_pc)
# ###############################################################
# # To be added
# # Gaussian process regression