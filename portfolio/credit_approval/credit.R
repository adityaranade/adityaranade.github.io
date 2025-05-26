library(reshape2)
library(ggplot2)
library(dplyr)
library(ggh4x)
library(ggcorrplot)
library(GGally) # for pairs plot using ggplot framework
# library(car) # to calculate the VIF values
library(cmdstanr)
# library(bayesplot)
# library(rstanarm)
###############################################################
# Get starbucks data from github repo
path <- "https://raw.githubusercontent.com/adityaranade/portfolio/refs/heads/main/credit/data/crx.data"
data0 <- read.table(path, sep=",")
###############################################################
# Data processing
head(data0)
# V16 is the response +/-
# Convert the response to 0 and 1
data0$response <- ifelse(data0$V16 =="+",1,ifelse(data0$V16 =="-",0,NA))

# Check the first 6 rows of the dataset
data0 |> head()

# Check the type of data
data0 |> str()

# Convert the data into appropriate factors or numbers
data0$response <- data0$response
data0$V2 <- data0$V2 |> as.numeric()
data0$V3 <- data0$V3 |> as.numeric()
data0$V8 <- data0$V8 |> as.numeric()
data0$V11 <- data0$V11 |> as.numeric()
data0$V15 <- data0$V15 |> as.numeric()

# Combine only numerical data along with the response
data1 <- data0 |> dplyr::select(response,V2,V3,V8,V11,V15)

data1 |> str()

# Check the rows which do not have any entries
sum(is.na(data1)) # 12 NA values

# identify the rows which have NA values
na_ind <- which(is.na(data1$V2))

# Exclude the rows which has NA values
data <- data1[-na_ind,-c(2,3)]
data |> is.na() |> sum()
###############################################################
# EDA
# Data for histogram
melted_data <- melt(data, id="response")

# Plot the histogram of all the variables
ggplot(melted_data,aes(value))+
  geom_histogram(aes(),bins = 20)+
  # geom_histogram(aes(y = after_stat(density)),bins = 20)+
  facet_grid2(response~variable, scales="free")+theme_bw()
###############################################################
# split the data into training and testing data
seed <- 23
set.seed(seed)

ind <- sample(floor(0.75*nrow(data)),
              replace = FALSE)

# Training dataset
data_train <- data[ind,]
# Testing dataset
data_test <- data[-c(ind),]
###############################################################
library(MASS)
# Fit an ordinal logistic regression model
model <- glm(response ~ V8 + V11 + V15,
             family = binomial(link='logit'),data = data_train)

# Check the summary of the model
model |> summary()

# Predictions on the testing data
y_pred <- predict(model, data_test, type = "response")
y_predicted <- ifelse(y_pred > 0.5,1,0) |> as.factor()

# confusion matrix
conf_matrix <- caret::confusionMatrix(as.factor(data_test$response), y_predicted)
conf_matrix

misclassification_error <- mean(y_predicted != data_test$response)
print(paste('Accuracy',1-misclassification_error))

# ROC curve
library(ROCR)
pr <- prediction(y_pred, data_test$response)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

# AUC 
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc #0.7968867
###############################################################
# Read the STAN file
file_stan <- "logistic_regression.stan"

# Compile stan model
model_stan <- cmdstan_model(stan_file = file_stan,
                            cpp_options = list(stan_threads = TRUE))
model_stan$check_syntax()

x_train <- data_train[,-1]
y_train <- data_train[,1]
x_test <- data_test[,-1]
y_test <- data_test[,1]

x_train <- x_train |> as.matrix()
x_test <- x_test |> as.matrix()

standata <- list(N1 = nrow(x_train),
                 Y1 = y_train,
                 K = ncol(x_train),
                 X1 = x_train,
                 N2 = nrow(x_test),
                 X2 = x_test)

fit_optim <- model_stan$optimize(data = standata,
                                 seed = seed,
                                 threads =  10)

fsum_optim <- as.data.frame(fit_optim$summary())

# The optimized parameter would be 
par_ind <- 2:(ncol(x_train)+2)
opt_pars <- fsum_optim[par_ind,]
opt_pars

# starting value of parameters
start_parameters <- rep(list(list(alpha = opt_pars[1,2],
                                  beta = opt_pars[-1,2])),4)

# Run the MCMC with optimized values as the starting values
fit <- model_stan$sample(
  data = standata,
  init = start_parameters,
  seed = seed,
  iter_warmup = 10000,
  iter_sampling = 10000,
  chains = 4,
  parallel_chains = 4,
  refresh = 1000,
  threads =  8,
  save_warmup = FALSE)

# Summary
fit$summary()

# Save the summary
fsum <- as.data.frame(fit$summary())

# Plot posterior distribution of parameters
bayesplot::color_scheme_set("gray")
bayesplot::mcmc_dens(fit$draws(c("alpha","beta")))

# Trace plots
bayesplot::color_scheme_set("brewer-Spectral")
bayesplot::mcmc_trace(fit$draws(c("alpha","beta")))

# Check the predictions
pred_ind <- (max(par_ind)+1):(max(par_ind)+length(y_test))
# predicted probability
pred_prob <- fsum[pred_ind,2]
pred_outcome <- ifelse(pred_prob>0.5,1,0)

# Generate the confusion matrix
conf_matrix2 <- caret::confusionMatrix(as.factor(pred_outcome),as.factor(y_test))
conf_matrix2

misclassification_error2 <- mean(pred_outcome != y_test)
print(paste('Accuracy',1-misclassification_error2))

# ROC curve
library(ROCR)
pr2 <- prediction(pred_outcome, y_test)
prf2 <- performance(pr2, measure = "tpr", x.measure = "fpr")
plot(prf2)

# AUC 
auc2 <- performance(pr2, measure = "auc")
auc2 <- auc2@y.values[[1]]
auc2 #0.7968867
################################################################################