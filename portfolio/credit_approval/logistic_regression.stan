data {
int<lower = 0> N1; // training sample size
array[N1] int<lower=0, upper=1> Y1; // training response
int<lower = 0> K; // number of variables
matrix[N1,K] X1; // matrix of input variables
int<lower = 0> N2; // testing sample size
matrix[N2,K] X2; // testing input variables
}

parameters {
// The (unobserved) model parameters that we want to recover
real alpha;
vector[K] beta;
}

// transformed parameters {
// vector[N1] y_prob;
// y_prob = inv_logit(alpha + X1*beta);
// }

model {
// A logistic regression model
// Y1 ~ bernoulli(y_prob);
Y1 ~ bernoulli_logit(alpha + X1*beta);
// Prior models for the unobserved parameters
alpha ~ normal(0, 1);
beta ~ normal(1, 1);
}

generated quantities {
// Using the fitted model for probabilistic prediction.
// array[N2] int<lower=0, upper=1> Y2; // training response
vector [N2] Y2;
for (n2 in 1:N2){
Y2[n2] = inv_logit(alpha + X2[n2,]*beta);
}
}
