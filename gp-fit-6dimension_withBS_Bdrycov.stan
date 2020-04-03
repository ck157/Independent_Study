data {
  int<lower=1> N; //training data size
  int<lower=1> variables; //number of variables or dimension
  matrix[N,variables] x; //training inputs
  vector[N] y; //training outputs
  vector[N] blackscholes; //Black-Scholes Price
}

parameters {
  vector<lower=0>[variables] theta;
  real<lower=0> sigma2;
  real<lower=0> gamma2;
//   real mu;
}
// transformed parameters{
//   vector[N] muvec = rep_vector(mu, N);
// }
model {
  matrix[N, N] L_K;
  matrix[N, N] K;
  vector[N] muvec;
  muvec = blackscholes;
  //For Loop to go through matrix of x and compute black-scholes
  
  
  
  // defining our custom covariance matrix
  for (i in 1:N) {
    K[i, i] = gamma2 + sigma2 + 1e-7; //diagonal
    for (j in (i+1):N) { //off
      K[i,j] = 1.0;
      for (l in 1:variables){
        K[i, j] = K[i,j]*(2*fmin(x[i,l],x[j,l])/theta[l] + (exp(-theta[l]*x[i,l])+exp(-theta[l]*x[j,l])-exp(-theta[l]*fabs(x[i,l]-x[j,l]))-1)/pow(theta[l],2));
        // K[i, j] = gamma2*exp(-dot_product(theta,square(x[i,] - x[j,])));
        K[j, i] = K[i, j];
      }
    }
  }
  L_K = cholesky_decompose(K); //intermediate step for efficient inverse
  
  // priors
  for (v in 1:variables) {
    theta[v] ~ inv_gamma(5,5);
  }
  sigma2 ~ inv_gamma(1,1);
  gamma2 ~ inv_gamma(1,1);
  // mu ~ normal(0,1000);
  
  // sampling model
  y ~ multi_normal_cholesky(muvec, L_K);
  
}
