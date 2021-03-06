functions {
  vector gp_pred_rng(matrix x_test,
                     vector y1, matrix x_train,
                     vector theta, real gamma2, real sigma2, vector mu, vector bs) {
    int N1 = rows(y1);
    int N2 = rows(x_test[,1]);
    vector[N2] f2;
    matrix[N1, N1] L_K;
    vector[N1] mu_vec1;
    vector[N2] mu_vec2;
    vector[N1] L_K_div_y1;
    vector[N1] K_div_y1;
    matrix[N1,N2] k_x1_x2;
    vector[N2] f2_mu;
    matrix[N1,N2] v_pred;
    matrix[N2,N2] cov_f2;
    matrix[N1,N1] K;
    matrix[N2,N2] K2;
    
    {
      for (i in 1:N1) {
        K[i, i] = gamma2 + sigma2; //diagonal
        for (j in (i+1):N1) { //off-diagonals
          K[i, j] = gamma2*exp(-dot_product(theta,square(x_train[i,] - x_train[j,])));
          K[j, i] = K[i, j];
        }  
      }   
      L_K = cholesky_decompose(K);
      
      mu_vec1 = mu; #Needs to be changed with Black Shocles
      mu_vec2 = bs; #Needs to be changed with Black Shocles
      
      L_K_div_y1 = mdivide_left_tri_low(L_K, (y1 - mu_vec1));
      K_div_y1 = mdivide_right_tri_low(L_K_div_y1', L_K)';
  
      for (i in 1:N1) {
        for (j in 1:N2) { //off-diagonals
          k_x1_x2[i, j] = gamma2*exp(-dot_product(theta,square(x_train[i,] - x_test[j,])));
        }  
      }   
        
      f2_mu = mu_vec2 + (k_x1_x2' * K_div_y1);
      v_pred = mdivide_left_tri_low(L_K, k_x1_x2);
       
      for (i in 1:N2) {
        K2[i, i] = gamma2 + 1e-8; //diagonal
        for (j in (i+1):N2) { //off-diagonals
          K2[i, j] = gamma2*exp(-dot_product(theta,square(x_test[i,] - x_test[j,])));
          K2[j, i] = K2[i, j];
        }  
      }   
      cov_f2 = K2 - v_pred' * v_pred;
      f2 = multi_normal_rng(f2_mu, cov_f2);
    }
    return f2;
  }
}   

data {
  int<lower=1> N1; //training size
  int variables; //# of variables
  matrix [N1,variables] x_train; //training input
  vector[N1] y1; //training output
  int<lower=1> N2; //testing size
  matrix [N2,variables] x_test; //testing input
  vector<lower=0>[variables] theta;
  real<lower=0> sigma2;
  real<lower=0> gamma2;
  vector[N1] mu;
  vector[N2] bs;
}

parameters{}
model{}

generated quantities {
  vector[N2] y_predict = gp_pred_rng(x_test, y1, x_train, theta, gamma2, sigma2, mu, bs);
}
