data {
  int <lower=0> K;        // number of observed categories (equal to C + 1)
  int <lower=0> n_sub;    // number of subjects
  int <lower=0> n_item;   // number of items
  int <lower=1, upper=K> r[n_sub, n_item]; // array of responses
 }

parameters {
  vector[K] tau[n_item];   // free step parameters parameter
  real b[n_item];          // item difficulty parameter
  real<lower=0, upper=4> a[n_item]; // item discrimination
  vector[n_sub] theta;     // latent trait
}

model {
  theta ~ normal(0, 1);
  b ~ normal(0, 2);  
  a ~ normal(0, 0.5);
  
  for (j in 1:n_item) {
    tau[j, 1] ~ normal(0, 0.001); // proxy for "fixing" tau?
    tau[j, 2] ~ lognormal(0.739925, 1); 
    tau[j, 3] ~ lognormal(0.995465, 1); 
    tau[j, 4] ~ lognormal(1.249535, 1); 
  }
  

  for (i in 1:n_sub) {
    for (j in 1:n_item) {
      vector[K] cat_log_probs;
      vector[K] cat_probs;

       vector[K] prob;
     vector[K] nominator;
     for (k in 1:K) {
     nominator[k]=exp(a[j]*((k-1)*(theta[i]-b[j])+sum(tau[j, 1:k]))) + 
                  exp(a[j]*((2*K-k)*(theta[i]-b[j])+sum(tau[j,1:k])));
                  
     }
      prob=nominator/sum(nominator);
      
       // cat_probs = softmax(prob);
//       r[i, j] ~ categorical(cat_probs);
       
       r[i, j] ~ categorical(prob);
      // r[i,j] ~ categorical_lpmf(prob);
    }
  }
}
