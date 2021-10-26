data {
  int <lower=0> K;        // number of observed categories (equal to C + 1)
  int <lower=0> n_sub;    // number of subjects
  int <lower=0> n_item;   // number of items
  int <lower=1, upper=K> r[n_sub, n_item]; // array of responses
 }

parameters {
  vector[K] tau[n_item];   // free step parameters parameter
  real b[n_item];          // item difficulty parameter
  real<lower=0> a[n_item]; // item discrimination
  vector[n_sub] theta;     // latent trait
}

model {
  theta ~ normal(0, 1);
  b ~ normal(0, 2);  
  a ~ lognormal(0, 0.5);
  
  for (j in 1:n_item) {
    tau[j] ~ normal(0, 5);
    }

  for (i in 1:n_sub) {
    for (j in 1:n_item) {
      vector[K] cat_log_probs;
      vector[K] cat_probs;

      for (k in 1:K) {
        real p = a[j] * ( (k - 1) * (theta[i] - b[j]) + sum(tau[j, 1:k]) );
        real p_inv = a[j] * ( ( (2 * K - 1) - (k - 1) ) * (theta[i] - b[j]) + sum(tau[j, 1:k]) );
        cat_log_probs[k] = log_sum_exp(p, p_inv);
      }
      
      cat_probs = softmax(cat_log_probs);
      r[i, j] ~ categorical(cat_probs);
    }
  }
}
