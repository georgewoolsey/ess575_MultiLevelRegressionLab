
  model{
    ## priors
      # y priors
      sigma ~ dunif(0,100)
      tau_y <- 1/sigma^2
      # alpha priors
      kappa ~ dnorm(0,1E-6)
      eta ~ dnorm(0,1E-6)
      varsigma_alpha ~ dunif(0,100)
      tau_alpha <- 1/varsigma_alpha^2
      # beta priors
        mu_beta ~ dnorm(0,1E-6)
        varsigma_beta ~ dunif(0,100)
        tau_beta <- 1/varsigma_beta^2
        # include a group level effect of fertilizer type on the slope of the emission
          for(k in 1:n.ferts){
            beta[k] ~ dnorm(mu_beta, tau_beta)
          }
    
    ## likelihood
    # intercept (alpha) likelihood
      # represent the effect of soil carbon ...
        # ...on the intercept using the deterministic model below to predict alpha_j
      for(j in 1:n.sites){
        mu_alpha[j] <- kappa + eta * w[j]
        alpha[j] ~ dnorm(mu_alpha[j], tau_alpha)
      }
    # y likelihood
      for(i in 1:length(log.emission)) {
        log_mu[i] <- alpha[group[i]] + beta[fertilizer[i]] * log.n.input.centered[i]
        log.emission[i] ~ dnorm(log_mu[i], tau_y)
      }
  }
  
