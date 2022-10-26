
  model{
    ## priors
      # y priors
      beta ~ dnorm(0,1E-6)
      sigma ~ dunif(0,100)
      tau_y <- 1/sigma^2
      # alpha priors
      kappa ~ dnorm(0,1E-6)
      eta ~ dnorm(0,1E-6)
      varsigma ~ dunif(0,100)
      tau_alpha <- 1/varsigma^2
    
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
        log_mu[i] <- alpha[group[i]] + beta * log.n.input.centered[i]
        log.emission[i] ~ dnorm(log_mu[i], tau_y)
      }
  
    ## quantities of interest
      # predicted emissions ACROSS SITES
        alpha_pred ~ dnorm(kappa, tau_alpha)
        for(i in 1:length(log.n.input.centered.pred)){
          log_mu_pred[i] <- alpha_pred + beta * log.n.input.centered.pred[i]
          mu_pred[i] <- exp(log_mu_pred[i])
        }
  }
  
