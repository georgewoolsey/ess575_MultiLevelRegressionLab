
  model{
      # priors
      alpha ~ dnorm(0,1E-6)
      beta ~ dnorm(0,1E-6)
      sigma ~ dunif(0,100)
      tau <- 1/sigma^2
    
      # likelihood
      for (i in 1:length(log.emission)) {
        log_mu[i] <- alpha + beta * log.n.input.centered[i]
        log.emission[i] ~ dnorm(log_mu[i], tau)
      }
    
      ## quantities of interest
        # predicted emissions
        for (j in 1:length(log.n.input.centered.pred)) {
          log_mu_pred[j] <- alpha + beta * log.n.input.centered.pred[j]
          mu_pred[j] <- exp(log_mu_pred[j])
        }
  }
  
