
  model{
    # priors
    # allow the intercept alpha to vary across sites
      for(j in 1:n.sites){
        alpha[j] ~ dnorm(0,1E-6) 
      }
    # the slope beta is constant across sites
    beta ~ dnorm(0,1E-6)
    sigma ~ dunif(0,100)
    tau <- 1/sigma^2
  
    # likelihood
    for(i in 1:length(log.emission)) {
      log_mu[i] <- alpha[group[i]] + beta * log.n.input.centered[i]
      log.emission[i] ~ dnorm(log_mu[i], tau)
    }
  
    ## quantities of interest
      # predicted emissions
      ## from the JAGS primer: 
        # If you have two product symbols in the conditional distribution with different indices
          # ...and two subscripts in the quantity of interest i.e. quantity[i, j] 
          # ...then this dual product is specified in JAGS using nested for loops:
      for(i in 1:length(log.n.input.centered.pred)) {
        for(j in 1:n.sites){
          log_mu_site_pred[i, j] <- alpha[j] + beta * log.n.input.centered.pred[i]
        } # end j
      } # end i
  }
  
