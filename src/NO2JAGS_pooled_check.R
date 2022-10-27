
  model{
    
    # priors
    alpha ~ dnorm(0,1E-6)
    beta ~ dnorm(0,1E-6)
    sigma ~ dunif(0,100)
    tau <- 1/sigma^2
  
    # likelihood
    for (i in 1:length(log.emission)) {
      log_mu[i] <- alpha + beta * log.n.input.centered[i]
      # returns density (for continuous) because l.h.s. is data (deterministic b/c defined in data)
      log.emission[i] ~ dnorm(log_mu[i], tau)
      # posterior predictive distribution of y.new (for model checking)
        # returns random number generator because l.h.s. is not data (i.e. it is unknown: stochastic node)
      log.emission_sim[i] ~ dnorm(log_mu[i], tau)
      # sum of squares 
          sq[i] <- (log.emission[i]-log_mu[i])^2
          sq_sim[i] <- (log.emission_sim[i]-log_mu[i])^2
    }
  
    ## quantities of interest
      #posterior predictive checks
        # test statistics y
        mean_y <- mean(log.emission)
        min_y <- min(log.emission)
        sd_y <- sd(log.emission)
        fit_y <- sum(sq)
        # test statistics y_sim
        mean_y_sim <- mean(log.emission_sim)
        min_y_sim <- min(log.emission_sim)
        sd_y_sim <- sd(log.emission_sim)
        fit_y_sim <- sum(sq_sim)
        # p-values
        p_val_mean <- step(mean_y_sim - mean_y)
        p_val_min <- step(min_y_sim - min_y)
        p_val_sd <- step(sd_y_sim - sd_y)
        p_val_fit <- step(fit_y_sim - fit_y)
  }
  
