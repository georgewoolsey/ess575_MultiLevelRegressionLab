
  model{
    ## priors
      # coefficient of correlation for covariance matrix
      rho ~ dunif(-1,1)
      # y priors
      sigma ~ dunif(0,100)
      tau_y <- 1/sigma^2
      # alpha priors
        mu_alpha ~ dnorm(0,1E-6)
        varsigma_alpha ~ dunif(0,100)
      # beta priors
        mu_beta ~ dnorm(0,1E-6)
        varsigma_beta ~ dunif(0,100)
      #### ??????????????????????????????????????????????????????
      # model group effects on intercepts and slopes
      #### ??????????????????????????????????????????????????????
        # http://www.stat.yale.edu/~jtc5/238/materials/jags_4.3.0_manual_with_distributions.pdf
          # p. 53
          # y[1:N] ~ dmnorm(mu[1:N], Omega[1:N, 1:N])
          # ........
          # .....While the following construction should work in theory:
          #   y ~ dnorm(mu, Omega)
          #   Omega <- inverse(Sigma)
          #   in practice it may lead to runtime errors if Sigma cannot be inverted.
          # let's hope that Sigma can be inverted ;D !!!!!!!
        # put mu_alpha and mu_beta in matrix
          mu_matrix[1, 1] <- mu_alpha # row 1, column 1
          mu_matrix[1, 2] <- mu_beta # row 1, column 2
        # define Sigma matrix (variance-covariance matrix)
          # varsigma^2 
          Sigma_matrix[1, 1] <- varsigma_alpha^2 # row 1, column 1
          Sigma_matrix[2, 2] <- varsigma_beta^2 # row 2, column 2
          # Cov(alpha,beta) = rho * varsigma_alpha * varsigma_beta
          Sigma_matrix[2, 1] <- rho * varsigma_alpha * varsigma_beta # row 2, column 1
          Sigma_matrix[1, 2] <- rho * varsigma_alpha * varsigma_beta # row 1, column 2
          # invert Sigma matrix
          Omega_matrix <- inverse(Sigma_matrix)
        # model group effects on intercepts and slopes
          for (j in 1:n.sites) {
            B[j,1:2] ~ dmnorm(mu_matrix, Omega_matrix)
            alpha[j] <- B[j,1] # row j, column 1
            beta[j] <- B[j,2] # row j, column 2
          }
    ## likelihood
    # y likelihood
      for(i in 1:length(log.emission)) {
        log_mu[i] <- alpha[group[i]] + beta[group[i]] * log.n.input.centered[i]
        log.emission[i] ~ dnorm(log_mu[i], tau_y)
      }
  }
  
