bonsai$methods(

  automaticBurnin = function( verbose = TRUE ) {

    "Performs automatic burnin."

    if ( verbose ) {
      x <- has_posterior_log * length(posterior_log_directories) + has_prior_log * length(posterior_log_directories) +
        has_posterior_trees * length(posterior_trees_directories) + has_prior_trees * length(prior_trees_directories)
      y <- 0
      posterior_step <- has_posterior_log + has_posterior_trees
      prior_step <- has_prior_log + has_prior_trees
      cat("\nPerforming automatic burnin.\n")
      bar <- txtProgressBar(style=bar_style, width=50, char="=")
    }

    if ( has_posterior_log | has_posterior_trees ) {

      max <- pmax(length(posterior_log_directories),length(posterior_trees_directories))

      for ( run in 1:max ) {

        # Get the parameters for this run
        these_parameters <- list()
        if ( run <= length(posterior_log_directories) ) {
          for ( param in 1:length(parameters) ) {
            these_parameters[[length(these_parameters) + 1]] <- c(parameters[[param]]$posterior_burnin_samples[[run]],parameters[[param]]$posterior_samples[[run]])
          }
        }
        if ( run <= length(posterior_trees_directories) ) {
          for ( clade in 1:length(clades) ) {
            these_parameters[[length(these_parameters) + 1]] <- c(clades[[clade]]$posterior_burnin_samples[[run]],clades[[clade]]$posterior_samples[[run]])
          }
        }
        these_parameters <- coda::as.mcmc(do.call(cbind,these_parameters))

        # Now, find the time-slice with the best ESS
        num_gen                     <- nrow(these_parameters)
        slices                      <- as.integer(num_gen * seq(0.1,0.9,length.out=9))
        ess                         <- numeric(9)
        for ( i in 1:9 ) {
          ess_values                <- coda::effectiveSize(these_parameters[-c(1:slices[i]),])
          ess[i]                    <- min(ess_values[ess_values > 0])
        }
        new_burnin                  <- slices[which.max(ess)] / num_gen
        if ( run <= length(posterior_log_directories) ) {
          for ( param in parameters )    param$setBurnin(new_burnin,run)
        }
        if ( run <= length(posterior_trees_directories) ) {
          for ( clade in clades )    clade$setBurnin(new_burnin,run)
        }

        y <- y + posterior_step
        if ( verbose ) setTxtProgressBar(bar,y/x)

      }

      if ( has_joint_posterior_distributions ) {
        joint_posterior_distributions_clean <<- FALSE
      }

    }

    if ( has_prior_log | has_prior_trees ) {

      max <- pmax(length(prior_log_directories),length(prior_trees_directories))

      for ( run in 1:max ) {

        # Get the parameters for this run
        these_parameters <- list()
        if ( run <= length(prior_log_directories) ) {
          for ( param in 1:length(parameters) ) {
            these_parameters[[length(these_parameters) + 1]] <- c(parameters[[param]]$prior_burnin_samples[[run]],parameters[[param]]$prior_samples[[run]])
          }
        }
        if ( run <= length(prior_trees_directories) ) {
          for ( clade in 1:length(clades) ) {
            these_parameters[[length(these_parameters) + 1]] <- c(clades[[clade]]$prior_burnin_samples[[run]],clades[[clade]]$prior_samples[[run]])
          }
        }
        these_parameters <- coda::as.mcmc(do.call(cbind,these_parameters))

        # Now, find the time-slice with the best ESS
        num_gen                     <- nrow(these_parameters)
        slices                      <- as.integer(num_gen * seq(0.1,0.9,length.out=9))
        ess                         <- numeric(9)
        for ( i in 1:9 ) {
          ess_values                <- coda::effectiveSize(these_parameters[-c(1:slices[i]),])
          ess[i]                    <- min(ess_values[ess_values > 0])
        }
        new_burnin                  <- slices[which.max(ess)] / num_gen
        if ( run <= length(prior_log_directories) ) {
          for ( param in parameters )    param$setBurnin(new_burnin,run)
        }
        if ( run <= length(prior_trees_directories) ) {
          for ( clade in clades )    clade$setBurnin(new_burnin,run)
        }
        y <- y + prior_step
        if ( verbose ) setTxtProgressBar(bar,y/x)

      }

      if ( has_joint_prior_distributions ) {
        joint_prior_distributions_clean <<- FALSE
      }

    }

  }

)
