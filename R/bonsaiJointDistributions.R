bonsai$methods(

  jointDistributions = function( verbose = TRUE ) {

    # This function is currently for numerical parameters ONLY.

    if ( verbose ) {
      n <- has_posterior_log * length(posterior_log_directories) + has_prior_log * length(posterior_log_directories) +
        (length(posterior_log_directories) > 1) + (length(prior_log_directories) > 1)
      c <- 0
      posterior_step <- has_posterior_log
      prior_step <- has_prior_log
      cat("\nPerforming correlation analysis.\n")
      bar <- txtProgressBar(style=bar_style, width=50, char="=")
    }

    # Posterior
    if ( has_joint_posterior_distributions ) {

      # For each run, make a matrix of correlation coefficients between parameters
      num_runs <- length(posterior_log_directories)
      num_params <- length(parameters)
      matrix_list <- vector("list",num_runs)

      for ( run in 1:num_runs ) {

        correlation_matrix <- matrix(NA, nrow = num_params, ncol = num_params )
        for ( i in 2:num_params ) {
          for ( j in 1:(i - 1) ) {
            x <- parameters[[i]]$posterior_samples[[run]]
            y <- parameters[[j]]$posterior_samples[[run]]
            correlation_test <- cor.test(x, y, method = "kendall")
            correlation_matrix[i,j] <- correlation_test$estimate
            correlation_matrix[j,i] <- correlation_test$p.value
          }
        }

        matrix_list[[run]] <- correlation_matrix

        c <- c + posterior_step
        if ( verbose ) setTxtProgressBar(bar,c/n)

      }

      if ( num_runs > 1 ) {

        correlation_matrix <- matrix(NA, nrow = num_params, ncol = num_params )
        for ( i in 2:num_params ) {
          for ( j in 1:(i - 1) ) {
            x <- parameters[[i]]$posterior_combined_samples
            y <- parameters[[j]]$posterior_combined_samples
            correlation_test <- cor.test(x, y, method = "kendall")
            correlation_matrix[i,j] <- correlation_test$estimate
            correlation_matrix[j,i] <- correlation_test$p.value
          }
        }

        matrix_list[[num_runs + 1]] <- correlation_matrix

        c <- c + posterior_step
        if ( verbose ) setTxtProgressBar(bar,c/n)

      }

      posterior_correlations <<- matrix_list

    }

    # Prior
    if ( has_joint_prior_distributions ) {

      num_runs <- length(prior_log_directories)
      num_params <- length(parameters)
      matrix_list <- vector("list",num_runs)

      for ( run in 1:num_runs ) {

        correlation_matrix <- matrix(NA, nrow = num_params, ncol = num_params )
        for ( i in 2:num_params ) {
          for ( j in 1:(i - 1) ) {
            x <- parameters[[i]]$prior_samples[[run]]
            y <- parameters[[j]]$prior_samples[[run]]
            correlation_test <- cor.test(x, y, method = "kendall")
            correlation_matrix[i,j] <- correlation_test$estimate
            correlation_matrix[j,i] <- correlation_test$p.value
          }
        }

        matrix_list[[run]] <- correlation_matrix

        c <- c + prior_step
        if ( verbose ) setTxtProgressBar(bar,c/n)

      }

      if ( num_runs > 1 ) {

        correlation_matrix <- matrix(NA, nrow = num_params, ncol = num_params )
        for ( i in 2:num_params ) {
          for ( j in 1:(i - 1) ) {
            x <- parameters[[i]]$prior_combined_samples
            y <- parameters[[j]]$prior_combined_samples
            correlation_test <- cor.test(x, y, method = "kendall")
            correlation_matrix[i,j] <- correlation_test$estimate
            correlation_matrix[j,i] <- correlation_test$p.value
          }
        }

        matrix_list[[num_runs + 1]] <- correlation_matrix

        c <- c + prior_step
        if ( verbose ) setTxtProgressBar(bar,c/n)

      }

    }

    prior_correlations <<- matrix_list

  },

  plotCorrelations = function(posterior = TRUE, ...) {

    recover()

    if ( posterior & has_joint_posterior_distributions ) {

      m <- posterior_correlations[[1]]
      m[upper.tri(m)] <- NA

      breaks <- pretty(c(-1,1), n = 12)
      colors <- rev(RColorBrewer::brewer.pal(length(breaks) - 1,"RdBu"))

      image(x=1:length(parameters),y=1:length(parameters),z=t(m[nrow(m):1,]),breaks=breaks,col=colors,xaxt="n",yaxt="n",bty="n",xlab=NA,ylab=NA,...)
      text(y = length(parameters):1, x = 1:length(parameters) - 0.55, labels = parameter_names, pos=4)

    }

    if ( !posterior & has_joint_prior_distributions ) {

      # UNDER CONSTRUCTION

    }

  },

  plotJointDistribution = function(parameter_1,parameter_2,posterior=TRUE,pch=19,...) {

    x <- parameters[parameter_1]
    y <- parameters[parameter_2]

    if ( posterior ) {

      x_num_runs <- x[[1]]$posterior_num_runs
      y_num_runs <- y[[1]]$posterior_num_runs
      num_runs <- pmin(x_num_runs,y_num_runs)

      # Compute the x-limit and y-limit
      xlim <- range(unlist(x[[1]]$posterior_samples))
      ylim <- range(unlist(y[[1]]$posterior_samples))

      plot(0,col=NA,ylim=ylim,xlim=xlim,xlab=parameter_1,ylab=parameter_2,...)
      for ( run in 1:num_runs ) {

        # Get the samples themselves
        x_samples <- x[[1]]$posterior_samples[[run]]
        y_samples <- y[[1]]$posterior_samples[[run]]

        # Get the ESS values for the samples
        x_ess <- x[[1]]$posterior_ess[run]
        y_ess <- y[[1]]$posterior_ess[run]
        min_ess <- pmin(x_ess,y_ess,200)

        # Thin the sample to the ESS values
        x_samples_thinned <- x_samples[as.integer(seq.int(1,length(x_samples),length.out=min_ess))]
        y_samples_thinned <- y_samples[as.integer(seq.int(1,length(y_samples),length.out=min_ess))]

        points(x = x_samples_thinned,
               y = y_samples_thinned,
               pch = pch,
               col = run,
               ...)

      }

    } else {

      x_num_runs <- x[[1]]$prior_num_runs
      y_num_runs <- y[[1]]$prior_num_runs
      num_runs <- pmin(x_num_runs,y_num_runs)

      # Compute the x-limit and y-limit
      xlim <- range(unlist(x[[1]]$prior_samples))
      ylim <- range(unlist(y[[1]]$prior_samples))

      plot(0,col=NA,ylim=ylim,xlim=xlim,xlab=parameter_1,ylab=parameter_2,...)
      for ( run in 1:num_runs ) {

        # Get the samples themselves
        x_samples <- x[[1]]$prior_samples[[run]]
        y_samples <- y[[1]]$prior_samples[[run]]

        # Get the ESS values for the samples
        x_ess <- x[[1]]$prior_ess[run]
        y_ess <- y[[1]]$prior_ess[run]
        min_ess <- pmin(x_ess,y_ess,200)

        # Thin the sample to the ESS values
        x_samples_thinned <- x_samples[as.integer(seq.int(1,length(x_samples),length.out=min_ess))]
        y_samples_thinned <- y_samples[as.integer(seq.int(1,length(y_samples),length.out=min_ess))]

        points(x = x_samples_thinned,
               y = y_samples_thinned,
               pch = pch,
               col = run,
               ...)

      }

    }

  }
)
