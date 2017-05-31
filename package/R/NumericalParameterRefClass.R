# TODO:

#' Numerical parameter reference class.
#'
#' @field name                        The parameter's name.
#'
#' @field has_posterior               Logical indicating whether there are samples from the posterior for the parameter.
#' @field posterior_num_runs          The number of runs under the posterior (if any).
#' @field posterior_num_generations   The number of samples taken from the posterior (after burnin) for each run.
#' @field posterior_num_burnin        The number of samples taken from the posterior (before burnin) for each run.
#' @field posterior_samples           List of samples from the posterior (after burnin). Each list element is a set of samples from a particular MCMC run.
#' @field posterior_burnin_samples    List of samples from the posterior (before burnin). Each list element is a set of samples from a particular MCMC run.
#' @field posterior_mean              The estimated mean of the parameter's marginal posterior distribution for each run.
#' @field posterior_credible_interval The estimated credible interval of the parameter's marginal posterior distribution for each run.
#' @field posterior_ess               The effective number of samples taken from the parameter's marginal posterior distribution for each run.
#' @field posterior_geweke            The test-statistic for Geweke's diagnostic for the parameter's posterior samples for each run.
#'
#' @field has_prior                   Logical indicating whether there are samples from the prior for the parameter.
#' @field prior_num_runs              The number of runs under the prior (if any).
#' @field prior_num_generations       The number of samples taken from the prior (after burnin) for each run.
#' @field prior_num_burnin            The number of samples taken from the prior (before burnin) for each run.
#' @field prior_samples               List of samples from the prior (after burnin). Each list element is a set of samples from a particular MCMC run.
#' @field prior_burnin_samples        List of samples from the prior (before burnin). Each list element is a set of samples from a particular MCMC run.
#' @field prior_mean                  The estimated mean of the parameter's marginal prior distribution for each run.
#' @field prior_credible_interval     The estimated credible interval of the parameter's marginal prior distribution for each run.
#' @field prior_ess                   The effective number of samples taken from the parameter's marginal prior distribution for each run.
#' @field prior_geweke                The test-statistic for Geweke's diagnostic for the parameter's prior samples for each run.


NumericalParameter <- setRefClass(
  Class = "NumericalParameter",

  field = c(

    name = "character",

    # Diagnostics to use
    ess = "logical",
    geweke = "logical",
    ks = "logical",

    # Posterior properties
    has_posterior = "logical",
    posterior_clean = "logical",
    has_posterior_combined = "logical",
    posterior_combined_clean = "logical",
    posterior_num_runs = "integer",
    posterior_num_generations = "integer",
    posterior_burnin = "integer",

    # Posterior samples
    posterior_samples = "list",
    posterior_burnin_samples = "list",
    posterior_combined_samples = "numeric",

    # Posterior summaries
    posterior_mean = "numeric",
    posterior_credible_interval = "list",

    posterior_combined_mean = "numeric",
    posterior_combined_credible_interval = "numeric",

    # Posterior diagnostics
    posterior_ess = "numeric",
    posterior_combined_ess = "numeric",
    posterior_ess_flag = "numeric",

    posterior_geweke = "numeric",
    posterior_geweke_flag = "numeric",

    posterior_ks = "numeric",
    posterior_combined_ks = "numeric",
    posterior_ks_flag = "numeric",

    # Prior properties
    has_prior = "logical",
    prior_clean = "logical",
    has_prior_combined = "logical",
    prior_combined_clean = "logical",
    prior_num_runs = "integer",
    prior_num_generations = "integer",
    prior_burnin = "integer",

    # Prior samples
    prior_samples = "list",
    prior_burnin_samples = "list",
    prior_combined_samples = "numeric",

    # Prior summaries
    prior_mean = "numeric",
    prior_credible_interval = "list",

    prior_combined_mean = "numeric",
    prior_combined_credible_interval = "numeric",

    # Prior diagnostics
    prior_ess = "numeric",
    prior_combined_ess = "numeric",
    prior_ess_flag = "numeric",

    prior_geweke = "numeric",
    prior_geweke_flag = "numeric",

    prior_ks = "numeric",
    prior_combined_ks = "numeric",
    prior_ks_flag = "numeric",

    # Analytical prior distribution
    has_analytical_prior = "logical",
    analytical_pdf = "function",
    analytical_cdf = "function"

  ), # end fields

  methods = list(

    ###############
    # Constructor #
    ###############

    initialize = function(name,
                          posterior_samples = NULL,
                          prior_samples = NULL,
                          burnin = 0.25,
                          ...) {

      "Create an object of class NumericalParameter."

      # Name of the parameter.

      name <<- name

      # We don't know yet whether we've been handed posterior
      # and prior samples. Also, we assume we don't have
      # an analytical prior distribution (if so, this would be
      # set manually later).

      posterior_num_runs     <<- 0L
      prior_num_runs         <<- 0L
      has_posterior          <<- FALSE
      has_posterior_combined <<- FALSE
      has_prior              <<- FALSE
      has_prior_combined     <<- FALSE
      has_analytical_prior   <<- FALSE

      # Right now, diagnostics are ESS and geweke, and are always on.
      # In the future, this will allow to turn on and off certain
      # diagnostics, and will incorporate more of them.

      ess    <<- TRUE
      geweke <<- TRUE
      ks     <<- TRUE

      # Add the prior samples (if they exist)
      for( prior in prior_samples ) addSamples(prior, burnin, FALSE)

      # Add the posterior samples (if they exist)
      for( post in posterior_samples ) addSamples(post, burnin)

    },

    #######################
    # Getters and setters #
    #######################

    addSamples = function(samples, burnin, posterior = TRUE) {

      if ( posterior ) {

        has_posterior      <<- TRUE
        posterior_num_runs <<- posterior_num_runs + 1L
        num_samples        <- length(samples)

        posterior_burnin[posterior_num_runs]           <<- as.integer(num_samples * burnin)
        posterior_num_generations[posterior_num_runs]  <<- as.integer(num_samples - posterior_burnin[posterior_num_runs])
        posterior_burnin_samples[[posterior_num_runs]] <<- samples[1:posterior_burnin[posterior_num_runs]]
        posterior_samples[[posterior_num_runs]]        <<- samples[-c(1:posterior_burnin[posterior_num_runs])]

        posterior_clean[posterior_num_runs]            <<- FALSE
        if ( posterior_num_runs > 1 ) {
          has_posterior_combined                       <<- TRUE
          posterior_combined_clean                     <<- FALSE
        } else {
          posterior_combined_clean                     <<- TRUE
        }

      } else {

        has_prior      <<- TRUE
        prior_num_runs <<- prior_num_runs + 1L
        num_samples    <- length(samples)

        prior_burnin[prior_num_runs]           <<- as.integer(num_samples * burnin)
        prior_num_generations[prior_num_runs]  <<- as.integer(num_samples - prior_burnin[prior_num_runs])
        prior_burnin_samples[[prior_num_runs]] <<- samples[1:prior_burnin[prior_num_runs]]
        prior_samples[[prior_num_runs]]        <<- samples[-c(1:prior_burnin[prior_num_runs])]

        prior_clean[prior_num_runs]            <<- FALSE
        if ( prior_num_runs > 1 ) {
          has_prior_combined                   <<- TRUE
          prior_combined_clean                 <<- FALSE
        } else {
          prior_combined_clean                 <<- TRUE
        }

      }

      cleanup()

    }, # end initialize

    setBurnin = function(burnin, run, posterior = TRUE) {

      if ( posterior ) {

        if ( run > posterior_num_runs ) {
          stop("Selected run is greater than the number of posterior runs.")
        }

        num_samples <- posterior_num_generations[run] + posterior_burnin[run]
        samples <- c(posterior_burnin_samples[[run]],posterior_samples[[run]])

        posterior_burnin[run]           <<- as.integer(num_samples * burnin)
        posterior_num_generations[run]  <<- as.integer(num_samples - posterior_burnin[run])
        posterior_burnin_samples[[run]] <<- samples[1:posterior_burnin[run]]
        posterior_samples[[run]]        <<- samples[-c(1:posterior_burnin[run])]

        posterior_clean[run]            <<- FALSE
        posterior_combined_clean        <<- FALSE

      } else {

        if ( run > prior_num_runs ) {
          stop("Selected run is greater than the number of prior runs.")
        }

        num_samples <- prior_num_generations[run] + prior_burnin[run]
        samples <- c(prior_burnin_samples[[run]],prior_samples[[run]])

        prior_burnin[run]           <<- as.integer(num_samples * burnin)
        prior_num_generations[run]  <<- as.integer(num_samples - prior_burnin[run])
        prior_burnin_samples[[run]] <<- samples[1:prior_burnin[run]]
        prior_samples[[run]]        <<- samples[-c(1:prior_burnin[run])]

        prior_clean[run]            <<- FALSE
        prior_combined_clean        <<- FALSE

      }

      cleanup()

    },

    setAnalyticalPrior = function( analytical_prior, params ) {

      if ( is.null(names(params)) ) {

        # user didn't provide named parameters
        stop("params must be a named vector of parameters of the analytical distribution.")

      } else {

        analytical_pdf <<- get(paste("d",analytical_prior,sep=""))
        analytical_cdf <<- get(paste("p",analytical_prior,sep=""))

        # user provided named parameters
        names <- names(formals(analytical_pdf))

        for ( param in 1:length(params) ) {
          if ( names(params[param]) %in% names ) {
            formals(analytical_pdf)[[names(params[param])]] <<- as.numeric(params[param])
            formals(analytical_cdf)[[names(params[param])]] <<- as.numeric(params[param])
          }
        }

        has_analytical_prior <<- TRUE
        cleanup()

      }

    },

    #####################
    # Parameter cleanup #
    #####################

    cleanup = function() {

      "Ensures that all the parameter summaries and diagnostics are up-to-date."

      # Posterior samples
      if ( has_posterior ) {

        if ( has_posterior_combined & !posterior_combined_clean ) {

          # Mark the run as clean
          posterior_combined_clean <<- TRUE

          # Combine the posterior samples
          posterior_combined_samples <<- unlist(posterior_samples)

          # Parameter summaries
          posterior_combined_mean <<- mean(posterior_combined_samples)
          posterior_combined_credible_interval <<- quantile(posterior_combined_samples,prob=c(0.025,0.975))

          # Effective sample size
          if ( ess ) {
            posterior_combined_ess <<- coda::effectiveSize(posterior_combined_samples)
          }

          # Compare posterior to the (combined) prior
          if ( ks & has_prior ) {
            if ( has_prior_combined ) {
              posterior_combined_ks <<- 1 - suppressWarnings(ks.test(x=posterior_combined_samples,y=prior_combined_samples))$p.value
            } else {
              posterior_combined_ks <<- 1 - suppressWarnings(ks.test(x=posterior_combined_samples,y=prior_samples[[1]]))$p.value
            }
          }

        }

        for ( run in 1:posterior_num_runs ) {

          if ( !posterior_clean[run] ) {

            # Mark the run as clean
            posterior_clean[run] <<- TRUE

            # Parameter summaries
            posterior_mean[run] <<- mean(posterior_samples[[run]])
            posterior_credible_interval[[run]] <<- quantile(posterior_samples[[run]],prob=c(0.025,0.975))

            # Effective sample size
            if ( ess ) {
              posterior_ess[run] <<- as.numeric(coda::effectiveSize(posterior_samples[[run]]))
            }

            # Geweke's diagnostic
            if ( geweke ) {
              posterior_geweke[run] <<- 2 * (1 - pnorm(abs(as.numeric(coda::geweke.diag(posterior_samples[[run]])$z))))
            }

            # Compare posterior to the (combined) prior
            if ( ks & has_prior ) {
              if ( has_prior_combined ) {
                posterior_ks[run] <<- 1 - suppressWarnings(ks.test(x=posterior_samples[[run]],y=prior_combined_samples))$p.value
              } else {
                posterior_ks[run] <<- 1 - suppressWarnings(ks.test(x=posterior_samples[[run]],y=prior_samples[[1]]))$p.value
              }
            }

          }

        }

      }

      # Prior samples
      if ( has_prior ) {

        if ( has_prior_combined & !prior_combined_clean ) {

          # Mark the run as clean
          prior_combined_clean <<- TRUE

          # Combine the prior samples
          prior_combined_samples <<- unlist(prior_samples)

          # Parameter summaries
          prior_combined_mean <<- mean(prior_combined_samples)
          prior_combined_credible_interval <<- quantile(prior_combined_samples,prob=c(0.025,0.975))

          # Effective sample size
          if ( ess ) {
            prior_combined_ess <<- coda::effectiveSize(prior_combined_samples)
          }

          # Compare prior to the (combined) prior
          if ( ks ) {
            if ( has_prior_combined ) {
              prior_combined_ks <<- 1 - suppressWarnings(ks.test(x=prior_combined_samples,y=prior_combined_samples))$p.value
            } else {
              prior_combined_ks <<- 1 - suppressWarnings(ks.test(x=prior_combined_samples,y=prior_samples[[1]]))$p.value
            }
          }

        }

        for ( run in 1:prior_num_runs ) {

          if ( !prior_clean[run] ) {

            # Mark the run as clean
            prior_clean[run] <<- TRUE

            # Parameter summaries
            prior_mean[run] <<- mean(prior_samples[[run]])
            prior_credible_interval[[run]] <<- quantile(prior_samples[[run]],prob=c(0.025,0.975))

            # Effective sample size
            if ( ess ) {
              prior_ess[run] <<- as.numeric(coda::effectiveSize(prior_samples[[run]]))
            }

            # Geweke's diagnostic
            if ( geweke ) {
              prior_geweke[run] <<- 2 * (1 - pnorm(abs(as.numeric(coda::geweke.diag(prior_samples[[run]])$z))))
              # prior_geweke[run] <<- pnorm(as.numeric(coda::geweke.diag(prior_samples[[run]])$z))
            }

            # Compare prior to the (combined) prior
            if ( ks ) {
              if ( has_prior_combined ) {
                prior_ks[run] <<- 1 - suppressWarnings(ks.test(x=prior_samples[[run]],y=prior_combined_samples))$p.value
              } else {
                prior_ks[run] <<- 1 - suppressWarnings(ks.test(x=prior_samples[[run]],y=prior_samples[[1]]))$p.value
              }
            }

          }
        }
      }
    },

    ######################
    # Plotting functions #
    ######################

    show = function() {

      lines <- paste("Parameter name: ", name)

      lines <- c(lines, paste("\n  Number of posterior runs: ", posterior_num_runs, sep = ""))
      if ( posterior_num_runs > 0 ) {
        lines <- c(lines, paste("  Number of posterior samples: ", paste(posterior_num_generations, collapse = ", "), sep = ""))
        lines <- c(lines,paste("  Posterior means: ", paste( round(posterior_mean,3), collapse = ", "),", (",round(posterior_combined_mean,3),")", sep = ""))
      }

      lines <- c(lines, paste("\n  Number of prior runs: ", prior_num_runs, sep = ""))
      if ( prior_num_runs > 0 ) {
        lines <- c(lines, paste("  Number of prior samples: ", paste(prior_num_generations, collapse = ", "), sep = ""))
        lines <- c(lines,paste("  Prior means: ", paste( round(prior_mean,3), collapse = ", "),", (",round(prior_combined_mean,3),")", sep = ""))
      }

      lines <- c(lines,"\n")

      writeLines(lines)

    },

    summaryTable = function( posterior = TRUE ) {

      if ( posterior & has_posterior ) {

        # Make the matrix
        m <- data.frame(row.names = paste("run",1 : posterior_num_runs))
        # m$run <- 1 : posterior_num_runs

        # Fill in the summary statistics
        m$mean <- posterior_mean
        m['lower CI'] <- do.call(rbind,posterior_credible_interval)[,1]
        m['upper CI'] <- do.call(rbind,posterior_credible_interval)[,2]

        # Add optional additional columns
        if ( ess & length(posterior_ess) > 0 ) m['ESS'] <- posterior_ess
        if ( geweke & length(posterior_geweke) > 0 ) m['Geweke\'s p'] <- posterior_geweke
        if ( ks & has_prior & length(posterior_ks) > 0 ) m['KS'] <- posterior_ks

        if ( has_posterior_combined ) {

          combined_row <- data.frame("mean" = posterior_combined_mean,
                                     "lower CI" = posterior_combined_credible_interval[1],
                                     "upper CI" = posterior_combined_credible_interval[2],
                                     row.names = "combined", check.names = FALSE)

          if ( ess ) combined_row["ESS"] <- posterior_combined_ess
          if ( geweke ) combined_row['Geweke\'s p'] <- NA
          if ( ks & has_prior ) combined_row["KS"] <- posterior_combined_ks

          m <- rbind(m,combined_row)

        }

        return( m )

      }

      if ( !posterior & has_prior ) {

        # Make the matrix
        m <- data.frame(row.names = paste("run",1 : prior_num_runs))
        # m$run <- 1 : prior_num_runs

        # Fill in the summary statistics
        m$mean <- prior_mean
        m['lower CI'] <- do.call(rbind,prior_credible_interval)[,1]
        m['upper CI'] <- do.call(rbind,prior_credible_interval)[,2]

        # Add optional additional columns
        if ( ess & length(prior_ess) > 0 ) m['ESS'] <- prior_ess
        if ( geweke & length(prior_geweke) > 0 ) m['Geweke\'s p'] <- prior_geweke
        if ( ks & length(prior_ks) > 0 ) m['KS'] <- prior_ks

        if ( has_prior_combined ) {

          combined_row <- data.frame("mean" = prior_combined_mean,
                                     "lower CI" = prior_combined_credible_interval[1],
                                     "upper CI" = prior_combined_credible_interval[2],
                                     row.names = "combined", check.names = FALSE)

          if ( ess ) combined_row["ESS"] <- prior_combined_ess
          if ( geweke ) combined_row['Geweke\'s p'] <- NA
          if ( ks ) combined_row["KS"] <- prior_combined_ks

          m <- rbind(m,combined_row)

        }

        return( m )

      }

    },

    densities = function( col, posterior = TRUE, num_bins = 50, ... ) {

      "Marginal posterior densities for the parameter."

      if ( posterior & has_posterior ) {

        if ( missing(col) ) {
          col <- 1:posterior_num_runs
        }

        # Compute the x-limit
        xlim <- quantile(unlist(posterior_samples),prob=c(0.001,0.999))

        sample_densities <- vector("list",posterior_num_runs)
        combined_samples <- numeric()
        for ( i in 1:posterior_num_runs ) {
          sample_densities[[i]] <- density(posterior_samples[[i]],from=xlim[1],to=xlim[2])
        }
        if ( has_posterior_combined ){
          sample_densities[[posterior_num_runs + 1]] <- density(posterior_combined_samples,from=xlim[1],to=xlim[2])
        }

        ylim <- c(0,max(sapply(sample_densities,function(d) d$y)))

        plot(0,col=NA,ylim=ylim,xlim=xlim,xlab="value",ylab="posterior probability",main="marginal posterior",...)
        for ( i in 1:posterior_num_runs ) {
          lines(sample_densities[[i]],col=col[i],...)
        }
        if ( has_posterior_combined ){
          lines(sample_densities[[posterior_num_runs + 1]],col="black",lty=2,lwd=2,...)
        }
        if ( has_prior ) {
          if ( has_prior_combined ) {
            d <- density(prior_combined_samples,from=xlim[1],to=xlim[2])
          } else {
            d <- density(prior_samples[[1]],from=xlim[1],to=xlim[2])
          }
          lines(d,col="grey50",lty=2,lwd=2,...)
        }

      } else if ( has_prior ) {

        if ( missing(col) ) {
          col <- 1:prior_num_runs
        }

        # Compute the x-limit
        xlim <- quantile(unlist(prior_samples),prob=c(0.001,0.999))

        sample_densities <- vector("list",prior_num_runs)
        combined_samples <- numeric()
        for ( i in 1:prior_num_runs ) {
          sample_densities[[i]] <- density(prior_samples[[i]],from=xlim[1],to=xlim[2])
        }
        if ( has_prior_combined ){
          sample_densities[[prior_num_runs + 1]] <- density(prior_combined_samples,from=xlim[1],to=xlim[2])
        }

        ylim <- c(0,max(sapply(sample_densities,function(d) d$y)))

        plot(0,col=NA,ylim=ylim,xlim=xlim,xlab="value",ylab="prior probability",main="marginal posterior",...)
        for ( i in 1:prior_num_runs ) {
          lines(sample_densities[[i]],col=col[i],...)
        }
        if ( has_prior_combined ){
          lines(sample_densities[[prior_num_runs + 1]],col="black",lty=2,lwd=2,...)
        }
        if ( ks & has_analytical_prior ) {
          curve(analytical_pdf(x=x),from=xlim[1],to=xlim[2],n=1001,col="grey50",lty=2,add=TRUE,...)
        }

      }

    },

    boxplots = function( col, posterior = TRUE, ...) {

      "Boxplots of the marginal posterior distributions for the parameter."

      if ( posterior & has_posterior ) {

        if ( missing(col) ) {
          col <- 1:posterior_num_runs
        }

        samples <- vector("list",posterior_num_runs)
        for ( i in 1:posterior_num_runs ) {
          samples[[i]] <- posterior_samples[[i]]
        }

        labels <- 1:posterior_num_runs

        # Combine the samples
        if ( has_posterior_combined ) {
          samples[[posterior_num_runs + 1]] <- posterior_combined_samples
          labels <- c(labels,"combined")
          col <- c(col,"black")
        }

        # Compute the y-limit
        ylim <- range(unlist(samples))

        # Get the prior samples
        if ( has_prior ) {
          if ( has_prior_combined ) {
            samples[[length(samples) + 1]] <- prior_combined_samples
          } else {
            samples[[length(samples) + 1]] <- prior_samples[[1]]
          }
          labels <- c(labels,"prior")
          col <- c(col,"grey50")
        }

        boxplot(samples,names=labels,ylim=ylim,xlab="run",ylab="value",col=col,border=col,main="boxplots",...)

      } else if ( !posterior & has_prior ) {

        if ( missing(col) ) {
          col <- 1:prior_num_runs
        }

        # Collect the samples
        samples <- vector("list",prior_num_runs)
        for ( i in 1:prior_num_runs ) {
          samples[[i]] <- prior_samples[[i]]
        }

        labels <- 1:prior_num_runs

        # Combine the samples
        if ( has_prior_combined ) {
          samples[[prior_num_runs + 1]] <- prior_combined_samples
          labels <- c(labels,"combined")
          col <- c(col,"black")
        }

        # Compute the y-limit
        ylim <- range(unlist(samples))

        boxplot(samples,names=labels,ylim=ylim,xlab="run",ylab="value",col=col,border=col,main="boxplots",...)

      }

    },

    traces = function( col, posterior = TRUE, plot_burnin = TRUE, ...) {

      "Plot a time-series of the parameter values."

      if ( posterior & has_posterior ) {

        if ( missing(col) ) {
          col <- 1:posterior_num_runs
        }

        # Compute the x-limit
        if ( plot_burnin ) {
          xlim <- c(0,max(posterior_num_generations + posterior_burnin))
        } else {
          xlim <- c(0,max(posterior_num_generations))
        }

        ylim <- range(unlist(posterior_samples))

        # Plot
        plot(1,xlim=xlim,ylim=ylim,xlab="sample",ylab="value",main="time series",col=NA,...)
        for( i in 1:posterior_num_runs ) {
          if ( posterior_burnin[i] > 0 ) {
            xx <- 1:(posterior_burnin[i]+1)
            xx <- xx[seq.int(1,length(xx),10)]
            yy <- c(posterior_burnin_samples[[i]],posterior_samples[[i]][1])
            yy <- yy[seq.int(1,length(yy),10)]
            lines(x=xx,y=yy,col="grey50")
          }
          xx <- posterior_burnin[i] + 1:posterior_num_generations[i]
          xx <- xx[seq.int(1,length(xx),10)]
          yy <- posterior_samples[[i]]
          yy <- yy[seq.int(1,length(yy),10)]
          lines(x=xx,y=yy,col=col[i])
        }
        if ( plot_burnin ) {
          segments(x0 = posterior_burnin, y0 = rep(ylim[1],posterior_num_runs), y1 = rep(ylim[2],posterior_num_runs), col = col, lty = 2, ...)
        }


      } else if ( !posterior & has_prior ) {

        if ( missing(col) ) {
          col <- 1:prior_num_runs
        }

        # Compute the x-limit
        if ( plot_burnin ) {
          xlim <- c(0,max(prior_num_generations + prior_burnin))
        } else {
          xlim <- c(0,max(prior_num_generations))
        }

        ylim <- range(unlist(prior_samples))

        # Plot
        plot(1,xlim=xlim,ylim=ylim,xlab="sample",ylab="value",main="time series",col=NA,...)
        for( i in 1:prior_num_runs ) {
          if ( prior_burnin[i] > 0 ) {
            xx <- 1:(prior_burnin[i]+1)
            xx <- xx[seq.int(1,length(xx),10)]
            yy <- c(prior_burnin_samples[[i]],prior_samples[[i]][1])
            yy <- yy[seq.int(1,length(yy),10)]
            lines(x=xx,y=yy,col="grey50")
          }
          xx <- prior_burnin[i] + 1:prior_num_generations[i]
          xx <- xx[seq.int(1,length(xx),10)]
          yy <- prior_samples[[i]]
          yy <- yy[seq.int(1,length(yy),10)]
          lines(x=xx,y=yy,col=col[i])
        }
        if ( plot_burnin ) {
          segments(x0 = prior_burnin, y0 = rep(ylim[1],prior_num_runs), y1 = rep(ylim[2],prior_num_runs), col = col, lty = 2,...)
        }

      }

    }

  ) # end methods

)
















