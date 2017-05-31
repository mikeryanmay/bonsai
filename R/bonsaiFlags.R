bonsai$methods(

  compileFlags = function() {

    ###################################
    # Compile flags for the posterior #
    ###################################

    if (has_posterior_log) {

      # visit each numerical parameter
      for(i in 1:length(parameters)) {

        # effective sample size diagnostic
        if (ess_diagnostic) {
          these_intervals <- 1 + findInterval(parameters[[i]]$posterior_ess,ess_diagnostic_levels)
          parameters[[i]]$posterior_ess_flag <<- these_intervals
        }

        # within-run convergence diagnostic
        if (within_run_convergence_diagnostic) {
          these_intervals <- 1 + findInterval(parameters[[i]]$posterior_geweke,within_run_convergence_diagnostic_levels)
          parameters[[i]]$posterior_geweke_flag <<- these_intervals
        }

        # between-run convergence diagnostic
        if (between_run_convergence_diagnostic) {
          # UNDER CONSTRUCTION
        }

        # prior divergence diagnostic
        if (prior_divergence_diagnostic) {
          these_intervals <- 1 + findInterval(parameters[[i]]$posterior_ks,prior_divergence_diagnostic_levels)
          parameters[[i]]$posterior_ks_flag <<- these_intervals
        }

      }

    }

    # visit each clade parameter
    if (has_posterior_trees) {

      # visit each clade
      for(i in 1:length(clades)) {

        # effective sample size diagnostic
        if (ess_diagnostic) {
          these_intervals <- 1 + findInterval(clades[[i]]$posterior_ess, ess_diagnostic_levels)
          clades[[i]]$posterior_ess_flag <<- these_intervals
        }

        # within-run convergence diagnostic
        if (within_run_convergence_diagnostic) {
          # these_intervals <- 1 + findInterval(clades[[i]]$posterior_chisq, within_run_convergence_diagnostic_levels)
          # clades[[i]]$posterior_chisq_flag <<- these_intervals
          these_intervals <- 1 + findInterval(clades[[i]]$posterior_geweke, within_run_convergence_diagnostic_levels)
          clades[[i]]$posterior_geweke_flag <<- these_intervals
        }

        # between-run convergence diagnostic
        if (between_run_convergence_diagnostic) {
          # UNDER CONSTRUCTION
        }

        # prior divergence diagnostic
        if (prior_divergence_diagnostic) {
          these_intervals <- 1 + findInterval(clades[[i]]$posterior_ks, prior_divergence_diagnostic_levels)
          clades[[i]]$posterior_ks_flag <<- these_intervals
        }

      }

    }

    ###############################
    # Compile flags for the prior #
    ###############################

    if (has_prior_log) {

      # visit each numerical parameter
      for(i in 1:length(parameters)) {

        # effective sample size diagnostic
        if (ess_diagnostic) {
          these_intervals <- 1 + findInterval(parameters[[i]]$prior_ess,ess_diagnostic_levels)
          parameters[[i]]$prior_ess_flag <<- these_intervals
        }

        # within-run convergence diagnostic
        if (within_run_convergence_diagnostic) {
          these_intervals <- 1 + findInterval(parameters[[i]]$prior_geweke, within_run_convergence_diagnostic_levels)
          parameters[[i]]$prior_geweke_flag <<- these_intervals
        }

        # between-run convergence diagnostic
        if (between_run_convergence_diagnostic) {
          # UNDER CONSTRUCTION
        }

        # prior divergence diagnostic
        if (prior_divergence_diagnostic) {
          these_intervals <- 1 + findInterval(parameters[[i]]$prior_ks,prior_divergence_diagnostic_levels)
          parameters[[i]]$prior_ks_flag <<- these_intervals
        }

      }

    }

    if (has_prior_trees) {

      # visit each clade
      for(i in 1:length(clades)) {

        # effective sample size diagnostic
        if (ess_diagnostic) {
          these_intervals <- 1 + findInterval(clades[[i]]$prior_ess,ess_diagnostic_levels)
          clades[[i]]$prior_ess_flag <<- these_intervals
        }

        # within-run convergence diagnostic
        if (within_run_convergence_diagnostic) {
          # these_intervals <- 1 + findInterval(clades[[i]]$posterior_chisq, within_run_convergence_diagnostic_levels)
          # clades[[i]]$posterior_chisq_flag <<- these_intervals
          these_intervals <- 1 + findInterval(clades[[i]]$posterior_geweke, within_run_convergence_diagnostic_levels)
          clades[[i]]$posterior_geweke_flag <<- these_intervals
        }

        # between-run convergence diagnostic
        if (between_run_convergence_diagnostic) {
          # UNDER CONSTRUCTION
        }

        # prior divergence diagnostic
        if (prior_divergence_diagnostic) {
          these_intervals <- 1 + findInterval(clades[[i]]$prior_ks,prior_divergence_diagnostic_levels)
          clades[[i]]$prior_ks_flag <<- these_intervals
        }

      }

    }

  },

  computeOmnibusStatistics = function() {

    # Compute statistics for the posterior
    if (has_posteriors) {

      num_posterior_runs <- length(posterior_log_directories)
      omnibus_diagnostics_posterior <<- list()
      nrows <- has_posterior_log * length(parameter_names) + has_posterior_trees * length(clade_names)
      rownames <- c(parameter_names,clade_names)

      for(i in 1:num_posterior_runs) {

        this_run_diagnostics <- matrix(NA,nrow=nrows,ncol=3,dimnames=list(rownames,c("ESS","convergence within run","divergence from prior")))

        if (has_posterior_log) {

          for(parameter in parameter_names) {
            this_run_diagnostics[parameter,1] <- parameters[[parameter]]$posterior_ess_flag[i]
            this_run_diagnostics[parameter,2] <- parameters[[parameter]]$posterior_geweke_flag[i]
            this_run_diagnostics[parameter,3] <- parameters[[parameter]]$posterior_ks_flag[i]
          }

        }

        if (has_posterior_trees) {

          for(clade in clade_names) {
            this_run_diagnostics[clade,1] <- clades[[clade]]$posterior_ess_flag[i]
            # this_run_diagnostics[clade,2] <- clades[[clade]]$posterior_chisq_flag[i]
            this_run_diagnostics[clade,2] <- clades[[clade]]$posterior_geweke_flag[i]
            this_run_diagnostics[clade,3] <- clades[[clade]]$posterior_ks_flag[i]
          }

        }

        omnibus_diagnostics_posterior[[i]] <<- this_run_diagnostics[,c(ess_diagnostic,within_run_convergence_diagnostic,prior_divergence_diagnostic)]
        omnibus_diagnostics_posterior_summary[i] <<- mean(this_run_diagnostics[,c(ess_diagnostic,within_run_convergence_diagnostic,prior_divergence_diagnostic)] == 1,na.rm=TRUE)

      }

    }

    # Compute statistics for the prior
    if (has_priors) {

      num_prior_runs <- length(prior_log_directories)
      omnibus_diagnostics_prior <<- list()
      nrows <- has_prior_log * length(parameter_names) + has_prior_trees * length(clade_names)
      rownames <- c(parameter_names,clade_names)

      for(i in 1:num_prior_runs) {

        this_run_diagnostics <- matrix(NA,nrow=nrows,ncol=3,dimnames=list(rownames,c("ESS","convergence_within","divergence")))

        if (has_prior_log) {

          for(parameter in parameter_names) {
            this_run_diagnostics[parameter,1] <- parameters[[parameter]]$prior_ess_flag[i]
            this_run_diagnostics[parameter,2] <- parameters[[parameter]]$prior_geweke_flag[i]
            this_run_diagnostics[parameter,3] <- parameters[[parameter]]$prior_ks_flag[i]
          }

        }

        if (has_prior_trees) {

          for(clade in clade_names) {
            this_run_diagnostics[clade,1] <- clades[[clade]]$prior_ess_flag[i]
            # this_run_diagnostics[clade,2] <- clades[[clade]]$prior_chisq_flag[i]
            this_run_diagnostics[clade,2] <- clades[[clade]]$prior_geweke_flag[i]
            this_run_diagnostics[clade,3] <- clades[[clade]]$prior_ks_flag[i]
          }

        }

        omnibus_diagnostics_prior[[i]] <<- this_run_diagnostics[,c(ess_diagnostic,within_run_convergence_diagnostic,prior_divergence_diagnostic)]
        omnibus_diagnostics_prior_summary[i] <<- mean(this_run_diagnostics[,c(ess_diagnostic,within_run_convergence_diagnostic,prior_divergence_diagnostic)] == 1,na.rm=TRUE)

      }

    }

  }

)
