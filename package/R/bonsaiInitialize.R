bonsai$methods(

  ###############
  # Constructor #
  ###############

  initialize = function(name,
                        output_directory,
                        input_directory                           = NULL,
                        posterior_directories                     = NULL,
                        prior_directories                         = NULL,
                        burnin                                    = 0.25,
                        auto_burnin                               = TRUE,
                        verbose                                   = TRUE,
                        warnings                                  = FALSE,
                        palette                                   = RColorBrewer::brewer.pal(8,"Set1"),
                        rmd_style                                 = "spacelab",
                        knitr_style                               = "default",
                        ess_diagnostic                            = TRUE,
                        ess_diagnostic_levels                     = c(200,500),
                        prior_divergence_diagnostic               = FALSE,
                        prior_divergence_diagnostic_levels        = 2,
                        within_run_convergence_diagnostic         = TRUE,
                        within_run_convergence_diagnostic_levels  = 0.05,
                        between_run_convergence_diagnostic        = FALSE,
                        between_run_convergence_diagnostic_levels = 1,
                        outgroup                                  = NULL,
                        clade_posterior_threshold                 = 0.5,
                        ignore_list                               = c(),
                        ...) {

    if ( interactive() ) {
      bar_style <<- 3
    } else {
      bar_style <<- 1
    }

    palette <<- palette
    knitr_style <<- knitr_style
    rmd_style <<- rmd_style

    name <<- name
    output_directory <<- output_directory
    has_tex <<- has_markdown <<- FALSE
    verbose <<- verbose

    # Set diagnostics
    ess_diagnostic <<- ess_diagnostic
    ess_diagnostic_levels <<- ess_diagnostic_levels
    prior_divergence_diagnostic <<- prior_divergence_diagnostic
    prior_divergence_diagnostic_levels <<- prior_divergence_diagnostic_levels
    within_run_convergence_diagnostic <<- within_run_convergence_diagnostic
    within_run_convergence_diagnostic_levels <<- within_run_convergence_diagnostic_levels
    between_run_convergence_diagnostic <<- between_run_convergence_diagnostic
    between_run_convergence_diagnostic_levels <<- between_run_convergence_diagnostic_levels

    if ( verbose ) cat("Locating input.\n")
    # First, figure out what sort of input we've been given.
    if ( !is.null(input_directory) ) {

      # We've been pointed to a directory.
      files_in_dir <- list.files(input_directory, recursive=TRUE, full.names=TRUE)

      # Identify posterior samples in the directory.
      has_posteriors <<- any(grepl("posterior", files_in_dir))
      if ( has_posteriors ) posterior_directories <- grep("posterior", files_in_dir, value=TRUE)

      # Identify prior samples in the directory.
      has_priors <<- any(grepl("prior", files_in_dir))
      if ( has_priors ) prior_directories <- grep("prior", files_in_dir, value=TRUE)

      # If unable to identify posterior or prior samples, complain.
      if ( !has_posteriors & !has_priors ) stop("The provided directory does not appear to include posterior or prior samples.",call.=FALSE)

    } else if ( !is.null(posterior_directories) | !is.null(prior_directories) ) {

      # We've been given paths to the posterior, prior, or both.
      if ( !is.null(posterior_directories) ) {
        has_posteriors <<- TRUE
        posterior_directories <- posterior_directories
      }

      if ( !is.null(prior_directories) ) {
        has_priors <<- TRUE
        prior_directories <- prior_directories
      } else {
        has_priors <<- FALSE
      }

    } else {
      # We haven't been given anything.
      stop("You must either provide a directory in which the samples are contained or paths to the samples themselves.",call.=FALSE)
    }

    # Now, we must process the input we've been given.
    # First, determine what kind of files we have.
    # MrBayes will provide .p files, BEAST and RevBayes will provide .log files.
    # All programs should provide .trees files.

    if ( verbose ) cat("Processing input.\n")
    if ( has_posteriors ) {

      posterior_extensions <- tools::file_ext(posterior_directories)
      posterior_log        <- "log" %in% posterior_extensions
      posterior_p          <- "p" %in% posterior_extensions
      posterior_t          <- "t" %in% posterior_extensions
      posterior_trees      <- "trees" %in% posterior_extensions
      posterior_tree       <- "tree" %in% posterior_extensions

      if ( posterior_log ) {
        has_posterior_log <<- TRUE
        posterior_log_directories <<- posterior_directories[posterior_extensions == "log"]
      } else if ( posterior_p ) {
        has_posterior_log <<- TRUE
        posterior_log_directories <<- posterior_directories[posterior_extensions == "p"]
      } else {
        has_posterior_log <<- FALSE
        if (warnings) warning("No posterior log files were located. Please ensure that your posterior log files have one of these extensions: .p, .log",call.=FALSE)
      }

      if ( posterior_trees ) {
        has_posterior_trees <<- TRUE
        posterior_trees_directories <<- posterior_directories[posterior_extensions == "trees"]
      } else if ( posterior_t ) {
        has_posterior_trees <<- TRUE
        posterior_trees_directories <<- posterior_directories[posterior_extensions == "t"]
      } else {
        has_posterior_trees <<- FALSE
        if (warnings) warning("No posterior tree files were located. Please ensure that your posterior tree files have one of these extensions: .trees",call.=FALSE)
      }

      if ( posterior_tree ) {
        has_posterior_tree <<- TRUE
        posterior_tree_directories <<- posterior_directories[posterior_extensions == "tree"]
      } else {
        has_posterior_tree <<- FALSE
        if (warnings) warning("No posterior summary tree files were located. Please ensure that your summary tree files have one of these extensions: .tree",call.=FALSE)
      }

    } else {
      has_posterior_log <<- FALSE
      has_posterior_trees <<- FALSE
      has_posterior_tree <<- FALSE
    }

    if ( has_priors ) {

      prior_extensions <- tools::file_ext(prior_directories)
      prior_log        <- "log" %in% prior_extensions
      prior_p          <- "p" %in% prior_extensions
      prior_trees      <- "trees" %in% prior_extensions
      prior_t          <- "t" %in% prior_extensions
      prior_tree       <- "tree" %in% prior_extensions

      if ( prior_log ) {
        has_prior_log <<- TRUE
        prior_log_directories <<- prior_directories[prior_extensions == "log"]
      } else if ( prior_p ) {
        has_prior_log <<- TRUE
        prior_log_directories <<- prior_directories[prior_extensions == "p"]
      } else {
        has_prior_log <<- FALSE
        if (warnings) warning("No prior log files were located. Please ensure that your prior log files have one of these extensions: .p, .log",call.=FALSE)
      }

      if ( prior_trees ) {
        has_prior_trees <<- TRUE
        prior_trees_directories <<- prior_directories[prior_extensions == "trees"]
      } else if ( prior_t ) {
        has_prior_trees <<- TRUE
        prior_trees_directories <<- prior_directories[prior_extensions == "t"]
      } else {
        has_prior_trees <<- FALSE
        if (warnings) warning("No prior tree files were located. Please ensure that your prior tree files have one of these extensions: .trees",call.=FALSE)
      }

      if ( prior_tree ) {
        has_prior_tree <<- TRUE
        prior_tree_directories <<- prior_directories[prior_extensions == "tree"]
      } else {
        has_prior_tree <<- FALSE
        if (warnings) warning("No prior summary tree files were located. Please ensure that your summary tree files have one of these extensions: .tree",call.=FALSE)
      }

    } else {
      has_prior_log   <<- FALSE
      has_prior_trees <<- FALSE
      has_prior_tree  <<- FALSE
    }

    # Now we have to read in the numerical samples.
    if ( verbose ) {
      x <- length(posterior_log_directories) * has_posterior_log + length(prior_log_directories) * has_prior_log
      bar <- txtProgressBar(style=bar_style, width=50, char="=")
    }

    if ( has_posterior_log ) {

      posterior_samples <- posterior_params <- vector("list",length(posterior_log_directories))

      for ( file in 1:length(posterior_log_directories) ){

        # Determine the comment character.
        line <- readLines(posterior_log_directories[file],n=1)
        char <- strsplit(line,"")[[1]][1]
        if (char %in% c("#","[")) char <- char else char <- ""

        # Read in the samples, assign them to the container, store the parameter names
        samples <- read.table(posterior_log_directories[file],header=TRUE,comment.char=char,check.names=FALSE)

        # Remove any samples that match parameter names in the ignore list.
        # We want exact matches, so first we're going to strip out any brackets
        # or braces.
        if ( length(ignore_list) > 0 ) {
          these_parameter_names <- colnames(samples)
          these_parameter_names <- sapply(strsplit(these_parameter_names,"[",fixed=TRUE),function(x) x[1])
          these_parameter_names <- sapply(strsplit(these_parameter_names,"{",fixed=TRUE),function(x) x[1])
          samples <- samples[,!these_parameter_names %in% ignore_list]
        }

        # Assign the samples.
        posterior_samples[[file]] <- samples
        posterior_params[[file]]  <- colnames(samples)

        if ( verbose ) setTxtProgressBar(bar,file/x)

      }

      # Check if all the posteriors have the same parameters.
      if ( length(unique(paste(posterior_params))) != 1 ) {
        stop("Posterior log files do not all contain the same set of parameters.",call.=FALSE)
      } else {
        posterior_params <- posterior_params[[1]]
      }

    }

    if ( has_prior_log ) {

      prior_samples <- prior_params <- vector("list",length(prior_log_directories))

      for ( file in 1 : length(prior_log_directories) ){

        # Determine the comment character.
        line <- readLines(prior_log_directories[file],n=1)
        char <- strsplit(line,"")[[1]][1]
        if (char %in% c("#","[")) char <- char else char <- ""

        # Read in the samples, assign them to the container, store the parameter names
        samples               <- read.table(prior_log_directories[file],header=TRUE,comment.char=char,check.names=FALSE)

        # Remove any samples that match parameter names in the ignore list.
        # We want exact matches, so first we're going to strip out any brackets
        # or braces.
        if ( length(ignore_list) > 0 ) {
          these_parameter_names <- colnames(samples)
          these_parameter_names <- sapply(strsplit(these_parameter_names,"[",fixed=TRUE),function(x) x[1])
          these_parameter_names <- sapply(strsplit(these_parameter_names,"{",fixed=TRUE),function(x) x[1])
          samples <- samples[,!these_parameter_names %in% ignore_list]
        }

        prior_samples[[file]] <- samples
        prior_params[[file]]  <- colnames(samples)

        if ( verbose ) setTxtProgressBar(bar,(file + length(posterior_log_directories) * has_posterior_log)/x)

      }

      # Check if all the priors have the same parameters.
      if ( length(unique(paste(prior_params))) != 1 ) {
        stop("Prior log files do not all contain the same set of parameters.",call.=FALSE)
      } else {
        prior_params <- prior_params[[1]]
      }

    }

    if ( has_posterior_log & has_prior_log ) {
      if ( !all(posterior_params == prior_params) ) {
        # Posterior and prior parameters do not match.
        stop("Posterior and prior log files do not contain the same set of parameters.",call.=FALSE)
      } else {
        parameter_names <<- posterior_params[-1]
      }
    } else if ( has_posterior_log ) {
      parameter_names <<- posterior_params[-1]
    } else if ( has_prior_log ) {
      parameter_names <<- prior_params[-1]
    } else {
      stop("Oops.",call.=FALSE) # How did you get here???
    }
    # end numerical parameter processing.

    # Initialize the numerical parameters.
    if ( has_posterior_log | has_prior_log ) {

      if ( verbose ) {
        cat("\nInitializing numerical parameters.\n")
        bar <- txtProgressBar(style=bar_style, width=50, char="=")
      }

      for ( i in 1:length(parameter_names) ) {

        # Get the parameter name.
        this_parameter <- parameter_names[i]

        # Get the posterior samples for this parameter.
        these_posterior_samples <- vector("list",length(posterior_log_directories))
        if ( has_posterior_log ) {
          for ( j in 1:length(these_posterior_samples) ) these_posterior_samples[[j]] <- posterior_samples[[j]][[this_parameter]]
        } else {
          these_posterior_samples <- NULL
        }

        # Get the prior samples for this parameter.
        these_prior_samples <- vector("list",length(prior_log_directories))
        if ( has_prior_log ) {
          for ( j in 1:length(these_prior_samples) ) these_prior_samples[[j]] <- prior_samples[[j]][[this_parameter]]
        } else {
          these_prior_samples <- NULL
        }

        parameters[[i]] <<- NumericalParameter(this_parameter, these_posterior_samples, these_prior_samples)

        if ( verbose ) setTxtProgressBar(bar,i/length(parameter_names))

      }

      names(parameters) <<- parameter_names

    } # end numerical parameter initialization.

    # Tree parameters #
    # Now we need to read in the tree samples.
    if ( verbose ) {
      cat("\nProcessing trees.\n")
      x <- length(posterior_trees_directories) * has_posterior_trees + length(prior_trees_directories) * has_prior_trees
      bar <- txtProgressBar(style=bar_style, width=50, char="=")
    }

    if ( has_posterior_trees ) {

      posterior_trees <- vector("list",length(posterior_trees_directories))

      for ( file in 1:length(posterior_trees_directories) ){
        # Read in the trees and assign them to their temporary container.
        # Check if the file is nexus or newick
        first_line <- readLines(posterior_trees_directories[file], n=1)
        if ( grepl("#NEXUS", first_line) ) {
          # NEXUS format
          samples <- rncl::read_nexus_phylo(posterior_trees_directories[file])
        } else {
          # not NEXUS format
          samples <- ape::read.tree(posterior_trees_directories[file])
        }
        posterior_trees[[file]] <- samples
        if ( verbose ) setTxtProgressBar(bar,file/x)
      }

      # # DEBUG: add some random annotations to tip labels for debugging purposes
      # if ( length(posterior_trees) > 1 ) {
      #   for (i in 1:length(posterior_trees) ) {
      #     for (j in 1:length(posterior_trees[[i]])) {
      #       old_tip_labels <- posterior_trees[[i]][[j]]$tip.label
      #       new_tip_labels <- paste0(old_tip_labels,"[&index=",sample(1:length(old_tip_labels)),"]")
      #       posterior_trees[[i]][[j]]$tip.label = new_tip_labels
      #     }
      #   }
      # }

      # Make sure to strip out any annotations that may have
      # made it into the tip labels
      if ( length(posterior_trees) > 1 ) {
        for (i in 1:length(posterior_trees) ) {
          for (j in 1:length(posterior_trees[[i]])) {
            if( any(grepl("\\[", posterior_trees[[i]][[j]]$tip.label)) ) {
              posterior_trees[[i]][[j]]$tip.label <- gsub("\\[(.*?)\\]","",posterior_trees[[i]][[j]]$tip.label)
            }
          }
        }
      }

      # Make sure all the trees have the same labels.
      if ( length(posterior_trees) > 1 ) {
        for (i in 2:length(posterior_trees) ) {
          mismatch <- any(!posterior_trees[[i]][[1]]$tip.label %in% posterior_trees[[i-1]][[1]]$tip.label)
          if ( mismatch ) {
            stop("Posterior tree files do not all contain the same species labels.", call. = FALSE)
          }
        }
      }

      # Now ensure that all the trees are oriented the same way.
      if ( length(posterior_trees) > 1 ) {

        # Check if there is a specified outgroup
        if ( !is.null(outgroup) ) {

          root_against <- outgroup

        } else {

          # Choose a random outgroup
          root_against <- posterior_trees[[1]][[1]]$tip.label[1]

          # # Get the tree to orient against.
          # if ( FALSE & has_posterior_tree ) {
          #   # TODO: orient against one of the summary trees.
          # } else {
          #   # Orient against one of the sampled trees.
          #   exemplar <- list(posterior_trees[[1]][[1]])
          #   class(exemplar) <- "multiPhylo"
          # }
          #
          # # Orient the trees.
          # for ( trees in 1:length(posterior_trees) ) {
          #   these_trees <- c(exemplar,posterior_trees[[trees]])
          #   these_trees <- ape::.compressTipLabel(these_trees)
          #   posterior_trees[[trees]] <- these_trees[-1]
          # }

        }

        for ( trees in 1:length(posterior_trees) ) {
          posterior_trees[[trees]] <- lapply( posterior_trees[[trees]], root, outgroup=root_against)
          class(posterior_trees[[trees]]) <- "multiPhylo"
        }

      }

    }

    if ( has_prior_trees ) {

      prior_trees <- vector("list",length(prior_trees_directories))

      for ( file in 1 : length(prior_trees_directories) ){
        # Read in the trees and assign them to their temporary container
        # Check if the file is nexus or newick
        first_line <- readLines(prior_trees_directories[file], n=1)
        if ( grepl("#NEXUS", first_line) ) {
          # NEXUS format
          samples <- rncl::read_nexus_phylo(prior_trees_directories[file])
        } else {
          # not NEXUS format
          samples <- ape::read.tree(prior_trees_directories[file])
        }
        prior_trees[[file]] <- samples
        if ( verbose ) setTxtProgressBar(bar,(file + length(posterior_trees_directories) * has_posterior_trees)/x)
      }

      # Make sure all the trees have the same labels.
      if ( length(prior_trees) > 1 ) {
        for ( i in 2:length(prior_trees) ) {
          mismatch <- any(!prior_trees[[i]][[1]]$tip.label %in% prior_trees[[i-1]][[1]]$tip.label)
          if ( mismatch ) stop("prior tree files do not all contain the same species labels.",call. = FALSE)
        }
      }

      # Now ensure that all the trees are oriented the same way.
      if ( length(prior_trees) > 1 ){

        if ( !has_posterior_trees ) {

          # Check if there is a specified outgroup
          if ( !is.null(outgroup) ) {

            root_against <- outgroup

          } else {

            # Choose a random outgroup
            root_against <- prior_trees[[1]][[1]]$tip.label[1]

          }

        }

        for ( trees in 1:length(prior_trees) ) {
          prior_trees[[trees]] <- lapply( prior_trees[[trees]], root, outgroup=root_against)
          class(prior_trees[[trees]]) <- "multiPhylo"
        }

      }

    } # end clade parameter processing.

    # Initialize the clade parameters.
    if ( has_posterior_trees | has_prior_trees ) {

      if ( verbose ) {
        cat("\nInitializing clade parameters.\n")
        bar <- txtProgressBar(style=bar_style, width=50, char="=")
      }

      # We need to find the clades to track.
      if ( FALSE & has_posterior_tree ) {

        # TODO: just track clades in the summary tree

      } else {

        # Track the top x clades in the posterior samples.
        # First, we must concatenate all the posterior samples to make
        # sure we don't miss any clades.
        full_trees  <- Reduce(c,posterior_trees)
        full_parts  <- ape::prop.part(full_trees)
        full_parts_labels <- lapply(full_parts,function(x) sort(attr(full_parts,"labels")[x]))
        parts_order <- order(attr(full_parts,"number"), decreasing = TRUE)
        # sub_parts   <- full_parts[parts_order][attr(full_parts,"number")[parts_order] / length(full_trees) < 1.0]
        # sub_parts   <- full_parts[parts_order]

        # just get clades that have >=50% posterior probability
        num_posterior_tree_samples <- length(full_trees)
        posteror_probability_cutoff <- num_posterior_tree_samples / 2
        clade_posterior_probabilities <- attr(full_parts, "number") / length(full_trees)
        max_clades <- sum(clade_posterior_probabilities > clade_posterior_threshold)
        clades_to_track <- which(clade_posterior_probabilities > clade_posterior_threshold)

#         sub_parts <- full_parts[attr(full_parts, "number") > posteror_probability_cutoff]
#         sub_parts_labels <- full_parts_labels[attr(full_parts, "number") > posteror_probability_cutoff]
#         clades_to_track <- sub_parts
#         max_clades <- length(sub_parts)

        # Get the maximum number to track, based on the number
        # of species in the tree.
        # num_species <- posterior_trees[[1]][[1]]$Nnode + 1
        # max_clades  <- pmin(2 * num_species - 2, length(sub_parts))
        # max_clades <- pmin(30, length(sub_parts))

        # Now, construct a list of clades to track.
        # clades_to_track <- sub_parts[1:max_clades]

      }

      clade_names <<- paste0("clade_",1:max_clades)

      # Make the prop.parts for the sampled trees.
      if ( has_posterior_trees ) {
        posterior_parts <- vector("list",length(posterior_trees))
        for ( i in 1:length(posterior_trees) ) posterior_parts[[i]] <- lapply(posterior_trees[[i]],ape::prop.part)
      }

      if ( has_prior_trees ) {
        prior_parts <- vector("list",length(prior_trees))
        for ( i in 1:length(prior_trees) ) prior_parts[[i]] <- lapply(prior_trees[[i]],ape::prop.part)
      }

      for ( clade in 1:max_clades ) {

        # Get the parameter name.
        this_parameter <- as.character(clade)

        # Get the species in this clade.
        # these_species <- sort(attr(full_parts, "labels")[clades_to_track[[clade]]])
        these_species <- sort(attr(full_parts, "labels")[full_parts[[clades_to_track[clade]]]])

        # Get the posterior samples for this parameter.
        these_posterior_samples <- list()
        if ( has_posterior_trees ) {
          these_posterior_samples <- vector("list",length(posterior_parts))
          for ( i in 1:length(posterior_parts) ) {
            these_samples <- integer(length(posterior_parts[[i]]))
            for ( j in 1:length(posterior_parts[[i]])) {
              this_sample      <- posterior_parts[[i]][[j]]
              these_clades     <- lapply(this_sample,function(x) sort(attr(this_sample, "labels")[x]))
              these_samples[j] <- any(sapply(these_clades, identical, y=these_species))
            }
            these_posterior_samples[[i]] <- these_samples
          }
        }

        # Get the prior samples for the parameter.
        these_prior_samples <- list()
        if ( has_prior_trees ) {
          these_prior_samples <- vector("list",length(prior_parts))
          for ( i in 1:length(prior_parts) ) {
            these_samples <- integer(length(prior_parts[[i]]))
            for ( j in 1:length(prior_parts[[i]])) {
              this_sample      <- prior_parts[[i]][[j]]
              these_clades     <- lapply(this_sample,function(x) sort(attr(this_sample, "labels")[x]))
              these_samples[j] <- any(sapply(these_clades, identical, y=these_species))
            }
            these_prior_samples[[i]] <- these_samples
          }
        }

        clades[[clade]] <<- CladeParameter(this_parameter, these_species, these_posterior_samples, these_prior_samples )

        if ( verbose ) setTxtProgressBar(bar,clade/length(clade_names))

      }

      names(clades) <<- clade_names

    } # end clade parameter initialization.

    # Joint distributions for numerical parameters
    has_joint_posterior_distributions <<- FALSE
    has_joint_prior_distributions <<- FALSE
    # has_joint_posterior_distributions <<- TRUE
    # has_joint_prior_distributions <<- TRUE
    # jointDistributions()

    # Auto-burnin
    if ( auto_burnin ) automaticBurnin( verbose )

    # Diagnostics
    compileFlags()
    computeOmnibusStatistics()
    compareTrees()

  }, # end initialize

  show = function() {
    cat("A bonsai project named \"",name,"\".",sep="")
  }

)
