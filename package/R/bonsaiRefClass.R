#' bonsai reference class.
bonsai <- setRefClass(

  Class = "bonsai",

  field = c(

    # Name, I/O, etc.
    name = "character",
    output_directory = "character",

    # Settings
    bar_style = "numeric",
    palette = "character",
    verbose = "logical",
    rmd_style = "character",
    knitr_style = "character",

    # Diagnostics
    omnibus_diagnostics_posterior = "list",
    omnibus_diagnostics_posterior_summary = "numeric",
    omnibus_diagnostics_prior = "list",
    omnibus_diagnostics_prior_summary = "numeric",

    ess_diagnostic = "logical",
    ess_diagnostic_levels = "numeric",

    prior_divergence_diagnostic = "logical",
    prior_divergence_diagnostic_levels = "numeric",

    within_run_convergence_diagnostic = "logical",
    within_run_convergence_diagnostic_levels = "numeric",

    between_run_convergence_diagnostic = "logical",
    between_run_convergence_diagnostic_levels = "numeric",

    # Parameters
    parameter_names = "character",
    parameters = "list",

    # Clades
    clade_names = "character",
    clades = "list",

    # Posteriors
    has_posteriors = "logical",
    has_posterior_log = "logical",
    has_posterior_trees = "logical",
    has_posterior_tree = "logical",
    posterior_log_directories = "character",
    posterior_trees_directories = "character",
    posterior_tree_directories = "character",

    # Priors
    has_priors = "logical",
    has_prior_log = "logical",
    has_prior_trees = "logical",
    has_prior_tree = "logical",
    prior_log_directories = "character",
    prior_trees_directories = "character",
    prior_tree_directories = "character",

    # Joint distributions
    has_joint_posterior_distributions = "logical",
    joint_posterior_distributions_clean = "logical",
    posterior_correlations = "list",

    has_joint_prior_distributions = "logical",
    joint_prior_distributions_clean = "logical",
    prior_correlations = "list",

    # Compare trees
    posterior_compare_trees = "matrix",
    posterior_compare_trees_diagnostics = "matrix",

    prior_compare_trees = "matrix",
    prior_compare_trees_diagnostics = "matrix",

    # Documents
    has_tex = "logical",
    tex = "character",
    has_markdown = "logical",
    markdown = "character"

  ) # end fields

)
















