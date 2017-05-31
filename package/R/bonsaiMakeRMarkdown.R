bonsai$methods(

  makeRMarkdown = function () {

    "Generates an HTML from the bonsai object."

    # TODO: overwrite check here

    # Create a standard header
    header <- "---"
    header <- c(header,paste0("title:"))
    header <- c(header,"output: ")
    header <- c(header,"  html_document: ")
    header <- c(header,paste0("    theme: ",rmd_style))
    header <- c(header,"    toc: false")
    header <- c(header,"---")
    header <- c(header,"```{r, include = FALSE}")
    header <- c(header,"library(knitr)")
    header <- c(header,"library(bonsai)")
    header <- c(header,paste0("knit_theme$set(\"",knitr_style,"\")"))
    header <- c(header,"options(digits = 3)")
    header <- c(header,"palette(palette)")
    header <- c(header,"```")
    header <- c(header,"```{r global_options, include = FALSE}")
    header <- c(header,"knitr::opts_chunk$set(dev=\"png\")")
    header <- c(header,"```")

    bonsai_rmd  <- "_main_bonsai_page.Rmd"
    bonsai_html <- "_main_bonsai_page.html"

    # Determine if flag files will be made
    # has_flags <- length(flags) > 0 # currently disabled
    has_flags <- length(omnibus_diagnostics_posterior) > 0
    if (has_flags) {
      flags_summary_rmd    <- "flags.Rmd"
      flags_summary_html   <- "flags.html"
      flags_rmd_posterior  <- paste0("posterior_flags_run_",1:length(omnibus_diagnostics_posterior),"_bonsai_page.Rmd")
      flags_html_posterior <- paste0("posterior_flags_run_",1:length(omnibus_diagnostics_posterior),"_bonsai_page.html")
      flags_rmd_prior      <- paste0("prior_flags_run_",1:length(omnibus_diagnostics_prior),"_bonsai_page.Rmd")
      flags_html_prior     <- paste0("prior_flags_run_",1:length(omnibus_diagnostics_prior),"_bonsai_page.html")
    }

    # Determine if parameter files will be made
    has_parameters <- length(parameter_names) > 0
    if (has_parameters) {
      parameter_rmd  <- paste0(parameter_names,"_bonsai_page.Rmd")
      parameter_html <- paste0(parameter_names,"_bonsai_page.html")
    }
    parameter_summary_rmd  <- "numeric_parameters.Rmd"
    parameter_summary_html <- "numeric_parameters.html"

    # Determine if correlation files will be made
    # currently defaults to TRUE, but does nothing
    has_correlations  <- has_joint_prior_distributions | has_joint_posterior_distributions
    correlations_rmd  <- "correlations.Rmd"
    correlations_html <- "correlations.html"

    # Determine if clade files will be made
    has_clades <- length(clade_names) > 0
    if (has_clades & length(posterior_trees_directories)) {
      clade_rmd          <- paste0(clade_names,"_bonsai_page.Rmd")
      clade_html         <- paste0(clade_names,"_bonsai_page.html")
      compare_trees_rmd  <- paste0("compare_trees_bonsai_page.Rmd")
      compare_trees_html <- paste0("compare_trees_bonsai_page.html")
    }
    clade_summary_rmd  <- "clade_parameters.Rmd"
    clade_summary_html <- "clade_parameters.html"

    # Move to output directory
    orgDir <- getwd()
    dir.create(output_directory,recursive=TRUE,showWarnings=FALSE)
    setwd(output_directory)

    # Make a directory for pages, then move there
    if ( !dir.exists("bonsai_pages") ) {
      dir.create("bonsai_pages")
    }
    setwd("bonsai_pages")

    #################################
    ## Make the bonsai report page ##
    #################################

    lines <- header
    lines <- c(lines,paste0("#",name))
    if (has_flags) {
      lines <- c(lines,paste0("##[Diagnostics](",flags_summary_html,")","\n"))
    }
    if (has_parameters) {
      lines <- c(lines,paste0("##[Numerical parameters](",parameter_summary_html,")","\n"))
    }
    if (has_correlations) {
      lines <- c(lines,paste0("##[Numerical correlations](",correlations_html,")","\n"))
    }
    if (has_clades) {
      lines <- c(lines,paste0("##[Clade parameters](",clade_summary_html,")","\n"))
    }
    if (has_clades & length(posterior_trees_directories) > 1 ) {
      lines <- c(lines,paste0("##[Compare trees](",compare_trees_html,")","\n"))
    }
    writeLines(lines, con = bonsai_rmd)
    rmarkdown::render(bonsai_rmd, clean = TRUE, quiet = TRUE)

    #################################
    ## Make the flags summary page ##
    #################################

    if (has_flags) {
      lines <- header
      lines <- c(lines,paste0("#[",name,"](_main_bonsai_page.html)","\n"))
      lines <- c(lines,paste0("##[Diagnostics](_main_bonsai_page.html)","\n"))
      if (has_posteriors) {
        lines <- c(lines,"### Posterior","")
        for(i in 1:length(omnibus_diagnostics_posterior) ) {
          lines <- c(lines,paste0("[run ",i,"](",flags_html_posterior[i],")",ifelse(i == length(omnibus_diagnostics_posterior),"\n",", ")))
        }
      }
      if (has_priors) {
        lines <- c(lines,"### Prior","")
        for(i in 1:length(omnibus_diagnostics_prior) ) {
          lines <- c(lines,paste0("[run ",i,"](",flags_html_prior[i],")",ifelse(i == length(omnibus_diagnostics_prior),"\n",", ")))
        }
      }
      if (has_parameters) {
        lines <- c(lines,paste0("##[Numerical parameters](",parameter_summary_html,")","\n"))
      }
      if (has_correlations) {
        lines <- c(lines,paste0("##[Numerical correlations](",correlations_html,")","\n"))
      }
      if (has_clades) {
        lines <- c(lines,paste0("##[Clade parameters](",clade_summary_html,")","\n"))
      }
      if (has_clades & length(posterior_trees_directories) > 1 ) {
        lines <- c(lines,paste0("##[Compare trees](",compare_trees_html,")","\n"))
      }
      writeLines(lines, con = flags_summary_rmd)
      rmarkdown::render(flags_summary_rmd, clean = TRUE, quiet = TRUE)
    }

    ##########################
    ## Make the flags pages ##
    ##########################

    if (has_flags) {

      # First the posteriors
      if (has_posteriors) {

        for(i in 1:length(omnibus_diagnostics_posterior)) {

          lines <- header
          lines <- c(lines,paste0("#[",name,"](_main_bonsai_page.html)","\n"))
          lines <- c(lines,paste0("##[Diagnostics](_main_bonsai_page.html)","\n"))
          lines <- c(lines,"### Posterior","")
          for(j in 1:length(omnibus_diagnostics_posterior)){
            if (i != j) { # Make a link to the other parameter
              lines <- c(lines,paste0("[run ",j,"](",flags_html_posterior[j],")",ifelse(j == length(omnibus_diagnostics_posterior),"\n",", ")))
            } else {
              lines <- c(lines,paste0("run ",j,ifelse(j == length(omnibus_diagnostics_posterior),"\n",", ")))
            }
          }
          m <- mean(omnibus_diagnostics_posterior[[i]] == 1, na.rm=TRUE)
          if (m > 0.20) {
            lines <- c(lines,paste0("#<span style=\"color:red\">**FAILURE:** `r 100 * mean(omnibus_diagnostics_posterior[[",i,"]] == 1, na.rm=TRUE)`% of diagnostics failed for this run!</span>"))
          } else if (m > 0.05) {
            lines <- c(lines,paste0("#<span style=\"color:orange\">**WARNING:** `r 100 * mean(omnibus_diagnostics_posterior[[",i,"]] == 1, na.rm=TRUE)`% of diagnostics failed for this run!</span>"))
          } else {
            lines <- c(lines,paste0("#<span style=\"color:green\">**SUCCESS:** `r 100 * mean(omnibus_diagnostics_posterior[[",i,"]] == 1, na.rm=TRUE)`% of diagnostics failed for this run.</span>"))
          }
          lines <- c(lines,"```{r, echo = FALSE}")
          lines <- c(lines,paste0("kable(ifelse(omnibus_diagnostics_posterior[[",i,"]]==1,\"fail\",\"pass\"),booktabs=TRUE,row.names=TRUE)"))
          lines <- c(lines,"```","")
          if (has_priors) {
            lines <- c(lines,"### Prior","")
            for(k in 1:length(omnibus_diagnostics_prior) ) {
              lines <- c(lines,paste0("[run ",k,"](",flags_html_prior[k],")",ifelse(k == length(omnibus_diagnostics_prior),"\n",", ")))
            }
          }
          if (has_parameters) {
            lines <- c(lines,paste0("##[Numerical parameters](",parameter_summary_html,")","\n"))
          }
          if (has_correlations) {
            lines <- c(lines,paste0("##[Numerical correlations](",correlations_html,")","\n"))
          }
          if (has_clades) {
            lines <- c(lines,paste0("##[Clade parameters](",clade_summary_html,")","\n"))
          }
          if (has_clades & length(posterior_trees_directories) > 1 ) {
            lines <- c(lines,paste0("##[Compare trees](",compare_trees_html,")","\n"))
          }

          writeLines(lines, con = flags_rmd_posterior[i])
          rmarkdown::render(flags_rmd_posterior[i], clean = TRUE, quiet = TRUE)

        }

      }

      # Now the priors
      if (has_priors) {

        for(i in 1:length(omnibus_diagnostics_prior)) {

          lines <- header
          lines <- c(lines,paste0("#[",name,"](_main_bonsai_page.html)","\n"))
          lines <- c(lines,paste0("##[Diagnostics](_main_bonsai_page.html)","\n"))
          if (has_posteriors) {
            lines <- c(lines,"### Posterior","")
            for(k in 1:length(omnibus_diagnostics_posterior) ) {
              lines <- c(lines,paste0("[run ",k,"](",flags_html_posterior[k],")",ifelse(k == length(omnibus_diagnostics_posterior),"\n",", ")))
            }
          }
          lines <- c(lines,"### Prior","")
          for(j in 1:length(omnibus_diagnostics_prior)){
            if (i != j) { # Make a link to the other parameter
              lines <- c(lines,paste0("[run ",j,"](",flags_html_prior[j],")",ifelse(j == length(omnibus_diagnostics_prior),"\n",", ")))
            } else {
              lines <- c(lines,paste0("run ",j,ifelse(j == length(omnibus_diagnostics_prior),"\n",", ")))
            }
          }
          m <- mean(omnibus_diagnostics_prior[[i]] == 1, na.rm=TRUE)
          if (m > 0.20) {
            lines <- c(lines,paste0("#<span style=\"color:red\">**FAILURE:** `r 100 * mean(omnibus_diagnostics_prior[[",i,"]] == 1, na.rm=TRUE)`% of diagnostics failed for this run!</span>"))
          } else if (m > 0.05) {
            lines <- c(lines,paste0("#<span style=\"color:orange\">**WARNING:** `r 100 * mean(omnibus_diagnostics_prior[[",i,"]] == 1, na.rm=TRUE)`% of diagnostics failed for this run!</span>"))
          } else {
            lines <- c(lines,paste0("#<span style=\"color:green\">**SUCCESS:** `r 100 * mean(omnibus_diagnostics_prior[[",i,"]] == 1, na.rm=TRUE)`% of diagnostics failed for this run.</span>"))
          }
          lines <- c(lines,"```{r, echo = FALSE}")
          lines <- c(lines,paste0("kable(ifelse(omnibus_diagnostics_prior[[",i,"]]==1,\"fail\",\"pass\"),booktabs=TRUE,row.names=TRUE)"))
          lines <- c(lines,"```","\n")
          if (has_parameters) {
            lines <- c(lines,paste0("##[Numerical parameters](",parameter_summary_html,")","\n"))
          }
          if (has_correlations) {
            lines <- c(lines,paste0("##[Numerical correlations](",correlations_html,")","\n"))
          }
          if (has_clades) {
            lines <- c(lines,paste0("##[Clade parameters](",clade_summary_html,")","\n"))
          }
          if (has_clades & length(posterior_trees_directories) > 1 ) {
            lines <- c(lines,paste0("##[Compare trees](",compare_trees_html,")","\n"))
          }
          writeLines(lines, con = flags_rmd_prior[i])
          rmarkdown::render(flags_rmd_prior[i], clean = TRUE, quiet = TRUE)

        }

      }

    }

    ###############################################
    ## Make the numerical parameter summary page ##
    ###############################################

    if (has_parameters) {
      lines <- header
      lines <- c(lines,paste0("#[",name,"](_main_bonsai_page.html)","\n"))
      if (has_flags) {
        lines <- c(lines,paste0("##[Diagnostics](",flags_summary_html,")","\n"))
      }
      lines <- c(lines,paste0("##[Numerical parameters](_main_bonsai_page.html)","\n"))
      for(i in 1:length(parameter_names)){
        this_parameter <- parameter_names[i]
        this_file      <- parameter_html[i]
        lines          <- c(lines,paste0("[",this_parameter,"](",this_file,")",ifelse(i == length(parameter_names),"\n",", ")))
      }
      if (has_correlations) {
        lines <- c(lines,paste0("##[Numerical correlations](",correlations_html,")","\n"))
      }
      if (has_clades) {
        lines <- c(lines,paste0("##[Clade parameters](",clade_summary_html,")","\n"))
      }
      if (has_clades & length(posterior_trees_directories) > 1 ) {
        lines <- c(lines,paste0("##[Compare trees](",compare_trees_html,")","\n"))
      }
      writeLines(lines, con = parameter_summary_rmd)
      rmarkdown::render(parameter_summary_rmd, clean = TRUE, quiet = TRUE)
    }

    #################################################
    ## Make the individual parameter summary pages ##
    #################################################

    if (has_parameters) {

      if ( verbose ) {
        cat("\nCompiling reports for numerical parameters.\n")
        x <- length(parameter_names) * has_posterior_log + length(parameter_names) * has_prior_log
        bar <- txtProgressBar(style=bar_style, width=50, char="=")
        y <- 0
      }

      for(i in 1:length(parameter_names)) {

        this_parameter <- parameter_names[i]
        lines <- header
        lines <- c(lines,paste0("#[",name,"](_main_bonsai_page.html)","\n"))
        if (has_flags) {
          lines <- c(lines,paste0("##[Diagnostics](",flags_summary_html,")","\n"))
        }
        lines <- c(lines,paste0("##[Numerical parameters](",parameter_summary_html,")","\n"))

        for(j in 1:length(parameter_names)){
          if (i != j) { # Make a link to the other parameter
            lines <- c(lines,paste0("[",parameter_names[j],"](",parameter_html[j],")",ifelse(j == length(parameter_names),"\n",", ")))
          } else {
            lines <- c(lines,paste0(this_parameter,ifelse(j == length(parameter_names),"\n",", ")))
          }
        }

        if (has_posterior_log) {
          lines <- c(lines,"#### Posterior distribution")
          lines <- c(lines,"```{r, echo = FALSE, include = TRUE, fig.align=\"center\", fig.height = 3 }")
          lines <- c(lines,"par(las = 2, oma=c(0,0,0,0),mar=0.1+c(5,4,4,0))")
          lines <- c(lines,"layout.matrix <- matrix(1:3,nrow=1,byrow=TRUE)")
          lines <- c(lines,"layout(layout.matrix)")
          lines <- c(lines,paste0("parameters[[\"",this_parameter,"\"]]$traces()"))
          lines <- c(lines,paste0("parameters[[\"",this_parameter,"\"]]$densities()"))
          lines <- c(lines,paste0("parameters[[\"",this_parameter,"\"]]$boxplots()"))
          lines <- c(lines,"```","")
          lines <- c(lines,"```{r, echo = FALSE}")
          lines <- c(lines,paste0("kable(parameters[[\"",this_parameter,"\"]]$summaryTable(),booktabs=TRUE,row.names=TRUE)"))
          lines <- c(lines,"```","")
          y <- y + 1
        }

        if (has_prior_log) {
          lines <- c(lines,"#### Prior distribution")
          lines <- c(lines,"```{r, echo = FALSE, include = TRUE, fig.align=\"center\", fig.height = 3 }")
          lines <- c(lines,"par(las = 2, oma=c(0,0,0,0),mar=0.1+c(5,4,4,0))")
          lines <- c(lines,"layout.matrix <- matrix(1:3,nrow=1,byrow=TRUE)")
          lines <- c(lines,"layout(layout.matrix)")
          lines <- c(lines,paste0("parameters[[\"",this_parameter,"\"]]$traces(posterior=FALSE)"))
          lines <- c(lines,paste0("parameters[[\"",this_parameter,"\"]]$densities(posterior=FALSE)"))
          lines <- c(lines,paste0("parameters[[\"",this_parameter,"\"]]$boxplots(posterior=FALSE)"))
          lines <- c(lines,"```","")
          lines <- c(lines,"```{r, echo = FALSE}")
          lines <- c(lines,paste0("kable(parameters[[\"",this_parameter,"\"]]$summaryTable(posterior=FALSE),booktabs=TRUE,row.names=TRUE)"))
          lines <- c(lines,"```","")
          y <- y + 1
        }
        if (has_correlations) {
          lines <- c(lines,paste0("##[Numerical correlations](",correlations_html,")","\n"))
        }
        if (has_clades) {
          lines <- c(lines,paste0("##[Clade parameters](",clade_summary_html,")","\n"))
        }
        if (has_clades & length(posterior_trees_directories) > 1 ) {
          lines <- c(lines,paste0("##[Compare trees](",compare_trees_html,")","\n"))
        }
        writeLines(lines, con = parameter_rmd[i])
        rmarkdown::render(parameter_rmd[i], clean = TRUE, quiet = TRUE)
        setTxtProgressBar(bar,y/x)
      }
    }

    ################################
    ## Make the correlations page ##
    ################################

    if (has_correlations) {
      lines <- header
      lines <- c(lines,paste0("#[",name,"](_main_bonsai_page.html)","\n"))
      if (has_flags) {
        lines <- c(lines,paste0("##[Diagnostics](",flags_summary_html,")","\n"))
      }
      if (has_parameters) {
        lines <- c(lines,paste0("##[Numerical parameters](",parameter_summary_html,")","\n"))
      }
      lines <- c(lines,paste0("##[Numerical correlations](_main_bonsai_page.html)","\n"))
      lines <- c(lines,"This is a placeholder.\n")
      if (has_clades) {
        lines <- c(lines,paste0("##[Clade parameters](",clade_summary_html,")","\n"))
      }
      if (has_clades & length(posterior_trees_directories) > 1 ) {
        lines <- c(lines,paste0("##[Compare trees](",compare_trees_html,")","\n"))
      }
      writeLines(lines, con = correlations_rmd)
      rmarkdown::render(correlations_rmd, clean = TRUE, quiet = TRUE)
    }

    ###########################################
    ## Make the clade parameter summary page ##
    ###########################################

    if (has_clades) {
      lines <- header
      lines <- c(lines,paste0("#[",name,"](_main_bonsai_page.html)","\n"))
      if (has_flags) {
        lines <- c(lines,paste0("##[Diagnostics](",flags_summary_html,")","\n"))
      }
      if (has_parameters) {
        lines <- c(lines,paste0("##[Numerical parameters](",parameter_summary_html,")","\n"))
      }
      if (has_correlations) {
        lines <- c(lines,paste0("##[Numerical correlations](",correlations_html,")","\n"))
      }
      lines <- c(lines,paste0("##[Clade parameters](_main_bonsai_page.html)","\n"))
      for(i in 1:length(clade_names)){
        this_clade <- clade_names[i]
        this_file  <- clade_html[i]
        lines      <- c(lines,paste0("[",this_clade,"](",this_file,")",ifelse(i == length(clade_names),"\n",", ")))
      }
      if (has_clades & length(posterior_trees_directories) > 1 ) {
        lines <- c(lines,paste0("##[Compare trees](",compare_trees_html,")","\n"))
      }
      writeLines(lines, con = clade_summary_rmd)
      rmarkdown::render(clade_summary_rmd, clean = TRUE, quiet = TRUE)
    }

    #############################################
    ## Make the individual clade summary pages ##
    #############################################

    if (has_clades) {

      if ( verbose ) {
        cat("\nCompiling reports for clade parameters.\n")
        x <- length(clade_names) * has_posterior_trees + length(clade_names) * has_prior_trees
        bar <- txtProgressBar(style=bar_style, width=50, char="=")
        y <- 0
      }

      for(i in 1:length(clade_names)) {

        this_clade <- clade_names[i]
        lines <- header
        lines <- c(lines,paste0("#[",name,"](_main_bonsai_page.html)","\n"))
        if (has_flags) {
          lines <- c(lines,paste0("##[Diagnostics](",flags_summary_html,")","\n"))
        }
        if (has_parameters) {
          lines <- c(lines,paste0("##[Numerical parameters](",parameter_summary_html,")","\n"))
        }
        lines <- c(lines,paste0("##[Clade parameters](",clade_summary_html,")","\n"))

        for(j in 1:length(clade_names)){
          if (i != j) { # Make a link to the other clades
            lines <- c(lines,paste0("[",clade_names[j],"](",clade_html[j],")",ifelse(j == length(clade_names),"\n",", ")))
          } else {
            lines <- c(lines,paste0(this_clade,ifelse(j == length(clade_names),"\n",", ")))
          }
        }

        lines <- c(lines,"#### Species in this clade")
        lines <- c(lines,paste(gsub(" ","_",clades[[this_clade]]$species),collapse=", "),"")

        if (has_posterior_trees) {
          lines <- c(lines,"#### Posterior distribution")
          lines <- c(lines,"```{r, echo = FALSE, include = TRUE, fig.align=\"center\", fig.height = 3 }")
          lines <- c(lines,"par(las = 2, oma=c(0,0,0,0),mar=0.1+c(5,4,4,0))")
          lines <- c(lines,"layout.matrix <- matrix(1:3,nrow=1,byrow=TRUE)")
          lines <- c(lines,"layout(layout.matrix)")
          lines <- c(lines,paste0("clades[[\"",this_clade,"\"]]$traces()"))
          lines <- c(lines,paste0("clades[[\"",this_clade,"\"]]$densities()"))
          lines <- c(lines,paste0("clades[[\"",this_clade,"\"]]$boxplots()"))
          lines <- c(lines,"```","")
          lines <- c(lines,"```{r, echo = FALSE}")
          lines <- c(lines,paste0("kable(clades[[\"",this_clade,"\"]]$summaryTable(),booktabs=TRUE,row.names=TRUE)"))
          lines <- c(lines,"```","")
          y <- y + 1
        }

        if (has_prior_trees) {
          lines <- c(lines,"#### Prior distribution")
          lines <- c(lines,"```{r, echo = FALSE, include = TRUE, fig.align=\"center\", fig.height = 3 }")
          lines <- c(lines,"par(las = 2, oma=c(0,0,0,0),mar=0.1+c(5,4,4,0))")
          lines <- c(lines,"layout.matrix <- matrix(1:3,nrow=1,byrow=TRUE)")
          lines <- c(lines,"layout(layout.matrix)")
          lines <- c(lines,paste0("clades[[\"",this_clade,"\"]]$traces(posterior=FALSE)"))
          lines <- c(lines,paste0("clades[[\"",this_clade,"\"]]$densities(posterior=FALSE)"))
          lines <- c(lines,paste0("clades[[\"",this_clade,"\"]]$boxplots(posterior=FALSE)"))
          lines <- c(lines,"```","")
          lines <- c(lines,"```{r, echo = FALSE}")
          lines <- c(lines,paste0("kable(clades[[\"",this_clade,"\"]]$summaryTable(posterior=FALSE),booktabs=TRUE,row.names=TRUE)"))
          lines <- c(lines,"```","")
          y <- y + 1
        }
        if (has_clades & length(posterior_trees_directories) > 1 ) {
          lines <- c(lines,paste0("##[Compare trees](",compare_trees_html,")","\n"))
        }
        writeLines(lines, con = clade_rmd[i])
        rmarkdown::render(clade_rmd[i], clean = TRUE, quiet = TRUE)
        setTxtProgressBar(bar,y/x)
      }
    }

    #################################
    ## Make the compare trees page ##
    #################################

    if (has_clades & length(posterior_trees_directories) > 1) {
      lines <- header
      lines <- c(lines,paste0("#[",name,"](_main_bonsai_page.html)","\n"))
      if (has_flags) {
        lines <- c(lines,paste0("##[Diagnostics](",flags_summary_html,")","\n"))
      }
      if (has_parameters) {
        lines <- c(lines,paste0("##[Numerical parameters](",parameter_summary_html,")","\n"))
      }
      if (has_correlations) {
        lines <- c(lines,paste0("##[Numerical correlations](",correlations_html,")","\n"))
      }
      lines <- c(lines,paste0("##[Clade parameters](",clade_summary_html,")","\n"))
      lines <- c(lines,paste0("##[Compare trees](_main_bonsai_page.html)","\n"))

      lines <- c(lines,"```{r, echo = FALSE, include = TRUE, fig.align=\"center\", fig.height = 10, fig.width = 10 }")
      lines <- c(lines,paste0(".self$plotCompareTrees(pch=3)"))
      lines <- c(lines,"```","")

      writeLines(lines, con = compare_trees_rmd)
      rmarkdown::render(compare_trees_rmd, clean = TRUE, quiet = TRUE)

    }

    ###########################
    ## End HTML construction ##
    ###########################

    ## Clean up the .Rmd's
    file.remove(list.files(pattern=".Rmd"))

    # Reset the directory
    setwd(orgDir)

    cat("\n")

  }

)
