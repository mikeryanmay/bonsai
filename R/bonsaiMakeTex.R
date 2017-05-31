bonsai$methods(

  makeTeX = function() {

    "Generates a pdf from the bonsai object."

    output_name <- paste0(name,"_bonsai_report.Rnw")

    if ( !has_tex ) {

      ## Preamble
      lines <- "\\documentclass[a4paper,12pt]{article}"
      lines <- c(lines,"\\usepackage{geometry}")
      lines <- c(lines,"\\usepackage[usenames,dvipsnames]{xcolor}")
      lines <- c(lines,"\\usepackage{booktabs}")
      lines <- c(lines,"\\usepackage[hidelinks]{hyperref}")

      ## Title
      lines <- c(lines,"",paste0("\\title{",name," \\\\ A \\texttt{bonsai} report.}"))
      lines <- c(lines,"","\\author{}")
      lines <- c(lines,"","\\date{}")

      ## Begin the document
      lines <- c(lines,"","\\begin{document}")
      lines <- c(lines,"","\\maketitle")

      ## knitr settings
      lines <- c(lines,"<<set, include = FALSE>>=")
      lines <- c(lines,"library(knitr)")
      lines <- c(lines,"library(bonsai)")
      lines <- c(lines,"knit_theme$set(\"default\")")
      lines <- c(lines,"options(digits = 3)")
      lines <- c(lines,"palette(palette)")
      lines <- c(lines,"@")

      ## Table of contents
      lines <- c(lines,"","\\newpage")
      lines <- c(lines,"","\\tableofcontents")

      ## Posteriors
      ## For each parameter, make plots
      if ( has_posterior_log ) {
        lines <- c(lines,"\\newpage","\\section{Posterior distributions}")
        for ( i in 1:length(parameter_names) ) {
          # lines <- c(lines,paste0("\\subsection{Posterior summary for: ",gsub("_"," ",parameter_names[i]),"}"))
          lines <- c(lines,paste0("\\subsection{",gsub("_"," ",parameter_names[i]),"}"))
          # Now add plots
          lines <- c(lines,paste0("<<label=\"posterior_",parameter_names[i],"_log\", echo = FALSE, include = TRUE, fig.align=\"center\", fig.height = 3 >>="))
          lines <- c(lines,"par(las = 2, oma=c(0,0,0,0),mar=0.1+c(5,4,4,0))")
          lines <- c(lines,"layout.matrix <- matrix(1:3,nrow=1,byrow=TRUE)")
          lines <- c(lines,"layout(layout.matrix)")
          lines <- c(lines,paste0("parameters[[\"",parameter_names[i],"\"]]$traces()"))
          lines <- c(lines,paste0("parameters[[\"",parameter_names[i],"\"]]$densities()"))
          lines <- c(lines,paste0("parameters[[\"",parameter_names[i],"\"]]$boxplots()"))
          lines <- c(lines,"@")
          lines <- c(lines,"\\begin{table}[h]")
          lines <- c(lines,"\\centering")
          lines <- c(lines,"<<echo = FALSE>>=")
          lines <- c(lines,paste0("kable(parameters[[\"",parameter_names[i],"\"]]$summaryTable(),booktabs=TRUE,row.names=TRUE)"))
          lines <- c(lines,"@")
          lines <- c(lines,"\\end{table}")
          lines <- c(lines,"\\newpage")
        }
      }

      ## For each clade, make plots
      if ( has_posterior_trees ) {
        lines <- c(lines,"\\newpage","\\section{Posterior distributions of clades}")
        for ( i in 1:length(clade_names) ) {
          # lines <- c(lines,paste0("\\subsection{Posterior summary for: ",gsub("_"," ",clade_names[i]),"}"))
          lines <- c(lines,paste0("\\subsection{",gsub("_"," ",clade_names[i]),"}"))
          # Get the species names
          lines <- c(lines,"",paste0("Species in this clade: \\emph{",gsub("_"," ",paste0(clades[[clade_names[i]]]$species,collapse=", ")),".}"))
          # Now add plots
          lines <- c(lines,paste0("<<label=\"posterior_",clade_names[i],"_log\", echo = FALSE, include = TRUE, fig.align=\"center\", fig.height = 3 >>="))
          lines <- c(lines,"par(las = 2, oma=c(0,0,0,0),mar=0.1+c(5,4,4,0))")
          lines <- c(lines,"layout.matrix <- matrix(1:3,nrow=1,byrow=TRUE)")
          lines <- c(lines,"layout(layout.matrix)")
          lines <- c(lines,paste0("clades[[\"",clade_names[i],"\"]]$traces()"))
          # lines <- c(lines,paste0("clades[[\"",clade_names[i],"\"]]$densities()"))
          lines <- c(lines,paste0("clades[[\"",clade_names[i],"\"]]$compareTrees()"))
          lines <- c(lines,paste0("clades[[\"",clade_names[i],"\"]]$boxplots(border=NA)"))
          lines <- c(lines,"@")
          lines <- c(lines,"\\begin{table}[h]")
          lines <- c(lines,"\\centering")
          lines <- c(lines,"<<echo = FALSE>>=")
          lines <- c(lines,paste0("kable(clades[[\"",clade_names[i],"\"]]$summaryTable(),booktabs=TRUE,row.names=TRUE)"))
          lines <- c(lines,"@")
          lines <- c(lines,"\\end{table}")
          lines <- c(lines,"\\newpage")
        }
      }

      ## Priors
      ## For each parameter, make plots
      if ( has_prior_log ) {
        lines <- c(lines,"\\newpage","\\section{Prior distributions}")
        for ( i in 1:length(parameter_names) ) {
          # lines <- c(lines,paste0("\\subsection{Prior summary for: ",gsub("_"," ",parameter_names[i]),"}"))
          lines <- c(lines,paste0("\\subsection{",gsub("_"," ",parameter_names[i]),"}"))
          # Now add plots
          lines <- c(lines,paste0("<<label=\"prior_",parameter_names[i],"_log\", echo = FALSE, include = TRUE, fig.align=\"center\", fig.height = 3 >>="))
          lines <- c(lines,"par(las = 2, oma=c(0,0,0,0),mar=0.1+c(5,4,4,0))")
          lines <- c(lines,"layout.matrix <- matrix(1:3,nrow=1,byrow=TRUE)")
          lines <- c(lines,"layout(layout.matrix)")
          lines <- c(lines,paste0("parameters[[\"",parameter_names[i],"\"]]$traces(posterior=FALSE)"))
          lines <- c(lines,paste0("parameters[[\"",parameter_names[i],"\"]]$densities(posterior=FALSE)"))
          lines <- c(lines,paste0("parameters[[\"",parameter_names[i],"\"]]$boxplots(posterior=FALSE)"))
          lines <- c(lines,"@")
          lines <- c(lines,"\\begin{table}[h]")
          lines <- c(lines,"\\centering")
          lines <- c(lines,"<<echo = FALSE>>=")
          lines <- c(lines,paste0("kable(parameters[[\"",parameter_names[i],"\"]]$summaryTable(posterior=FALSE),booktabs=TRUE,row.names=TRUE)"))
          lines <- c(lines,"@")
          lines <- c(lines,"\\end{table}")
          lines <- c(lines,"\\newpage")
        }
      }

      ## For each clade, make plots
      if ( has_prior_trees ) {
        lines <- c(lines,"\\newpage","\\section{Prior distributions}")
        for ( i in 1:length(clade_names) ) {
          # lines <- c(lines,paste0("\\subsection{Prior summary for: ",gsub("_"," ",clade_names[i]),"}"))
          lines <- c(lines,paste0("\\subsection{",gsub("_"," ",clade_names[i]),"}"))
          # Get the species names
          lines <- c(lines,"",paste0("Species in this clade: \\emph{",gsub("_"," ",paste0(clades[[clade_names[i]]]$species,collapse=", ")),".}"))
          # Now add plots
          lines <- c(lines,paste0("<<label=\"prior_",clade_names[i],"_log\", echo = FALSE, include = TRUE, fig.align=\"center\", fig.height = 3 >>="))
          lines <- c(lines,"par(las = 2, oma=c(0,0,0,0),mar=0.1+c(5,4,4,0))")
          lines <- c(lines,"layout.matrix <- matrix(1:3,nrow=1,byrow=TRUE)")
          lines <- c(lines,"layout(layout.matrix)")
          lines <- c(lines,paste0("clades[[\"",clade_names[i],"\"]]$traces(posterior=FALSE)"))
          # lines <- c(lines,paste0("clades[[\"",clade_names[i],"\"]]$densities(posterior=FALSE)"))
          lines <- c(lines,paste0("clades[[\"",clade_names[i],"\"]]$compareTrees(posterior=FALSE)"))
          lines <- c(lines,paste0("clades[[\"",clade_names[i],"\"]]$boxplots(posterior=FALSE,border=NA)"))
          lines <- c(lines,"@")
          lines <- c(lines,"\\begin{table}[h]")
          lines <- c(lines,"\\centering")
          lines <- c(lines,"<<echo = FALSE>>=")
          lines <- c(lines,paste0("kable(clades[[\"",clade_names[i],"\"]]$summaryTable(posterior=FALSE),booktabs=TRUE,row.names=TRUE)"))
          lines <- c(lines,"@")
          lines <- c(lines,"\\end{table}")
          lines <- c(lines,"\\newpage")
        }
      }

      ## End the document
      lines <- c(lines,"","\\end{document}")

      tex <<- lines
      has_tex <<- TRUE

    }

    # First, change directory (just temporarily)
    orgDir <- getwd()
    setwd(output_directory)

    # Now, write the lines to file
    writeLines(tex, con = output_name)

    # Use knitr on the file to create a .tex
    file <- knitr::knit(output_name, quiet = TRUE)

    # Run pdflatex on the .tex
    tools::texi2pdf(file, clean = TRUE)
    setwd(orgDir)


  }

)
