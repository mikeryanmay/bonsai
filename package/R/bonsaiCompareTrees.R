bonsai$methods(

  compareTrees = function() {

    # for each pair of runs, compare clade posterior probabilities
    if (has_posterior_trees) {

      nruns <- length(posterior_trees_directories)
      if (nruns < 2) return(NULL)

      # compute the clade posterior probabilities
      posterior_clade_probabilities <- do.call(rbind,lapply(clades,function(x) x$posterior_mean))

      # compute the compare trees plots
      posterior_compare_trees <<- matrix(list(),nruns,nruns)
      posterior_compare_trees_diagnostics <<- matrix(list(),nruns,nruns)

      for(i in 2:nruns) {
        for(j in 1:(i-1)) {
          x <- posterior_clade_probabilities[,i]
          y <- posterior_clade_probabilities[,j]
          posterior_compare_trees[i,j] <<- list(list(x,y))
          posterior_compare_trees_diagnostics[i,j] <<- suppressWarnings(wilcox.test(x,y,paired=TRUE)$p.value)
        }
      }

    }

    if (has_prior_trees) {

      nruns <- length(prior_trees_directories)
      if (nruns < 2) return(NULL)

      # compute the clade posterior probabilities
      prior_clade_probabilities <- do.call(rbind,lapply(clades,function(x) x$prior_mean))

      # compute the compare trees plots
      prior_compare_trees <<- matrix(list(),nruns,nruns)
      prior_compare_trees_diagnostics <<- matrix(list(),nruns,nruns)

      for(i in 2:nruns) {
        for(j in 1:(i-1)) {
          x <- prior_clade_probabilities[,i]
          y <- prior_clade_probabilities[,j]
          prior_compare_trees[i,j] <<- list(list(x,y))
          prior_compare_trees_diagnostics[i,j] <<- suppressWarnings(wilcox.test(x,y,paired=TRUE)$p.value)
        }
      }

    }

  },

  plotCompareTrees = function(posterior = TRUE, ...) {

    if (has_posterior_trees & posterior) {

      nruns <- nrow(posterior_compare_trees)
      layout.matrix <- matrix(0,nruns - 1,nruns - 1)
      layout.matrix[upper.tri(layout.matrix,diag=TRUE)] <- 1:choose(nruns,2)
      layout.matrix <- t(layout.matrix)

      # Compute the breaks and assemble the color palette
      # breaks <- seq(0.05,0.9,0.05)
      # blues  <- brewer.pal(9,"Blues")
      # reds   <- brewer.pal(9,"Reds")
      # colors <- c(rev(reds),"white",blues)

      breaks <- seq(0.05,0.95,0.1)
      colors <- paste0(brewer.pal(11,"RdBu"),"20")

      par(mar=c(0,0,0,0),oma=c(3,3,3,3))
      layout(layout.matrix)
      for(i in 2:nruns) {
        for(j in 1:(i-1)) {
          yy <- posterior_compare_trees[i,j][[1]][[1]]
          xx <- posterior_compare_trees[i,j][[1]][[2]]
          p  <- posterior_compare_trees_diagnostics[i,j][[1]]
          c  <- colors[1 + findInterval(p,breaks)]
          plot(x=xx,y=yy,xlim=c(0,1),ylim=c(0,1),xlab=NA,ylab=NA,xaxt="n",yaxt="n",type="n",bty="n")
          # polygon(x=c(-10,10,10,-10),y=c(-10,-10,10,10),col=c,border=NA)
          abline(a=0,b=1,lty=2)
          # legend("topleft",legend=round(p,3),bty="n")
          points(x=xx,y=yy,...)
          box()
          # if(i == nruns) axis(1, las=2, lwd=0, lwd.tick=1)
          # if(j == 1)     axis(2, las=2, lwd=0, lwd.tick=1)
          if(i == nruns) mtext(text=paste0("run ",j), line=1, side=1)
          if(j == 1)     mtext(text=paste0("run ",i), line=1, side=2)
        }
      }

    }

    if (has_prior_trees & !posterior) {

      nruns <- nrow(prior_compare_trees)
      layout.matrix <- matrix(0,nruns - 1,nruns - 1)
      layout.matrix[upper.tri(layout.matrix,diag=TRUE)] <- 1:choose(nruns,2)
      layout.matrix <- t(layout.matrix)

      par(mar=c(0,0,0,0),oma=c(3,3,3,3))
      layout(layout.matrix)
      for(i in 2:nruns) {
        for(j in 1:(i-1)) {
          yy <- prior_compare_trees[i,j][[1]][[1]]
          xx <- prior_compare_trees[i,j][[1]][[2]]
          p  <- prior_compare_trees_diagnostics[i,j][[1]]
          c  <- colors[1 + findInterval(p,breaks)]
          plot(x=xx,y=yy,xlim=c(0,1),ylim=c(0,1),xlab=NA,ylab=NA,xaxt="n",yaxt="n",type="n",bty="n")
          # polygon(x=c(-10,10,10,-10),y=c(-10,-10,10,10),col=c,border=NA)
          abline(a=0,b=1,lty=2)
          # legend("topleft",legend=round(p,3),bty="n")
          points(x=xx,y=yy,...)
          box()
          # if(i == nruns) axis(1, las=2, lwd=0, lwd.tick=1)
          # if(j == 1)     axis(2, las=2, lwd=0, lwd.tick=1)
          if(i == nruns) mtext(text=paste0("run ",j), line=1, side=1)
          if(j == 1)     mtext(text=paste0("run ",i), line=1, side=2)
        }
      }

    }

  }

)
