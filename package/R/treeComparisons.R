treeComparisons <- function(x_trees, y_trees, x_labels, y_labels, outgroup, burnin=0.25, ...) {

  # read in the x-trees
  num_x_trees    <- length(x_trees)
  x_tree_samples <- lapply(x_trees, read.tree)
  x_tree_samples <- lapply(x_tree_samples, function(x) x[-c(1:(length(x)*burnin))])

  # read in the y-trees
  num_y_trees    <- length(y_trees)
  y_tree_samples <- lapply(y_trees, read.tree)
  y_tree_samples <- lapply(y_tree_samples, function(y) y[-c(1:(length(y)*burnin))])

  # make the x-by-y comparisons
  comparison_matrix <- matrix(list(),num_y_trees,num_x_trees)

  for(y in 1:num_y_trees) {

    # get these y-trees
    these_y_trees <- y_tree_samples[[y]]
    these_y_species <- these_y_trees[[1]]$tip.label

    for(x in 1:num_x_trees) {

      # get these x-trees
      these_x_trees <- x_tree_samples[[x]]
      these_x_species <- these_x_trees[[1]]$tip.label

      # remove any species that are not in both trees
      remove_these_species <- setdiff(these_x_species, these_y_species)

      if ( length(remove_these_species) > 0 ) {
        these_x_trees <- lapply(these_x_trees, drop.tip, tip=remove_these_species)
        these_y_trees <- lapply(these_y_trees, drop.tip, tip=remove_these_species)
      }

      # root the remaining trees on a common tip
      # TODO: check that defined outgroup is in both samples of trees
      if ( missing(outgroup) ) {
        outgroup <- these_x_trees[[1]]$tip.label
      }
      these_x_trees <- lapply(these_x_trees, root, outgroup=outgroup, resolve.root=FALSE)
      these_y_trees <- lapply(these_y_trees, root, outgroup=outgroup, resolve.root=FALSE)

      # compute posterior clade probabilities
      x_parts  <- ape::prop.part(these_x_trees)
      y_parts  <- ape::prop.part(these_y_trees)

      x_parts_labels <- lapply(x_parts,function(x) sort(attr(x_parts,"labels")[x]))
      y_parts_labels <- lapply(y_parts,function(x) sort(attr(y_parts,"labels")[x]))

      # find the clades that are sampled in either analysis
      all_clades <- unique(do.call(c,list(x_parts_labels,y_parts_labels)))

      x_probs <- numeric(length(all_clades))
      y_probs <- numeric(length(all_clades))
      for(i in 1:length(all_clades)) {
        x_prob <- attr(x_parts,"number")[sapply(x_parts_labels, identical, all_clades[[i]])] / length(these_x_trees)
        if(length(x_prob) == 0) x_prob <- 0
        y_prob <- attr(y_parts,"number")[sapply(y_parts_labels, identical, all_clades[[i]])] / length(these_y_trees)
        if(length(y_prob) == 0) y_prob <- 0
        x_probs[i] <- x_prob
        y_probs[i] <- y_prob
      }

      res <- list(x_probs=x_probs,y_probs=y_probs)

      comparison_matrix[[y,x]] <- res

    }

  }

  # make the layout matrix
  layout.mat <- matrix(1:(num_x_trees * num_y_trees), num_y_trees, num_x_trees, byrow=TRUE)
  layout(layout.mat)
  par(mar=c(0,0,0,0), oma=c(5,5,1,1))

  # make the r-squared colors
  breaks <- seq(0,1,length.out=21)
  col <- heat.colors(20)
  col[20] <- "#92C5DE"

  # plot the x-by-y comparison
  for(y in 1:num_y_trees) {

    for(x in 1:num_x_trees) {

      # get the splits
      this_x <- comparison_matrix[[y,x]]$x_probs
      this_y <- comparison_matrix[[y,x]]$y_probs

      # compute the r-squared
      sse <- sum((this_y - this_x)^2)
      r_2 <- 1 - sse / sum(this_y^2)
      bg_col <- col[findInterval(r_2, breaks, all.inside=TRUE)]

      # plot the splits
      plot(this_x, this_y, xlim=c(0,1), ylim=c(0,1), type="n", xaxt="n", yaxt="n", xlab=NA, ylab=NA)
      polygon(x=c(-2,2,2,-2), y=c(-2,-2,2,2), col=bg_col)
      abline(a=0, b=1, lty=2)
      points(this_x, this_y, ...)
      text(0,1,labels=sprintf("%.2f",r_2))

      # make axis labels
      if (x == 1) {
        if( missing(x_labels) ) {
          mtext(y, 2, line=3)
        } else {
          mtext(y_labels[y], 2, line=3)
        }
      }

      if (y == num_y_trees) {
        if( missing(y_labels) ) {
          mtext(x, 1, line=3)
        } else {
          mtext(x_labels[x], 1, line=3)
        }
      }

    }

  }

}



















