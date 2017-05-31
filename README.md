# bonsai

Semi-automated diagnosis for phylogenetic Markov-chain Monte Carlo.

## Installation

First, make sure you have `devtools` installed. Then, install bonsai directly from github using `install_github()`:

```R
# install devtools
install.packages("devtools")

# load the devtools package
library(devtools)

# install bonsai
install_github("mikeryanmay/bonsai/package")
```

## Using bonsai

### The easy way

The easiest way to use `bonsai` is to place all of your MCMC output (from a single analysis) in a single directory (with no other MCMC output in that directory!) and name the output files so that `bonsai` will automatically recognize them:

- Samples from the posterior should have "posterior" in their name.
- Samples from the prior (if you have them) should have "prior" in their name.
- Numerical model parameters (i.e., not trees) should be in files with the .log file extension.
- Tree parameters (if you have them) should be in files with the .trees file extension.
- If you have multiple chains, follow the above conventions and make sure the file names are distinct.

See the output folders output/output_eight_chains for an example. If you've followed these rules, then you can simply point `bonsai` at the directory containing your files as follows:

```R
library(bonsai)

# assuming you are in the bonsai/ directory

# specify the input folder
input_dir  <- "examples/primates/output_eight_chains"

# specify the output folder (here it is the same as the input)
output_dir <- "examples/primates/output_eight_chains"

# name the bonsai project
name <- "primates HKY Gamma eight chains"

# build the bonsai project
project <- bonsai(name, output_dir, input_dir)
```
Executing the above code will create a `bonsai` object in R memory. Next, we can generate a verbose report in markdown format:

```R
# compile the markdown
project$makeRMarkdown()
```

This will generate a folder that contains the summary in the provided output directory. The main page of interest is called \_main\_bonsai\_page.html. See an example at [here](https://rawgit.com/mikeryanmay/bonsai/master/examples/primates/output_eight_chains/bonsai_pages/_main_bonsai_page.html).

### The hard way

Stay tuned.

## Interpreting the bonsai report

### Diagnostics

The "Diagnostics" page contains a high-level summary of the health of each of the MCMC chains. By clicking on a particular chain, you can see the overall failure rate of the MCMC diagnostics for that chain (convergence and ESS); this number should be less than 5% for a healthy MCMC run. For example, [this chain](https://rawgit.com/mikeryanmay/bonsai/master/examples/primates/output_eight_chains/bonsai_pages/posterior_flags_run_1_bonsai_page.html) failed because a third of the MCMC diagnostics were critical failures (p < 0.05 for convergence diagnostics, and ESS < 500).

### Numerical parameters

The "Numerical parameters" page provides detailed summaries for each numerical parameter (one per page) across each of the MCMC chains.

### Clade parameters

The "Clade parameters" page provides detailed summaries for each clade across each of the MCMC chains. Clades are treated as binary (presence-absence) random variables.


### Compare trees

The "Compare trees" compares the posterior probabilities of clades sampled between MCMC chains. This provides information about whether independent chains are providing similar estimates of the marginal posterior distribution of tree topologies.

Along the x-axis of each panel is the posterior probability of each clade for run i; on the y-axis is the posterior probability of each clade for run j. If the MCMC chains are healthy, then they should be agreeing about the posterior probabilities of individual clades; therefore, a "healthy" MCMCs are indicated by each comparison falling along the one-to-one line within each panel, as in [this example](https://rawgit.com/mikeryanmay/bonsai/master/examples/primates/output_eight_chains/bonsai_pages/compare_trees_bonsai_page.html).

## Batching bonsai

Work in progress.
