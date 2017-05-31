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

This will generate a folder that contains the summary in the provided output directory. The main page of interest is called \_main\_bonsai\_page.html.

### The hard way

Work in progress

## Interpreting the bonsai report



## Batching bonsai
