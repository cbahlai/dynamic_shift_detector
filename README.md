# The Monarch Regime project

In this repo, we develop a novel break-point analysis tool for population time series data, building on the methods developed by Bahlai et al 2015, Ecological Applications. The tool uses the Ricker model as the basis for a dynamic regime, and iterates through alll break point combinations, using information-theoretic decision tools (ie AICc) to determine best fits. In this repo, we develop the tool, simulate data under a variety of conditions to test the tool, and apply the tool to  case studies of monarch butterfly overwintering and multicolored Asian ladybeetle invasions. This tool is scripted entirely in R.

## File navigation

**regime_shift_detector.R** - contains functions to detect regime shifts in population time series. The function RSdetector() takes raw time series data and generates a complete report on fits, best fits, break points, and regression parameters for models with best fits

**monarch_example.R** - applies the regime shift detector analysis to monarch overwintering data from Mexico

**plot_monarch_figures.R** - plots output from monarch example

**simulations.R**- a set of functions that creates time series data using secified parameters, and then a set of funtions to test if the parameters input match the ones detected by the regime shift detector, and the code that creates simulations under a variety of conditions, runs it through the comparison functions, and tallies the outputs, outputs a CSV file to the **'simresults' folder**

**plot simulation results.R** - takes the simulation outputs and creates plots based on varying one input at a time to see the RSdetector's performance under differing conditions- outputs figures as PDF vector graphis to the **'figs' folder**

**tests folder** contains a version of the RSdetector code and test data used in development

**writing folder** contains bits of writing, abstracts, etc, relevant to presentations and publications on this project






*by regime, I mean the set of dynamic rules, or constants, in an equation describing how populations of an organism change throught time. I am not referring to authoritarian government and/or the rules imposed by them. Nature (and the things people do to nature) is the government of butterflies.*
