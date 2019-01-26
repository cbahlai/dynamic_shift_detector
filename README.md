# The Monarch Regime project

## Authors/developers: Christie A. Bahlai [@cbahlai](https://github.com/cbahlai) and Elise F. Zipkin [@ezipkin](https://github.com/ezipkin)

In this repository, we develop a novel break-point analysis tool for population time series data, building on the methods described in (Bahlai et al 2015, Ecological Applications)[https://doi.org/10.1890/14-2022.1]. The tool uses the Ricker model as the data-generating process for a dynamic regime, iterates through all break point combinations, and uses information-theoretic decision tools (i.e. Akaike's Information Criteron) to determine best fits. In this repository we develop the tool, simulate data under a variety of conditions to demonstrate the tool, and apply the tool to two case studies: overwintering populations of monarch butterflies and invasions of multicolored Asian ladybeetle. This tool is scripted entirely in R.

## File navigation

**regime_shift_detector.R** - contains functions to detect regime shifts in population time series. The function RSdetector() takes raw time series data and generates a complete report on fits, best fits, break points, and regression parameters for models with best fits

**monarch_example.R** - applies the regime shift detector analysis to monarch overwintering data from Mexico

**plot_monarch_figures.R** - plots output from monarch example, places outputs in **figs folder**

**harmonia_example.R** - applies the regime shift detector analysis to harmonia ladybeetle population data from Kellogg Biological Station. Includes data cleaning/manipulation code after Bahlai et al 2015.

**plot_harmona_figures.R** - plots output from harmonia example, places outputs in **figs folder**

**simulations.R**- a set of functions that creates time series data using secified parameters, and then a set of funtions to test if the parameters input match the ones detected by the regime shift detector, and the code that creates simulations under a variety of conditions, runs it through the comparison functions, and tallies the outputs, outputs a CSV file to the **'simresults' folder**

**plot_simulation_results.R** - takes the simulation outputs and creates plots based on varying one input at a time to see the RSdetector's performance under differing conditions- outputs figures as PDF vector graphis to the **'figs' folder**

**casestudydata folder** contains data for Harmonia case study. Monarch study data are proprietary

**simresults folder** contains simpulation output CSVs, in sets of one iteration of each case per file

**figs folder** PDF versions of all figs produced be scripts are held in this folder

**tests folder** contains a version of the RSdetector code and test data used in development

**writing folder** contains bits of writing, abstracts, etc, relevant to presentations and publications on this project








*by regime, I mean the set of dynamic rules, or constants, in an equation describing how populations of an organism change throught time. I am not referring to authoritarian government and/or the rules imposed by them. Nature (and the things people do to nature) is the government of butterflies.*


<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.
