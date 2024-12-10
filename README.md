# CoupledMultiplex
Group Behavior and the effect on epidemic severity

Extended R code for the paper "Improving pandemic mitigation policies across communities through coupled dynamics of risk perception and infection". 
The new project includes group behavior.

R code to conduct the simulations and analyse the results of the paper:

- `FunctionsForHealthPaper_katie_version.R` or `FunctionsForHealthPaper_katie_version_highcut.R` is the functions used for the network modelling
- `ScriptForHealthPaper2_katie_version.R` runs the simulations
- `AnalysisScript_Revised_katie_version.R` analyses the results to produce the figures and tables in the manuscript and supplementary

Due to the size of the files, runs had to be broken into different scripts. The files may contain numbers at the end such as 14 or words such as nocut. Numbers indicate the groups used for that simulation (see groupid.csv). Words like "nocut" and "highcut" indicate there was a policy change such as not cutting or cutting a lot of connections when adhereing to a policy.  

The folder `params/` contains parameters sts and networks configuration:
- Five `csv`s with the  parameter sets used to run the simulations and in the analysis.
- Nine `.RDS` in the subfolder `params/networks2/`, used to create the multiplex networks used in the simulations

```bash
Rscript ScriptForHealthPaper2_katie_version.R 
Rscript AnalysisScript_Revised_katie_version.R
```
For the original code go to https://github.com/matthewsilk/CoupledDynamicsNetworkPaper/

There are old versions of files in this repository as well.
