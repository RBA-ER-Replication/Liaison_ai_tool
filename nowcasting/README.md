# Introduction 
This folder contains all the source code related to *Section 5 - An Empirical Application: Nowcasting Wages Growth* of the paper [An AI-powered Tool for Central Bank Business Liaisons: Quantitative Indicators and On-demand Insights from Firms](https://arxiv.org/abs/2506.18505). 

In this section, similar data used for the actual empirical exercise is used to assess whether incorporating text-based measures improve predictive performance of a nowcasting model. The textual measures were created with our Business Liaison TARS using real liaison data. For more information on how these measure are created, see *Section 4 - New Capabilities* section of the paper, or the respective folder in this repo. 

Specifically, the code in this folder of the repo will:
* Take in a file containing time series data related to assessing private-sector wages growth in Australia
* Use the specifications set in `fit_predict.R` to build a in-sample OLS/Regularised regression to nowcast the one step forward out-of-sample period.
* Assess the performance of each model by outputting results to an excel file, or by plotting results using `analysis.R` file.   

# Code Structure
```
Repo
├── Data
│   ├── rdp-2025-06-graph-data.xlsx (graph data to plot figures in paper)
│   └── nowcasting_df.xlsx (real aggregated time series data for nowcasting WPI growth)
└── nowcasting
    ├── nowcasting.Rproj (R project file to ensure working in correct directory)
    ├── requirements.txt (contains package versions required for replication)
    ├── fit_predict.R (main script for running nowcasting exercise)
    ├── Functions.R (utility functions for running and assessing nowcasting)
    └── analysis.R (assessing nowcasting performance and plot results and figures from paper)  
```

# Getting Started
### Quick Start 
1. Open `nowcasting.Rproj` to ensure you are working in the correct directory for running each of the scripts. 
3. Open `fit_predict.R` and adjust any specifications in the parameter section of the file.
4. Execute script to run nowcasting exercise. This will output results and model files into `../Data/`.
5. See `../Data/` for results of nowcast, or open and run `analysis.R` to plot results and variable selection figure from paper.  

### Requirements
For R and package versions of the original paper, see `requirements.txt`. Check your R package versions using `packageVersion("<package_name>")`. Also ensure the nowcasting data (`Nowcasting_data.xlsx`) is in the `../Data/` directory. The program **RStudio** is needed to open and use the `.Rproj` file in each of the subfolders. In our work we used version 2024.04.0. For more information, see https://www.posit.co.

### Adjustable parameters
As part of the `fit_predict.R` script, it is possible to change different parameters that are inputs into the nowcasting to change certain parts of the specification, such as nowcast period or target variable on LHS of regression. The adjustable parameters are: 
 - **dataset_name** (string): location of input dataframe containing target and regressor variables (quarterly values).
 - **suffix** (string): name added to output files to define different variations of parameters selected.
 - **baseline** (boolean): If TRUE, code will only run OLS and regularised regressions for the Baseline model specification.
 - **oos_start, oos_end** (date): Start and end quarters to be used for out of sample periods, 2015Q1 to 2024Q3 used in original paper.
 - **target** (string): Names of variable used as target variable in nowcasting.
 - **manual models** (list): named list of equations that will be fit as OLS models, must have one model named "Baseline".
 - **lambda_seq** (list): list of numerical values for lambda that will be grid-searched for choosing best regularisation specification
 - **ridge_grid, lasso_grid, elasticnet_grid** (list): numerical values for alpha (0=ridge, 1=lasso, list=elastic net) and lambda that will be grid-searched for choosing best regularisation specification

All variable are grouped in a sectioned clearly defined in the code.

### Input Data
The provided input data `Data/nowcasting_df.xlsx` begins in Q1 2006 and ends in Q3 2024. The variable descriptions and types are specified in the Appendix of the paper. The data includes all variables used to produce the results in the paper, except the gap variables, which are explained in the following paragraph. Names of the variables use snake case in the dataframe instead of LaTeX symbols like in the appendix, but the names have been chosen such that they should map easily between the two styles.   

The unemployment and underutilisation gap variables in the provided data have been calculated using a piecewise estimation of the NAIRU and NAIRLU measures. This is broadly reflective of the actual model estimates used at the RBA. The estimated measures are created by beginning at initial levels of 6.5% (for NAIRU) and 8% (for NAIRLU) in the 1990s, then linearly falling to 4.5% (for NAIRU) and 6% (for NAIRLU) until 2019Q1 and flat thereafter. For more information of the RBA's assessment of the measures, see the [In Depth - Full Employment](https://www.rba.gov.au/publications/smp/2024/feb/in-depth-full-employment.html) chapter of the February 2024 Statement on Monetary Policy.

### Advanced Adjustments
Currently the code is optimised for assessing private-sector WPI growth using a baseline OLS Phillips Curve model as well as one augmented with staff-based and text-based measures extracted from the business liaison TARS. There are adjustable parameters that allow a user to change the target variable and baseline model specification if required. Whilst significant effort has been made to ensure only the adjustable parameters are required to do this, further testing and adjustments may be required if extensive alterations are made (such as multiple regularised model specifications, mixed-frequency regressions, etc).



