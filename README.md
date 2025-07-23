# An AI-powered Tool for Central Bank Business Liaisons: Quantitative Indicators and On-demand Insights from Firms

**Authors**: Nicholas Gray, Finn Lattimore, Kate McLoughlin and Callan Windsor

## Description

This `README` file contains general instructions on how to replicate the tools and results presented in RDP 2025-06. The code and data are structured for two main uses:

1. To replicate the creation of a business liaison textual analysis and retrieval system (TARS), using artificial data as a placeholder to preserve the confidential nature of unaggregated liaison textual information.
2. To support replication of the paper’s findings, including the construction of text-based indicators and their application in nowcasting exercises.

## License
This repository contains both code and data, each under different licenses:

- **Code**: Licensed under the BSD 3-Clause License. 
- **All datasets in the `Data` subfolder**: Licensed under Creative Commons Attribution 4.0 International (CC-BY-4.0)

Please refer to the respective license files for full details.

## Coding Languages

- **Python**: If you do not have Python installed, a fully working free scientific distribution, which includes all of the necessary packages, can be found here: https://www.anaconda.com/download/.  
  The code has been written in Python 3.10. Required packages are listed in the `environment.yml` file in the `backend` subfolder.

- **R**: If you do not have R installed, a fully working free scientific distribution, which includes all of the necessary packages, can be found here: https://cran.r-project.org/. The code has been written in R 4.4.0. Required packages are listed in `requirements.txt` files in the respective subfolders.

- The program **RStudio** is needed to open and use the `.Rproj` file in each of the subfolders. In our work we used version 2024.04.0. For more information, see https://www.posit.co.

Questions, comments and bug reports can be sent to `windsorc` and `grayn` at domain `rba.gov.au`.

## Quick Guide

- Clone or download zip file of this repository.
- Unpack into desired directory.
- Ensure Python and R environments are set up using the provided environment files.
- Refer to the `README` files in each folder for detailed instructions on running specific components.

## Folder Contents
- `README` is this read me file.
- `LICENSE` contains the license agreement that covers all code in this repo (Data subfolder is covered by separate agreement).
- `Data` is a folder that contains all input data files used across each of the folders.
- `backend` is a folder containing code for replicating a data extraction process like the tool described in the paper.
- `frontend` is a folder containing code to create a dashboard like the tool described in the paper.
- `Capabilities` is a folder containing code for building the text-based measures described in the paper.
- `nowcasting` is a folder containing code for running the empirical exercise outlined in section 5 of the paper.


### *Subfolder: Data*

This subfolder contains the data that is used by default in each of the other subfolders of the repo. All the data for demonstrating how to build and use a tool like that described in the paper has been artificially generated. As a result, using this data for replication will look different to results shown in the paper. This is to ensure the confidential nature of liaison discussions is maintained, while allowing for exploration of a tool that would use this confidential information within the RBA. The data for running the nowcasting exercise is real aggregate data and is similar to what was used in the results in the paper.

- `LICENSE-data.md` contains the license agreement that covers all data in this subfolder (code outside this folder is covered by separate agreement).
- `rdp-2025-06-graph-data.xlsx` provides the data used to plot figures in the main paper in an excel format.
- `Example Liaison Summary Note.docx`: A Word document that provides input for the extraction step in the `backend` code. This document is artificially generated and does not contain any confidential information about real firms.
- `Example_liaison_data.csv`: Provides input for the extraction step in the `backend` code. This data is artificially generated and does not contain any confidential information about real firms.
- `nowcasting_df.xlsx`: Provides aggregated time series data for nowcasting growth in the wage price index (WPI). This is the same liaison data as the results in the paper, except for the gap measures that use confidential model estimates of the NAIRU and NAIRLU. Instead, piecewise representations of NAIRU and NAIRLU are provided that broadly reflect the real model estimates used in the paper.
- `dictionary.xlsx`: Provides a list of keywords and their associated qualifiers in an Excel format. These keywords underpin the construction of textual measures of topic exposure and tone. There are lists of keywords for the wages and labour topics, split into different tabs based on whether it is counting just topic words, or topic words with positive/negative qualifiers.
- `LM_dictionary.xlsx`: Provides a guide for selecting language model outputs and setting thresholds to construct textual measures of topic exposure and tone in an Excel format.
- `uncertainty_dict.xlsx`: Provides a list of keywords underpinning the construction of the uncertainty measure in an Excel format. This is the same set of keywords used to create the measure shown in Figure 8 of the paper.
- `liaison.sqlite`: Provides a SQLite database which is queried in the code to extract liaison-like text data for app demo and index construction. This data is artificially generated and does not contain any confidential information about real firms. As a result, using this data for replication will look different to results shown in the paper.

### *Subfolder: backend*
This subfolder contains code for building the Text Analysis and Retrieval System (TARS) database using artificial data reflective of real liaison information as a placeholder for the confidential liaison data used in the paper.

- `README`: The read me file for this subfolder and contains further details on replicating the backend process of building a TARS.
- `TARS_Extraction.ipynb`: A Python notebook that demonstrates text extraction from a Word document structured like a typical liaison summary document discussed in Section 3 of the paper.
- `TARS_Enrichment.ipynb`: A Python notebook that demonstrates text enrichment using a suite of language models. This includes tagging text with a set of pre-defined topics, sentiment, and precise extractions of numerical quantities.
- `TARSml.py`: Contains functions that leverage the language models pulled from the public Hugging Face repository.
- `TARSutils.py`: Contains functions used to extract from Word documents, and to support the enrichment of text using language models.
- `environment.yml`: A YAML file for setting up the Python environment.

### *Subfolder: frontend*

This subfolder contains code to build the Shiny dashboard application that uses the TARS database using artificial data reflective of real liaison information as a placeholder for the confidential liaison data used in the paper.

- `README`: The read me file for this subfolder and contains further details on running the dashboard.
- `frontend.Rproj`: An R project file. When opened, it will initiate an R workspace that allows for easier replication of code.
- `app.R`: The main file to run the dashboard.
- `Code`: A subfolder that contains UI, server logic, user-defined functions, and Word2Vec model to run the dashboard. For more details on these files, refer to the `README` file mentioned above.
- `markdown`: A subfolder that contains R Markdown and PNG files for generating the landing page of the dashboard. For more details on these files, refer to the `README` file mentioned above.
- `requirements.txt`: A dependency file that outlines which R packages (and their versions) are required for a working replication environment.

### *Subfolder: Capabilities*

Contains code for constructing text-based indicators using a TARS that contains artificial data reflective of real liaison information as a placeholder for the confidential liaison data used in the paper.

- `README`: The read me file for this subfolder and contains further details on building text-based measures outlined in the paper.
- `Capabilities.Rproj`: An R project file. When opened, it will initiate an R workspace that allows for easier replication of code.
- `Dictionary_based_indices.R`: Code that uses a dictionary of words to build textual measures of topic exposure and tone. This code was applied to the confidential liaison data to build the dictionary-based measures of wages topic exposure and tone shown in Figure 7 of the paper.
- `LM_based_indices.R`: Code that uses the output of a language model to build textual measures of topic exposure and tone. This code was applied to the confidential liaison data to build the dictionary-based measures of wages topic exposure and tone shown in Figure 7 of the paper.
- `Uncertainty_index.R`: Code that uses a dictionary of words to build textual measures of firm uncertainty. This code was applied to the confidential liaison data to build the uncertainty measure shown in Figure 8 of the paper.
- `Numerical_extraction_indices.R`: Code that uses extracted numerical quantities to create aggregate series that can reflect economic measures. This version of the code specifically extracts aggregate measures of wages and price inflation and was used on confidential liaison data to build the numerical extraction of price inflation shown in Figure 9 of the paper.
- `Plot_measures.R`: contains code to create figures 1, 4, 7-9 of the paper, using the graph data file in the `Data` subfolder.
- `data_gen_utils.R`: Contains utility functions which are sourced and used in the above code. This is done automatically in each of the above scripts.
- `requirements.txt`: A dependency file that outlines which R packages (and their versions) are required for a working


### *Subfolder: nowcasting*
This subfolder contains code for the empirical nowcasting exercise using the same liaison data as the results in the paper, except for the gap measures that use confidential model estimates of the NAIRU and NAIRLU. As a result, using this data for replication may be slightly different to results shown in the paper.

- `README`: is the read me file for this subfolder and contains further details on running the nowcasting exercise.
- `nowcasting.Rproj`: is an R project file. When opened, it will initiate an R workspace that allows for easier replication of code.
- `fit_predict.R`: Main script for running nowcasting. In this script, the data will be imported and split up into each of the nowcasting windows. Then each of these windows will be run through OLS and regularised regressions to build models that are used to make out-of-sample predictions for each window. Finally, each models out-of-predictions are compared to the real values and the errors are compared to a baseline Phillips curve OLS specification. The output is the RMSE, and significance compared to the baseline, as well as each of the model files.
- `Functions.R`: Utility functions sourced and used by the scripts to assist with building the data windows and regression models, as well as functions for analysing the results.
- `analysis.R`: Script for evaluating and visualizing nowcasting performance. This script takes the outputs from `fit_predict.R` and the graph data file in the `Data` subfolder as it’s input. This script will allow you to plot figures 14, 15 and D1 of the appendix.
- `requirements.txt` is a dependency file that outlines which R packages (and their versions) are required for a working replication environment.



## Code Structure
```
Repo
├── Data
│   ├── LICENSE-data.md (License agreement for data in this subfolder)
│   ├── rdp-2025-06-graph-data.xlsx (graph data to plot figures in paper)
│   ├── Example Liaison Summary Note.docx (input into Extraction step in backend)
│   ├── Example_liaison_data.csv (input into Enrichment step in backend)
│   ├── nowcasting_df.xlsx (real aggregated time series data for nowcasting WPI growth)
│   ├── dictionary.xlsx (dictionaries for topic indices in Capabilities)
│   ├── LM_dictionary.xlsx (guide for LM-based topic indices in Capabilities)
│   ├── uncertainty_dict.xlsx (dictionary for uncertainty index in Capabilities)
│   └── liaison.sqlite (TARS containing liaison-like text data for app demo and building indices)
├── backend
│   ├── TARS_Enrichment.ipynb (demo on extracting text from word document)
│   ├── TARS_Extraction.ipynb (demo on enriching liaison-like text)
│   ├── TARSml.py (contains NLP model files used to enrich the text)
│   ├── TARSutils.py (contains utility functions for extracting, cleaning and enriching text)
│   └── enviroment.yml (file for building working python environment to run code)
├── frontend
│   ├── frontend.Rproj (R project file to ensure working in correct directory)
│   ├── requirements.txt (contains package versions required for replication in R)
│   ├── app.R (file to run shiny dashboard)
│   └── Code
│       ├── Function.R (user-defined functions)
│       ├── server.R (app execution objects)
│       ├── ui.R (app visual objects)
│       ├── variables.R (user-defined objects)
│       ├── w2v.bin (Word2Vec model binary)
│       ├── theme.txt (app manual theme selection) 
│       ├── Create_w2v_model.R (non-app code to create Word2Vec model)
│       └── markdown
│           └── Rmd and PNG files for landing page of application
├── Capabilities
│   ├── Capabilities.Rproj (R project file to ensure working in correct directory)
│   ├── requirements.txt (contains package versions required for replication in R)
│   ├── Dictionary_based_indices.R (topic exposure and tone using dictionary)
│   ├── LM_based_indices.R (topic exposure and tone using LM)
│   ├── Uncertainty_index.R (uncertainty using dictionary)
│   ├── Numerical_extraction_indices.R (numerical quantity extraction using LMs)
│   ├── Plot_measures.R (script to plot figures from paper)
│   └── data_gen_utils.R (contains utility functions for generating textual indices)    
└── nowcasting
    ├── nowcasting.Rproj (R project file to ensure working in correct directory)
    ├── requirements.txt (contains package versions required for replication)
    ├── fit_predict.R (main script for running nowcasting exercise)
    ├── Functions.R (utility functions for running and assessing nowcasting)
    └── analysis.R (assessing nowcasting performance and plot results) 
```
