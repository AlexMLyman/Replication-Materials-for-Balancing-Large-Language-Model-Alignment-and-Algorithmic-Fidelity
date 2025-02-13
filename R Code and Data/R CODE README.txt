This file explains the R analysis scripts and datafiles needed to replicate the bulk of the analysis in the main paper and appendix for “Balancing Large Language Model Algorithmic Fidelity with” by Lyman et al in Sociological Methods and Research.

There are three types of files included for these replications – an R analysis file (entitled “Balancing Large Language Model Alignment_R Analysis.R”), two .csv files for study 1 (“Labeled ratings.csv” which is the validation data for the coding by GPT-4o” and “study1.csv” which is the data required to replicate the study 1 analyses in the main text and appendix), and three folders of data required to replicate the study 2 analyses (“base”, “human”, and “instruct”, all of which contain .csv files of data used by the R analysis file). These files should all be downloaded into the same folder or directory for the R analysis file to read all of the files in correctly to run the analyses. The three folders should be located in the folder/location as the other files and should be left as folders – that is, users should not move all of the spreadsheets from these folders to another location. Doing so will disrupt the way the R analysis file imports the data for Study 2.

Some additional details about the data files:

“Labeling ratings.csv” contains the set of LLM responses that a set of human coders and GPT-4o coded for study 1. It is used in the validation portion of the paper. The rows of this dataset are the different coders, and the columns are the different documents that were coded (the original text is not included in this file). The R analysis file transposes and reformats this file for the analyses in the paper.

“study1.csv” includes the set of LLM generated attitudes from the benchmarking tasks. The rows of this dataset are different LLM responses and the columns are the variables used in Study 1. This contains both the original responses from the LLMs as well as the coding performed by GPT-4o. Some recoding and data preparation is done in the R analysis file prior to the analyses shown in the paper.

The folders of files contain the data from Study 2, both the original human responses (from Rothschild et al 2019) and the different LLMs. For the LLM generated responses, there are separate spreadsheets for each model family (i.e., Gemma, Llama, Mistral/Mixtral), model size (in billions of parameters) and target in the task (i.e., LLM was asked democrats or Republicans). The R analysis file imports the data from these folders and merges them into a single file for analysis.

Once these files have been saved in the same location as the R analysis file, the .R file can be run from start to finish to generate the results from the paper and appendix. The file will save a set of figures (appearing in the paper and appendix) in the same location where the replication files are saved.

For information on the Python files included for the paper, please see the separate instructions/README for those items.
