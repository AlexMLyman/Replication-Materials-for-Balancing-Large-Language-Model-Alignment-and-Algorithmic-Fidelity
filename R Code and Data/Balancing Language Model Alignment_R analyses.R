####Balancing Language Model Alignment, Lyman et al#####
#9 February 2025

####Intro material####
#This file contains the data analyses for the paper, as conducted in R
#See the separate Python replication files for code to generate the responses 
#from the LLMs.

#These commands were run with R version 4.4.1, on Windows 11 x64 (build 22631)

####This function helps with locating the replication data files on the user's computer
##A function (for replication purposes) that works with various R configurations to identify the local path of this master script on a user's personal computer. This script calls the remainder of the replication files using that path:

stub <- function() {}
thisPath <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  if (length(grep("^-f$", cmdArgs)) > 0) {
    # R console option
    normalizePath(dirname(cmdArgs[grep("^-f", cmdArgs) + 1]))[1]
  } else if (length(grep("^--file=", cmdArgs)) > 0) {
    # Rscript/R console option
    scriptPath <- normalizePath(dirname(sub("^--file=", "", cmdArgs[grep("^--file=", cmdArgs)])))[1]
  } else if (Sys.getenv("RSTUDIO") == "1") {
    if (rstudioapi::isAvailable(version_needed=NULL,child_ok=FALSE)) {
      # RStudio interactive
      dirname(rstudioapi::getActiveDocumentContext()$path)
    } else if (is.null(knitr::current_input(dir = TRUE)) == FALSE) {
      # Knit
      knitr::current_input(dir = TRUE)
    } else {
      # R markdown on RStudio
      getwd()
    }
  } else if (is.null(attr(stub, "srcref")) == FALSE) {
    # 'source'd via R console
    dirname(normalizePath(attr(attr(stub, "srcref"), "srcfile")$filename))
  } else {
    stop("Cannot find file path")
  }
}

####GPT-4o coding validation####
#Load the necessary packages:
library(irr)
library(readr)

#Load the rater data:
ratings <- read_csv(here::here(thisPath(),"Labeled ratings.csv"),col_names = FALSE)
#Transpose this data object
ratings <- t(ratings)
#Make the first row the column names
colnames(ratings) <- ratings[1,]
#Remove the first row
ratings <- ratings[-1,]
ratings=as.data.frame(ratings)
#Remove response that isn't coded for one code:
ratings2=na.omit(ratings)
#Make the ratings numeric
ratings2$`Rater 1`=as.numeric(ratings2$`Rater 1`)
ratings2$`Rater 2`=as.numeric(ratings2$`Rater 2`)
ratings2$`Rater 3`=as.numeric(ratings2$`Rater 3`)
ratings2$`GPT-4o`=as.numeric(ratings2$`GPT-4o`)

##Analyses##
#Fleiss kappa reported in the main text; other metrics found in the online appendix.
#3 human coders:
#Fleiss kappa
kappam.fleiss(ratings2[,c(1,2,3)])
#Adding in GPT-4o
#Fleiss kappa
kappam.fleiss(ratings2[,c(1,2,3,4)])

####Study 1, benchmarking task####
#Load the required packages:
library(tidyverse)

#Load in and prep the data:
d <- read_csv(here::here(thisPath(),"study1.csv"))
d=d[,2:ncol(d)]


#Data processing to prep for the analyses:
d$instruct=d$type
d$model_2[d$model=="gemma-2-27b"|d$model=="gemma-2-27b-it"|d$model=="gemma-2-9b"|
            d$model=="gemma-2-9b-it"]="gemma-2"
d$model_2[d$model=="llama-3-70b"|d$model=="llama-3-70b-instruct"|d$model=="llama-3-8b"|
            d$model=="llama-3-8b-instruct"]="llama-3"
d$model_2[d$model=="mistral-7b"|d$model=="mistral-7b-it"]="mistral"
d$model_2[d$model=="mixtral"|d$model=="mixtral-7b-it"]="mixtral"
d$prompting[d$instruction=="no_instructions"]="baseline"
d$prompting[d$instruction=="basic_instructions"]="basic"
d$prompting[d$instruction=="advanced_instructions"]="advanced"
d$model_old=d$model
d$model=d$model_2
d$parameters[d$model_old=="gemma-2-9b"|d$model_old=="gemma-2-9b-it"]="9b"
d$parameters[d$model_old=="gemma-2-27b"|d$model_old=="gemma-2-27b-it"]="27b"
d$parameters[d$model_old=="llama-3-8b"|d$model_old=="llama-3-8b-instruct"]="8b"
d$parameters[d$model_old=="llama-3-70b"|d$model_old=="llama-3-70b-instruct"]="70b"
d$parameters[d$model_old=="mistral-7b"|d$model_old=="mistral-7b-it"]="7b"
d$parameters[d$model_old=="mixtral"|d$model_old=="mixtral-7b-it"]="8X7b"

all_data=d

#Gemma models have "Jew" instead of "Jewish". Let's recode it:
all_data$self[all_data$self=="Jew"]="Jewish"
all_data$other[all_data$other=="Jew"]="Jewish"
all_data$topic[all_data$topic=="Jew"]="Jewish"

#Recode the IVs:
#Let's re-order the factor levels so the color purple is the baseline
#We also want to do this so all the levels of the same category are together
all_data$self = factor(all_data$self, levels = c("purple", "orange", "green", "White",
                                                 "Black", "Hispanic", "Asian", "male",
                                                 "female", "non-binary person", "Christian",
                                                 "Jewish", "Muslim", "atheist",
                                                 "straight", "gay", "lesbian", "bisexual",
                                                 "Republican", "Independent", "Democrat"))
#Now let's do the same thing by the variable "other"
all_data$other = as.factor(all_data$other)
all_data$other = factor(all_data$other, levels = c("purple", "orange", "green", "White",
                                                   "Black", "Hispanic", "Asian", "male",
                                                   "female", "non-binary person", "Christian",
                                                   "Jewish", "Muslim", "atheist",
                                                   "straight", "gay", "lesbian", "bisexual",
                                                   "Republican", "Independent", "Democrat"))

#Make a variable indicating if all_data$other and all_data$self match
all_data$id_match = ifelse(all_data$other == all_data$self, "identity match", "identity mismatch")

all_data$category[all_data$self=="purple"|all_data$self=="orange"|all_data$self=="green"]="Favorite color"
all_data$category[all_data$self=="White"|all_data$self=="Black"|all_data$self=="Hispanic"|all_data$self=="Asian"]="Race"
all_data$category[all_data$self=="male"|all_data$self=="female"|all_data$self=="non-binary person"]="Gender"
all_data$category[all_data$self=="Christian"|all_data$self=="Jewish"|all_data$self=="Muslim"|all_data$self=="atheist"]="Religion"
all_data$category[all_data$self=="straight"|all_data$self=="gay"|all_data$self=="lesbian"|all_data$self=="bisexual"]="Sexual orientation"
all_data$category[all_data$self=="Republican"|all_data$self=="Independent"|all_data$self=="Democrat"]="Political party"

# Now let's do that for the values of the variable "rating"
all_data$rating = as.factor(all_data$rating)
all_data$rating = factor(all_data$rating, levels = c("neither like nor dislike",
                                                     "dislike", "like"))
#Recode topic variable to be 0 when cargo shorts and 1 otherwise
all_data$topic_r = ifelse(all_data$topic == "cargo shorts", "Cargo shorts", "Group topic")

# Recode the DVs
# First, we'll need to make sure that "self" is a factor variable
all_data$self = as.factor(all_data$self)
# Recode the Refusal variable as a binary variable. Score it so "No" is 0 and "Yes" is 1
all_data$Refusal_r[all_data$refusal == "No"] = 0
all_data$Refusal_r[all_data$refusal == "Yes"] = 1

#Repeat that but with opinion instead:
all_data$Opinion_r[all_data$opinion == "No"] = 0
all_data$Opinion_r[all_data$opinion == "Yes"] = 1

#Now for negative instead:
all_data$Negative_r[all_data$negative == "No"] = 0
all_data$Negative_r[all_data$negative == "Yes"] = 1

#Now for harmful:
all_data$Harmful_r[all_data$harmful == "No"] = 0
all_data$Harmful_r[all_data$harmful == "Yes"] = 1

#cariacture
all_data$Caricature_r[all_data$caricature == "No"] = 0
all_data$Caricature_r[all_data$caricature == "Yes"] = 1

#Consistent
all_data$Consistent_r[all_data$consistent == "No"] = 0
all_data$Consistent_r[all_data$consistent == "Yes"] = 1

#Moralizing:
all_data$Moralizing_r[all_data$moralizing == "No"] = 0
all_data$Moralizing_r[all_data$moralizing == "Yes"] = 1

#dont_know
all_data$Dont_know_r [all_data$dont_know == "No"] = 0
all_data$Dont_know_r[all_data$dont_know == "Yes"] = 1

#assistant_commentary
all_data$Assistant_commentary_r [all_data$assistant_commentary == "No"] = 0
all_data$Assistant_commentary_r[all_data$assistant_commentary == "Yes"] = 1

#Now to create some versions of the all_data object to facilitate the comparisons
#in the paper:

#A version with only cargo shorts and only non-cargo shorts
topic_data=all_data[all_data$topic_r=="Group topic",]
cargo_data=all_data[all_data$topic_r=="Cargo shorts",]

#A version with only the base, non-instruct models and a version with only the advanced instruction models
bva_data=all_data[all_data$instruct=="base"|all_data$prompting=="advanced",]

#A version of the bva object with only cargo shorts and only non-cargo shorts
bva_topic_data=topic_data[topic_data$instruct=="base"|topic_data$prompting=="advanced",]
bva_cargo_data=cargo_data[cargo_data$instruct=="base"|cargo_data$prompting=="advanced",]

##Analyses##
##Figure 2##
# Function to calculate mean and standard error for a given dataset, instruction, prompting, and dependent variable
calculate_stats <- function(data, dependent_var, instruction, prompting = NULL) {
  if (is.null(prompting)) {
    mean_val <- mean(data[[dependent_var]][data$instruct == instruction], na.rm = TRUE)
    se_val <- sd(data[[dependent_var]][data$instruct == instruction], na.rm = TRUE) / sqrt(sum(!is.na(data[[dependent_var]][data$instruct == instruction])))
  } else {
    mean_val <- mean(data[[dependent_var]][data$instruct == instruction & data$prompting == prompting], na.rm = TRUE)
    se_val <- sd(data[[dependent_var]][data$instruct == instruction & data$prompting == prompting], na.rm = TRUE) / sqrt(sum(!is.na(data[[dependent_var]][data$instruct == instruction & data$prompting == prompting])))
  }
  return(list(mean = mean_val, se = se_val))
}

# Function to generate the dataframe with mean and standard error for different groups
generate_diff_df <- function(data_list, dependent_vars, instructions, promptings) {
  results <- data.frame(mean = numeric(0), se = numeric(0), group = character(0), instruct = character(0), dependent_var = character(0), dataset = character(0))
  
  for (data_name in names(data_list)) {
    for (dep_var in dependent_vars) {
      for (i in 1:length(instructions)) {
        if (is.null(promptings[[i]])) {
          stats <- calculate_stats(data_list[[data_name]], dep_var, instructions[i])
          group_label <- "Base model"
        } else {
          stats <- calculate_stats(data_list[[data_name]], dep_var, instructions[i], promptings[[i]])
          group_label <- paste("Instruct model (", promptings[[i]], " prompt)", sep = "")
        }
        results <- rbind(results, data.frame(mean = stats$mean, se = stats$se, group = group_label, instruct = instructions[i], dependent_var = dep_var, dataset = data_name))
      }
    }
  }
  
  return(results)
}

##This is for the different model types and prompts:##
data_list <- list(all = topic_data)
dependent_vars <- c("Refusal_r", "Moralizing_r", "Assistant_commentary_r") 

# Define the instructions and corresponding promptings
instructions <- c("base", "instruct", "instruct", "instruct")
promptings <- list(NULL, "baseline", "basic", "advanced")

# Generate the results dataframe for all datasets and dependent variables
results_df_2 <- generate_diff_df(data_list, dependent_vars, instructions, promptings)

##Reorder the group variable to make it correct
results_df_2$group=factor(results_df_2$group, levels=c("Base model", "Instruct model (baseline prompt)", "Instruct model (basic prompt)", "Instruct model (advanced prompt)"))

#Randomization inference SEs
set.seed(8122024)

results_df_2_ri=results_df_2
#Make a blank vector called SE that is empty and as long as the original dataframe
se_total=numeric(length(results_df_2$se))

# Function to perform the permutation test and calculate SEs
calculate_se <- function(data, dependent_vars, num_permutations = 1000) {
  # Initialize the SE vector
  se <- numeric(length(dependent_vars) * 4)
  
  # Loop over each dependent variable
  for (j in seq_along(dependent_vars)) {
    # Initialize an empty data frame to store the means
    means_df <- data.frame(
      mean_value1 = numeric(num_permutations),
      mean_value2 = numeric(num_permutations),
      mean_value3 = numeric(num_permutations),
      mean_value4 = numeric(num_permutations)
    )
    
    # Reshuffle and calculate means
    for (i in 1:num_permutations) {
      # Randomly reshuffle the current dependent variable
      shuffled_data <- data
      shuffled_data[[dependent_vars[j]]] <- sample(data[[dependent_vars[j]]], replace = FALSE)
      
      # Calculate the means for the four conditions
      means_df[i, ] <- c(
        mean(shuffled_data[[dependent_vars[j]]][shuffled_data$instruct == "base"], na.rm = TRUE),
        mean(shuffled_data[[dependent_vars[j]]][shuffled_data$instruct == "instruct" & shuffled_data$prompting == "baseline"], na.rm = TRUE),
        mean(shuffled_data[[dependent_vars[j]]][shuffled_data$instruct == "instruct" & shuffled_data$prompting == "basic"], na.rm = TRUE),
        mean(shuffled_data[[dependent_vars[j]]][shuffled_data$instruct == "instruct" & shuffled_data$prompting == "advanced"], na.rm = TRUE)
      )
    }
    
    # Calculate the standard errors for each condition
    se[((j-1)*4+1):(j*4)] <- apply(means_df, 2, sd)
  }
  
  return(se)
}

data <- topic_data
dependent_vars <- c("Refusal_r", "Moralizing_r", "Assistant_commentary_r") 

# Call the function to calculate SEs
se <- calculate_se(data, dependent_vars, num_permutations = 3000)

# Print the SE results
print(se)


#Add these to the se_total object
se_total=se

#Rewrite the SE object in results_df_ri
results_df_2_ri$se=se_total

results_df_2_ri$group=factor(results_df_2_ri$group, levels=c("Base model", "Instruct model (baseline prompt)", "Instruct model (basic prompt)", "Instruct model (advanced prompt)"))
results_df_2_ri$dependent_var=factor(results_df_2_ri$dependent_var, levels=c("Refusal_r", "Moralizing_r", "Assistant_commentary_r"))
#Labels for the figure:
new_labels <- c(
  "Assistant_commentary_r" = "Assistant commentary",
  "Moralizing_r" = "Moralizing",
  "Refusal_r" = "Refusal"
)
# Plot the results
fig_1=ggplot(results_df_2_ri, aes(x = group, y = mean, fill = group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean - (1.96 * se), ymax = mean + (1.96 * se)), width = 0.05) +
  geom_text(aes(label = sprintf("%.3f", mean), y = mean + (1.96 * se)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5) +
  theme_minimal() +
  theme(legend.position = "bottom",  # Position the legend at the bottom
        legend.title = element_text(size = 10),  # Adjust legend title size
        legend.text = element_text(size = 9),  # Adjust legend text size
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 15),
        #axis.text.x = element_blank(),  # Remove x-axis labels
        #axis.ticks.x = element_blank(),  # Remove x-axis ticks
        strip.placement = "outside",  # Ensure all labels are outside the plotting area
        panel.spacing = unit(1, "lines")) +  # Add space between facets
  scale_fill_manual(values = c("#3A3A3A", "#7F7F7F", "#BFBFBF", "#E6E6E6"),  # Assign distinct gray colors to groups
                    name = "Model and Prompt Type",  # Legend title
                    labels = c("Base model", 
                               "Aligned model (base prompt)", 
                               "Aligned model (basic prompt)", 
                               "Aligned model (advanced prompt)")) +  # Legend labels
  scale_x_discrete(labels = c("Base", 
                              "", 
                              "Aligned", 
                              "")) +
  labs(title = "",
       x = "",
       y = "Proportion of responses",
       caption = "") +
  facet_grid(~dependent_var, axes="all_x", labeller = labeller(dependent_var=new_labels))
fig_1

#Save the figure
jpeg(here::here(thisPath(),"Figure 2.jpeg"), width=10, height=8, units="in", res=600)
fig_1
dev.off()

##Figure 3##
data_list <- list(all = bva_topic_data)
#Change DVs to consistent, opinion, negative, and harmful
dependent_vars <- c("Consistent_r", "Opinion_r", "Negative_r", "Harmful_r")

# Define the instructions and corresponding promptings
instructions <- c("base", "instruct")
promptings <- list(NULL, "advanced")

# Generate the results dataframe for all datasets and dependent variables
results_df_2_fig_2 <- generate_diff_df(data_list, dependent_vars, instructions, promptings)

##Reorder the group variable to make it correct
results_df_2_fig_2$group=factor(results_df_2_fig_2$group, levels=c("Base model", "Instruct model (advanced prompt)"))

#Randomization inference SEs
set.seed(8122024)

results_df_2_fig_2_ri=results_df_2_fig_2
#Make a blank vector called SE that is empty and as long as the original dataframe
se_total=numeric(length(results_df_2_fig_2$se))

#Randomized SEs
calculate_se <- function(data, dependent_vars, num_permutations = 3000) {
  # Initialize the SE vector
  se <- numeric(length(dependent_vars) * 2)
  
  # Loop over each dependent variable
  for (j in seq_along(dependent_vars)) {
    # Initialize an empty data frame to store the means
    means_df <- data.frame(
      mean_value1 = numeric(num_permutations),
      mean_value2 = numeric(num_permutations)
    )
    
    # Reshuffle and calculate means
    for (i in 1:num_permutations) {
      # Randomly reshuffle the current dependent variable
      shuffled_data <- data
      shuffled_data[[dependent_vars[j]]] <- sample(data[[dependent_vars[j]]], replace = FALSE)
      
      # Calculate the means for the four conditions
      means_df[i, ] <- c(
        mean(shuffled_data[[dependent_vars[j]]][shuffled_data$prompting == "baseline"], na.rm = TRUE),
        mean(shuffled_data[[dependent_vars[j]]][shuffled_data$prompting == "advanced"], na.rm = TRUE)
      )
    }
    
    # Calculate the standard errors for each condition
    se[((j-1)*2+1):(j*2)] <- apply(means_df, 2, sd)  }
  
  return(se)
}

#Whole dataset:
data <- bva_topic_data
dependent_vars <- c("Consistent_r", "Opinion_r", "Negative_r", "Harmful_r")

# Call the function to calculate SEs
se <- calculate_se(data, dependent_vars, num_permutations = 3000)

# Print the SE results
print(se)

#Add these to the se_total object
se_total=se

#Rewrite the SE object in results_df_ri
results_df_2_fig_2_ri$se=se_total

results_df_2_fig_2_ri$group=factor(results_df_2_fig_2_ri$group, levels=c("Base model", "Instruct model (baseline prompt)", "Instruct model (basic prompt)", "Instruct model (advanced prompt)"))
results_df_2_fig_2_ri$dependent_var=factor(results_df_2_fig_2_ri$dependent_var, levels=c("Opinion_r", "Consistent_r", "Negative_r", "Harmful_r"))

#Labels for the figure:
new_labels <- c(
  "Consistent_r" = "Consistent",
  "Opinion_r" = "Opinion",
  "Negative_r" = "Negative",
  "Harmful_r" = "Harmful"
)
# Plot the results
fig_2=ggplot(results_df_2_fig_2_ri, aes(x = group, y = mean, fill = group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean - (1.96 * se), ymax = mean + (1.96 * se)), width = 0.05) +
  geom_text(aes(label = sprintf("%.3f", mean), y = mean + (1.96 * se)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5) +
  theme_minimal() +
  theme(legend.position = "bottom",  # Position the legend at the bottom
        legend.title = element_text(size = 10),  # Adjust legend title size
        legend.text = element_text(size = 9),  # Adjust legend text size
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 15),
        #axis.text.x = element_blank(),  # Remove x-axis labels
        #axis.ticks.x = element_blank(),  # Remove x-axis ticks
        strip.placement = "outside",  # Ensure all labels are outside the plotting area
        panel.spacing = unit(1, "lines")) +  # Add space between facets
  scale_fill_manual(values = c("#3A3A3A", "#E6E6E6"),  # Assign distinct gray colors to groups
                    name = "Model and Prompt Type",  # Legend title
                    labels = c("Base model", 
                               "Aligned model (advanced prompt)")) +  # Legend labels
  scale_x_discrete(labels = c("Base",
                              "Aligned")) +
  labs(title = "",
       x = "",
       y = "Proportion of responses",
       caption = "") +
  facet_grid(~dependent_var, axes="all_x", labeller = labeller(dependent_var=new_labels))
fig_2

jpeg(here::here(thisPath(),"Figure 3.jpeg"), width=10, height=8, units="in", res=600)
fig_2
dev.off()

##Figure 4##
data_list <- list(all = bva_topic_data,
                  dislike=bva_topic_data[bva_topic_data$rating=="dislike",],
                  like = bva_topic_data[bva_topic_data$rating=="like",],
                  neither = bva_topic_data[bva_topic_data$rating=="neither like nor dislike",])
#Change DVs to consistent, opinion, negative, and harmful
dependent_vars <- c("Consistent_r", "Opinion_r", "Negative_r", "Harmful_r")

# Define the instructions and corresponding promptings
instructions <- c("base", "instruct")
promptings <- list(NULL, "advanced")

# Generate the results dataframe for all datasets and dependent variables
results_df_2_fig_3 <- generate_diff_df(data_list, dependent_vars, instructions, promptings)

##Reorder the group variable to make it correct
results_df_2_fig_3$group=factor(results_df_2_fig_3$group, levels=c("Base model", "Instruct model (advanced prompt)"))

#Randomization inference SEs
set.seed(8122024)

results_df_2_fig_3_ri=results_df_2_fig_3
#Make a blank vector called SE that is empty and as long as the original dataframe
se_total=numeric(length(results_df_2_fig_3$se))

#Randomized SEs
calculate_se <- function(data, dependent_vars, num_permutations = 3000) {
  # Initialize the SE vector
  se <- numeric(length(dependent_vars) * 2)
  
  # Loop over each dependent variable
  for (j in seq_along(dependent_vars)) {
    # Initialize an empty data frame to store the means
    means_df <- data.frame(
      mean_value1 = numeric(num_permutations),
      mean_value2 = numeric(num_permutations)
    )
    
    # Reshuffle and calculate means
    for (i in 1:num_permutations) {
      # Randomly reshuffle the current dependent variable
      shuffled_data <- data
      shuffled_data[[dependent_vars[j]]] <- sample(data[[dependent_vars[j]]], replace = FALSE)
      
      # Calculate the means for the four conditions
      means_df[i, ] <- c(
        mean(shuffled_data[[dependent_vars[j]]][shuffled_data$prompting == "baseline"], na.rm = TRUE),
        mean(shuffled_data[[dependent_vars[j]]][shuffled_data$prompting == "advanced"], na.rm = TRUE)
      )
    }
    
    # Calculate the standard errors for each condition
    se[((j-1)*2+1):(j*2)] <- apply(means_df, 2, sd)  }
  
  return(se)
}

#Whole dataset:
data <- bva_topic_data
dependent_vars <- c("Consistent_r", "Opinion_r", "Negative_r", "Harmful_r")
# Call the function to calculate SEs
se <- calculate_se(data, dependent_vars, num_permutations = 3000)

# Likes:
data <- bva_topic_data[bva_topic_data$rating=="like",]
se_2 <- calculate_se(data, dependent_vars, num_permutations = 3000)

# Dislikes:
data <- bva_topic_data[bva_topic_data$rating=="dislike",]
se_3 <- calculate_se(data, dependent_vars, num_permutations = 3000)

# Neither like nor dislike:
data <- bva_topic_data[bva_topic_data$rating=="neither like nor dislike",]
se_4 <- calculate_se(data, dependent_vars, num_permutations = 3000)
#Add these to the se_total object
se_total=c(se, se_2, se_3, se_4)

#Rewrite the SE object in results_df_ri
results_df_2_fig_3_ri$se=se_total

results_df_2_fig_3_ri$group=factor(results_df_2_fig_3_ri$group, levels=c("Base model", "Instruct model (baseline prompt)", "Instruct model (basic prompt)", "Instruct model (advanced prompt)"))
results_df_2_fig_3_ri$dependent_var=factor(results_df_2_fig_3_ri$dependent_var, levels=c("Opinion_r","Consistent_r", "Negative_r", "Harmful_r"))
results_df_2_fig_3_ri$dataset=factor(results_df_2_fig_3_ri$dataset, levels=c("all", "dislike", "like", "neither"))

#Labels for the figure:
new_labels <- c(
  "Consistent_r" = "Consistent",
  "Opinion_r" = "Opinion",
  "Negative_r" = "Negative",
  "Harmful_r" = "Harmful"
)

new_labels_2 <- c(
  "all" = "All",
  "dislike" = "Dislike",
  "like" = "Like",
  "neither" = "Neither like nor dislike"
)

# Plot the results
fig_3=ggplot(results_df_2_fig_3_ri, aes(x = group, y = mean, fill = group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean - (1.96 * se), ymax = mean + (1.96 * se)), width = 0.05) +
  geom_text(aes(label = sprintf("%.3f", mean), y = mean + (1.96 * se)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5) +
  theme_minimal() +
  theme(legend.position = "bottom",  # Position the legend at the bottom
        legend.title = element_text(size = 10),  # Adjust legend title size
        legend.text = element_text(size = 9),  # Adjust legend text size
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 15),
        #axis.text.x = element_blank(),  # Remove x-axis labels
        #axis.ticks.x = element_blank(),  # Remove x-axis ticks
        strip.placement = "outside",  # Ensure all labels are outside the plotting area
        panel.spacing = unit(1, "lines")) +  # Add space between facets
  scale_fill_manual(values = c("#3A3A3A", "#E6E6E6"),  # Assign distinct gray colors to groups
                    name = "Model and Prompt Type",  # Legend title
                    labels = c("Base model", 
                               "Aligned model (advanced prompt)")) +  # Legend labels
  scale_x_discrete(labels = c("Base",
                              "Aligned")) +
  labs(title = "",
       x = "",
       y = "Proportion of responses",
       caption = "") +
  facet_grid(dataset~dependent_var, axes="all_x", labeller = labeller(dependent_var=new_labels, dataset=new_labels_2))
fig_3

jpeg(here::here(thisPath(),"Figure 4.jpeg"), width=10, height=20, units="in", res=600)
fig_3
dev.off()

##Figure 5##

#Construct this as a difference plot rather than a barplot

calculate_diff2 <- function(data, dependent_var) {
  # Calculate the mean for the base model
  mean_base <- mean(data[[dependent_var]][data$instruct == "base"], na.rm = TRUE)
  
  # Calculate the mean for the instruct model
  mean_instruct <- mean(data[[dependent_var]][data$instruct == "instruct"&data$prompting=="advanced"], na.rm = TRUE)
  
  # Calculate the standard deviations for both groups
  sd_base <- sd(data[[dependent_var]][data$instruct == "base"], na.rm = TRUE)
  sd_instruct <- sd(data[[dependent_var]][data$instruct == "instruct"&data$prompting=="advanced"], na.rm = TRUE)
  
  # Calculate the sample sizes for both groups
  n_base <- sum(!is.na(data[[dependent_var]][data$instruct == "base"]))
  n_instruct <- sum(!is.na(data[[dependent_var]][data$instruct == "instruct"&data$prompting=="advanced"]))
  
  # Calculate the standard errors for both groups
  se_base <- sd_base / sqrt(n_base)
  se_instruct <- sd_instruct / sqrt(n_instruct)
  
  # Calculate the difference between the instruct and base means
  diff <- mean_base - mean_instruct
  
  # Calculate the standard error of the difference
  se_diff <- sqrt(se_base^2 + se_instruct^2)
  
  return(list(diff = diff, se = se_diff))
}

generate_diff_df2 <- function(data_list, dependent_vars) {
  results <- data.frame(diff = numeric(0), se = numeric(0), dependent_var = character(0), dataset = character(0))
  
  for (data_name in names(data_list)) {
    for (dep_var in dependent_vars) {
      # Calculate the difference and standard error using the calculate_diff function
      stats <- calculate_diff2(data_list[[data_name]], dep_var)
      
      # Add the results to the dataframe
      results <- rbind(results, data.frame(diff = stats$diff, se = stats$se, dependent_var = dep_var, dataset = data_name))
    }
  }
  
  return(results)
}



data_list <- list(all = bva_topic_data,
                  favorite_color = bva_topic_data[bva_topic_data$category=="Favorite color",],
                  gender = bva_topic_data[bva_topic_data$category=="Gender",],
                  political_party = bva_topic_data[bva_topic_data$category=="Political party",],
                  race = bva_topic_data[bva_topic_data$category=="Race",],
                  religion = bva_topic_data[bva_topic_data$category=="Religion",],
                  sexual_orientation = bva_topic_data[bva_topic_data$category=="Sexual orientation",])

#Change DVs to consistent, opinion, negative, and harmful
dependent_vars <- c("Consistent_r", "Opinion_r", "Negative_r", "Harmful_r")

# Generate the results dataframe for all datasets and dependent variables
results_df_2_fig_4 <- generate_diff_df2(data_list, dependent_vars)

##Reorder the group variable to make it correct
results_df_2_fig_4$dataset=factor(results_df_2_fig_4$dataset, levels=c("all", "favorite_color",
                                                                       "gender", "political_party",
                                                                       "race", "religion", "sexual_orientation"))
#Randomization inference SEs
set.seed(8122024)

results_df_2_fig_4_ri=results_df_2_fig_4
#Make a blank vector called SE that is empty and as long as the original dataframe
se_total=numeric(length(results_df_2_fig_4$se))

#Randomized SEs
calculate_se_multi <- function(data, dependent_vars, num_permutations = 3000) {
  # Initialize a numeric vector to store the SEs for each dependent variable
  se_vector <- numeric(length(dependent_vars))
  
  # Loop over each dependent variable
  for (i in seq_along(dependent_vars)) {
    dependent_var <- dependent_vars[i]
    
    # Calculate the observed means for the base and instruct models
    mean_base <- mean(data[[dependent_var]][data$instruct == "base"], na.rm = TRUE)
    mean_instruct <- mean(data[[dependent_var]][data$instruct == "instruct" & data$prompting == "advanced"], na.rm = TRUE)
    
    # Calculate the observed difference in means
    diff_observed <- mean_base - mean_instruct
    
    # Initialize a vector to store the differences from the permutations
    diff_permutations <- numeric(num_permutations)
    
    # Perform permutation testing to calculate the distribution of differences under the null hypothesis
    for (j in 1:num_permutations) {
      # Randomly reshuffle the dependent variable within the data
      shuffled_data <- data
      shuffled_data[[dependent_var]] <- sample(data[[dependent_var]], replace = FALSE)
      
      # Calculate the means for the reshuffled data
      mean_base_perm <- mean(shuffled_data[[dependent_var]][shuffled_data$instruct == "base"], na.rm = TRUE)
      mean_instruct_perm <- mean(shuffled_data[[dependent_var]][shuffled_data$instruct == "instruct" & shuffled_data$prompting == "advanced"], na.rm = TRUE)
      
      # Calculate the difference in means for the reshuffled data
      diff_permutations[j] <- mean_instruct_perm - mean_base_perm
    }
    
    # Calculate the standard error of the observed difference as the standard deviation of the permutation differences
    se_diff <- sd(diff_permutations)
    
    # Store the SE in the vector
    se_vector[i] <- se_diff
  }
  
  return(se_vector)
}


#Whole dataset:
data <- bva_topic_data
dependent_var <- c("Consistent_r", "Opinion_r", "Negative_r", "Harmful_r")
se <- calculate_se_multi(data, dependent_var, num_permutations = 3000)

#Favorite color dataset:
data <- bva_topic_data[bva_topic_data$category=="Favorite color",]
se_1 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Gender dataset:
data <- bva_topic_data[bva_topic_data$category=="Gender",]
se_2 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Political party dataset:
data <- bva_topic_data[bva_topic_data$category=="Political party",]
se_3 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Race dataset:
data <- bva_topic_data[bva_topic_data$category=="Race",]
se_4 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Religion dataset:
data <- bva_topic_data[bva_topic_data$category=="Religion",]
se_5 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Sexual orientation dataset:
data <- bva_topic_data[bva_topic_data$category=="Sexual orientation",]
se_6 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Add these to the se_total object
se_total=c(se, se_1, se_2, se_3, se_4, se_5, se_6)

#Rewrite the SE object in results_df_ri
results_df_2_fig_4_ri$se=se_total

results_df_2_fig_4_ri$dependent_var=factor(results_df_2_fig_4_ri$dependent_var, levels=c("Opinion_r","Consistent_r",  "Negative_r", "Harmful_r"))

#Labels for the figure:
new_labels <- c(
  "Consistent_r" = "Consistent",
  "Opinion_r" = "Opinion",
  "Negative_r" = "Negative",
  "Harmful_r" = "Harmful"
)

new_labels2 <- c(
  "all" = "Overall",
  "favorite_color" = "Favorite color",
  "gender" = "Gender",
  "political_party" = "Political party",
  "race" = "Race",
  "religion" = "Religion",
  "sexual_orientation" = "Sexual orientation"
)
# Plot the results
fig_4a <- ggplot(results_df_2_fig_4_ri, aes(x = dataset, y = diff, color = dataset, linetype = dataset, shape = dataset)) +
  geom_errorbar(aes(ymin = diff - (1.96 * se), ymax = diff + (1.96 * se)), width = 0.05) +
  geom_point(size = 3, position = position_dodge(width = 0.9)) +  # Add points at the center of the error bars
  geom_text(aes(label = sprintf("%.3f", diff), y = diff + (1.96 * se)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5) +
  theme_minimal() +
  theme(legend.position = "none",  # Position the legend at the bottom
        legend.title = element_text(size = 10),  # Adjust legend title size
        legend.text = element_text(size = 9),  # Adjust legend text size
        strip.placement = "outside",  # Ensure all labels are outside the plotting area
        panel.spacing = unit(1, "lines")) +  # Add space between facets
  labs(title = "",
       x = "",
       y = "Difference between the base and instruct models \n(positive indicates more behavior in base models)",
       caption = "") +
  facet_grid(~dependent_var, labeller = labeller(dependent_var = new_labels)) +
  scale_x_discrete(labels = c("all" = "All groups", 
                              "gender" = "Gender",
                              "race" = "Race",
                              "favorite_color" = "Favorite color",
                              "political_party" = "Political party",
                              "religion" = "Religion",
                              "sexual_orientation" = "Sexual orientation")) +
  scale_y_continuous(limits = c(-0.3, 0.55)) +
  scale_shape_manual(values = c(16, 17, 18, 19, 15, 8, 7)) +  # Use different shapes for points
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "2262", "aa", "18", "solid"))  # Use different line types for points
fig_4a

#Plot just for cargo shorts
data_list <- list(all = bva_cargo_data,
                  favorite_color = bva_cargo_data[bva_cargo_data$category=="Favorite color",],
                  gender = bva_cargo_data[bva_cargo_data$category=="Gender",],
                  political_party = bva_cargo_data[bva_cargo_data$category=="Political party",],
                  race = bva_cargo_data[bva_cargo_data$category=="Race",],
                  religion = bva_cargo_data[bva_cargo_data$category=="Religion",],
                  sexual_orientation = bva_cargo_data[bva_cargo_data$category=="Sexual orientation",])

#Change DVs to consistent, opinion, negative, and harmful
dependent_vars <- c("Consistent_r", "Opinion_r", "Negative_r", "Harmful_r")

# Generate the results dataframe for all datasets and dependent variables
results_df_2_fig_4_c <- generate_diff_df2(data_list, dependent_vars)

##Reorder the group variable to make it correct
results_df_2_fig_4_c$dataset=factor(results_df_2_fig_4_c$dataset, levels=c("all", "favorite_color",
                                                                           "gender", "political_party",
                                                                           "race", "religion", "sexual_orientation"))
#Randomization inference SEs
set.seed(8122024)

results_df_2_fig_4_c_ri=results_df_2_fig_4_c
#Make a blank vector called SE that is empty and as long as the original dataframe
se_total=numeric(length(results_df_2_fig_4_c$se))

#Whole dataset:
data <- bva_cargo_data
dependent_var <- c("Consistent_r", "Opinion_r", "Negative_r", "Harmful_r")
se <- calculate_se_multi(data, dependent_var, num_permutations = 3000)

#Favorite color dataset:
data <- bva_cargo_data[bva_cargo_data$category=="Favorite color",]
se_1 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Gender dataset:
data <- bva_cargo_data[bva_cargo_data$category=="Gender",]
se_2 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Political party dataset:
data <- bva_cargo_data[bva_cargo_data$category=="Political party",]
se_3 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Race dataset:
data <- bva_cargo_data[bva_cargo_data$category=="Race",]
se_4 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Religion dataset:
data <- bva_cargo_data[bva_cargo_data$category=="Religion",]
se_5 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Sexual orientation dataset:
data <- bva_cargo_data[bva_cargo_data$category=="Sexual orientation",]
se_6 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Add these to the se_total object
se_total=c(se, se_1, se_2, se_3, se_4, se_5, se_6)

#Rewrite the SE object in results_df_ri
results_df_2_fig_4_c_ri$se=se_total

results_df_2_fig_4_c_ri$dependent_var=factor(results_df_2_fig_4_c_ri$dependent_var, levels=c("Opinion_r", "Consistent_r", "Negative_r", "Harmful_r"))

#Labels for the figure:
new_labels <- c(
  "Consistent_r" = "Consistent",
  "Opinion_r" = "Opinion",
  "Negative_r" = "Negative",
  "Harmful_r" = "Harmful"
)

new_labels2 <- c(
  "all" = "Overall",
  "favorite_color" = "Favorite color",
  "gender" = "Gender",
  "political_party" = "Political party",
  "race" = "Race",
  "religion" = "Religion",
  "sexual_orientation" = "Sexual orientation"
)
# Plot the results
fig_4_ca <- ggplot(results_df_2_fig_4_c_ri, aes(x = dataset, y = diff, color = dataset, linetype = dataset, shape = dataset)) +
  geom_errorbar(aes(ymin = diff - (1.96 * se), ymax = diff + (1.96 * se)), width = 0.05) +
  geom_point(size = 3, position = position_dodge(width = 0.9)) +  # Add points at the center of the error bars
  geom_text(aes(label = sprintf("%.3f", diff), y = diff + (1.96 * se)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5) +
  theme_minimal() +
  theme(legend.position = "none",  # Position the legend at the bottom
        legend.title = element_text(size = 10),  # Adjust legend title size
        legend.text = element_text(size = 9),  # Adjust legend text size
        strip.placement = "outside",  # Ensure all labels are outside the plotting area
        panel.spacing = unit(1, "lines")) +  # Add space between facets
  labs(title = "",
       x = "",
       y = "Difference between the base and instruct models \n(positive indicates more behavior in base models)",
       caption = "") +
  facet_grid(~dependent_var, labeller = labeller(dependent_var = new_labels)) +
  scale_x_discrete(labels = c("all" = "All groups", 
                              "gender" = "Gender",
                              "race" = "Race",
                              "favorite_color" = "Favorite color",
                              "political_party" = "Political party",
                              "religion" = "Religion",
                              "sexual_orientation" = "Sexual orientation")) +
  scale_y_continuous(limits = c(-0.3, 0.55)) +
  scale_shape_manual(values = c(16, 17, 18, 19, 15, 8, 7)) +  # Use different shapes for points
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "2262", "aa", "18", "solid"))  # Use different line types for points
fig_4_ca

library(cowplot)
fig_4=plot_grid(fig_4a+geom_hline(yintercept = 0, linetype = "dashed", color = "gray20")+
                  labs(y="(positive values indicate more behavior in base models)", caption=""), 
                fig_4_ca+geom_hline(yintercept = 0, linetype = "dashed", color = "gray20")+
                  labs(y = "Difference between base and instruct models"), ncol=1,
                labels = c("Sociodemographic Groups", "Cargo Shorts"))
fig_4

fig_4a <- fig_4a + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray20") +
  labs(y = "", caption = "")+
  coord_flip()

# Create the second plot with the horizontal line and customized labels
fig_4_ca <- fig_4_ca + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray20") +
  labs(y = "Difference between base and aligned models (positive values indicate more behavior in base models)")+
  coord_flip()

# Create the plot grid without labels
combined_plot <- plot_grid(fig_4a, fig_4_ca, ncol = 1)

# Add left-justified labels manually using ggdraw and draw_plot_label
fig_4 <- ggdraw() +
  draw_plot(combined_plot, 0, 0, 1, 1) +
  draw_plot_label(
    label = c("Sociodemographic Groups", "Cargo Shorts"),
    x = 0.02, y = c(1, 0.5),  # Position labels, adjust y to fit your layout
    hjust = 0, vjust = 1,  # Left-justify the labels
    size = 14  # Adjust size as needed
  )

fig_4
jpeg(here::here(thisPath(),"Figure 5.jpeg"), width=10, height=10, units="in", res=600)
fig_4
dev.off()

#How to determine where R saved the last file?
getwd()





####Study 2, pigeonholding partisans data####

library(tidyverse)

##Load the data, exists as many files, load each in and merge them##
human_dem <- read_csv(here::here(thisPath(),"human_data/human_dem.csv")) %>% 
  mutate(source = "human",
         family = "human",
         size = "human",
         instruct = "human",
         party = "Democrat",
         caseid = ID)
human_rep <- read_csv(here::here(thisPath(),"human_data/human_rep.csv")) %>%
  mutate(source = "human",
         family = "human",
         size = "human",
         instruct = "human",
         party = "Republican", 
         caseid = ID)
base_dem_gemma_2_9b <- read_csv(here::here(thisPath(),"base/dem-gemma-2-9b.csv")) %>%
  mutate(source = "gemma_2_9b",
         family = "gemma",
         size = "small",
         instruct = "base",
         party = "Democrat")
base_rep_gemma_2_9b <- read_csv(here::here(thisPath(),"base/rep-gemma-2-9b.csv")) %>%
  mutate(source = "gemma_2_9b",
         family = "gemma",
         size = "small",
         instruct = "base",
         party = "Republican")
base_dem_gemma_2_27b <- read_csv(here::here(thisPath(),"base/dem-gemma-2-27b.csv")) %>%
  mutate(source = "gemma_2_27b",
         family = "gemma",
         size = "large",
         instruct = "base",
         party = "Democrat")
base_rep_gemma_2_27b <- read_csv(here::here(thisPath(),"base/rep-gemma-2-27b.csv")) %>%
  mutate(source = "gemma_2_27b",
         family = "gemma",
         size = "large",
         instruct = "base",
         party = "Republican")
base_dem_llama_3_8b <- read_csv(here::here(thisPath(),"base/dem-llama-3-8b.csv")) %>%
  mutate(source = "llama_3_8b",
         family = "llama",
         size = "small",
         instruct = "base",
         party = "Democrat")
base_rep_llama_3_8b <- read_csv(here::here(thisPath(),"base/rep-llama-3-8b.csv")) %>%
  mutate(source = "llama_3_8b",
         family = "llama",
         size = "small",
         instruct = "base",
         party = "Republican")
base_dem_llama_3_70b <- read_csv(here::here(thisPath(),"base/dem-llama-3-70b.csv")) %>%
  mutate(source = "llama_3_70b",
         family = "llama",
         size = "large",
         instruct = "base",
         party = "Democrat")
base_rep_llama_3_70b <- read_csv(here::here(thisPath(),"base/rep-llama-3-70b.csv")) %>%
  mutate(source = "llama_3_70b",
         family = "llama",
         size = "large",
         instruct = "base",
         party = "Republican")
base_dem_mistral_7b <- read_csv(here::here(thisPath(),"base/dem-mistral-7b.csv")) %>%
  mutate(source = "mistral_7b",
         family = "mistral/mixtral",
         size = "small",
         instruct = "base",
         party = "Democrat")
base_rep_mistral_7b <- read_csv(here::here(thisPath(),"base/rep-mistral-7b.csv")) %>%
  mutate(source = "mistral_7b",
         family = "mistral/mixtral",
         size = "small",
         instruct = "base",
         party = "Republican")
base_dem_mixtral <- read_csv(here::here(thisPath(),"base/dem-mixtral.csv")) %>%
  mutate(source = "mixtral",
         family = "mistral/mixtral",
         size = "large",
         instruct = "base",
         party = "Democrat")
base_rep_mixtral <- read_csv(here::here(thisPath(),"base/rep-mistral-7b.csv")) %>%
  mutate(source = "mixtral",
         family = "mistral/mixtral",
         size = "large",
         instruct = "base",
         party = "Republican")
instruct_dem_gemma_2_9b <- read_csv(here::here(thisPath(),"instruct/dem-gemma-2-9b.csv")) %>%
  mutate(source = "gemma_2_9b",
         family = "gemma",
         size = "small",
         instruct = "instruct",
         party = "Democrat")
instruct_rep_gemma_2_9b <- read_csv(here::here(thisPath(),"instruct/rep-gemma-2-9b.csv")) %>%
  mutate(source = "gemma_2_9b",
         family = "gemma",
         size = "small",
         instruct = "instruct",
         party = "Republican")
instruct_dem_gemma_2_27b <- read_csv(here::here(thisPath(),"instruct/dem-gemma-2-27b.csv")) %>%
  mutate(source = "gemma_2_27b",
         family = "gemma",
         size = "large",
         instruct = "instruct",
         party = "Democrat")
instruct_rep_gemma_2_27b <- read_csv(here::here(thisPath(),"instruct/rep-gemma-2-27b.csv")) %>%
  mutate(source = "gemma_2_27b",
         family = "gemma",
         size = "large",
         instruct = "instruct",
         party = "Republican")
instruct_dem_llama_3_8b <- read_csv(here::here(thisPath(),"instruct/dem-llama-3-8b.csv")) %>%
  mutate(source = "llama_3_8b",
         family = "llama",
         size = "small",
         instruct = "instruct",
         party = "Democrat")
instruct_rep_llama_3_8b <- read_csv(here::here(thisPath(),"instruct/rep-llama-3-8b.csv")) %>%
  mutate(source = "llama_3_8b",
         family = "llama",
         size = "small",
         instruct = "instruct",
         party = "Republican")
dem_llama_3_70b <- read_csv(here::here(thisPath(),"instruct/dem-llama-3-70b.csv")) %>%
  mutate(source = "llama_3_70b",
         family = "llama",
         size = "large",
         instruct = "instruct",
         party = "Democrat")
instruct_rep_llama_3_70b <- read_csv(here::here(thisPath(),"instruct/rep-llama-3-70b.csv")) %>%
  mutate(source = "llama_3_70b",
         family = "llama",
         size = "large",
         instruct = "instruct",
         party = "Republican")
instruct_dem_mistral_7b <- read_csv(here::here(thisPath(),"instruct/dem-mistral-7b.csv")) %>%
  mutate(source = "mistral_7b",
         family = "mistral/mixtral",
         size = "small",
         instruct = "instruct",
         party = "Democrat")
instruct_rep_mistral_7b <- read_csv(here::here(thisPath(),"instruct/rep-mistral-7b.csv")) %>%
  mutate(source = "mistral_7b",
         family = "mistral/mixtral",
         size = "small",
         instruct = "instruct",
         party = "Republican")
instruct_dem_mixtral <- read_csv(here::here(thisPath(),"instruct/dem-mixtral.csv")) %>%
  mutate(source = "mixtral",
         family = "mistral/mixtral",
         size = "large",
         instruct = "instruct",
         party = "Democrat")
instruct_rep_mixtral <- read_csv(here::here(thisPath(),"instruct/rep-mixtral.csv")) %>%
  mutate(source = "mixtral",
         family = "mistral/mixtral",
         size = "large",
         instruct = "instruct",
         party = "Republican")

#Merge all the data
all_data2 <- bind_rows(human_dem, human_rep, base_dem_gemma_2_9b, base_rep_gemma_2_9b, base_dem_gemma_2_27b, base_rep_gemma_2_27b, base_dem_llama_3_8b, base_rep_llama_3_8b, base_dem_llama_3_70b, base_rep_llama_3_70b, base_dem_mistral_7b, base_rep_mistral_7b, base_dem_mixtral, base_rep_mixtral, instruct_dem_gemma_2_9b, instruct_rep_gemma_2_9b, instruct_dem_gemma_2_27b, instruct_rep_gemma_2_27b, instruct_dem_llama_3_8b, instruct_rep_llama_3_8b, dem_llama_3_70b, instruct_rep_llama_3_70b, instruct_dem_mistral_7b, instruct_rep_mistral_7b, instruct_dem_mixtral, instruct_rep_mixtral)

### Data cleaning
# Recode the DVs

# Recode the Refusal variable as a binary variable. Score it so "No" is 0 and "Yes" is 1
all_data2$Refusal_r[all_data2$refusal == "No"] = 0
all_data2$Refusal_r[all_data2$refusal == "Yes"] = 1

# Recode the extreme variables as binary variables. Score them so "No" is 0 and "Yes" is 1
all_data2$Extreme_r[all_data2$extreme== "No"] = 0
all_data2$Extreme_r[all_data2$extreme == "Yes"] = 1

# Recode the personality variable as a binary variable. Score it so "No" is 0 and "Yes" is 1
all_data2$Personality_r[all_data2$personality == "No"] = 0
all_data2$Personality_r[all_data2$personality == "Yes"] = 1

# Recode the Policy variable as a binary variable. Score it so "No" is 0 and "Yes" is 1
all_data2$Policy_r[all_data2$policy == "No"] = 0
all_data2$Policy_r[all_data2$policy == "Yes"] = 1

# Recode the social_groups variable as a binary variable. Score it so "No" is 0 and "Yes" is 1
all_data2$Social_groups_r[all_data2$social_groups == "No"] = 0
all_data2$Social_groups_r[all_data2$social_groups == "Yes"] = 1

# Recode the pos_neg variable as a binary variable, sore it so that "A little positive" and "very positive" both count as positive
all_data2$Positive_r[all_data2$pos_neg == "A little positive"] = 1
all_data2$Positive_r[all_data2$pos_neg == "Very positive"] = 1
all_data2$Positive_r[all_data2$pos_neg == "Neither"] = 0
all_data2$Positive_r[all_data2$pos_neg == "Neither positive nor negative"] = 0
all_data2$Positive_r[all_data2$pos_neg == "A little negative"] = 0
all_data2$Positive_r[all_data2$pos_neg == "Very negative"] = 0

# Recode the pos_neg variable as a binary variable, sore it so that "A little negative" and "very negative" both count as negative
all_data2$Negative_r[all_data2$pos_neg == "A little negative"] = 1
all_data2$Negative_r[all_data2$pos_neg == "Very negative"] = 1
all_data2$Negative_r[all_data2$pos_neg == "Neither"] = 0
all_data2$Negative_r[all_data2$pos_neg == "Neither positive nor negative"] = 0
all_data2$Negative_r[all_data2$pos_neg == "A little positive"] = 0
all_data2$Negative_r[all_data2$pos_neg == "Very positive"] = 0

# create a variable for any missing that identifies whether the word "NONE" is included in the columns word1, word2, word3, or word4
all_data2$Missing_r[all_data2$word1 == "NONE" | all_data2$word2 == "NONE" | all_data2$word3 == "NONE" | all_data2$word4 == "NONE"] = 1
all_data2$Missing_r[all_data2$word1 != "NONE" & all_data2$word2 != "NONE" & all_data2$word3 != "NONE" & all_data2$word4 != "NONE"] = 0

# using regular expressions, create an actual_partyid variable by creating a variable that equals "Democrat" if the word "Democrat" appears anywhere in the text of the variable in the prompt column
all_data2$Actual_partyid[grepl("Democrat", all_data2$system)] = "Democrat"
all_data2$Actual_partyid[grepl("Republican", all_data2$system)] = "Republican"
all_data2$Actual_partyid[grepl("Rebublicans", all_data2$system)] = "Republican"
all_data2$Actual_partyid[grepl("independent", all_data2$system)] = "Independent"


#extract PID with the id variable
pid_filter <- filter(all_data2, source == "gemma_2_27b" & party == "Democrat" & instruct == "base") %>% 
  select(caseid, Actual_partyid) %>%
  rename(actual_partyid2 = Actual_partyid)

#merge pid_filter matched on ID just into the data for "human" source in the all_data2 file, update the Actual_partyid column
all_data2 <- left_join(pid_filter, all_data2, by = "caseid", multiple = "all")

#create a variable for correct pid guess

all_data2 <- mutate(all_data2, pid_correct = case_when(partyid == "Democrat" & actual_partyid2 == "Democrat" ~ 1,
                                                       partyid == "Republican" & actual_partyid2 == "Republican" ~ 1,
                                                       partyid == "Independent" & actual_partyid2 == "Independent" ~ 1,
                                                       .default = 0))

##Analysis

#Figure 6
#using groupby, create a dataframe that calculates the mean and standard error for subsets of the data based on family, instruct, and party variables
pp_means <- all_data2 %>%
  filter(source != "gemma_2_9b") %>% 
  group_by(instruct) %>%
  summarize(mean_refusal = mean(Refusal_r, na.rm = TRUE),
            se_refusal = sd(Refusal_r, na.rm = TRUE) / sqrt(n()),
            mean_extreme = mean(Extreme_r, na.rm = TRUE),
            se_extreme = sd(Extreme_r, na.rm = TRUE) / sqrt(n()),
            mean_personality = mean(Personality_r, na.rm = TRUE),
            se_personality = sd(Personality_r, na.rm = TRUE) / sqrt(n()),
            mean_policy = mean(Policy_r, na.rm = TRUE),
            se_policy = sd(Policy_r, na.rm = TRUE) / sqrt(n()),
            mean_social_groups = mean(Social_groups_r, na.rm = TRUE),
            se_social_groups = sd(Social_groups_r, na.rm = TRUE) / sqrt(n()),
            mean_positive = mean(Positive_r, na.rm = TRUE),
            se_positive = sd(Positive_r, na.rm = TRUE) / sqrt(n()),
            mean_negative = mean(Negative_r, na.rm = TRUE),
            se_negative = sd(Negative_r, na.rm = TRUE) / sqrt(n()),
            mean_missing = mean(Missing_r, na.rm = TRUE),
            se_missing = sd(Missing_r, na.rm = TRUE) / sqrt(n()),
            mean_pid_correct= mean(pid_correct, na.rm = TRUE),
            se_pid_correct = sd(pid_correct, na.rm = TRUE) / sqrt(n())) 

# rearrange this data frame to be longer, so all the se and mean are in the same column
pp_means_long_mean <- pp_means %>%
  select(instruct, mean_refusal, mean_extreme, mean_personality, mean_policy, mean_social_groups, mean_positive, mean_negative, mean_missing) %>%
  pivot_longer(cols = c(mean_refusal, mean_extreme, mean_personality, mean_policy, mean_social_groups, mean_positive, mean_negative, mean_missing),
               names_to = "dependent_var",
               values_to = "mean")
#create a new variable that corresponds to the mean and se variables but do not include the mean_ or se_
pp_means_long_mean$dep_var <- str_remove(pp_means_long_mean$dependent_var, "mean_")
#remove the dependent_var column
pp_means_long_mean <- select(pp_means_long_mean, -dependent_var)

pp_means_long_se <- pp_means %>%
  select(instruct, se_refusal, se_extreme, se_personality, se_policy, se_social_groups, se_positive, se_negative, se_missing) %>%
  pivot_longer(cols = c(se_refusal, se_extreme, se_personality, se_policy, se_social_groups, se_positive, se_negative, se_missing),
               names_to = "dependent_var",
               values_to = "se")
#create a new variable that corresponds to the mean and se variables but do not include the mean_ or se_
pp_means_long_se$dep_var <- str_remove(pp_means_long_se$dependent_var, "se_")
#remove the dependent_var column
pp_means_long_se <- select(pp_means_long_se, -dependent_var)

#merge the two parts
pp_means_graph <- left_join(pp_means_long_mean, pp_means_long_se)

#start each of those variable names in dep_var with a capital letter
pp_means_graph$dep_var <- str_to_title(pp_means_graph$dep_var)

#limit it to just traits
pp_means_graph_traits <- filter(pp_means_graph, dep_var == "Personality" | dep_var == "Policy" | dep_var == "Social_groups" | dep_var == "Positive" | dep_var == "Negative" | dep_var == "Extreme")

#capitalize the words in "instruct"
pp_means_graph_traits$instruct <- str_to_title(pp_means_graph_traits$instruct)

#reorder the instruct variable
pp_means_graph_traits$instruct <- factor(pp_means_graph_traits$instruct, levels = c("Human", "Base", "Instruct"))

#in dep_var replace social_groups with social groups
pp_means_graph_traits$dep_var <- str_replace(pp_means_graph_traits$dep_var, "Social_groups", "Social Groups")

#create a new variable called facet, it equals "Content" if dep_var is "Personality" or "Policy" or "Social Groups" and "Tone" if dep_var is "Positive" or "Negative" or "Extreme"
pp_means_graph_traits$facet <- ifelse(pp_means_graph_traits$dep_var == "Personality" | pp_means_graph_traits$dep_var == "Policy" | pp_means_graph_traits$dep_var == "Social Groups", "Content", "Tone")

#order the dep_var variable 
pp_means_graph_traits$dep_var <- factor(pp_means_graph_traits$dep_var, levels = c("Personality", "Policy", "Social Groups", "Positive", "Negative", "Extreme"))

##Randomization inference calculations for the SEs
set.seed(8152024)

pp_means_graph_traits_ri=pp_means_graph_traits

#We need to write over the se column with randomization inference calculations
calculate_se <- function(data, dependent_vars, group_var, num_permutations = 3000) {
  # Initialize an empty list to store results
  results <- list()
  
  # Loop over each dependent variable
  for (j in seq_along(dependent_vars)) {
    # Initialize an empty matrix to store the means for each group
    means <- matrix(NA, nrow = num_permutations, ncol = 3) # One column for each value of group_var
    
    # Reshuffle and calculate the means for each dependent variable
    for (i in 1:num_permutations) {
      # Randomly reshuffle the "instruct" variable
      shuffled_data <- data
      shuffled_data$instruct <- sample(data$instruct, replace = FALSE)
      
      # Calculate the mean of the dependent variable for each value of group_var
      for (g in 1:3) {
        group_value <- unique(data[[group_var]])[g]
        means[i, g] <- mean(shuffled_data[[dependent_vars[j]]][shuffled_data[[group_var]] == group_value], na.rm = TRUE)
      }
    }
    
    # Calculate the standard error (SE) for the current dependent variable for each group
    se_values <- apply(means, 2, sd)
    
    # Create a data.frame with SEs, dependent variable, and group variable
    results[[j]] <- data.frame(
      Dependent_Variable = dependent_vars[j],
      Group_Variable = unique(data[[group_var]]),
      SE = se_values
    )
  }
  
  # Combine all results into a single data.frame
  final_results <- do.call(rbind, results)
  
  return(final_results)
}

#Exclude gemma-2-9b because it can't really do the task
data <- all_data2[all_data2$source!="gemma_2_9b",]
dependent_vars <- c("Extreme_r", "Personality_r", "Policy_r", "Social_groups_r",
                    "Positive_r", "Negative_r")
# Call the function to calculate SEs
se <- calculate_se(data, dependent_vars, "instruct", num_permutations = 3000)


#Rewrite the SE object
#to do that, first I need to rearrange/recode the data a bit

#First, rename the Group_variable column to instruct
se$instruct <- se$Group_Variable
se$Group_Variable <- NULL
#Then put the instruct variable to have sentence capitalization
se$instruct <- str_to_title(se$instruct)

#Then rename Dependent_Variable to dep_var
se$dep_var <- se$Dependent_Variable
se$Dependent_Variable <- NULL
#Then drop the _r from the values of dep_var
se$dep_var <- str_remove(se$dep_var, "_r")
se

#Finally, so the values are ordered the same way, sort the se object by the instruct variable
se <- se[order(se$instruct),]

#Now, I need to merge the se object with the pp_means_graph_traits_ri object
pp_means_graph_traits_ri$se=se$SE

#Now we can plot with just that!
fig_5_ri <- ggplot(pp_means_graph_traits_ri, aes(x = instruct, y = mean, fill = instruct)) +
  geom_bar(stat = "identity", position = position_dodge(width = .9)) +
  geom_errorbar(aes(ymin = mean - (1.96 * se), ymax = mean + (1.96 * se)), width = 0.05, , position = position_dodge(width = .9)) +
  geom_text(aes(label = sprintf("%.3f", mean), y = mean + (1.96 * se)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5) +
  theme_minimal() + 
  facet_wrap(~dep_var) +
  theme(legend.position = "bottom",  # Position the legend at the bottom
        legend.title = element_text(size = 10),  # Adjust legend title size
        legend.text = element_text(size = 9),  # Adjust legend text size
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 12),
        strip.placement = "outside",  # Ensure all labels are outside the plotting area
        panel.spacing = unit(1, "lines")) +  # Add space between facets
  scale_fill_manual(values = c("#3A3A3A", "#7F7F7F", "#BFBFBF", "#E6E6E6"),  # Assign distinct gray colors to groups
                    name = "Data Source",
                    labels = c("Human", 
                               "Base model", 
                               "Aligned model")) +  # Legend title
  labs(title = "",
       x = "",
       y = "Proportion of responses",
       caption = "") +
  scale_y_continuous(limits = c(0, 1))  #set y axis min and max values
fig_5_ri

jpeg(here::here(thisPath(),"Figure 6.jpeg"), width=15, height=15, units="in", res=600)
fig_5_ri
dev.off()

#Figure 7
#using groupby, create a dataframe that calculates the mean and standard error for subsets of the data based on family, instruct, and party variables
pp_means2 <- all_data2 %>%
  filter(source != "gemma_2_9b") %>% 
  group_by(instruct, party) %>%
  summarize(mean_refusal = mean(Refusal_r, na.rm = TRUE),
            se_refusal = sd(Refusal_r, na.rm = TRUE) / sqrt(n()),
            mean_extreme = mean(Extreme_r, na.rm = TRUE),
            se_extreme = sd(Extreme_r, na.rm = TRUE) / sqrt(n()),
            mean_personality = mean(Personality_r, na.rm = TRUE),
            se_personality = sd(Personality_r, na.rm = TRUE) / sqrt(n()),
            mean_policy = mean(Policy_r, na.rm = TRUE),
            se_policy = sd(Policy_r, na.rm = TRUE) / sqrt(n()),
            mean_social_groups = mean(Social_groups_r, na.rm = TRUE),
            se_social_groups = sd(Social_groups_r, na.rm = TRUE) / sqrt(n()),
            mean_positive = mean(Positive_r, na.rm = TRUE),
            se_positive = sd(Positive_r, na.rm = TRUE) / sqrt(n()),
            mean_negative = mean(Negative_r, na.rm = TRUE),
            se_negative = sd(Negative_r, na.rm = TRUE) / sqrt(n()),
            mean_missing = mean(Missing_r, na.rm = TRUE),
            se_missing = sd(Missing_r, na.rm = TRUE) / sqrt(n()),
            mean_pid_correct= mean(pid_correct, na.rm = TRUE),
            se_pid_correct = sd(pid_correct, na.rm = TRUE) / sqrt(n())) 

# rearrange this data frame to be longer, so all the se and mean are in the same column
pp_means2_long_mean <- pp_means2 %>%
  select(instruct, party, mean_refusal, mean_extreme, mean_personality, mean_policy, mean_social_groups, mean_positive, mean_negative, mean_missing) %>%
  pivot_longer(cols = c(mean_refusal, mean_extreme, mean_personality, mean_policy, mean_social_groups, mean_positive, mean_negative, mean_missing),
               names_to = "dependent_var",
               values_to = "mean")
#create a new variable that corresponds to the mean and se variables but do not include the mean_ or se_
pp_means2_long_mean$dep_var <- str_remove(pp_means2_long_mean$dependent_var, "mean_")
#remove the dependent_var column
pp_means2_long_mean <- select(pp_means2_long_mean, -dependent_var)

pp_means2_long_se <- pp_means2 %>%
  select(instruct, party, se_refusal, se_extreme, se_personality, se_policy, se_social_groups, se_positive, se_negative, se_missing) %>%
  pivot_longer(cols = c(se_refusal, se_extreme, se_personality, se_policy, se_social_groups, se_positive, se_negative, se_missing),
               names_to = "dependent_var",
               values_to = "se")
#create a new variable that corresponds to the mean and se variables but do not include the mean_ or se_
pp_means2_long_se$dep_var <- str_remove(pp_means2_long_se$dependent_var, "se_")
#remove the dependent_var column
pp_means2_long_se <- select(pp_means2_long_se, -dependent_var)

#merge the two parts
pp_means2_graph <- left_join(pp_means2_long_mean, pp_means2_long_se)

#start each of those variable names in dep_var with a capital letter
pp_means2_graph$dep_var <- str_to_title(pp_means2_graph$dep_var)

#limit it to just traits
pp_means2_graph_traits <- filter(pp_means2_graph, dep_var == "Personality" | dep_var == "Policy" | dep_var == "Social_groups" | dep_var == "Positive" | dep_var == "Negative" | dep_var == "Extreme")


#capitalize the words in "instruct"
pp_means2_graph_traits$instruct <- str_to_title(pp_means2_graph_traits$instruct)

#reorder the instruct variable
pp_means2_graph_traits$instruct <- factor(pp_means2_graph_traits$instruct, levels = c("Human", "Base", "Instruct"))

#in dep_var replace social_groups with social groups
pp_means2_graph_traits$dep_var <- str_replace(pp_means2_graph_traits$dep_var, "Social_groups", "Social Groups")

#create a new variable called facet, it equals "Content" if dep_var is "Personality" or "Policy" or "Social Groups" and "Tone" if dep_var is "Positive" or "Negative" or "Extreme"
pp_means2_graph_traits$facet <- ifelse(pp_means2_graph_traits$dep_var == "Personality" | pp_means2_graph_traits$dep_var == "Policy" | pp_means2_graph_traits$dep_var == "Social Groups", "Content", "Tone")

#order the dep_var variable 
pp_means2_graph_traits$dep_var <- factor(pp_means2_graph_traits$dep_var, levels = c("Personality", "Policy", "Social Groups", "Positive", "Negative", "Extreme"))

#create a new variable that is the combination of instruct and party
pp_means2_graph_traits$instruct_party <- paste(pp_means2_graph_traits$instruct, pp_means2_graph_traits$party)

#reorder the instruct_party variable
pp_means2_graph_traits$instruct_party <- factor(pp_means2_graph_traits$instruct_party, levels = c("Human Democrat", "Human Republican", "Base Democrat", "Base Republican", "Instruct Democrat", "Instruct Republican"))



##Randomization inference calculations for the SEs
set.seed(8152024)

pp_means2_graph_traits_ri=pp_means2_graph_traits

#We need to write over the se column with randomization inference calculations
calculate_se_party <- function(data, dependent_vars, group_var, party_var, num_permutations = 3000) {
  # Initialize an empty list to store results
  results <- list()
  
  # Loop over each dependent variable
  for (j in seq_along(dependent_vars)) {
    # Loop over each value of the party variable
    for (party_value in unique(data[[party_var]])) {
      # Filter data by the current party value
      party_data <- data[data[[party_var]] == party_value, ]
      
      # Initialize an empty matrix to store the means for each group
      means <- matrix(NA, nrow = num_permutations, ncol = 3) # One column for each value of group_var
      
      # Reshuffle and calculate the means for each dependent variable
      for (i in 1:num_permutations) {
        # Randomly reshuffle the "instruct" variable within the filtered party data
        shuffled_data <- party_data
        shuffled_data$instruct <- sample(party_data$instruct, replace = FALSE)
        
        # Calculate the mean of the dependent variable for each value of group_var within the current party value
        for (g in 1:3) {
          group_value <- unique(party_data[[group_var]])[g]
          means[i, g] <- mean(shuffled_data[[dependent_vars[j]]][shuffled_data[[group_var]] == group_value], na.rm = TRUE)
        }
      }
      
      # Calculate the standard error (SE) for the current dependent variable for each group
      se_values <- apply(means, 2, sd)
      
      # Create a data.frame with SEs, dependent variable, group variable, and party variable
      results[[length(results) + 1]] <- data.frame(
        Dependent_Variable = dependent_vars[j],
        Group_Variable = unique(party_data[[group_var]]),
        Party_Variable = party_value,
        SE = se_values
      )
    }
  }
  
  # Combine all results into a single data.frame
  final_results <- do.call(rbind, results)
  
  return(final_results)
}

#Exclude gemma-2-9b because it can't really do the task
data <- all_data2[all_data2$source!="gemma_2_9b",]
dependent_vars <- c("Extreme_r", "Personality_r", "Policy_r", "Social_groups_r",
                    "Positive_r", "Negative_r")
# Call the function to calculate SEs
se <- calculate_se_party(data, dependent_vars, "instruct", "party", num_permutations = 3000)

#Rewrite the SE object
#to do that, first I need to rearrange/recode the data a bit

#First, rename the Group_variable column to instruct
se$instruct <- se$Group_Variable
se$Group_Variable <- NULL
#Then put the instruct variable to have sentence capitalization
se$instruct <- str_to_title(se$instruct)

#Then rename Dependent_Variable to dep_var
se$dep_var <- se$Dependent_Variable
se$Dependent_Variable <- NULL
#Then drop the _r from the values of dep_var
se$dep_var <- str_remove(se$dep_var, "_r")
#Add a new variable called instruct_party that combines instruct and party
se$instruct_party <- paste(se$instruct, se$Party_Variable)
se

#Finally, so the values are ordered the same way, sort the se object by the instruct_party variable
se <- se[order(se$instruct_party),]

#Now, I need to merge the se object with the pp_means_graph_traits_ri object
pp_means2_graph_traits_ri$se=se$SE

# Plot the results
fig_6_ri <- ggplot(pp_means2_graph_traits_ri, aes(x = instruct, y = mean, fill = instruct_party)) +
  geom_bar(stat = "identity", position = position_dodge(width = .9)) +
  geom_errorbar(aes(ymin = mean - (1.96 * se), ymax = mean + (1.96 * se)), width = 0.05, , position = position_dodge(width = .9)) +
  geom_text(aes(label = sprintf("%.3f", mean), y = mean + (1.96 * se)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5) +
  theme_minimal() + 
  facet_wrap(~dep_var) +
  theme(legend.position = "bottom",  # Position the legend at the bottom
        legend.title = element_text(size = 10),  # Adjust legend title size
        legend.text = element_text(size = 9),  # Adjust legend text size
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 12),
        strip.placement = "outside",  # Ensure all labels are outside the plotting area
        panel.spacing = unit(1, "lines")) +  # Add space between facets
  scale_fill_manual(values = c("Blue4", "Red4", "Blue3", "Red3", "Blue1", "Red1"),  # Assign distinct gray colors to groups
                    name = "Data Source and Target",
                    labels = c("Human Democrat",
                               "Human Republican",
                               "Base Democrat",
                               "Base Republican",
                               "Aligned Democrat",
                               "Aligned Republican")) +  # Legend title
  labs(title = "",
       x = "",
       y = "Proportion of responses",
       caption = "") +
  scale_y_continuous(limits = c(0, 1))  #set y axis min and max values
fig_6_ri

jpeg(here::here(thisPath(),"Figure 7.jpeg"), width=15, height=15, units="in", res=600)
fig_6_ri
dev.off()


####Online appendix analyses:####

##Additional coder agreement statistics for Table A.2
#add in a random, low, and high coder as new columns to ratings2
set.seed(09022025)
ratings2$random_coder=sample(1:2, nrow(ratings2), replace=TRUE)
ratings2$low_coder=rep(1, nrow(ratings2))
ratings2$high_coder=rep(2, nrow(ratings2))

#3 human coders:
#Percent agreement
agree(ratings2[,c(1,2,3)])
#Fleiss kappa
kappam.fleiss(ratings2[,c(1,2,3)])
#Light's kappa
kappam.light(ratings2[,c(1,2,3)])
#ICC one way
icc(ratings2[,c(1,2,3)], model="oneway")
#ICC two way
icc(ratings2[,c(1,2,3)], model="twoway", type="agreement")

#Adding in GPT-4o
#Percent agreement
agree(ratings2[,c(1,2,3,4)])
#Fleiss kappa
kappam.fleiss(ratings2[,c(1,2,3,4)])
#Light's kappa
kappam.light(ratings2[,c(1,2,3,4)])
#ICC one way
icc(ratings2[,c(1,2,3,4)], model="oneway")
#ICC two way
icc(ratings2[,c(1,2,3,4)], model="twoway", type="agreement")

#Adding random coder:
#Percent agreement
agree(ratings2[,c(1,2,3,5)])
#Fleiss kappa
kappam.fleiss(ratings2[,c(1,2,3,5)])
#Light's kappa
kappam.light(ratings2[,c(1,2,3,5)])
#ICC one way
icc(ratings2[,c(1,2,3,5)], model="oneway")
#ICC two way
icc(ratings2[,c(1,2,3,5)], model="twoway", type="agreement")

#Low coder
#Percent agreement
agree(ratings2[,c(1,2,3,6)])
#Fleiss kappa
kappam.fleiss(ratings2[,c(1,2,3,6)])
#Light's kappa
kappam.light(ratings2[,c(1,2,3,6)])
#ICC one way
icc(ratings2[,c(1,2,3,6)], model="oneway")
#ICC two way
icc(ratings2[,c(1,2,3,6)], model="twoway", type="agreement")

#High coder
#Percent agreement
agree(ratings2[,c(1,2,3,7)])
#Fleiss kappa
kappam.fleiss(ratings2[,c(1,2,3,7)])
#Light's kappa
kappam.light(ratings2[,c(1,2,3,7)])
#ICC one way
icc(ratings2[,c(1,2,3,7)], model="oneway")
#ICC two way
icc(ratings2[,c(1,2,3,7)], model="twoway", type="agreement")

##Figure A.1##
# Function to calculate mean and standard error for a given dataset, instruction, prompting, and dependent variable
calculate_stats <- function(data, dependent_var, instruction, prompting = NULL) {
  if (is.null(prompting)) {
    mean_val <- mean(data[[dependent_var]][data$instruct == instruction], na.rm = TRUE)
    se_val <- sd(data[[dependent_var]][data$instruct == instruction], na.rm = TRUE) / sqrt(sum(!is.na(data[[dependent_var]][data$instruct == instruction])))
  } else {
    mean_val <- mean(data[[dependent_var]][data$instruct == instruction & data$prompting == prompting], na.rm = TRUE)
    se_val <- sd(data[[dependent_var]][data$instruct == instruction & data$prompting == prompting], na.rm = TRUE) / sqrt(sum(!is.na(data[[dependent_var]][data$instruct == instruction & data$prompting == prompting])))
  }
  return(list(mean = mean_val, se = se_val))
}

# Function to generate the dataframe with mean and standard error for different groups
generate_diff_df <- function(data_list, dependent_vars, instructions, promptings) {
  results <- data.frame(mean = numeric(0), se = numeric(0), group = character(0), instruct = character(0), dependent_var = character(0), dataset = character(0))
  
  for (data_name in names(data_list)) {
    for (dep_var in dependent_vars) {
      for (i in 1:length(instructions)) {
        if (is.null(promptings[[i]])) {
          stats <- calculate_stats(data_list[[data_name]], dep_var, instructions[i])
          group_label <- "Base model"
        } else {
          stats <- calculate_stats(data_list[[data_name]], dep_var, instructions[i], promptings[[i]])
          group_label <- paste("Instruct model (", promptings[[i]], " prompt)", sep = "")
        }
        results <- rbind(results, data.frame(mean = stats$mean, se = stats$se, group = group_label, instruct = instructions[i], dependent_var = dep_var, dataset = data_name))
      }
    }
  }
  
  return(results)
}

data_list <- list(all = topic_data,
                  gemma_2_9b = topic_data[topic_data$model=="gemma-2"&topic_data$parameters=="9b",],
                  gemma_2_27b = topic_data[topic_data$model=="gemma-2"&topic_data$parameters=="27b",],
                  llama_3_8b = topic_data[topic_data$model=="llama-3"&topic_data$parameters=="8b",],
                  llama_3_70b = topic_data[topic_data$model=="llama-3"&topic_data$parameters=="70b",],
                  mistral = topic_data[topic_data$model=="mistral",],
                  mixtral = topic_data[topic_data$model=="mixtral",]
)
dependent_vars <- c("Refusal_r", "Moralizing_r", "Assistant_commentary_r") 

# Define the instructions and corresponding promptings
instructions <- c("base", "instruct", "instruct", "instruct")
promptings <- list(NULL, "baseline", "basic", "advanced")

# Generate the results dataframe for all datasets and dependent variables
results_df_a_2 <- generate_diff_df(data_list, dependent_vars, instructions, promptings)

##Reorder the group variable to make it correct
results_df_a_2$group=factor(results_df_a_2$group, levels=c("Base model", "Instruct model (baseline prompt)", "Instruct model (basic prompt)", "Instruct model (advanced prompt)"))

#Randomization inference SEs
set.seed(8122024)

results_df_a_2_ri=results_df_a_2
#Make a blank vector called SE that is empty and as long as the original dataframe
se_total=numeric(length(results_df_a_2$se))

# Function to perform the permutation test and calculate SEs
calculate_se <- function(data, dependent_vars, num_permutations = 1000) {
  # Initialize the SE vector
  se <- numeric(length(dependent_vars) * 4)
  
  # Loop over each dependent variable
  for (j in seq_along(dependent_vars)) {
    # Initialize an empty data frame to store the means
    means_df <- data.frame(
      mean_value1 = numeric(num_permutations),
      mean_value2 = numeric(num_permutations),
      mean_value3 = numeric(num_permutations),
      mean_value4 = numeric(num_permutations)
    )
    
    # Reshuffle and calculate means
    for (i in 1:num_permutations) {
      # Randomly reshuffle the current dependent variable
      shuffled_data <- data
      shuffled_data[[dependent_vars[j]]] <- sample(data[[dependent_vars[j]]], replace = FALSE)
      
      # Calculate the means for the four conditions
      means_df[i, ] <- c(
        mean(shuffled_data[[dependent_vars[j]]][shuffled_data$instruct == "base"], na.rm = TRUE),
        mean(shuffled_data[[dependent_vars[j]]][shuffled_data$instruct == "instruct" & shuffled_data$prompting == "baseline"], na.rm = TRUE),
        mean(shuffled_data[[dependent_vars[j]]][shuffled_data$instruct == "instruct" & shuffled_data$prompting == "basic"], na.rm = TRUE),
        mean(shuffled_data[[dependent_vars[j]]][shuffled_data$instruct == "instruct" & shuffled_data$prompting == "advanced"], na.rm = TRUE)
      )
    }
    
    # Calculate the standard errors for each condition
    se[((j-1)*4+1):(j*4)] <- apply(means_df, 2, sd)
  }
  
  return(se)
}

#Whole dataset:
data <- topic_data
dependent_vars <- c("Refusal_r", "Moralizing_r", "Assistant_commentary_r") 
# Call the function to calculate SEs
se <- calculate_se(data, dependent_vars, num_permutations = 3000)


#Gemma 2_9b dataset:
data <- topic_data[topic_data$model=="gemma-2"&topic_data$parameters=="9b",]
se_2 <- calculate_se(data, dependent_vars, num_permutations = 3000)

#Gemma 2_27b dataset:
data <- topic_data[topic_data$model=="gemma-2"&topic_data$parameters=="27b",]
se_3 <- calculate_se(data, dependent_vars, num_permutations = 3000)

#Llama 3_8b dataset:
data <- topic_data[topic_data$model=="llama-3"&topic_data$parameters=="8b",]
se_4 <- calculate_se(data, dependent_vars, num_permutations = 3000)

#Llama 3_70b dataset:
data <- topic_data[topic_data$model=="llama-3"&topic_data$parameters=="70b",]
se_5 <- calculate_se(data, dependent_vars, num_permutations = 3000)

#Mistral dataset:
data <- topic_data[topic_data$model=="mistral",]
se_6 <- calculate_se(data, dependent_vars, num_permutations = 3000)

#Mixtral dataset:
data <- topic_data[topic_data$model=="mixtral",]
se_7 <- calculate_se(data, dependent_vars, num_permutations = 3000)

#Add these to the se_total object
se_total=c(se, se_2, se_3, se_4, se_5, se_6, se_7)

#Rewrite the SE object in results_df_a_ri
results_df_a_2_ri$se=se_total

results_df_a_2_ri$group=factor(results_df_a_2_ri$group, levels=c("Base model", "Instruct model (baseline prompt)", "Instruct model (basic prompt)", "Instruct model (advanced prompt)"))
results_df_a_2_ri$dependent_var=factor(results_df_a_2_ri$dependent_var, levels=c("Refusal_r", "Moralizing_r", "Assistant_commentary_r"))
#Labels for the figure:
new_labels <- c(
  "Assistant_commentary_r" = "Assistant commentary",
  "Moralizing_r" = "Moralizing",
  "Refusal_r" = "Refusal"
)
# Plot the results
fig_1a=ggplot(results_df_a_2_ri, aes(x = group, y = mean, fill = group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean - (1.96 * se), ymax = mean + (1.96 * se)), width = 0.05) +
  geom_text(aes(label = sprintf("%.3f", mean), y = mean + (1.96 * se)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5) +
  theme_minimal() +
  theme(legend.position = "bottom",  # Position the legend at the bottom
        legend.title = element_text(size = 10),  # Adjust legend title size
        legend.text = element_text(size = 9),  # Adjust legend text size
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 15),
        #axis.text.x = element_blank(),  # Remove x-axis labels
        #axis.ticks.x = element_blank(),  # Remove x-axis ticks
        strip.placement = "outside",  # Ensure all labels are outside the plotting area
        panel.spacing = unit(1, "lines")) +  # Add space between facets
  scale_fill_manual(values = c("#3A3A3A", "#7F7F7F", "#BFBFBF", "#E6E6E6"),  # Assign distinct gray colors to groups
                    name = "Model and Prompt Type",  # Legend title
                    labels = c("Base model", 
                               "Aligned model (base prompt)", 
                               "Aligned model (basic prompt)", 
                               "Aligned model (advanced prompt)")) +  # Legend labels
  scale_x_discrete(labels = c("Base", 
                              "", 
                              "Aligned", 
                              "")) +
  labs(title = "",
       x = "",
       y = "Proportion of responses",
       caption = "") +
  facet_grid(dataset~dependent_var, axes="all_x", labeller = labeller(dependent_var=new_labels))
fig_1a

jpeg(here::here(thisPath(),"Figure A.1_appendix.jpeg"), width=10, height=15, units="in", res=600)
fig_1a
dev.off()

##Figure A.2##
data_list <- list(all = bva_topic_data,
                  gemma_2_9b = bva_topic_data[bva_topic_data$model=="gemma-2"&bva_topic_data$parameters=="9b",],
                  gemma_2_27b = bva_topic_data[bva_topic_data$model=="gemma-2"&bva_topic_data$parameters=="27b",],
                  llama_3_8b = bva_topic_data[bva_topic_data$model=="llama-3"&bva_topic_data$parameters=="8b",],
                  llama_3_70b = bva_topic_data[bva_topic_data$model=="llama-3"&bva_topic_data$parameters=="70b",],
                  mistral = bva_topic_data[bva_topic_data$model=="mistral",],
                  mixtral = bva_topic_data[bva_topic_data$model=="mixtral",]
)

#Change DVs to consistent, opinion, negative, and harmful
dependent_vars <- c("Consistent_r", "Opinion_r", "Negative_r", "Harmful_r")

# Define the instructions and corresponding promptings
instructions <- c("base", "instruct")
promptings <- list(NULL, "advanced")

# Generate the results dataframe for all datasets and dependent variables
results_df_2_fig_2_a <- generate_diff_df(data_list, dependent_vars, instructions, promptings)

##Reorder the group variable to make it correct
results_df_2_fig_2_a$group=factor(results_df_2_fig_2_a$group, levels=c("Base model", "Instruct model (advanced prompt)"))

#Randomization inference SEs
set.seed(8122024)

results_df_2_fig_2_a_ri=results_df_2_fig_2_a
#Make a blank vector called SE that is empty and as long as the original dataframe
se_total=numeric(length(results_df_2_fig_2_a$se))

#Randomized SEs
calculate_se <- function(data, dependent_vars, num_permutations = 3000) {
  # Initialize the SE vector
  se <- numeric(length(dependent_vars) * 2)
  
  # Loop over each dependent variable
  for (j in seq_along(dependent_vars)) {
    # Initialize an empty data frame to store the means
    means_df <- data.frame(
      mean_value1 = numeric(num_permutations),
      mean_value2 = numeric(num_permutations)
    )
    
    # Reshuffle and calculate means
    for (i in 1:num_permutations) {
      # Randomly reshuffle the current dependent variable
      shuffled_data <- data
      shuffled_data[[dependent_vars[j]]] <- sample(data[[dependent_vars[j]]], replace = FALSE)
      
      # Calculate the means for the four conditions
      means_df[i, ] <- c(
        mean(shuffled_data[[dependent_vars[j]]][shuffled_data$prompting == "baseline"], na.rm = TRUE),
        mean(shuffled_data[[dependent_vars[j]]][shuffled_data$prompting == "advanced"], na.rm = TRUE)
      )
    }
    
    # Calculate the standard errors for each condition
    se[((j-1)*2+1):(j*2)] <- apply(means_df, 2, sd)  }
  
  return(se)
}

#Whole dataset:
data <- bva_topic_data
dependent_vars <- c("Consistent_r", "Opinion_r", "Negative_r", "Harmful_r")
# Call the function to calculate SEs
se <- calculate_se(data, dependent_vars, num_permutations = 3000)

#Gemma 2_9b dataset:
data <- bva_topic_data[bva_topic_data$model=="gemma-2"&bva_topic_data$parameters=="9b",]
se_2 <- calculate_se(data, dependent_vars, num_permutations = 3000)

#Gemma 2_27b dataset:
data <- bva_topic_data[bva_topic_data$model=="gemma-2"&bva_topic_data$parameters=="27b",]
se_3 <- calculate_se(data, dependent_vars, num_permutations = 3000)

#Llama 3_8b dataset:
data <- bva_topic_data[bva_topic_data$model=="llama-3"&bva_topic_data$parameters=="8b",]
se_4 <- calculate_se(data, dependent_vars, num_permutations = 3000)

#Llama 3_70b dataset:
data <- bva_topic_data[bva_topic_data$model=="llama-3"&bva_topic_data$parameters=="70b",]
se_5 <- calculate_se(data, dependent_vars, num_permutations = 3000)

#Mistral dataset:
data <- bva_topic_data[bva_topic_data$model=="mistral",]
se_6 <- calculate_se(data, dependent_vars, num_permutations = 3000)

#Mixtral dataset:
data <- bva_topic_data[bva_topic_data$model=="mixtral",]
se_7 <- calculate_se(data, dependent_vars, num_permutations = 3000)

#Add these to the se_total object
se_total=c(se, se_2, se_3, se_4, se_5, se_6, se_7)

#Rewrite the SE object in results_df_ri
results_df_2_fig_2_a_ri$se=se_total

results_df_2_fig_2_a_ri$group=factor(results_df_2_fig_2_a_ri$group, levels=c("Base model", "Instruct model (baseline prompt)", "Instruct model (basic prompt)", "Instruct model (advanced prompt)"))
results_df_2_fig_2_a_ri$dependent_var=factor(results_df_2_fig_2_a_ri$dependent_var, levels=c("Opinion_r", "Consistent_r", "Negative_r", "Harmful_r"))

#Labels for the figure:
new_labels <- c(
  "Consistent_r" = "Consistent",
  "Opinion_r" = "Opinion",
  "Negative_r" = "Negative",
  "Harmful_r" = "Harmful"
)
# Plot the results
fig_2_a=ggplot(results_df_2_fig_2_a_ri, aes(x = group, y = mean, fill = group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean - (1.96 * se), ymax = mean + (1.96 * se)), width = 0.05) +
  geom_text(aes(label = sprintf("%.3f", mean), y = mean + (1.96 * se)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5) +
  theme_minimal() +
  theme(legend.position = "bottom",  # Position the legend at the bottom
        legend.title = element_text(size = 10),  # Adjust legend title size
        legend.text = element_text(size = 9),  # Adjust legend text size
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 15),
        #axis.text.x = element_blank(),  # Remove x-axis labels
        #axis.ticks.x = element_blank(),  # Remove x-axis ticks
        strip.placement = "outside",  # Ensure all labels are outside the plotting area
        panel.spacing = unit(1, "lines")) +  # Add space between facets
  scale_fill_manual(values = c("#3A3A3A", "#E6E6E6"),  # Assign distinct gray colors to groups
                    name = "Model and Prompt Type",  # Legend title
                    labels = c("Base model", 
                               "Aligned model (advanced prompt)")) +  # Legend labels
  scale_x_discrete(labels = c("Base",
                              "Aligned")) +
  scale_y_continuous(limits = c(-0.1, 1)) +
  labs(title = "",
       x = "",
       y = "Proportion of responses",
       caption = "") +
  facet_grid(dataset~dependent_var, axes="all_x", labeller = labeller(dependent_var=new_labels))
fig_2_a

jpeg(here::here(thisPath(),"Figure A.2.jpeg"), width=10, height=15, units="in", res=600)
fig_2_a
dev.off()

##Figure A.3##
#Should involve averaging over a different element of the data - the self variable

#Differences dataset:
data_list <- list(all = bva_topic_data,
                  white = bva_topic_data[bva_topic_data$self=="White",],
                  black = bva_topic_data[bva_topic_data$self=="Black",],
                  hispanic = bva_topic_data[bva_topic_data$self=="Hispanic",],
                  asian = bva_topic_data[bva_topic_data$self=="Asian",],
                  male = bva_topic_data[bva_topic_data$self=="male",],
                  female = bva_topic_data[bva_topic_data$self=="female",],
                  non_binary = bva_topic_data[bva_topic_data$self=="non-binary person",],
                  christian = bva_topic_data[bva_topic_data$self=="Christian",],
                  jewish = bva_topic_data[bva_topic_data$self=="Jewish",],
                  muslim = bva_topic_data[bva_topic_data$self=="Muslim",],
                  atheist = bva_topic_data[bva_topic_data$self=="atheist",],
                  straight = bva_topic_data[bva_topic_data$self=="straight",],
                  gay = bva_topic_data[bva_topic_data$self=="gay",],
                  lesbian = bva_topic_data[bva_topic_data$self=="lesbian",],
                  bisexual = bva_topic_data[bva_topic_data$self=="bisexual",],
                  republican = bva_topic_data[bva_topic_data$self=="Republican",],
                  independent = bva_topic_data[bva_topic_data$self=="Independent",],
                  democrat = bva_topic_data[bva_topic_data$self=="Democrat",]
)

#Change DVs to consistent, opinion, negative, and harmful
dependent_vars <- c("Consistent_r", "Opinion_r", "Negative_r", "Harmful_r")

# Generate the results dataframe for all datasets and dependent variables
results_df_2_fig_4_r3 <- generate_diff_df2(data_list, dependent_vars)

##Reorder the group variable to make it correct
results_df_2_fig_4_r3$dataset=factor(results_df_2_fig_4_r3$dataset, levels=c("all", "white", "black","hispanic",
                                                                             "asian", "male", "female", "non_binary",
                                                                             "christian", "jewish", "muslim", "atheist",
                                                                             "straight", "gay", "lesbian", "bisexual",
                                                                             "republican", "independent", "democrat"))


#RI standard errors:
data <- bva_topic_data
dependent_var <- c("Consistent_r", "Opinion_r", "Negative_r", "Harmful_r")
se <- calculate_se_multi(data, dependent_var, num_permutations = 3000)

#White dataset:
data <- bva_topic_data[bva_topic_data$self=="White",]
se_1 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#black dataset:
data <- bva_topic_data[bva_topic_data$self=="Black",]
se_2 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Hispanic dataset
data <- bva_topic_data[bva_topic_data$self=="Hispanic",]
se_3 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Asian dataset
data <- bva_topic_data[bva_topic_data$self=="Asian",]
se_4 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Male dataset
data <- bva_topic_data[bva_topic_data$self=="male",]
se_5 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Female
data <- bva_topic_data[bva_topic_data$self=="female",]
se_6 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Non-binary
data <- bva_topic_data[bva_topic_data$self=="non-binary person",]
se_7 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Christian
data <- bva_topic_data[bva_topic_data$self=="Christian",]
se_8 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Jewish
data <- bva_topic_data[bva_topic_data$self=="Jewish",]
se_9 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Muslim
data <- bva_topic_data[bva_topic_data$self=="Muslim",]
se_10 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Atheist
data <- bva_topic_data[bva_topic_data$self=="atheist",]
se_11 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Straight
data <- bva_topic_data[bva_topic_data$self=="straight",]
se_12 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Gay
data <- bva_topic_data[bva_topic_data$self=="gay",]
se_13 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Lesbian
data <- bva_topic_data[bva_topic_data$self=="lesbian",]
se_14 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Bisexual
data <- bva_topic_data[bva_topic_data$self=="bisexual",]
se_15 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Republican
data <- bva_topic_data[bva_topic_data$self=="Republican",]
se_16 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Independent
data <- bva_topic_data[bva_topic_data$self=="Independent",]
se_17 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Democrat
data <- bva_topic_data[bva_topic_data$self=="Democrat",]
se_18 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Add these to the se_total object
se_total=c(se, se_1, se_2, se_3, se_4, se_5, se_6,
           se_7, se_8, se_9, se_10, se_11, se_12, 
           se_13, se_14, se_15, se_16, se_17, se_18)

#Rewrite the SE object in results_df_ri
results_df_2_fig_4_r3$se=se_total

results_df_2_fig_4_r3$dependent_var=factor(results_df_2_fig_4_r3$dependent_var, levels=c("Opinion_r","Consistent_r",  "Negative_r", "Harmful_r"))

#Labels for the figure:
new_labels <- c(
  "Consistent_r" = "Consistent",
  "Opinion_r" = "Opinion",
  "Negative_r" = "Negative",
  "Harmful_r" = "Harmful"
)

new_labels2 <- c(
  "all" = "Overall",
  "white" = "White",
  "black" = "Black",
  "hispanic" = "Hispanic",
  "asian" = "Asian",
  "male" = "Male",
  "female" = "Female",
  "non_binary" = "Non-binary",
  "christian" = "Christian",
  "jewish" = "Jewish",
  "muslim" = "Muslim",
  "atheist" = "Atheist",
  "straight" = "Straight",
  "gay" = "Gay",
  "lesbian" = "Lesbian",
  "bisexual" = "Bisexual",
  "republican" = "Republican",
  "independent" = "Independent",
  "democrat" = "Democrat"
)
# Plot the results
fig_4_r3 <- ggplot(results_df_2_fig_4_r3, aes(x = dataset, y = diff, color = dataset)) +
  geom_errorbar(aes(ymin = diff - (1.96 * se), ymax = diff + (1.96 * se)), width = 0.05) +
  geom_point(size = 3, position = position_dodge(width = 0.9)) +  # Add points at the center of the error bars
  geom_text(aes(label = sprintf("%.3f", diff), y = diff + (1.96 * se)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5) +
  theme_minimal() +
  theme(legend.position = "none",  # Position the legend at the bottom
        legend.title = element_text(size = 10),  # Adjust legend title size
        legend.text = element_text(size = 9),  # Adjust legend text size
        strip.placement = "outside",  # Ensure all labels are outside the plotting area
        panel.spacing = unit(1, "lines")) +  # Add space between facets
  labs(title = "",
       x = "",
       y = "Difference between the base and instruct models \n(positive indicates more behavior in base models)",
       caption = "") +
  facet_grid(~dependent_var, labeller = labeller(dependent_var = new_labels)) +
  scale_x_discrete(labels = c( "all" = "Overall",
                               "white" = "White",
                               "black" = "Black",
                               "hispanic" = "Hispanic",
                               "asian" = "Asian",
                               "male" = "Male",
                               "female" = "Female",
                               "non_binary" = "Non-binary",
                               "christian" = "Christian",
                               "jewish" = "Jewish",
                               "muslim" = "Muslim",
                               "atheist" = "Atheist",
                               "straight" = "Straight",
                               "gay" = "Gay",
                               "lesbian" = "Lesbian",
                               "bisexual" = "Bisexual",
                               "republican" = "Republican",
                               "independent" = "Independent",
                               "democrat" = "Democrat")) +
  scale_y_continuous(limits = c(-0.5, 0.75)) #+
#scale_shape_manual(values = c(16, 17, 18, 19, 15, 8, 7)) +  # Use different shapes for points
#scale_linetype_manual(values = c("solid", "dashed", "dotted", "2262", "aa", "18", "solid"))  # Use different line types for points
fig_4_r3

#Plot just for cargo shorts
#Differences dataset:
data_list <- list(all = bva_cargo_data,
                  white = bva_cargo_data[bva_cargo_data$self=="White",],
                  black = bva_cargo_data[bva_cargo_data$self=="Black",],
                  hispanic = bva_cargo_data[bva_cargo_data$self=="Hispanic",],
                  asian = bva_cargo_data[bva_cargo_data$self=="Asian",],
                  male = bva_cargo_data[bva_cargo_data$self=="male",],
                  female = bva_cargo_data[bva_cargo_data$self=="female",],
                  non_binary = bva_cargo_data[bva_cargo_data$self=="non-binary person",],
                  christian = bva_cargo_data[bva_cargo_data$self=="Christian",],
                  jewish = bva_cargo_data[bva_cargo_data$self=="Jewish",],
                  muslim = bva_cargo_data[bva_cargo_data$self=="Muslim",],
                  atheist = bva_cargo_data[bva_cargo_data$self=="atheist",],
                  straight = bva_cargo_data[bva_cargo_data$self=="straight",],
                  gay = bva_cargo_data[bva_cargo_data$self=="gay",],
                  lesbian = bva_cargo_data[bva_cargo_data$self=="lesbian",],
                  bisexual = bva_cargo_data[bva_cargo_data$self=="bisexual",],
                  republican = bva_cargo_data[bva_cargo_data$self=="Republican",],
                  independent = bva_cargo_data[bva_cargo_data$self=="Independent",],
                  democrat = bva_cargo_data[bva_cargo_data$self=="Democrat",]
)

#Change DVs to consistent, opinion, negative, and harmful
dependent_vars <- c("Consistent_r", "Opinion_r", "Negative_r", "Harmful_r")

# Generate the results dataframe for all datasets and dependent variables
results_df_2_fig_4_r3_c <- generate_diff_df2(data_list, dependent_vars)

##Reorder the group variable to make it correct
results_df_2_fig_4_r3_c$dataset=factor(results_df_2_fig_4_r3_c$dataset, levels=c("all", "white", "black","hispanic",
                                                                                 "asian", "male", "female", "non_binary",
                                                                                 "christian", "jewish", "muslim", "atheist",
                                                                                 "straight", "gay", "lesbian", "bisexual",
                                                                                 "republican", "independent", "democrat"))


#RI standard errors:
data <- bva_cargo_data
dependent_var <- c("Consistent_r", "Opinion_r", "Negative_r", "Harmful_r")
se <- calculate_se_multi(data, dependent_var, num_permutations = 3000)

#White dataset:
data <- bva_cargo_data[bva_cargo_data$self=="White",]
se_1 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#black dataset:
data <- bva_cargo_data[bva_cargo_data$self=="Black",]
se_2 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Hispanic dataset
data <- bva_cargo_data[bva_cargo_data$self=="Hispanic",]
se_3 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Asian dataset
data <- bva_cargo_data[bva_cargo_data$self=="Asian",]
se_4 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Male dataset
data <- bva_cargo_data[bva_cargo_data$self=="male",]
se_5 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Female
data <- bva_cargo_data[bva_cargo_data$self=="female",]
se_6 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Non-binary
data <- bva_cargo_data[bva_cargo_data$self=="non-binary person",]
se_7 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Christian
data <- bva_cargo_data[bva_cargo_data$self=="Christian",]
se_8 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Jewish
data <- bva_cargo_data[bva_cargo_data$self=="Jewish",]
se_9 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Muslim
data <- bva_cargo_data[bva_cargo_data$self=="Muslim",]
se_10 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Atheist
data <- bva_cargo_data[bva_cargo_data$self=="atheist",]
se_11 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Straight
data <- bva_cargo_data[bva_cargo_data$self=="straight",]
se_12 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Gay
data <- bva_cargo_data[bva_cargo_data$self=="gay",]
se_13 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Lesbian
data <- bva_cargo_data[bva_cargo_data$self=="lesbian",]
se_14 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Bisexual
data <- bva_cargo_data[bva_cargo_data$self=="bisexual",]
se_15 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Republican
data <- bva_cargo_data[bva_cargo_data$self=="Republican",]
se_16 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Independent
data <- bva_cargo_data[bva_cargo_data$self=="Independent",]
se_17 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Democrat
data <- bva_cargo_data[bva_cargo_data$self=="Democrat",]
se_18 <- calculate_se_multi(data, dependent_vars, num_permutations = 3000)

#Add these to the se_total object
se_total=c(se, se_1, se_2, se_3, se_4, se_5, se_6,
           se_7, se_8, se_9, se_10, se_11, se_12, 
           se_13, se_14, se_15, se_16, se_17, se_18)

#Rewrite the SE object in results_df_ri
results_df_2_fig_4_r3_c$se=se_total

results_df_2_fig_4_r3_c$dependent_var=factor(results_df_2_fig_4_r3_c$dependent_var, levels=c("Opinion_r","Consistent_r",  "Negative_r", "Harmful_r"))

#Labels for the figure:
new_labels <- c(
  "Consistent_r" = "Consistent",
  "Opinion_r" = "Opinion",
  "Negative_r" = "Negative",
  "Harmful_r" = "Harmful"
)

new_labels2 <- c(
  "all" = "Overall",
  "white" = "White",
  "black" = "Black",
  "hispanic" = "Hispanic",
  "asian" = "Asian",
  "male" = "Male",
  "female" = "Female",
  "non_binary" = "Non-binary",
  "christian" = "Christian",
  "jewish" = "Jewish",
  "muslim" = "Muslim",
  "atheist" = "Atheist",
  "straight" = "Straight",
  "gay" = "Gay",
  "lesbian" = "Lesbian",
  "bisexual" = "Bisexual",
  "republican" = "Republican",
  "independent" = "Independent",
  "democrat" = "Democrat"
)
# Plot the results
fig_4_r3_c <- ggplot(results_df_2_fig_4_r3_c, aes(x = dataset, y = diff, color = dataset)) +
  geom_errorbar(aes(ymin = diff - (1.96 * se), ymax = diff + (1.96 * se)), width = 0.05) +
  geom_point(size = 3, position = position_dodge(width = 0.9)) +  # Add points at the center of the error bars
  geom_text(aes(label = sprintf("%.3f", diff), y = diff + (1.96 * se)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5) +
  theme_minimal() +
  theme(legend.position = "none",  # Position the legend at the bottom
        legend.title = element_text(size = 10),  # Adjust legend title size
        legend.text = element_text(size = 9),  # Adjust legend text size
        strip.placement = "outside",  # Ensure all labels are outside the plotting area
        panel.spacing = unit(1, "lines")) +  # Add space between facets
  labs(title = "",
       x = "",
       y = "Difference between the base and instruct models \n(positive indicates more behavior in base models)",
       caption = "") +
  facet_grid(~dependent_var, labeller = labeller(dependent_var = new_labels)) +
  scale_x_discrete(labels = c( "all" = "Overall",
                               "white" = "White",
                               "black" = "Black",
                               "hispanic" = "Hispanic",
                               "asian" = "Asian",
                               "male" = "Male",
                               "female" = "Female",
                               "non_binary" = "Non-binary",
                               "christian" = "Christian",
                               "jewish" = "Jewish",
                               "muslim" = "Muslim",
                               "atheist" = "Atheist",
                               "straight" = "Straight",
                               "gay" = "Gay",
                               "lesbian" = "Lesbian",
                               "bisexual" = "Bisexual",
                               "republican" = "Republican",
                               "independent" = "Independent",
                               "democrat" = "Democrat")) +
  scale_y_continuous(limits = c(-0.5, 0.75)) #+
#scale_shape_manual(values = c(16, 17, 18, 19, 15, 8, 7)) +  # Use different shapes for points
#scale_linetype_manual(values = c("solid", "dashed", "dotted", "2262", "aa", "18", "solid"))  # Use different line types for points
fig_4_r3_c

fig_4_r3_flip <- fig_4_r3 + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray20") +
  ggtitle("Sociodemographic groups") +
  labs(y = "Difference between the base and instruct models (positive indicates more behavior in base models)", caption = "")+
  coord_flip()

fig_4_r3_flip

fig_4_r3_c_flip <- fig_4_r3_c + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray20") +
  ggtitle("Cargo shorts") +
  labs(y = "Difference between the base and instruct models (positive indicates more behavior in base models)", caption = "")+
  coord_flip()

fig_4_r3_c_flip

library(cowplot)
fig_4_r3_combined=plot_grid(fig_4_r3_flip+
                              labs(y="", caption=""), 
                            fig_4_r3_c_flip+
                              labs(y = "Difference between base and instruct models (positive values indicate more behavior in base models)"), ncol=1)
fig_4_r3_combined

jpeg(here::here(thisPath(),"Figure A.3.jpeg"), width=12, height=15, units="in", res=600)
fig_4_r3_combined
dev.off()

