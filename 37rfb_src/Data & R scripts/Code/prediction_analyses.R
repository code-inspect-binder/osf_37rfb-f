# Script written by Bram Bulté (2020) and adapted by Eva Koch.
# This script contains the analyses of the eye-gaze and RT data gathered by Eva Koch in 2019
# for a study on the predictive processing of German morphosyntax (affixation in German weak verbs).

# Set working directory to source file location
library(rstudioapi)
set_wd <- function() {
  library(rstudioapi) # make sure you have it installed
  current_path <- getActiveDocumentContext()$path # get the path of your current open file
  setwd(dirname(current_path)) # set the working directory to the source file location
  print(getwd()) # display the directory
}
set_wd()



#####################################################
## BOOTSTRAPPED CLUSTER-BASED PERMUTATION ANALYSIS ##
#####################################################

# Clear workspace
rm(list = ls())

# Load packages
devtools::install_github("jwdink/eyetrackingR") # Download eyetrackingR package
library("eyetrackingR")
library("pbapply") # For a progress bar in the cluster-analysis function
library("dplyr")
library("ggplot2")



## EYE-GAZE DATA PREPARATION

# Load data
dataset_exp1 <- readRDS("../data/data_experiment1.rds") # Load data (from 'data' folder)

# Remove participant(s) with dyslexia
dataset_exp1 <- dataset_exp1[which(dataset_exp1$dyslexia == "no"), ]

# Remove trials with RT < 4710 ms (i.e., presses before disambiguating information in sentence)
dataset_exp1 <- dataset_exp1[which(dataset_exp1$rt > 4710), ]

# Remove RTs < 6210 ms for baseline trials (i.e., presses before disambiguating information in sentence in baseline trials)
dataset_exp1 <- filter(dataset_exp1, !(rt < 6210 & trial_condition_new == "same number"))

# Replace values after button press by 1 for target sample, and 0 for distractor sample
dataset_exp1$average_target_sample_count_proportion [dataset_exp1$time_ms_absolute > dataset_exp1$rt] <- 1
dataset_exp1$average_distractor_sample_count_proportion [dataset_exp1$time_ms_absolute > dataset_exp1$rt] <- 0

# Add 'trackloss' column (if not looking at IA_1 or IA_2, then trackloss = 1)
dataset_exp1 <- within(dataset_exp1, {
  trackloss <- ifelse(average_target_sample_count_proportion == 0 & average_distractor_sample_count_proportion == 0, 1, 0)
})

# Prepare dataframe for eye-tracking analyses
eyetrackingr.data.exp1 <- make_eyetrackingr_data(dataset_exp1,
  participantcolumn = "participant_number",
  trial_column = "item_nr",
  time_column = "time_ms_absolute",
  trackloss_column = "trackloss",
  aoi_columns = c("average_target_sample_count_proportion", "average_distractor_sample_count_proportion"),
  treat_non_aoi_looks_as_missing = TRUE
)

# Analyze amount of trackloss by subjects and trials
(trackloss_exp1 <- trackloss_analysis(data = eyetrackingr.data.exp1))
hist(trackloss_exp1$TracklossForTrial)

# Remove trackloss per trial > 50%
eyetrackingr.data.exp1 <- clean_by_trackloss(data = eyetrackingr.data.exp1, trial_prop_thresh = .5)

# Analyze trackloss per subject
trackloss_clean_exp1 <- trackloss_analysis(data = eyetrackingr.data.exp1)
(trackloss_clean_subjects_exp1 <- unique(trackloss_clean_exp1[, c("participant_number", "TracklossForParticipant")]))

# Get mean samples contributed per trials, with SD
mean(1 - trackloss_clean_subjects_exp1$TracklossForParticipant)
sd(1 - trackloss_clean_subjects_exp1$TracklossForParticipant)
max(trackloss_clean_subjects_exp1$TracklossForParticipant)



## PREPARE DATAFRAMES FOR ANALYSIS

# Dataframe with proportion of looks towards target as outcome variable (for overall analysis, and analyses within L1 and L2 groups separately)
response_time_exp1 <- make_time_sequence_data(eyetrackingr.data.exp1,
  time_bin_size = 50,
  predictor_columns = c("trial_condition_new", "group"),
  aois = c("average_target_sample_count_proportion"),
  summarize_by = "participant_number"
)

# Dataframe with the difference between looks towards target in prediction trials and baseline trials, per participant per bin (for the analysis comparing L1 and L2 directly)
response_time_exp1_diff <- response_time_exp1 # Rename dataframe
response_time_exp1_diff$ID <- paste(response_time_exp1_diff$TimeBin, response_time_exp1_diff$participant_number)
response_time_exp1_diff_test <- response_time_exp1_diff[which(response_time_exp1_diff$trial_condition_new == "different number"), ]
response_time_exp1_diff_cont <- response_time_exp1_diff[which(response_time_exp1_diff$trial_condition_new == "same number"), ]
response_time_exp1_diff <- response_time_exp1_diff[which(response_time_exp1_diff$trial_condition_new == "different number"), ]
response_time_exp1_diff$Prop_control <- response_time_exp1_diff_cont$Prop
response_time_exp1_diff$Prop_diff <- response_time_exp1_diff$Prop - response_time_exp1_diff$Prop_control
response_time_exp1_diff$Prop <- NULL
response_time_exp1_diff$Prop <- response_time_exp1_diff$Prop_diff



## CLUSTER ANALYSIS 0: FULL SAMPLE; baseline vs. prediction trials (IV = trial condition; DV = proportion of looks)

# Visualize timecourse
plot(response_time_exp1, predictor_column = "trial_condition_new") +
  theme_light() +
  coord_cartesian(ylim = c(0, 1))

# T-tests (using participant-averaged data)
num_sub_exp1 <- length(unique((eyetrackingr.data.exp1$participant_number)))
threshold_t_exp1 <- qt(p = 1 - .05 / 2, df = num_sub_exp1 - 1) # Pick threshold for t based on alpha = .05, two-tailed

df_timeclust_exp1 <- make_time_cluster_data(response_time_exp1,
  test = "t.test", paired = TRUE,
  predictor_column = "trial_condition_new",
  threshold = threshold_t_exp1
)

# Identify potential clusters
plot(df_timeclust_exp1) + ylab("T-Statistic") + theme_light() # Plot potential clusters
summary(df_timeclust_exp1) # Summarize potential clusters in table

# Identify significant clusters, based on 2500 iterations
clust_analysis_exp1 <- analyze_time_clusters(df_timeclust_exp1, within_subj = TRUE, paired = TRUE, samples = 2500) # Cluster analysis
plot(clust_analysis_exp1) + theme_light() # Plot significant clusters
summary(clust_analysis_exp1) # Summarize significant clusters in table



## CLUSTER ANALYSES 1: L1 GROUP ONLY

# Create separate dataframe for L1
response_time_exp1_L1 <- response_time_exp1[which(response_time_exp1$group == "L1"), ]

# Visualize timecourse of the L1 data
plot(response_time_exp1_L1, predictor_column = "trial_condition_new") +
  theme_light() +
  coord_cartesian(ylim = c(0, 1))

# T-tests L1
num_sub_exp1_L1 <- length(unique((response_time_exp1_L1$participant_number)))
threshold_t_exp1_L1 <- qt(
  p = 1 - .05 / 2,
  df = num_sub_exp1_L1 - 1
) # Pick threshold for t based on alpha = .05, two-tailed

df_timeclust_exp1_L1 <- make_time_cluster_data(response_time_exp1_L1,
  test = "t.test", paired = TRUE,
  predictor_column = "trial_condition_new",
  threshold = threshold_t_exp1_L1
)

# Identify potential clusters L1
plot(df_timeclust_exp1_L1) + ylab("T-Statistic") + theme_light() # Plot potential clusters
summary(df_timeclust_exp1_L1) # Summarize potential clusters in table

# Identify significant clusters L1
clust_analysis_exp1_L1 <- analyze_time_clusters(df_timeclust_exp1_L1, within_subj = TRUE, paired = TRUE, samples = 2500) # Analyze
plot(clust_analysis_exp1_L1) + theme_light() # Plot significant clusters
summary(clust_analysis_exp1_L1) # Summarize significant clusters in table



## CLUSTER ANALYSES 2: L2 GROUP ONLY

# Create separate dataframe for L2
response_time_exp1_L2 <- response_time_exp1[which(response_time_exp1$group == "L2"), ]

# Visualize timecourse of the L2 data
plot(response_time_exp1_L2, predictor_column = "trial_condition_new") +
  theme_light() +
  coord_cartesian(ylim = c(0, 1))

# T-tests L2
num_sub_exp1_L2 <- length(unique((response_time_exp1_L2$participant_number)))
threshold_t_exp1_L2 <- qt(
  p = 1 - .05 / 2,
  df = num_sub_exp1_L2 - 1
) # Pick threshold for t based on alpha = .05, two-tailed

df_timeclust_exp1_L2 <- make_time_cluster_data(response_time_exp1_L2,
  test = "t.test", paired = TRUE,
  predictor_column = "trial_condition_new",
  threshold = threshold_t_exp1_L2
)

# Identify potential clusters L2
plot(df_timeclust_exp1_L2) + ylab("T-Statistic") + theme_light() # Plot potential clusters
summary(df_timeclust_exp1_L2) # Summarize potential clusters in table

# Identify significant clusters L2
clust_analysis_exp1_L2 <- analyze_time_clusters(df_timeclust_exp1_L2, within_subj = TRUE, paired = TRUE, samples = 2500) # Analyze
plot(clust_analysis_exp1_L2) + theme_light() # Plot significant clusters
summary(clust_analysis_exp1_L2) # Summarize significant clusters in table



## CLUSTER ANALYSIS 3: L1 vs. L2

# DV = Proportion of looks test/prediction minus control/baseline items per participant per bin, IV = L1 vs. L2

# Visualize timecourse of the L1 vs. L2 data
plot(response_time_exp1_diff, predictor_column = "group") +
  theme_light() +
  coord_cartesian(ylim = c(-0.25, 0.5))

# T-tests
num_sub_exp1_L1L2 <- length(unique((response_time_exp1_diff$participant_number)))
threshold_t_exp1_L1L2 <- qt(
  p = 1 - .05 / 2,
  df = num_sub_exp1_L1L2 - 2
) # Pick threshold for t based on alpha = .05, two-tailed

df_timeclust_exp1_L1L2 <- make_time_cluster_data(response_time_exp1_diff,
  test = "t.test", paired = FALSE,
  predictor_column = "group",
  threshold = threshold_t_exp1_L1L2
)

# Identify potential clusters
plot(df_timeclust_exp1_L1L2) + ylab("T-Statistic") + theme_light() # Plot potential clusters
summary(df_timeclust_exp1_L1L2) # Summarize potential clusters in table

# Identify significant clusters, based on 2500 iterations
clust_analysis_exp1_L1L2 <- analyze_time_clusters(df_timeclust_exp1_L1L2, within_subj = FALSE, paired = FALSE, samples = 2500) # Cluster analysis
plot(clust_analysis_exp1_L1L2) + theme_light() # Plot significant clusters
summary(clust_analysis_exp1_L1L2) # Summarize significant clusters in table






################################################
## REACTION TIMES ANALYSES WITH MIXED-EFFECTS ##
################################################

# Information: Mixed-effects of the RT data of the button-press responses.

# Clear workspace
rm(list = ls())

# Load packages
library("lme4") # For modeling
library("Matrix")
library("MuMIn") # For obtaining R2 marginal and conditional
library("lmerTest")



## RT DATA PREPARATION

# Load dataset
dataset_exp1 <- readRDS("../data/data_experiment1.rds") # Load data (from 'data' folder)

# Trim original dataset: Keep only 1 RT per trial
rt_data <- dataset_exp1[which(dataset_exp1$BIN_INDEX == 1), ]
rm(dataset_exp1)

# Remove participant(s) with dyslexia
rt_data <- rt_data[which(rt_data$dyslexia == "no"), ]

# Remove RTs < 4710 ms (i.e., presses before disambiguating information in sentence)
rt_data <- rt_data[which(rt_data$rt > 4710), ]

# Remove RTs < 6210 ms for baseline trials (i.e., presses before disambiguating information in sentence in baseline trials)
rt_data <- filter(rt_data, !(rt < 6210 & trial_condition_new == "same number"))

# Remove some useless columns
rt_data$BIN_INDEX <- NULL
rt_data$AVERAGE_BLINK_SAMPLE_COUNT <- NULL
rt_data$AVERAGE_BLINK_SAMPLE_COUNT_. <- NULL
rt_data$AVERAGE_EXCLUDED_SAMPLE_COUNT <- NULL
rt_data$AVERAGE_EXCLUDED_SAMPLE_COUNT_ <- NULL
rt_data$AVERAGE_IA_1_SAMPLE_COUNT <- NULL
rt_data$AVERAGE_IA_2_SAMPLE_COUNT <- NULL
rt_data$AVERAGE_IA_0_SAMPLE_COUNT <- NULL
rt_data$AVERAGE_IA_1_SAMPLE_COUNT_. <- NULL
rt_data$AVERAGE_IA_2_SAMPLE_COUNT_. <- NULL
rt_data$AVERAGE_IA_0_SAMPLE_COUNT_. <- NULL
rt_data$EYE_TRACKED <- NULL
rt_data$IA_1_ID <- NULL
rt_data$IA_2_ID <- NULL
rt_data$IA_0_ID <- NULL
rt_data$IP_LABEL <- NULL
rt_data$time_ms_IP <- NULL
rt_data$time_ms_absolute <- NULL
rt_data$average_target_sample_count <- NULL
rt_data$average_target_sample_count_proportion <- NULL
rt_data$average_distractor_sample_count <- NULL
rt_data$average_distractor_sample_count_proportion <- NULL
rt_data$average_null_sample_count <- NULL
rt_data$average_null_sample_count_proportion <- NULL
rt_data$average_blink_sample_count <- NULL
rt_data$average_blink_sample_count_proportion <- NULL
rt_data$average_excluded_sample_count <- NULL
rt_data$average_excluded_sample_count_proportion <- NULL

# Recode conditions experiment 1 ('verb_form' = SG vs. PL)
rt_data$verb_form <- rt_data$trial_condition
rt_data$verb_form <- gsub("diff_3pl", "PL", rt_data$verb_form)
rt_data$verb_form <- gsub("diff_3sg", "SG", rt_data$verb_form)
rt_data$verb_form <- gsub("same_3pl", "PL", rt_data$verb_form)
rt_data$verb_form <- gsub("same_3sg", "SG", rt_data$verb_form)

# Shorter variable names for better readability
rt_data$s_id <- rt_data$participant_number # Subject ID
rt_data$participant_number <- NULL
rt_data$tc <- rt_data$trial_condition_new # Trial condition
rt_data$tc <- gsub("same number", "same", rt_data$tc)
rt_data$tc <- gsub("different number", "diff", rt_data$tc)
rt_data$trial_condition_new <- NULL
rt_data$l <- rt_data$group # Language group
rt_data$group <- NULL
rt_data$vf <- rt_data$verb_form # Target (also called: verb form, target number)
rt_data$verb_form <- NULL
rt_data$wm <- rt_data$operationspan_score # Working memory
rt_data$operationspan_score <- NULL
rt_data$tr_nr <- rt_data$TRIAL_INDEX # Trial number
rt_data$TRIAL_INDEX <- NULL

# Make sure we're dealing with factors
rt_data$tc <- as.factor(rt_data$tc)
rt_data$l <- as.factor(rt_data$l)
rt_data$vf <- as.factor(rt_data$vf)

# Change reference categories (condition = baseline; target number = SG; group = L1)
rt_data$tc <- relevel(rt_data$tc, ref = "same")
rt_data$l <- relevel(rt_data$l, ref = "L1")
rt_data$vf <- relevel(rt_data$vf, ref = "SG")

# Center continuous variables
scale(rt_data$wm, scale = FALSE)
scale(rt_data$tr_nr, scale = FALSE)



## MODELING
model <- lmer(rt ~ 1 + tc * vf + tc * l + tc * wm + tc * tr_nr + (1 | s_id) + (1 | item_code), rt_data)



## GET MODEL OUTPUT

# Get model summary
summary(model)

# Obtain R2 marginal and R2 conditional
r.squaredGLMM(model)

# Obtain AIC
AIC(model)

# Obtain -2LL
LL <- logLik(model)
Minus2LL <- LL * (-2) # (equals REML)

# Ask for CI:
se_model <- sqrt(diag(vcov(model)))
(tab_model <- cbind(Est = fixef(model), LL = fixef(model) - 1.96 * se_model, UL = fixef(model) + 1.96 * se_model)) # CIs

# Estimated means & pairwise comparisons
(lsm <- lsmeansLT(model))
lsmeansLT(model, pairwise = TRUE)



# # # End of script # # #