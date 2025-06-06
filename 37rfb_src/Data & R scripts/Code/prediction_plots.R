# Script written in 2020/2021 by Bram Bulté (data preparation) & Eva Koch (graphing).
# This script contains the graphs accompanying the analyses of the eye-gaze and RT data gathered by Eva Koch in 2019
# for a study on the predictive processing of German morphosyntax.

# Set working directory to source file location
library(rstudioapi)
set_wd <- function() {
  library(rstudioapi)
  current_path <- getActiveDocumentContext()$path # get the path of your current open file
  setwd(dirname(current_path)) # set the working directory to the source file location
  print(getwd()) # display the directory
}
set_wd()

# Clear workspace
rm(list = ls())



###################################
## GRAPHING OF EYE-TRACKING DATA ##
###################################

## GENERAL PREPARATION OF EYE-TRACKING DATA

# Load packages
devtools::install_github("jwdink/eyetrackingR") # Download eyetrackingR package
library("eyetrackingR")
library("dplyr")
library("ggplot2")
library("ggpubr") # For arranging plots

# Load full dataset
dataset <- readRDS("../data/data_experiment1.rds")

# Replace values after button press by 1 for target sample, and 0 for distractor sample
dataset$average_target_sample_count_proportion [dataset$time_ms_absolute > dataset$rt] <- 1
dataset$average_distractor_sample_count_proportion [dataset$time_ms_absolute > dataset$rt] <- 0

# Add 'trackloss' column (if not looking at IA_1 or IA_2, then trackloss = 1)
dataset <- within(dataset, {
  trackloss <- ifelse(average_target_sample_count_proportion == 0 & average_distractor_sample_count_proportion == 0, 1, 0)
})

# Remove participant with dyslexia
dataset <- dataset[which(dataset$dyslexia == "no"), ]

# Remove trials with RT < 4710 ms (i.e., presses before disambiguating information in sentence)
dataset <- dataset[which(dataset$rt > 4710), ]

# Remove RTs < 6210 ms for baseline trials (i.e., presses before disambiguating information in sentence in baseline trials)
dataset <- filter(dataset, !(rt < 6210 & trial_condition_new == "same number"))

# Reorder factor levels: (this will be useful once we create the plot legend)
dataset$trial_condition <- factor(dataset$trial_condition, levels = c("diff_3sg", "diff_3pl", "same_3sg", "same_3pl"))
dataset$trial_condition_new <- factor(dataset$trial_condition_new, levels = c("different number", "same number"))

# Prepare dataframe for eye-tracking analysis & graphs (package eyetrackingR required)
eyetrackingr.data <- make_eyetrackingr_data(dataset,
  participant_column = "participant_number",
  trial_column = "item_nr",
  time_column = "time_ms_absolute",
  trackloss_column = "trackloss",
  aoi_columns = c("average_target_sample_count_proportion", "average_distractor_sample_count_proportion"),
  treat_non_aoi_looks_as_missing = TRUE
)

# Remove trackloss per trial > 50% (removed 4 trials)
eyetrackingr.data <- clean_by_trackloss(data = eyetrackingr.data, trial_prop_thresh = .5)

# Determine reference levels
levels(eyetrackingr.data$trial_condition) # Check order
eyetrackingr.data$trial_condition <- factor(eyetrackingr.data$trial_condition, levels(eyetrackingr.data$trial_condition) [c(3, 4, 1, 2)]) # Change order

levels(eyetrackingr.data$trial_condition_new) # Check order
eyetrackingr.data$trial_condition_new <- relevel(eyetrackingr.data$trial_condition_new, ref = "same number") # Change order

levels(eyetrackingr.data$group) # Check order



## PREPARE DATAFRAMES FOR PLOTTNG

# Generate time-binned dataframe; summarize by subjects (using "trial_condition": 4 conditions)
responsetime_exp1_TCxT <- make_time_sequence_data(eyetrackingr.data,
  time_bin_size = 50,
  predictor_columns = c("trial_condition"),
  aois = c("average_target_sample_count_proportion"),
  summarize_by = "participant_number"
)

# Generate time-binned dataframe; summarize by subjects (using "trial_condition_new": 2 conditions)
responsetime_exp1_TCnew <- make_time_sequence_data(eyetrackingr.data,
  time_bin_size = 50,
  predictor_columns = c("trial_condition_new", "group"),
  aois = c("average_target_sample_count_proportion"),
  summarize_by = "participant_number"
)



## PLOTTING

# GRAPH: TIMEPLOT - TRIAL CONDITION x TARGET NUMBER [FIGURE S1.1]

# Plot (greyscale)
plot_TCxT <- ggplot(
  responsetime_exp1_TCxT,
  aes(Time, Prop,
    colour = trial_condition,
    linetype = trial_condition,
    fill = trial_condition # You need to put colour, linetype and fill in the aes, if you later want to adapt them manually
  )
) +
  theme_bw() + # Different style (other: theme_light())
  theme(axis.text.x = element_text(angle = 90)) + # Put x-axis text in a 90% angle
  scale_x_continuous(breaks = seq(0, 9000, 500)) + # X-axis: a tick mark every 500ms
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) + # Y-axis: a tick mark every 0.25
  labs(x = "Time from trial onset (in ms)", y = "Fixation proportion towards target") + # Add labels for axes
  scale_color_manual(values = c("grey55", "grey55", "black", "black"), name = "Trial condition x Target", labels = c("Baseline Singular", "Baseline Plural", "Prediction Singular", "Prediction Plural")) + # Manually determine line colors + and rename labels
  scale_fill_manual(values = c("grey55", "grey55", "black", "black"), name = "Trial condition x Target", labels = c("Baseline Singular", "Baseline Plural", "Prediction Singular", "Prediction Plural")) + # Manually determine ribbon fill color
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed"), name = "Trial condition x Target", labels = c("Baseline Singular", "Baseline Plural", "Prediction Singular", "Prediction Plural")) + # Manually determine line types
  theme(legend.justification = c(1, 0), legend.position = c(0.98, 0.02)) + # Put the legend into the box (bottom right corner)
  geom_vline(xintercept = 4500, colour = "grey30") + # Vertical line to mark onset Verb audio
  annotate("text", x = 4350, y = 0, hjust = 0, angle = 90, label = "Verb onset", colour = "grey30") + # Add label (-150ms)
  geom_vline(xintercept = 6000, colour = "grey30") + # Vertical line to mark onset Subject audio
  annotate("text", x = 5850, y = 0, hjust = 0, angle = 90, label = "Subject NP onset", colour = "grey30") + # Add label (-150ms)
  stat_summary(fun.data = mean_cl_boot, geom = "ribbon", colour = NA, alpha = 0.3, na.rm = TRUE) + # Ribbon to display CIs
  stat_summary(fun.y = mean, geom = "line", size = 0.6, na.rm = TRUE) # Line to connect the data points (means)
plot_TCxT

# Save plot
plotwidth <- 10; plotheight <- 5
ggsave("../plots/exp1_timeplot_TCxTarget.png", width = plotwidth, height = plotheight, units = "in", dpi = 320)



# GRAPH: TIMEPLOT CLUSTER - TRIAL CONDITION - L1

# Keep only L1 data:
responsetime_exp1_TC_L1 <- responsetime_exp1_TCnew[which(responsetime_exp1_TCnew$group == "L1"), ]

# L1: 5050 - 6650 (significant positive cluster), 6800 - 8700 (significant negative cluster)

# Plot (greyscale)
plot_cluster_TC_L1 <- ggplot(
  responsetime_exp1_TC_L1,
  aes(Time, Prop,
    colour = trial_condition_new,
    fill = trial_condition_new # You need to put colour, linetype and fill in the aes, if you later want to adapt them manually
  )
) +
  theme_bw() + # Different style (other: theme_light())
  ggtitle(expression(paste("L1 group (", italic("n"), " = 31)"))) + # Add plot title (optional)
  theme(plot.title = element_text(hjust = 0.5)) + # Plot title: centered position (optional)
  theme(axis.text.x = element_text(angle = 90)) + # Put x-axis text in a 90% angle
  scale_x_continuous(breaks = seq(0, 9000, 500)) + # X-axis: a tick mark every 500ms
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) + # Y-axis: a tick mark every 0.25
  labs(x = "Time from trial onset (in ms)", y = "Fixation proportion towards target") + # Add labels for axes
  scale_color_manual(values = c("grey55", "black"), name = "Trial condition", labels = c("Baseline", "Prediction")) + # Manually determine line colors + and rename legend labels
  scale_fill_manual(values = c("grey55", "black"), name = "Trial condition", labels = c("Baseline", "Prediction")) + # Manually determine ribbon fill color
  theme(legend.justification = c(1, 0), legend.position = c(0.98, 0.02)) + # Put the legend into the box (bottom right corner)
  annotate("rect", xmin = 5050, xmax = 6650, ymin = -Inf, ymax = Inf, alpha = 0.4, color = NA, fill = "grey") + # Shaded area to mark significant cluster
  annotate("rect", xmin = 6800, xmax = 8700, ymin = -Inf, ymax = Inf, alpha = 0.4, color = NA, fill = "grey") + # Shaded area to mark significant cluster
  # annotate("rect", xmin = 4710, xmax = 6210, ymin = -Inf, ymax = Inf, alpha = 0.4, color = NA, fill = "grey") + # Shaded area to mark the prediction window
  geom_vline(xintercept = 4500, colour = "grey30") + # Vertical line to mark onset Verb audio
  annotate("text", x = 4350, y = 0, hjust = 0, angle = 90, label = "Verb onset", colour = "grey30") + # Add label (-150ms)
  geom_vline(xintercept = 6000, colour = "grey30") + # Vertical line to mark onset Subject audio
  annotate("text", x = 5850, y = 0, hjust = 0, angle = 90, label = "Subject NP onset", colour = "grey30") + # Add label (-150ms)
  stat_summary(fun.data = mean_cl_boot, geom = "ribbon", colour = NA, alpha = 0.3, na.rm = TRUE) + # Ribbon to display CIs
  stat_summary(fun.y = mean, geom = "line", size = 0.6, na.rm = TRUE) # Line to connect the data points (means)
plot_cluster_TC_L1

# Save plot
plotwidth <- 10; plotheight <- 5
ggsave("../plots/exp1_timeplot_cluster_TC_L1.png", width = plotwidth, height = plotheight, units = "in", dpi = 320) # For plot without title
ggsave("../plots/exp1_timeplot_cluster_TC_L1_title.png", width = plotwidth, height = plotheight, units = "in", dpi = 320) # For plot with title



# GRAPH: TIMEPLOT CLUSTER - TRIAL CONDITION - L2

# Keep only L2 data:
responsetime_exp1_TC_L2 <- responsetime_exp1_TCnew[which(responsetime_exp1_TCnew$group == "L2"), ]

# L2: 5200 - 6650 (significant positive cluster), 7150 - 7950 (significant negative cluster)

# Plot (greyscale)
plot_cluster_TC_L2 <- ggplot(
  responsetime_exp1_TC_L2,
  aes(Time, Prop,
    colour = trial_condition_new,
    fill = trial_condition_new # You need to put colour, linetype and fill in the aes, if you later want to adapt them manually
  )
) +
  theme_bw() + # Different style (other: theme_light())
  ggtitle(expression(paste("L2 group (", italic("n"), " = 30)"))) + # Add plot title (optional)
  theme(plot.title = element_text(hjust = 0.5)) + # Plot title: centered position (optional)
  theme(axis.text.x = element_text(angle = 90)) + # Put x-axis text in a 90% angle
  scale_x_continuous(breaks = seq(0, 9000, 500)) + # X-axis: a tick mark every 500ms
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) + # Y-axis: a tick mark every 0.25
  labs(x = "Time from trial onset (in ms)", y = "Fixation proportion towards target") + # Add labels for axes
  scale_color_manual(values = c("grey55", "black"), name = "Trial condition", labels = c("Baseline", "Prediction")) + # Manually determine line colors + and rename legend labels
  scale_fill_manual(values = c("grey55", "black"), name = "Trial condition", labels = c("Baseline", "Prediction")) + # Manually determine ribbon fill color
  theme(legend.justification = c(1, 0), legend.position = c(0.98, 0.02)) + # Put the legend into the box (bottom right corner)
  annotate("rect", xmin = 5200, xmax = 6650, ymin = -Inf, ymax = Inf, alpha = 0.4, color = NA, fill = "grey") + # Shaded area to mark significant cluster
  annotate("rect", xmin = 7150, xmax = 7950, ymin = -Inf, ymax = Inf, alpha = 0.4, color = NA, fill = "grey") + # Shaded area to mark significant cluster
  # annotate("rect", xmin = 4710, xmax = 6210, ymin = -Inf, ymax = Inf, alpha = 0.4, color = NA, fill = "grey") + # Shaded area to mark the prediction window
  geom_vline(xintercept = 4500, colour = "grey30") + # Vertical line to mark onset Verb audio
  annotate("text", x = 4350, y = 0, hjust = 0, angle = 90, label = "Verb onset", colour = "grey30") + # Add label (-150ms)
  geom_vline(xintercept = 6000, colour = "grey30") + # Vertical line to mark onset Subject audio
  annotate("text", x = 5850, y = 0, hjust = 0, angle = 90, label = "Subject NP onset", colour = "grey30") + # Add label (-150ms)
  stat_summary(fun.data = mean_cl_boot, geom = "ribbon", colour = NA, alpha = 0.3, na.rm = TRUE) + # Ribbon to display CIs
  stat_summary(fun.y = mean, geom = "line", size = 0.6, na.rm = TRUE) # Line to connect the data points (means)
plot_cluster_TC_L2

# Save plot
plotwidth <- 10; plotheight <- 5
ggsave("../plots/exp1_timeplot_cluster_TC_L2.png", width = plotwidth, height = plotheight, units = "in", dpi = 320) # For plot without title
ggsave("../plots/exp1_timeplot_cluster_TC_L2_title.png", width = plotwidth, height = plotheight, units = "in", dpi = 320) # For plot with title



# GRAPH: TIMEPLOT CLUSTER - COMBINATION OF TRIAL CONDITION L1 & TRIAL CONDITION L2 [FIGURE 4]

# Arrange plots
ggarrange(plot_cluster_TC_L1, plot_cluster_TC_L2, ncol = 1, nrow = 2)

# Save plot
plotwidth <- 10; plotheight <- 8
ggsave("../plots/exp1_timeplot_cluster_L1+L2.png", width = plotwidth, height = plotheight, units = "in", dpi = 600)



# GRAPH: TIMEPLOT CLUSTER - L1 VS. L2 [FIGURE 5]

# Calculate proportion of looks test/prediction minus control/baseline items per participant per bin (= DV), L1 vs. L2 (IV)
responsetime_exp1_diff <- responsetime_exp1_TCnew
responsetime_exp1_diff$ID <- paste(responsetime_exp1_diff$TimeBin, responsetime_exp1_diff$participant_number)
responsetime_exp1_diff_test <- responsetime_exp1_diff[which(responsetime_exp1_diff$trial_condition_new == "different number"), ]
responsetime_exp1_diff_cont <- responsetime_exp1_diff[which(responsetime_exp1_diff$trial_condition_new == "same number"), ]
responsetime_exp1_diff <- responsetime_exp1_diff[which(responsetime_exp1_diff$trial_condition_new == "different number"), ]
responsetime_exp1_diff$Prop_control <- responsetime_exp1_diff_cont$Prop
responsetime_exp1_diff$Prop_diff <- responsetime_exp1_diff$Prop - responsetime_exp1_diff$Prop_control
responsetime_exp1_diff$Prop <- NULL
responsetime_exp1_diff$Prop <- responsetime_exp1_diff$Prop_diff

# Plot (greyscale)
plot_cluster_L1vsL2 <- ggplot(
  responsetime_exp1_diff,
  aes(Time, Prop,
    colour = group,
    fill = group # You need to put colour, linetype and fill in the aes, if you later want to adapt them manually
  )
) +
  theme_bw() + # Different style (other: theme_light())
  ggtitle("L1 group vs. L2 group") + # Add plot title (optional)
  theme(plot.title = element_text(hjust = 0.5)) + # Plot title: centered position (optional)
  theme(axis.text.x = element_text(angle = 90)) + # Put x-axis text in a 90% angle
  scale_x_continuous(breaks = seq(0, 9000, 500)) + # X-axis: a tick mark every 500ms
  scale_y_continuous(breaks = seq(-0.25, 0.5, 0.25), limits = c(-0.25, 0.5)) + # Y-axis: a tick mark every 0.25
  labs(x = "Time from trial onset (in ms)", y = "Difference in fixation proportion towards target") + # Add labels for axes
  scale_color_manual(values = c("grey55", "black"), name = "Group") + # Manually determine line colors + and rename legend labels
  scale_fill_manual(values = c("grey55", "black"), name = "Group") + # Manually determine ribbon fill color
  theme(legend.justification = c(1, 0), legend.position = c(0.98, 0.02)) + # Put the legend into the box (bottom right corner)
  # annotate("rect", xmin = 4710, xmax = 6210, ymin = -Inf, ymax = Inf, alpha = 0.4, color = NA, fill = "grey") + # Shaded area to mark the prediction window
  geom_vline(xintercept = 4500, colour = "grey30") + # Vertical line to mark onset Verb audio
  annotate("text", x = 4350, y = -0.25, hjust = 0, angle = 90, label = "Verb onset", colour = "grey30") + # Add label (-150ms)
  geom_vline(xintercept = 6000, colour = "grey30") + # Vertical line to mark onset Subject audio
  annotate("text", x = 5850, y = -0.25, hjust = 0, angle = 90, label = "Subject NP onset", colour = "grey30") + # Add label (-150ms)
  stat_summary(fun.data = mean_cl_boot, geom = "ribbon", colour = NA, alpha = 0.3, na.rm = TRUE) + # Ribbon to display CIs
  stat_summary(fun.y = mean, geom = "line", size = 0.6, na.rm = TRUE) # Line to connect the data points (means)

# No shaded areas for significant clusters, because there were none.
plot_cluster_L1vsL2

# Save plot
plotwidth <- 10; plotheight <- 5
ggsave("../plots/exp1_timeplot_cluster_L1vsL2.png", width = plotwidth, height = plotheight, units = "in", dpi = 320) # For plot without title
ggsave("../plots/exp1_timeplot_cluster_L1vsL2_title.png", width = plotwidth, height = plotheight, units = "in", dpi = 320) # For plot with title



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



####################################
## GRAPHING OF REACTION-TIME DATA ##
####################################

## DATA PREPARATION: REACTION TIMES ANALYSIS WITH MIXED-EFFECTS ANALYSIS

# Clear workspace
rm(list = ls())

# Load packages
library("lme4") # For modeling
library("Matrix")
library("stringr") # For renaming factor levels
library("sjPlot") # Needed for plotting
library("ggplot2") # Needed for plotting

# Load full dataset
dataset <- readRDS("../data/data_experiment1.rds")

# Trim original dataset -> Keep only 1 RT per trial
rt_data <- dataset[which(dataset$BIN_INDEX == 1), ]

# Remove participant with dyslexia
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

# Recode conditions ('verb_form' = SG vs. PL)
rt_data$verb_form <- rt_data$trial_condition
rt_data$verb_form <- gsub("diff_3pl", "PL", rt_data$verb_form)
rt_data$verb_form <- gsub("diff_3sg", "SG", rt_data$verb_form)
rt_data$verb_form <- gsub("same_3pl", "PL", rt_data$verb_form)
rt_data$verb_form <- gsub("same_3sg", "SG", rt_data$verb_form)

# Shorter variable names for better readability
rt_data$t_c <- rt_data$trial_condition_new # Trial condition
rt_data$t_c <- gsub("same number", "same", rt_data$t_c)
rt_data$t_c <- gsub("different number", "diff", rt_data$t_c)
rt_data$t_c <- as.factor(rt_data$t_c)
rt_data$trial_condition_new <- NULL
rt_data$vc <- rt_data$item_verbclass # Verb class (weak, strong)
rt_data$item_verbclass <- NULL
rt_data$vf <- rt_data$verb_form # Verb form (target / target number)
rt_data$vf <- as.factor(rt_data$vf)
rt_data$verb_form <- NULL
rt_data$wm <- rt_data$operationspan_score # Working memory
rt_data$operationspan_score <- NULL
rt_data$l <- rt_data$group # Language group
rt_data$group <- NULL
rt_data$lts <- rt_data$lextale_score # Lextale score
rt_data$lextale_score <- NULL
rt_data$tr_nr <- rt_data$TRIAL_INDEX # Trial number
rt_data$TRIAL_INDEX <- NULL
rt_data$s_id <- rt_data$participant_number # Subject ID
rt_data$participant_number <- NULL

# Center variables (necessary for modeling)
scale(rt_data$wm, scale = FALSE)
scale(rt_data$lts, scale = FALSE)
scale(rt_data$tr_nr, scale = FALSE)



## ADDITIONAL PREPARATION FOR PLOTTING

# Rename trial condition & levels
rt_data$condition <- rt_data$t_c
rt_data$condition <- str_replace_all(rt_data$condition, "same", "Baseline") # change strings: replace same number by Baseline
rt_data$condition <- str_replace_all(rt_data$condition, "diff", "Prediction") # change strings: replace diff number by Prediction

# Rename target number levels
rt_data$vf <- str_replace_all(rt_data$vf, "SG", "Singular")
rt_data$vf <- str_replace_all(rt_data$vf, "PL", "Plural")

# Turn into factor & adapt reference levels for graphing & modeling: condition = baseline; target number = SG; group = L1
rt_data$condition <- as.factor(rt_data$condition)
rt_data$vf <- as.factor(rt_data$vf)
rt_data$l <- as.factor(rt_data$l)

levels(rt_data$condition)
levels(rt_data$vf)
levels(rt_data$l)

# rt_data$condition <- factor(rt_data$condition, levels = c("Baseline", "Prediction"))
rt_data$vf <- factor(rt_data$vf, levels = c("Singular", "Plural"))
# rt_data$l <- factor(rt_data$l, levels = c("L1", "L2"))



## MODELING

model <- lmer(rt ~ 1 + condition * vf + condition * l + condition * wm + condition * tr_nr + (1 | s_id) + (1 | item_code), rt_data)



## GRAPHING OF PREDICTOR EFFECTS (RT)

# Documentation for plot_model function:
# https://www.rdocumentation.org/packages/sjPlot/versions/2.8.9/topics/plot_model
# Documentation for set_theme():
# https://www.rdocumentation.org/packages/sjPlot/versions/2.8.9/topics/set_theme



# GRAPHS: COMBINATION CATEGORICAL VARIABLES: TRIAL CONDITION x GROUP AND TRIAL CONDITION x TARGET (+ SWITCH AXES) [FIGURE 6]
set_theme(
  geom.outline.color = "black",
  title.align = "center",
  axis.title.color = "black",
  axis.textcolor.x = "black",
  axis.textcolor.y = "black",
  title.color = "black",
  base = theme_bw()
)

# Plot Group A
plot_mixed_RT_Group_A <- plot_model(model,
  type = "eff", # To get estimated means (predicted values: marginal effects)
  axis.lim = c(5350, 7500),
  terms = c("condition", "l"), # Select factors
  legend.title = "Group", # Ommit legend title
  axis.title = c("Trial condition", "Reaction time"),
  title = "", # Plot title
  colors = c("grey55", "black")
) + theme(plot.title = element_text(face = "bold")) + # Bold print for plot title
  theme(legend.position = "top") + # Put legend on top
  scale_x_discrete(limits = c("Baseline", "Prediction"), expand = c(0.7, 0)) # Move x axis ticks closer together
plot_mixed_RT_Group_A

# Plot Group B
plot_mixed_RT_Group_B <- plot_model(model,
  type = "eff", # To get estimated means (predicted values: marginal effects)
  axis.lim = c(5350, 7500),
  terms = c("l", "condition"), # Select factors
  legend.title = "Trial condition", # Ommit legend title
  axis.title = c("Group", "Reaction time"),
  title = "", # Plot title
  colors = c("grey55", "black")
) + theme(plot.title = element_text(face = "bold")) + # Bold print for plot title
  theme(legend.position = "top") + # Put legend on top
  scale_x_discrete(limits = c("L1", "L2"), expand = c(0.7, 0)) # Move x axis ticks closer together
plot_mixed_RT_Group_B

# Plot Target A
plot_mixed_RT_Target_A <- plot_model(model,
  type = "eff", # To get estimated means (predicted values: marginal effects)
  axis.lim = c(5350, 7500),
  terms = c("condition", "vf"), # Select factors
  legend.title = "Target", # Ommit legend title
  axis.title = c("Trial condition", "Reaction time"),
  title = "", # Plot title
  colors = c("grey55", "black")
) + theme(plot.title = element_text(face = "bold")) + # Bold print for plot title
  theme(legend.position = "top") + # Put legend on top
  scale_x_discrete(limits = c("Baseline", "Prediction"), expand = c(0.7, 0)) # Move x axis ticks closer together
plot_mixed_RT_Target_A

# Plot Target B
plot_mixed_RT_Target_B <- plot_model(model,
  type = "eff", # To get estimated means (predicted values: marginal effects)
  axis.lim = c(5350, 7500),
  terms = c("vf", "condition"), # Select factors
  legend.title = "Trial condition", # Ommit legend title
  axis.title = c("Target", "Reaction time"),
  title = "", # Plot title
  colors = c("grey55", "black")
) + theme(plot.title = element_text(face = "bold")) + # Bold print for plot title
  theme(legend.position = "top") + # Put legend on top
  scale_x_discrete(limits = c("Singular", "Plural"), expand = c(0.7, 0)) # Move x axis ticks closer together
plot_mixed_RT_Target_B

# Arrange plots
row1 <- ggarrange(plot_mixed_RT_Group_A, plot_mixed_RT_Group_B, plot_mixed_RT_Target_A, plot_mixed_RT_Target_B, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)

# Save
plotwidth <- 10; plotheight <- 10
ggsave("../plots/exp1_RT_Group+Target_switched.png", width = plotwidth, height = plotheight, units = "in", dpi = 600)



# GRAPH: COMBINATION CONTINUOUS VARIABLES - WORKING MEMORY & TRIAL NUMBER [FIGURE 7]
set_theme(
  geom.outline.color = "black",
  title.align = "center",
  axis.title.color = "black",
  axis.textcolor.x = "black",
  axis.textcolor.y = "black",
  title.color = "black",
  base = theme_bw()
)

# TRIAL CONDITION x WORKING MEMORY
plot_mixed_RT_WM <- plot_model(model,
  type = "eff", # To get estimated means (predicted values: marginal effects)
  axis.lim = c(5350, 7500),
  terms = c("wm", "condition"), # Select variables
  legend.title = "Trial condition",
  axis.title = c("Operation span test score", "Reaction time"),
  title = "Working memory", # Plot title
  colors = c("grey55", "black"),
  alpha = 0.3
) + theme(plot.title = element_text(face = "bold")) + # Bold print for plot title
  theme(legend.position = "top") +
  scale_x_continuous(breaks = seq(70, 100, 5)) # Lower limit, upper limit, tick mark every ...
plot_mixed_RT_WM

# Save plot
plotwidth <- 5; plotheight <- 5
dev.copy(png, "../plots/exp1_RT_WM.png", width = plotwidth, height = plotheight, units = "in", res = 320)
dev.off()

# TRIAL CONDITION x TRIAL NUMBER
plot_mixed_RT_trialnr <- plot_model(model,
  type = "eff", # To get estimated means (predicted values: marginal effects)
  axis.lim = c(5350, 7500),
  terms = c("tr_nr", "condition"), # Select variables
  legend.title = "Trial condition",
  axis.title = c("Trial number", "Reaction time"),
  title = "Trial number", # Plot title
  colors = c("grey55", "black"),
  alpha = 0.3
) + theme(plot.title = element_text(face = "bold")) + # Bold print for plot title
  theme(legend.position = "top") +
  scale_x_continuous(breaks = seq(0, 70, 10)) # Lower limit, upper limit, tick mark every ...
plot_mixed_RT_trialnr

# Save plot
plotwidth <- 5; plotheight <- 5
dev.copy(png, "../plots/exp1_RT_trialnr.png", width = plotwidth, height = plotheight, units = "in", res = 320)
dev.off()

# Arrange plots
ggarrange(plot_mixed_RT_WM, plot_mixed_RT_trialnr, labels = c("A", "B"), ncol = 2, nrow = 1)

# Save combined plot
plotwidth <- 10; plotheight <- 5
ggsave("../plots/exp1_RT_WM+trialnr.png", width = plotwidth, height = plotheight, units = "in", dpi = 600)



# # # End of script # # #