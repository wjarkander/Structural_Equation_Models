# Final Code for SEM Project: ####
# Clicking the document outline button top right is encouraged!

## Packages ####
library(lavaan) # Main analysis package
library(tidyverse) # For data wrangling with dplyr and ggplot
library(ggcorrplot) # For correlation plots with ggplot
library(ggpubr) # For arranging more than 1 plot on a single graphic
library(xtable) # For exporting tables to latex
library(psych) # For the KMO test

## DATA ####

### Importing data ####
# Setting the working directory if necessary
setwd("~/Desktop/Computer project submission")

# Importing as a tibble
AIdata <- read_csv("data.csv")

# Investigating the data
glimpse(AIdata)

### Data Cleaning ####
# Get rid of the columns we definitely aren't interested in 
# The first is just NAs, the 2nd and 3rd are the names of the AI tools used, which isn't really the focus of the study
AIdata <- AIdata %>% select(-c(response_date, ai_tools_raw, ai_tools_standardized))

# We have variable classes that we don't want. The ordinal variables are all character class when we want them as ordinal

#### Fixing the "frequency" based ordinal variables first
freqordinals <- c("organizes_notes_before_tests", "uses_supplementary_resources", "procrastination_frequency",
                  "study_groups_or_tutoring", "satisfaction_with_grades", "career_satisfaction_affects_motivation", 
                  "program_meets_expectations", "ai_use_frequency_for_study", "verifies_work_with_ai_after_completion")

# The categories for these ordinals 
freqs <- c("Never", "Rarely", "Occasionally", "Almost always", "Always")

# Applying to the data
AIdata <- AIdata %>% 
  # We need to use all_of because the variables are stored in an object
  # And the dot because that way R knows when to apply the re-structuring
  mutate(across(all_of(freqordinals), ~ factor(., levels = freqs, ordered = TRUE)))

#### Fixing the dependence based ordinals. The same procedure as above
depordinals <- c("perceived_ai_dependence", "perceived_creativity_reduction_from_ai", "prompt_engineering_knowledge")
deps <- c("Not at all", "Probably not", "Not sure", "Probably yes", "A lot")
AIdata <- AIdata %>% 
  mutate(across(all_of(depordinals), ~ factor(., levels = deps, ordered = TRUE)))

#### Fixing the groups, making them factors
groupvars <- c("gender", "non_local_student", "current_university_condition", "receives_university_support")
AIdata <- AIdata %>% 
  mutate(across(all_of(groupvars), as.factor))
glimpse(AIdata)

#### The Binary variables that are indicators, we make these ordered variables
YesNoordinals <- c("failed_evaluated_activity_due_to_ai", "uses_ai_for_nonacademic_activities")
AIdata <- AIdata %>% 
  mutate(across(all_of(YesNoordinals), ~ factor(., levels = c("No", "Yes"), ordered = TRUE)))

#### And all the others with other ordinal categories
AIdata <- AIdata %>% 
  mutate(ai_use_in_academic_tasks_is_fraud = factor(ai_use_in_academic_tasks_is_fraud,
                                                    levels = c("Strongly disagree", "Disagree", "Undecided", "Agree", "Strongly agree"),
                                                    ordered = TRUE))

AIdata <- AIdata %>% 
  mutate(ai_helped_understand_topics = factor(ai_helped_understand_topics,
                                              levels = c("Definitely no", "Probably no", "Neutral", "Probably yes", "Definitely yes"),
                                              ordered = TRUE))

AIdata <- AIdata %>% 
  mutate(self_rated_academic_performance = factor(self_rated_academic_performance,
                                                  levels = c("Poor", "Fair", "Good", "Excellent", "Outstanding"),
                                                  ordered = TRUE))

AIdata <- AIdata %>% 
  mutate(independent_study_time = factor(independent_study_time,
                                         levels = c("0-1", "1-2", "2-3", "3-4", "4-5", "More than 5"),
                                         ordered = TRUE))

AIdata <- AIdata %>% 
  mutate(study_method_effectiveness = factor(study_method_effectiveness,
                                             levels = c("Not effective at all", "Slightly effective", "Neutral", "Effective", "Very effective"),
                                             ordered = TRUE))

# New look at the data
glimpse(AIdata) # We have all the variable classes we want

### Data Exploration ####

#### Response Distributions ####

##### Ordinal data ####

# We store all the ordinal variables in their own data frame
ordinals <- AIdata %>%
  # We utilise the objects freqordinals, and depordinals we defined in the data cleaning 
  select(c(all_of(freqordinals), all_of(depordinals), ai_use_in_academic_tasks_is_fraud, ai_helped_understand_topics,
           self_rated_academic_performance, independent_study_time, study_method_effectiveness))

# We want to create a table of percentages at every level
table1 <- ordinals %>%
  # We make them all numeric to account for the fact they were all on different scales
  mutate(across(everything(), as.numeric)) %>%
  # We put the variable names in a column and responses in a column
  pivot_longer(cols = everything(), values_to = "Response", names_to = "Variable") %>%
  # We group by variable and response in order to later count the number of each specific outcome (same variable same response)
  group_by(Variable, Response) %>%
  count() %>% 
  # We transform these counts into percentages. 357 is our sample size
  mutate(n = round((n/357)*100, digits = 1)) %>%
  # We reformat so that it is easy to view in a table
  pivot_wider(names_from = Response, values_from = n)

# We print the table for our latex document
xtable(table1) # We add extra formatting in latex

##### Continuous data ####
continuous <- AIdata %>%
  # We select our continuous variables
  select(age_years, current_program_term, cumulative_gpa_papa, n_ai_tools) 

table2 <- continuous %>%
  # Like ordinal, we put our variables into one column
  pivot_longer(cols = everything(), values_to = "Response", names_to = "Variable") %>%
  # We only group by variable this time as each response is not a distinct level
  group_by(Variable) %>%
  # And we calculate chosen summary statistics
  summarise(
    Mean   = round(mean(Response), 2),
    SD     = round(sd(Response), 2),
    Median = round(median(Response), 2),
    Min    = min(Response),
    Max    = max(Response)
  )

# Printing the table for latex
xtable(table2)

##### Binary data ####

# Saving the data
binary <- AIdata %>%
  select(gender, failed_evaluated_activity_due_to_ai, uses_ai_for_nonacademic_activities)

table3 <- binary %>%
  # We make them all numeric to account for the fact they were all on different scales
  mutate(across(everything(), as.integer)) %>%
  # We put the variable names in a column and responses in a column
  pivot_longer(cols = everything(), values_to = "Response", names_to = "Variable") %>%
  # We group by variable and response in order to later count the number of each specific outcome (same variable same response)
  group_by(Variable, Response) %>%
  count() %>% 
  # We transform these counts into percentages. 357 is our sample size
  mutate(`%` = round((n/357)*100, digits = 1)) 

# We insert these values into our latex document
table3

#### Correlation Matrix ####

## Converting our ordinal variables to values
AIdata_allnumeric <- AIdata %>%
  mutate(across(where(is.ordered), as.integer))

## Removing the group variables
AIdata_allnumeric <- AIdata_allnumeric %>%
  select(-c(gender, non_local_student, current_university_condition, receives_university_support, academic_program))

# We rename the variables to save space in the plot later
colnames(AIdata_allnumeric) <- c("Age", "Term", "GPA", "IndStudy", "OrgNotes",
                                 "Effective", "SuppResources", "Procrastination", "StudyGroups",
                                 "SelfRatedPerform", "GradeSatisf", "CareerAffectsMotiv", 
                                 "ProgExpecs", "NAITools", "AIUseFreq", "AIHelpsUnderstand",
                                 "AI@UniFraud", "FailedDueToAI", "AIVerifies", "AIDependence",
                                 "AIReduceCreat", "GoodPrompter", "OtherAIUse")

# We calculate the correlation matrix with the lavaan function lavCor.
# It calculates the appropriate correlation depending on the data type, eg, polychoric for 2x ordinals
CorrMatrix <- lavCor(object = AIdata_allnumeric,
                    # You have to specify the ordinal/binary variables 
                    ordered = c("IndStudy", "OrgNotes", "Effective", "SuppResources", "Procrastination", "StudyGroups",
                                "SelfRatedPerform", "GradeSatisf", "CareerAffectsMotiv", "ProgExpecs", "NAITools", 
                                "AIUseFreq", "AIHelpsUnderstand", "AI@UniFraud", "FailedDueToAI", "AIVerifies", 
                                "AIDependence", "AIReduceCreat", "GoodPrompter", "OtherAIUse"))

# Making the first correlation plot
CorrPlot1 <- ggcorrplot(CorrMatrix,
                            lab = T,
                            lab_size = 2) +
  theme_bw() + 
  theme(text = element_text(family = "mono"),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 55, hjust = 1))

# Looking at the plot
CorrPlot1

# Making the second, grouped, correlation plot

# We rearrange the columns so the groups are together
GroupedPlotData <- AIdata_allnumeric %>%
  select(AIUseFreq, AIHelpsUnderstand, AIDependence, AIVerifies,
         GPA, Effective, SelfRatedPerform, GradeSatisf, 
         AIReduceCreat, Procrastination, FailedDueToAI,
         IndStudy, SuppResources, OrgNotes, everything())

# We calculate a new correlation matrix
GroupedCorrMatrix <- lavCor(object = GroupedPlotData,
                           ordered = c("IndStudy", "OrgNotes",
                                       "Effective", "SuppResources", "Procrastination", "StudyGroups",
                                       "SelfRatedPerform", "GradeSatisf", "CareerAffectsMotiv", 
                                       "ProgExpecs", "NAITools", "AIUseFreq", "AIHelpsUnderstand",
                                       "AI@UniFraud", "FailedDueToAI", "AIVerifies", "AIDependence",
                                       "AIReduceCreat", "GoodPrompter", "OtherAIUse"))

# And making the plot
CorrPlot2 <- ggcorrplot(GroupedCorrMatrix,
                          lab = T,
                          lab_size = 2) +
  theme_bw() + 
  theme(text = element_text(family = "mono"),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 55, hjust = 1)) + 
  # A lot of line segments. Not pretty but don't know a more efficient way
  geom_segment(x = 0.5, xend = 0.5, y = 0, yend = 4.5, colour = "darkgreen", linewidth = 1.5) +
  geom_segment(x = 4.5, xend = 4.5, y = 0, yend = 4.5, colour = "darkgreen", linewidth = 1.5) +
  geom_segment(x = 0.5, xend = 4.5, y = 0.5, yend = 0.5, colour = "darkgreen", linewidth = 1.5) +
  geom_segment(x = 0.5, xend = 4.5, y = 4.5, yend = 4.5, colour = "darkgreen", linewidth = 1.5) + 
  geom_segment(x = 4.5, xend = 4.5, y = 4.5, yend = 8.5, colour = "darkgreen", linewidth = 1.5) +
  geom_segment(x = 4.5, xend = 8.5, y = 4.5, yend = 4.5, colour = "darkgreen", linewidth = 1.5) +
  geom_segment(x = 4.5, xend = 8.5, y = 8.5, yend = 8.5, colour = "darkgreen", linewidth = 1.5) +
  geom_segment(x = 8.5, xend = 8.5, y = 4.5, yend = 8.5, colour = "darkgreen", linewidth = 1.5) +
  geom_segment(x = 8.5, xend = 11.5, y = 8.5, yend = 8.5, colour = "darkgreen", linewidth = 1.5) +
  geom_segment(x = 8.5, xend = 11.5, y = 11.5, yend = 11.5, colour = "darkgreen", linewidth = 1.5) +
  geom_segment(x = 8.5, xend = 8.5, y = 8.5, yend = 11.5, colour = "darkgreen", linewidth = 1.5) +
  geom_segment(x = 11.5, xend = 11.5, y = 8.5, yend = 11.5, colour = "darkgreen", linewidth = 1.5) +
  geom_segment(x = 11.5, xend = 11.5, y = 11.5, yend = 14.5, colour = "darkgreen", linewidth = 1.5) +
  geom_segment(x = 11.5, xend = 14.5, y = 11.5, yend = 11.5, colour = "darkgreen", linewidth = 1.5) +
  geom_segment(x = 11.5, xend = 14.5, y = 14.5, yend = 14.5, colour = "darkgreen", linewidth = 1.5) +
  geom_segment(x = 14.5, xend = 14.5, y = 11.5, yend = 14.5, colour = "darkgreen", linewidth = 1.5)

# Displaying the plots next to each other
ggarrange(CorrPlot1, CorrPlot2)

#### KMO Test ####
psych::KMO(AIdata_allnumeric)

##ANALYSIS ####

### Hypothesised Model ####
model_1 <- '
  StudyHabits =~ organizes_notes_before_tests + independent_study_time + uses_supplementary_resources
  AIUse =~ ai_helped_understand_topics + ai_use_frequency_for_study + verifies_work_with_ai_after_completion + perceived_ai_dependence
  Risks =~ procrastination_frequency + failed_evaluated_activity_due_to_ai + perceived_creativity_reduction_from_ai
  Success =~ satisfaction_with_grades + self_rated_academic_performance + cumulative_gpa_papa + study_method_effectiveness

  Success ~ AIUse + StudyHabits + Risks
  Risks ~ AIUse
  AIUse ~ StudyHabits
'

# Running the fit
fit_1 <- sem(model = model_1,
             data = AIdata,
             # We standardise latent variances
             std.lv = T,
             # We use the WLSMV estimator designed for ordinal data
             estimator = "WLSMV",
             # We have to specify all the ordinal/binary variables
             ordered = c("organizes_notes_before_tests", "independent_study_time", "uses_supplementary_resources",
                         "ai_helped_understand_topics", "ai_use_frequency_for_study", "verifies_work_with_ai_after_completion", "perceived_ai_dependence",
                         "procrastination_frequency", "failed_evaluated_activity_due_to_ai", "perceived_creativity_reduction_from_ai",
                         "satisfaction_with_grades", "self_rated_academic_performance", "study_method_effectiveness"))

# Checking fit measures
summary(fit_1, standardize = T, fit.measures = T, rsquare = T)
# RMSEA: 0.098, CFI: 0.819, TLI: 0.771, SRMR: 0.097. There's some room for improvement
modificationindices(fit_1, sort = T)

# We generate a table for our latex document
xtable(data.frame(RMSEA = 0.098, SRMR = 0.097, ScaledCFI = 0.819, ScaledTLI = 0.771))

### The first modification ####
# From the modification indices we choose to cross load supplementary resources onto AI-use
model_2 <- '
  StudyHabits =~ organizes_notes_before_tests + independent_study_time + uses_supplementary_resources
  AIUse =~ ai_helped_understand_topics + ai_use_frequency_for_study + verifies_work_with_ai_after_completion + perceived_ai_dependence + uses_supplementary_resources
  Risks =~ procrastination_frequency + failed_evaluated_activity_due_to_ai + perceived_creativity_reduction_from_ai
  Success =~ satisfaction_with_grades + self_rated_academic_performance + cumulative_gpa_papa + study_method_effectiveness

  Success ~ AIUse + StudyHabits + Risks
  Risks ~ AIUse
  AIUse ~ StudyHabits
'

fit_2 <- sem(model = model_2,
             data = AIdata,
             std.lv = T,
             estimator = "WLSMV",
             ordered = c("organizes_notes_before_tests", "independent_study_time", "uses_supplementary_resources",
                         "ai_helped_understand_topics", "ai_use_frequency_for_study", "verifies_work_with_ai_after_completion", "perceived_ai_dependence",
                         "procrastination_frequency", "failed_evaluated_activity_due_to_ai", "perceived_creativity_reduction_from_ai",
                         "satisfaction_with_grades", "self_rated_academic_performance", "study_method_effectiveness"))

summary(fit_2, standardize = T, fit.measures = T, rsquare = T)

# We generate a table for our latex document
xtable(data.frame(RMSEA = 0.077, SRMR = 0.083, ScaledCFI = 0.890, ScaledTLI = 0.859))

modificationindices(fit_2, sort = T) # No modification indices jump out at us.
# Instead we decide to tackle the fact that supplementary loads better onto AI-use than 
# it's original latent, Study Habits. We decide to drop it

### The second modification ####
# Drop supplementary resources from study habits
model_3 <- '
  StudyHabits =~ organizes_notes_before_tests + independent_study_time
  AIUse =~ ai_helped_understand_topics + ai_use_frequency_for_study + verifies_work_with_ai_after_completion + perceived_ai_dependence + uses_supplementary_resources
  Risks =~ procrastination_frequency + failed_evaluated_activity_due_to_ai + perceived_creativity_reduction_from_ai
  Success =~ satisfaction_with_grades + self_rated_academic_performance + cumulative_gpa_papa + study_method_effectiveness

  Success ~ AIUse + StudyHabits + Risks
  Risks ~ AIUse
  AIUse ~ StudyHabits
'

fit_3 <- sem(model = model_3,
             data = AIdata,
             std.lv = T,
             estimator = "WLSMV",
             ordered = c("organizes_notes_before_tests", "independent_study_time", 
                         "ai_helped_understand_topics", "ai_use_frequency_for_study", "verifies_work_with_ai_after_completion", "perceived_ai_dependence", "uses_supplementary_resources",
                         "procrastination_frequency", "failed_evaluated_activity_due_to_ai", "perceived_creativity_reduction_from_ai",
                         "satisfaction_with_grades", "self_rated_academic_performance", "study_method_effectiveness"))

summary(fit_3, standardize = T, fit.measures = T, rsquare = T)

# We generate a table for our latex document
xtable(data.frame(RMSEA = 0.082, SRMR = 0.086, ScaledCFI = 0.875, ScaledTLI = 0.842))

modificationindices(fit_3, sort = T)

### Final modification ####
# The modification indices suggest correlating satisfaction with grades and self rated academic perf
# For a mi of 10.661. We do that
model_4 <- '
  StudyHabits =~ organizes_notes_before_tests + independent_study_time
  AIUse =~ ai_helped_understand_topics + ai_use_frequency_for_study + verifies_work_with_ai_after_completion + perceived_ai_dependence + uses_supplementary_resources
  Risks =~ procrastination_frequency + failed_evaluated_activity_due_to_ai + perceived_creativity_reduction_from_ai
  Success =~ satisfaction_with_grades + self_rated_academic_performance + cumulative_gpa_papa + study_method_effectiveness

  Success ~ AIUse + StudyHabits + Risks
  Risks ~ AIUse
  AIUse ~ StudyHabits
  
  satisfaction_with_grades ~~ self_rated_academic_performance
'

fit_4 <- sem(model = model_4,
             data = AIdata,
             estimator = "WLSMV",
             std.lv = T,
             ordered = c("organizes_notes_before_tests", "independent_study_time", 
                         "ai_helped_understand_topics", "ai_use_frequency_for_study", "verifies_work_with_ai_after_completion", "perceived_ai_dependence", "uses_supplementary_resources",
                         "procrastination_frequency", "failed_evaluated_activity_due_to_ai", "perceived_creativity_reduction_from_ai",
                         "satisfaction_with_grades", "self_rated_academic_performance", "study_method_effectiveness"))

summary(fit_4, standardize = T, fit.measures = T, rsquare = T)

# We generate a table for our latex document
xtable(data.frame(RMSEA = 0.080, SRMR = 0.085, ScaledCFI = 0.883, ScaledTLI = 0.850))

modificationindices(fit_4, sort = T)

## MULTIGROUP ####

### The shift to 3 factors ####
threelv_1 <- '
  StudyHabits =~ organizes_notes_before_tests + independent_study_time
  AIUse =~ ai_helped_understand_topics + ai_use_frequency_for_study + verifies_work_with_ai_after_completion + perceived_ai_dependence + uses_supplementary_resources + perceived_creativity_reduction_from_ai
  Success =~ satisfaction_with_grades + self_rated_academic_performance + cumulative_gpa_papa + study_method_effectiveness

  Success ~ AIUse + StudyHabits
  AIUse ~ StudyHabits
  
  satisfaction_with_grades ~~ self_rated_academic_performance
'

threelvfit_1 <- sem(model = threelv_1,
                    data = AIdata,
                    std.lv = T,
                    estimator = "WLSMV",
                    ordered = c("organizes_notes_before_tests", "independent_study_time", 
                                "ai_helped_understand_topics", "ai_use_frequency_for_study", "verifies_work_with_ai_after_completion", "perceived_ai_dependence", "uses_supplementary_resources", "perceived_creativity_reduction_from_ai",
                                "satisfaction_with_grades", "self_rated_academic_performance", "study_method_effectiveness"))

summary(threelvfit_1, standardize = T, fit.measures = T, rsquare = T)

xtable(data.frame(RMSEA = 0.084, SRMR = 0.075, ScaledCFI = 0.903, ScaledTLI = 0.872))

# Drop creativity
threelv_2 <- '
  StudyHabits =~ organizes_notes_before_tests + independent_study_time
  AIUse =~ ai_helped_understand_topics + ai_use_frequency_for_study + verifies_work_with_ai_after_completion + perceived_ai_dependence + uses_supplementary_resources
  Success =~ satisfaction_with_grades + self_rated_academic_performance + cumulative_gpa_papa + study_method_effectiveness

  Success ~ AIUse + StudyHabits
  AIUse ~ StudyHabits
  
  satisfaction_with_grades ~~ self_rated_academic_performance
'

threelvfit_2 <- sem(model = threelv_2,
                    data = AIdata,
                    std.lv = T,
                    estimator = "WLSMV",
                    ordered = c("organizes_notes_before_tests", "independent_study_time", 
                                "ai_helped_understand_topics", "ai_use_frequency_for_study", "verifies_work_with_ai_after_completion", "perceived_ai_dependence", "uses_supplementary_resources",
                                "satisfaction_with_grades", "self_rated_academic_performance", "study_method_effectiveness"))

summary(threelvfit_2, standardize = T, fit.measures = T, rsquare = T)

xtable(data.frame(RMSEA = 0.073, SRMR = 0.067, ScaledCFI = 0.938, ScaledTLI = 0.914))

modificationindices(threelvfit_2, sort = T)

# Drop supplementary
threelv_3 <- '
  StudyHabits =~ organizes_notes_before_tests + independent_study_time
  AIUse =~ ai_helped_understand_topics + ai_use_frequency_for_study + verifies_work_with_ai_after_completion + perceived_ai_dependence
  Success =~ satisfaction_with_grades + self_rated_academic_performance + cumulative_gpa_papa + study_method_effectiveness

  Success ~ AIUse + StudyHabits
  AIUse ~ StudyHabits
  
  satisfaction_with_grades ~~ self_rated_academic_performance
'

threelvfit_3 <- sem(model = threelv_3,
                    data = AIdata,
                    std.lv = T,
                    estimator = "WLSMV",
                    ordered = c("organizes_notes_before_tests", "independent_study_time", 
                                "ai_helped_understand_topics", "ai_use_frequency_for_study", "verifies_work_with_ai_after_completion", "perceived_ai_dependence",
                                "satisfaction_with_grades", "self_rated_academic_performance", "study_method_effectiveness"))

summary(threelvfit_3, standardize = T, fit.measures = T, rsquare = T)

xtable(data.frame(RMSEA = 0.067, SRMR = 0.062, ScaledCFI = 0.953, ScaledTLI = 0.932))

modificationindices(threelvfit_3, sort = T)

### Measurement Invariance ####

# We select the variables in our final multigroup model
MGdata <- AIdata %>%
  select(c(gender, organizes_notes_before_tests, independent_study_time,
           ai_helped_understand_topics, ai_use_frequency_for_study, verifies_work_with_ai_after_completion, perceived_ai_dependence,
           satisfaction_with_grades, self_rated_academic_performance, cumulative_gpa_papa, study_method_effectiveness))

# We convert to continuous
MGdata_num <- MGdata %>%
  mutate(across(where(is.ordered), as.numeric))

# Split the data sets
MGdata_num_men <- MGdata_num %>%
  filter(gender == "Male")

MGdata_num_women <- MGdata_num %>%
  filter(gender == "Female")

# Test the model fits for both
MGfit_num_all <- sem(model = threelv_3,
                     data = MGdata_num,
                     std.lv = T,
                     estimator = "MLR")

MGfit_num_men <- sem(model = threelv_3,
                 data = MGdata_num_men,
                 std.lv = T,
                 estimator = "MLR")

MGfit_num_women <- sem(model = threelv_3,
                   data = MGdata_num_women,
                   std.lv = T,
                   estimator = "MLR")

# Fits
summary(MGfit_num_all, standardize = T, fit.measures = T, rsquare = T)
summary(MGfit_num_men, standardize = T, fit.measures = T, rsquare = T)
summary(MGfit_num_women, standardize = T, fit.measures = T, rsquare = T)

#### Metric, Scalar, Strict ####
# We'll check the full measurement invariance anyway

# Proceeding with the measurement tests
Baseline <- sem(model = threelv_3, 
                data = MGdata_num, 
                estimator = "MLR",
                group = "gender",
                meanstructure = T)

First <- sem(model = threelv_3, 
             data = MGdata_num, 
             estimator = "MLR",
             group = "gender",
             meanstructure = T,
             group.equal = c("loadings"))

Second <- sem(model = threelv_3, 
              data = MGdata_num, 
              estimator = "MLR",
              group = "gender",
              meanstructure = T,
              group.equal = c("loadings", "intercepts"))

Third <- sem(model = threelv_3, 
             data = MGdata_num, 
             estimator = "MLR",
             group = "gender",
             meanstructure = T,
             group.equal = c("loadings", "intercepts", "residuals"))

lavTestLRT(Baseline, First, Second, Third)

# Yeah not completely invariate for sure.

# Takk

