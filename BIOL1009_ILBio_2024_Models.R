###########################################################
## BTL Dissertation Chapter
## con't CBS Inclusive Excellence Project
## Katie Krueger 
## 
## Worked on File Last: Feb 1, 2025
#############################################################

##############################
## 0 - Load libraries
##############################
# library(broom) 
# library(corrr)
library(dplyr)
library(gtools)
library(ggplot2)
library(readr)
library(tidyr)
library(rlang)

############################## 
## 1 - Source File 
##############################
# Katie
setwd("/Users/katiekrueger/Library/CloudStorage/Box-Box/Krueger- Inclusive Excellence/")

#Call and Read in data file 
Bio_All <- read.csv("btl_all_1009_TRIO_Proj_Final_Export.csv")

#Call and Read in codebook file 
codbook_BTL <- read.csv("Codebook_TRIO_btl_all_BIOL1009.csv")

#Deal with Missing Data
#Add NAs to all empty strings & "blank" (e.g., spaces, NA values already) cells 
#Add new column for EDHD_1620 Enrollment
Bio_All <- Bio_All |> 
  mutate(Subject_Course = na_if(trimws(Subject_Course), "")) |>  # Trim whitespace and replace empty strings with NA
  mutate(Enrollment_IL_Bio = case_when(
    Subject_Course == "EDHD_1620" ~ 1,               # 1 if "EDHD_1620"
    is.na(Subject_Course) ~ 0,                       # 0 if NA
    Subject_Course == "" ~ 0,                        # 0 if empty string (after trimming)
    TRUE ~ NA_real_))                                # NA for all other cases





##############################
## ?? - End Jan, Modeling
##############################

# Load the necessary package
library(MASS)
library(dplyr)

# Example data preparation (replace 'your_data' with your actual data frame)
Bio_All$crse_grade_off <- factor(Bio_All$crse_grade_off, 
                                 ordered = TRUE)  # Ensure outcome is ordered

# Replace empty strings with NA only in character columns
Bio_All <- Bio_All |>
  dplyr::mutate_if(is.character, ~na_if(., ""))

#########
# recreating grade distribution but w/ binning
# chat gpt helped write a function for binning
bin_grades <- function(grade) {
  if (grade %in% c('A', 'A-', 'A+')) {
    return('A')
  } else if (grade %in% c('B+', 'B', 'B-')) {
    return('B')
  } else if (grade %in% c('C+', 'C', 'C-')) {
    return('C')
  } else if (grade %in% c('D+', 'D')) {
    return('D')
  } else if (grade %in% c('F')) {
    return('F')
  } else if (grade %in% c('W')) {
    return('W')
  } else {
    return(NA)
  }
}

# Create new column with grades binned
Bio_All <- Bio_All |>
  mutate(Grade_bin_off = map_chr(crse_grade_off, bin_grades))

# Level grades as a factor (using Base R)
grade_order <- c("A", "B", "C", "D", "F", "W", "NA")
Bio_All$Grade_bin_off <- factor(Bio_All$Grade_bin_off, 
                                levels = grade_order)
# Check the levels
levels(Bio_All$Grade_bin_off)

#####
#Add new column for EDHD_1620 Enrollment
unique(Bio_All$Subject_Course)
Bio_All <- Bio_All |> 
  mutate(Subject_Course = na_if(trimws(Subject_Course), "")) |>  # Trim whitespace and replace empty strings with NA
  mutate(Enrollment_IL_Bio = case_when(
    Subject_Course == "EDHD_1620" ~ 1,  # 1 if "EDHD_1620" (so IL Bio Enrolled => 1)
    is.na(Subject_Course) ~ 0,          # 0 if NA
    Subject_Course == "" ~ 0,           # 0 if empty string (after trimming)
    TRUE ~ NA_real_))                   # NA for all other cases
unique(Bio_All$Enrollment_IL_Bio)


#####
# Subset Bio_All so I only have one Grade output per student
# 1) Count unique deid values using length
length(unique(Bio_All$deid))
# There are 11807 unique students in this data set. Check this once I subset

# 2) Subset, should have 11807 rows after subsetting
Bio_All_forOLR <- Bio_All %>%
  distinct(deid, .keep_all = TRUE)
# View the result
length(unique(Bio_All_forOLR$deid))



######
# Ensure predictors are factors & leveled appropriately 
unique(Bio_All_forOLR$sex)
Bio_All_forOLR$sex <- factor(Bio_All_forOLR$sex, levels = c("Female", "Male"))
levels(Bio_All_forOLR$sex)

unique(Bio_All_forOLR$urm)
Bio_All_forOLR$urm <- factor(Bio_All_forOLR$urm, levels = c("No", "Yes", "NA"))
levels(Bio_All_forOLR$urm)

unique(Bio_All_forOLR$fgen)
Bio_All_forOLR$fgen <- factor(Bio_All_forOLR$fgen, 
                              levels = c("Continuing generation", "First generation", "NA"))
levels(Bio_All_forOLR$fgen)

Bio_All_forOLR$Enrollment_IL_Bio <- factor(Bio_All_forOLR$Enrollment_IL_Bio, levels = c("0", "1"))
levels(Bio_All_forOLR$Enrollment_IL_Bio)

unique(Bio_All_forOLR$nnes)
Bio_All_forOLR$nnes <- factor(Bio_All_forOLR$nnes, levels = c("No", "Yes"))
levels(Bio_All_forOLR$nnes)

unique(Bio_All_forOLR$academic_program)
Bio_All_forOLR$academic_program <- factor(Bio_All_forOLR$academic_program)
Bio_All_forOLR$academic_program <- relevel(Bio_All_forOLR$academic_program, ref = "College of Biological Sciences")
levels(Bio_All_forOLR$academic_program)

unique(Bio_All_forOLR$pes)
Bio_All_forOLR$pes <- factor(Bio_All_forOLR$pes)
levels(Bio_All_forOLR$pes)

unique(Bio_All_forOLR$trio)
Bio_All_forOLR$trio <- factor(Bio_All_forOLR$trio)
levels(Bio_All_forOLR$trio)


##############
# Fit ordinal logistic regression models
##############

# Does enrollment in IL Biology (1,0) predict official BIOL1009 grade (binned by letter)
m0_OLR <- MASS::polr(Grade_bin_off ~ 
                       1, 
                     data = Bio_All_forOLR, 
                     method = "logistic")  # Using logistic regression
# Summarize the model
summary(m0_OLR)


m1_OLR <- MASS::polr(Grade_bin_off ~ 
                       Enrollment_IL_Bio, 
                     data = Bio_All_forOLR, 
                     method = "logistic")  # Using logistic regression

# Summarize the model
summary(m1_OLR)
# Conclusions: 
# Enrollment_IL_Bio is a strong predictor of for getting a higher grade in BIOL1009 (t-value = 9.11)
# I know this because the coefficient of 1.243 is positive (as Enrollment_IL_Bio increases, the likelihood of a student earning a higher ordinal grade also increases.)
# Enrollment_IL_Bio is numeric; 0 = no ILBio, and 1 = yes ILBio (confirmed if factored so that ref = no)
# Further, each border (jump between letter grades) is significant except the border between W & NA which makes sense as NA is just missing data
# The model fit seems okay with a low residual deviance (22963.02) and AIC value of 22977.02 ...
# Note, there were "3921 observations deleted due to missingness" CHECK LATER


m2_OLR <- MASS::polr(Grade_bin_off ~ 
                       Enrollment_IL_Bio + sex, 
                     data = Bio_All_forOLR, 
                     method = "logistic")  # Using logistic regression

# Summarize the model
summary(m2_OLR)
# Sex doesn't matter (coefficient for sexMale is small 0.03351 and not sig) 
# Being a male compared to a female (reference category), does not significantly increase the odds of receiving a higher grade

m3_OLR <- MASS::polr(Grade_bin_off ~ 
                       Enrollment_IL_Bio + fgen, 
                     data = Bio_All_forOLR, 
                     method = "logistic")  # Using logistic regression

# Summarize the model
summary(m3_OLR)
# Sex doesn't matter (coefficient for sexMale is small 0.03351 and not sig) 
# Being a male compared to a female (reference category), does not significantly increase the odds of receiving a higher grade

m4_OLR <- MASS::polr(Grade_bin_off ~ 
                       Enrollment_IL_Bio + nnes, 
                     data = Bio_All_forOLR, 
                     method = "logistic")  # Using logistic regression
# Warning Message: design appears to be rank-deficient, so dropping some coefs
# Summarize the model
summary(m4_OLR)
# Note: I did see a sig results for both predictors Enrollment_IL_Bio + nnes
# I did however think that there is enough overlap with nnes (First lang is not English)
# so I worry about some level of collinearity between these two. 

m5_OLR <- MASS::polr(Grade_bin_off ~ 
                       Enrollment_IL_Bio + age_first_day_of_term, 
                     data = Bio_All_forOLR, 
                     method = "logistic")  # Using logistic regression

# Summarize the model
summary(m5_OLR)
# Enrollment_IL_Bio & the age of the student on the first day of the term are strong predictors of for getting a higher grade in BIOL1009 (t-value = 9.372 & 5.269 respectively).
# I know this because the coefficient of 1.28232 is positive (as Enrollment_IL_Bio increases, the likelihood of a student earning a higher ordinal grade also increases.)
# Enrollment_IL_Bio is numeric; 0 = no ILBio, and 1 = yes ILBio (confirmed if factored so that ref = no)
# Further, each border (jump between letter grades) is significant except the border between W & NA which makes sense as NA is just missing data
# The model fit seems okay with a low residual deviance (22963.02) and AIC value of 22977.02 ...

# For Poster: 
# Enrollment in IL Biology course & a greater age at the start of the term are both are both strong predictors of earning a higher grade in BIOL1009 (t-value = 9.372 & 5.269 respectively).
# 

m5_OLR_interaction <- MASS::polr(Grade_bin_off ~ 
                       Enrollment_IL_Bio * age_first_day_of_term, 
                     data = Bio_All_forOLR, 
                     method = "logistic")  # Using logistic regression
summary(m5_OLR_interaction)

m6_OLR <- MASS::polr(Grade_bin_off ~ 
                       Enrollment_IL_Bio + pes, 
                     data = Bio_All_forOLR, 
                     method = "logistic")  # Using logistic regression
# Summarize the model
summary(m6_OLR)
# The positive coefficient (1.2241) for pes here (reference group is no, not PES) that 
# students who participate in the program have a higher likelihood of achieving 
# higher grades (A > B > C > D > F), relative to students who do not participate
# in pes (t-value = 17.793).

m7_OLR <- MASS::polr(Grade_bin_off ~ 
                       Enrollment_IL_Bio + trio, 
                     data = Bio_All_forOLR, 
                     method = "logistic")  # Using logistic regression
# Summarize the model
summary(m7_OLR)
# Enrollment in IL Biology has a coefficient of 0.1126, which suggests that students enrolled in IL Biology are slightly more likely to achieve a higher grade compared to those not enrolled, though the effect is not statistically significant (t-value = 0.578).
# trioY (Participation in the TRIO program) has a coefficient of 1.3360, which indicates that students in the TRIO program are 3.8 times more likely to achieve a higher grade than those who are not in the program (t-value = 8.027, statistically significant).

m7_OLR_interaction <- MASS::polr(Grade_bin_off ~ Enrollment_IL_Bio * trio, 
                                 data = Bio_All_forOLR, 
                                 method = "logistic")
summary(m7_OLR_interaction)
# While being in the TRIO program is positively associated with receiving higher grades, the effect of IL Biology enrollment on grades does not significantly differ for TRIO versus non-TRIO students. The interaction term does not significantly alter the interpretation of the main effects.
# Not sure this interaction is helpful as there are very few students NOT in Trio that take IL Bio.



# NOTE: Ran model with Enrollment_IL_Bio PLUS other predictors
# Each one got Warning Message: design appears to be rank-deficient, so dropping some coefs
# ^^^ Same for predictors alone: urm, fgen, academic_program



print(AIC(m0_OLR, m1_OLR, m5_OLR, m5_OLR_interaction))
# Adding the Interaction Term (m5_OLR_interaction) Doesn't Improve the Model: 
# The very small increase in AIC (22951.20 vs. 22950.38) indicates that adding 
# the interaction between Enrollment_IL_Bio and age_first_day_of_term does not 
# result in a significantly better fit to the data. In fact, the AIC has barely 
# increased, suggesting that the additional complexity of the interaction term 
# does not justify its inclusion.

# Model Comparison: Given that m5_OLR has the lowest AIC (22950.38) among the 
# models you're comparing, m5_OLR seems to be the best-fitting model among the 
# ones tested, as it balances model complexity and goodness of fit well.



print(AIC(m0_OLR, m1_OLR, m5_OLR, m6_OLR, m7_OLR))
# Check the number of observations used in each model
length(m0_OLR$fitted.values)  # Number of observations for m0_OLR
length(m1_OLR$fitted.values)  # Number of observations for m1_OLR
length(m5_OLR$fitted.values)  # Number of observations for m5_OLR
length(m6_OLR$fitted.values)  # Number of observations for m6_OLR
length(m7_OLR$fitted.values)  # Number of observations for m7_OLR
# **Issues different lengths. Deal with later


######
## For Poster (Feb 3, 2025)

#Repeated from above m5_OLR
m5_OLR_poster <- MASS::polr(Grade_bin_off ~ 
                        Enrollment_IL_Bio + age_first_day_of_term, 
                      data = Bio_All_forOLR, 
                      method = "logistic")  # Using logistic regression
 
# Summarize the model
summary(m5_OLR_poster)

##GET P VALUES FOR POSTER
# Coefficients and standard errors from the summary(m5_ORL_poster) above
coef_vals <- c(1.28232, 0.04208)  # Coefficients for Enrollment_IL_Bio and age_first_day_of_term
se_vals <- c(0.136824, 0.007986)  # Standard errors for the coefficients
# Calculate t-values (Estimate / Standard Error)
t_vals <- coef_vals / se_vals
# Calculate p-values from t-values (assuming normal distribution; 2-tailed test)
p_vals <- 2 * (1 - pnorm(abs(t_vals)))  # Two-tailed p-values
p_vals
#Output = [1] 0.000000e+00 1.370038e-07


##COMPUTE McFadden's R2 VALUES FOR POSTER
# Note: A simple R2 like for linear regression does not exist for logisitc models...McFadden's is used with Logistic models.
# Extract log-likelihood for the model
logLik_model_value <- logLik(m5_OLR_poster)[1]  # Correct extraction of log-likelihood
# Fit a null model (intercept only)
m5_null_poster <- MASS::polr(Grade_bin_off ~ 1, data = Bio_All_forOLR, method = "logistic")
# Log-likelihood for the null model
logLik_null_value <- logLik(m5_null_poster)[1]  # Correct extraction of log-likelihood
# McFadden's R-squared
McFadden_R2 <- 1 - (logLik_model_value / logLik_null_value)
McFadden_R2

# Alternatives to McFadden's
# Cox-Snell R² (Note: limited in that range of values can exceed 1...so 
# interpretion can be tricky... way to account for that is Nagelkerke R²)
# Extract log-likelihood for the full model
logLik_full <- logLik(m5_OLR_poster)
# Extract log-likelihood for the null model
logLik_null <- logLik(m5_null_poster)
# Number of observations
n <- nrow(Bio_All_forOLR)
# Cox-Snell R²
cox_snell_R2 <- 1 - (exp(logLik_null - logLik_full))^(2 / n)
cox_snell_R2

# Nagelkerke R² requires calculating the likelihood of the saturated model. 
# In logistic regression, the saturated model is the one where each observation 
# has its own category (i.e., each outcome is perfectly predicted by the model). 
# Calculate likelihood of the saturated model (perfect fit)
logLik_saturated <- logLik(update(m5_OLR_poster, . ~ . + 1))

# Nagelkerke R²
nagelkerke_R2 <- cox_snell_R2 / (1 - (exp(logLik_null - logLik_saturated))^(2 / n))
nagelkerke_R2










############
## Look at average 
hist(Bio_All$cl01)
hist(Bio_All$all_science_confidence_mean)

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Remove NA values from the data to avoid errors in ggplot
Bio_All_forOLR_clean <- Bio_All_forOLR %>%
  filter(!is.na(all_science_confidence_mean) & !is.na(group))

# Calculate the sample sizes for each group, excluding NA values
group_sizes <- Bio_All_forOLR_clean %>%
  group_by(group) %>%
  summarize(n = n()) %>%
  ungroup()

# View the sample sizes for each group
print(group_sizes)
# A tibble: 4 × 3
#group                             n group_label                          
#<chr>                         <int>      <chr>                                
# 1 No PES & No TRIO               5864   No PES & No TRIO (n=5864)            
# 2 Yes PES, No TRIO                536   Yes PES, No TRIO (n=536)             
# 3 Yes PES, Yes TRIO, No IL Bio     77   Yes PES, Yes TRIO, No IL Bio (n=77)  
# 4 Yes PES, Yes TRIO, Yes IL Bio   129   Yes PES, Yes TRIO, Yes IL Bio (n=129)


# Combine the group names with sample sizes for legend labels
group_sizes$group_label <- paste(group_sizes$group, " (n=", group_sizes$n, ")", sep="")

# Define a custom color palette for each group (adjusted to new group names)
group_colors <- c("No PES & No TRIO" = "#0073e6",    # Blue
                  "Yes PES, No TRIO" = "#00bf7d",    # Green
                  "Yes PES, Yes TRIO, No IL Bio" = "#00b4c5",  # Teal
                  "Yes PES, Yes TRIO, Yes IL Bio" = "#5928ed")    # Purple

# Make sure we only keep valid groups in the plotting data
valid_groups <- group_sizes %>%
  filter(!is.na(group)) %>%
  pull(group)

# Define the labels for the x-axis
labels <- c("1" = "Not at all like me",
            "2" = "Not much like me",
            "3" = "Somewhat like me",
            "4" = "Mostly like me",
            "5" = "Very much like me")

# Plot with LOESS curve and proportions on the y-axis
ggplot(Bio_All_forOLR_clean %>% filter(group %in% valid_groups), aes(x = all_science_confidence_mean, fill = group)) + 
  # Density curve with area fill and color mapped to group
  geom_density(aes(color = group, y = ..density..), alpha = 0.8, size = 1.2, adjust = 1.5) +  # Adjust for smoother curve and proportions
  # Facet by group with free y-axis, arranged in one row
  facet_grid(~group, scales = "free_y") +  # Change from facet_wrap() to facet_grid() for side-by-side
  theme_minimal() +
  labs(title = "Density of All Science Confidence Mean by Group",
       x = "All Science Confidence Mean",
       y = "Proportion") +  # Change y-axis label to "Proportion"
  scale_x_continuous(limits = c(0, 5), breaks = 1:5, labels = c("1" = "Not at all like me", 
                                                                "2" = "Not much like me", 
                                                                "3" = "Somewhat like me", 
                                                                "4" = "Mostly like me", 
                                                                "5" = "Very much like me")) +  # Custom x-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  # Add vertical line at x = 2.5 (midpoint)
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "maroon", size = 1) +
  # Set custom colors for groups and adjust the LOESS line to be a darker shade of the same color
  scale_fill_manual(values = group_colors) + 
  scale_color_manual(values = group_colors) + 
  # Remove the legend
  theme(legend.position = "none") + 
  # Optional: Adjust facet label size and position if necessary
  theme(strip.text = element_text(size = 12, face = "plain"))



########## 
# On Pre-surveys Do students view themselves as scientists? 
# Question coded as cl01 
# "How well does the following describe the way you think of yourself? A Science Person" 
# Response options were coded 1-5 and relate to likert scale of...
# Not at all like me, Not much like me, Somewhat like me, Mostly like me, Very much like me.


# Load necessary libraries
library(ggplot2)
library(dplyr)

# Step 1: Subset the data for Pre-BIOL 1009 survey_time
Bio_All_Pre <- Bio_All %>%
  filter(survey_time == "Pre-BIOL 1009")

# Step 2: Check for duplicate students based on "deid"
duplicate_students <- Bio_All_Pre %>%
  group_by(deid) %>%
  filter(n() > 1) %>%
  ungroup()

# View the duplicate students (if any)
print(duplicate_students)

# Step 3: Subset again, removing duplicates
Bio_All_Pre_unique <- Bio_All_Pre %>%
  distinct(deid, .keep_all = TRUE)

# Step 4: Create the new group variable (same as before)
Bio_All_Pre_unique <- Bio_All_Pre_unique %>%
  mutate(group = case_when(
    pes == "N" & trio == "N" ~ "No PES & No TRIO",  # Non-PES and Non-TRIO
    pes == "Y" & trio == "N" ~ "Yes PES, No TRIO",  # PES & Non-TRIO
    trio == "Y" & Enrollment_IL_Bio == "0" ~ "Yes PES, Yes TRIO, No IL Bio",  # PES & TRIO & Non-IL Bio enrolled
    trio == "Y" & Enrollment_IL_Bio == "1" ~ "Yes PES, Yes TRIO, Yes IL Bio",  # PES & TRIO & IL Bio enrolled
    TRUE ~ NA_character_  # In case there are any other combinations
  ))

# Step 5: Remove the NA group from the data
Bio_All_Pre_unique <- Bio_All_Pre_unique %>%
  filter(!is.na(group))

# Step 6: Calculate the sample sizes for each group (removing NAs)
group_sizes <- Bio_All_Pre_unique %>%
  group_by(group) %>%
  summarize(n = n()) %>%
  ungroup()

# Combine the group names with sample sizes for legend labels
group_sizes$group_label <- paste(group_sizes$group, " (n=", group_sizes$n, ")", sep="")

# Step 7: Calculate the proportions for each response (1–5) by group
response_proportions <- Bio_All_Pre_unique %>%
  group_by(group, cl01) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  left_join(group_sizes, by = "group") %>%
  mutate(proportion = count / n)  # Calculate proportions

# Store the sample sizes in a separate tibble for later use
sample_sizes_tibble <- group_sizes

# Step 8: Define the color palette for groups (same as before)
group_colors <- c("No PES & No TRIO" = "#0073e6",    # Blue
                  "Yes PES, No TRIO" = "#00bf7d",    # Green
                  "Yes PES, Yes TRIO, No IL Bio" = "#00b4c5",  # Teal
                  "Yes PES, Yes TRIO, Yes IL Bio" = "#5928ed")    # Purple

# Step 9: Create the histogram with proportions (no sample size text)
ggplot(response_proportions, aes(x = cl01, y = proportion, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", color = NA, alpha = 0.8) +  # Remove black lines by setting color = NA
  facet_grid(~group, scales = "free_y") +  # Facet by group
  theme_minimal() +
  labs(title = "Proportions of Responses to 'A Science Person' by Group",
       x = "Response to 'How well does the following describe the way you think of yourself? A Science Person'",
       y = "Proportion") +
  scale_x_continuous(breaks = 1:5, labels = c("1" = "Not at all like me",
                                              "2" = "Not much like me",
                                              "3" = "Somewhat like me",
                                              "4" = "Mostly like me",
                                              "5" = "Very much like me")) +  # Custom x-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  scale_fill_manual(values = group_colors) +  # Set fill color to match previous plot
  theme(legend.position = "none") +  # Remove legend
  theme(strip.text = element_text(size = 12, face = "plain"))  # Optional: Adjust facet label size and position

# View the sample sizes tibble for later use
print(sample_sizes_tibble)
# # A tibble: 4 × 3
# group                               n     group_label                          
# <chr>                             <int>   <chr>                                
#   1 No PES & No TRIO               5456   No PES & No TRIO (n=5456)            
#   2 Yes PES, No TRIO                503   Yes PES, No TRIO (n=503)             
#   3 Yes PES, Yes TRIO, No IL Bio     78   Yes PES, Yes TRIO, No IL Bio (n=78)  
#   4 Yes PES, Yes TRIO, Yes IL Bio   119   Yes PES, Yes TRIO, Yes IL Bio (n=119)





#############
## Standardize the Model
#############

#vvvv Attempts to standardizing the grades NOTHING below is complete (2/1/2025)

## TRY 
# Attempt to Standardize Grades
# Step 1: Fit a model to predict current grades based on previous GPA
# Assuming Grade_bin_off is numeric (or treated as such for prediction purposes)
model_gpa <- lm(Grade_bin_off ~ GPA, data = Bio_All)

# Step 2: Use the model to predict the expected grade based on GPA
Bio_All$predicted_grade <- predict(model_gpa, newdata = Bio_All)

# Step 3: Calculate residuals (the difference between actual and predicted grade)
Bio_All$residuals <- Bio_All$Grade_bin_off - Bio_All$predicted_grade

# Step 4: (Optional) Standardize the residuals to get a standardized score
# This represents how a student's grade deviates from the expected grade based on their GPA
Bio_All$standardized_grade <- scale(Bio_All$residuals)

# Step 5: Inspect the standardized grades (mean should be ~0, sd should be ~1)
summary(Bio_All$standardized_grade)



## TRY AGAIN

# cum_gpa_bot => cumulative GPA before the term (I did NOT make the name!)
str(Bio_All_forOLR$cum_gpa_bot)
# change from character to numeric
Bio_All_forOLR$cum_gpa_bot <- as.numeric(as.character(Bio_All_forOLR$cum_gpa_bot))

# Steps
# 1) Fit an Ordinal logistic regression model (polr)
# Ordinal regression models the probability of the grades falling within 
# specific categories, while dealling with the natural ordereding of the categories.
# Can't use a linear regression model to predict current grades based on previous gpa, 
# b/c Grade_bin_off is categorical & not normally distributed
m_gpa <- MASS::polr(Grade_bin_off ~ cum_gpa_bot, data = Bio_All_forOLR, method = "logistic")
# Check the summary of the model
summary(m_gpa)

# 2) Use the model to predict the expected grade based on gpa
Bio_All_forOLR$predicted_grade <- predict(m_gpa, newdata = Bio_All_forOLR)

# 3) calculate the residuals based on the probabilities predicted by the model, 
# rather than using raw predicted grades (bc cum_gpa_bot is numeric and Grade_bin_off is categorical)
# Get predicted probabilities for each grade
pred_probs <- predict(m1_OLR, newdata = Bio_All_forOLR, type = "probs")

# Check the observed grades and corresponding predicted probabilities
print(head(Bio_All_forOLR$Grade_bin_off))   # Observed grades
print(head(pred_probs))                      # Predicted probabilities

# Remove levels of Grade_bin_off that are not present in pred_probs
Bio_All_forOLR$Grade_bin_off <- factor(Bio_All_forOLR$Grade_bin_off, 
                                       levels = intersect(levels(Bio_All_forOLR$Grade_bin_off), colnames(pred_probs)))

# Extract the predicted probabilities for the observed grades (use the correct indexing)
observed_probs <- sapply(Bio_All_forOLR$Grade_bin_off, 
                         function(x) pred_probs[ , which(colnames(pred_probs) == as.character(x))])

# Calculate residuals as the difference between observed and predicted probabilities
#??? Bio_All_forOLR$residuals <- observed_probs - max(pred_probs, na.rm = TRUE)  # or adjust depending on model structure

# 4) Standardize the residuals to get a standardized score
# represents how a student's grade deviates from the expected grade based on their gpa
Bio_All_forOLR$standardized_grade <- scale(Bio_All_forOLR$residuals)

# 5) Check that the standardized grades have a mean about ~0 with sd ~1
summary(Bio_All_forOLR$standardized_grade)


### TRY AGAIN 
# 1) Fit an Ordinal logistic regression model
m_gpa <- MASS::polr(Grade_bin_off ~ cum_gpa_bot, data = Bio_All_forOLR, method = "logistic")

# 2) Use the model to predict the expected grade based on GPA
Bio_All_forOLR$predicted_grade <- predict(m_gpa, newdata = Bio_All_forOLR)

# 3) Get predicted probabilities for each grade category
pred_probs <- predict(m_gpa, newdata = Bio_All_forOLR, type = "probs")

# 4) Ensure Grade_bin_off levels match the predicted probabilities columns
Bio_All_forOLR$Grade_bin_off <- factor(Bio_All_forOLR$Grade_bin_off, 
                                       levels = intersect(levels(Bio_All_forOLR$Grade_bin_off), colnames(pred_probs)))

# 5) Extract the predicted probabilities for the observed grades
# Note: Predicted probabilities are numeric, so we'll convert the factor to numeric indices
observed_probs <- sapply(Bio_All_forOLR$Grade_bin_off, 
                         function(x) pred_probs[, which(colnames(pred_probs) == as.character(x))])

# 6) Calculate residuals
# You need to calculate the residuals between the observed and predicted probabilities
Bio_All_forOLR$residuals <- as.numeric(observed_probs) - Bio_All_forOLR$predicted_grade

# 7) Standardize the residuals
Bio_All_forOLR$standardized_grade <- scale(Bio_All_forOLR$residuals)

# 8) Remove rows with NA residuals or Grade_bin_off
Bio_All_forOLR_clean <- Bio_All_forOLR[!is.na(Bio_All_forOLR$residuals), ]

# 9) Check the summary of standardized grades
summary(Bio_All_forOLR_clean$standardized_grade)

















#Check Factor Variables: Ensure that all categorical variables (like sex, ethnicity, urm, fgen) have more than one level in your data. For example:
table(Bio_All$sex)
table(Bio_All$ethnicity)
table(Bio_All$urm)
table(Bio_All$fgen)



# Run the summary of the model
summary(m1_OLR)

# Display model coefficients and p-values
coef(m1_OLR)

# Display model coefficients and p-values
coef(m1_OLR)

#copied
mx_OLR <- MASS::polr(Grade_bin_off ~ 
                       sex + ethnicity + urm + fgen #IL BIO + , #control for... does PES/TRIO have an effect
                     interactions...
                     data = Bio_All, 
                     method = "logistic")  # can also use "probit" or "cloglog" for different link functions

soc & urm ---> covariance? Check
urm + fgen. ---> covariance? Check

Layer in age_first_day_of_term, trio, pes <--and likely interaction

# ...  

# Run the summary of the model
summary(model)

# Display model coefficients and p-values
coef(model)

# Display model coefficients and p-values
coef(model)

#############
## Didn't work
#############


# WARNING MESSAGES...
# Fit the ordinal logistic regression model
m0_OLR <- MASS::polr(Grade_bin_off ~ 
                       sex + ethnicity + urm + fgen, # Predictors (categorical and/or numeric)
                     data = Bio_All_forOLR, 
                     method = "logistic")  # can also use "probit" or "cloglog" for different link functions

# Warning/Error messages... design appears to be rank-deficient
# Common causes for Rank=Deficiency are Perfect multicollinearity, Dummy variable trap, and single factor categorical predictors
# For categorical variables, check if there’s perfect overlap across the levels (i.e., are any of the categorical variables perfectly predicted by others?). ... Don't think so
# Check for dummy variable trap
# Convert factors to dummy variables
dummy_data <- model.matrix(~ sex + ethnicity + urm + fgen - 1, data = Bio_All)
# Check correlations between dummy variables
cor(dummy_data, use = "complete.obs")
#Perfect Negative Correlation Between sexFemale and sexMale
#High Correlation Between urmYes and urmNo
#Moderate Correlation Between ethnicityWhite and urmNo
#High Negative Correlation Between fgenFirst generation and fgenContinuing generation
# ^^^Okay but duh these are binary variables... 



#############
## Sand Box 
#############

# Fit an ordinal logistic regression model
m <- polr(crse_grade_off ~ 
             predictor1 + predictor2 + ..., 
           data = , 
           Hess = TRUE) #Calc Hessian matrix & provides SEs for the coefficients
# Summary of the model
summary(m)






