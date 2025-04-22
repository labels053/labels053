############################################################
## CBS Inclusive Excellence Project
## 
## Worked on File Last: Nov 11, 2024
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
library(magrittr)

############################## 
## 1 - Source File 
##############################

# Katie
# setwd("/Users/katiekrueger/Library/CloudStorage/Box-Box/Krueger- Inclusive Excellence/")
# Lydia
setwd("/Users/Lydia/Box/Krueger- Inclusive Excellence")

#Call and Read in data file 
Bio_All <- read.csv("btl_all_1009_TRIO_Proj_Final_Export.csv")

#Call and Read in codebook file 
codbook_BTL <- read.csv("Codebook_TRIO_btl_all_BIOL1009.csv")

############################## 
## 2 - Wrangle Data: Cleaning Data
##############################

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

# Check that new column creation works
table(Bio_All$Subject_Course, useNA = "ifany")
table(Bio_All$Enrollment_IL_Bio, useNA = "ifany") 
# Yes

# Bio_all is in "Long Data" format with pre & post survey data in separate rows
# Subset student data 
Bio_All$survey_time <- as.character(Bio_All$survey_time)
sum(Bio_All$survey_time == "Pre-BIOL 1009", na.rm = TRUE)
# 6322
sum(Bio_All$survey_time == "Post-BIOL 1009", na.rm = TRUE)
# 5067
sum(is.na(Bio_All$survey_time))
# 5722

Bio_YesSurvey <- Bio_All |>
  filter(survey_time %in% c("Pre-BIOL 1009", "Post-BIOL 1009"))
# 11389 rows of data with survey data (either pre or post)
# confirmed this worked as Pre + Post (6322 + 5067 = 11389)

# More confirmation 
Bio_Pre <- Bio_All |>
  filter(survey_time == "Pre-BIOL 1009")
Bio_Pre <- Bio_YesSurvey |>
  filter(survey_time == "Pre-BIOL 1009")
Bio_Post <- Bio_All |>
  filter(survey_time == "Post-BIOL 1009")
Bio_Post <- Bio_YesSurvey |>
  filter(survey_time == "Post-BIOL 1009")

# NOTE: Bio_PreAndOrPost includes students who have either filled at least one survey
# NOTE: This means either pre survey only, post survey only, or both!
Bio_PreAndOrPost <- Bio_All |>
  filter(!is.na(survey_time))
unique(Bio_PreAndOrPost$survey_response_type)
# 11389 rows of data with survey data (either pre, post, or both)

# NOTE: Bio_PreAndPost includes students who have filled out both pre AND post survey data
Bio_PreAndPost <- Bio_PreAndOrPost |>
  filter(survey_response_type == "Both")
unique(Bio_PreAndPost$survey_response_type)
# 8502 rows of data / 2 =
# Number of students with both pre & post survey data is 4251

# NOTE: Bio_PreOrPost includes students who have filled out either pre OR post tests (but not both)
Bio_PreOrPost <- Bio_PreAndOrPost |>
  filter(survey_response_type == "Pre only" | survey_response_type == "Post only")
unique(Bio_PreOrPost$survey_response_type)
# 2887 rows of data = Number students with either pre OR post survey data


############################## 
## 3 - Data Exploration
##############################

# Explore Bio_PreAndPost df... Allows for pre/post survey data comparisons
unique(Bio_PreAndPost$survey_response_type)

sum(Bio_PreAndPost$pes == "Y", na.rm = TRUE) 
# 928 
# Number of PES students with pre/post survey data = 464 
sum(Bio_PreAndPost$trio == "Y", na.rm = TRUE)
# 250
# Number of PE students with pre/post survey data = 125
sum(Bio_PreAndPost$pes == "Y" & Bio_PreAndPost$trio == "Y", na.rm = TRUE)
# 246
# Number of PES & Trio students = 123 (Recall TRIO students are PES unless transfer in)
Bio_PreAndPost |> filter(pes == "N" & trio == "Y")
# There are 2 students that are Trio, but not PES (ie transfer students)
#deid 9379909 & deid 9723897

sum(Bio_PreAndPost$Subject_Course == "EDHD_1620", na.rm = TRUE) #ignores NA values
# 196 (divide by 2 to get number of students...each student has two rows in df)
# Number of students with pre/post survey data co-enrolled in IL Bio = 98


# COLOR PALETTE OPTIONS from Venngage
# Vibrant Palette: #00bf7d  #00b4c5 #0073e6 #2546f0 #5928ed
c("#00bf7d", "#00b4c5", "#0073e6", "#2546f0", "#5928ed")
# Pastel palette: #90d8b2 #8dd2dd #8babf1 #8b95f6 #9b8bf4
c("#90d8b2", "#8dd2dd", "#8babf1", "#8b95f6", "#9b8bf4") ... #fcc9b5
# Pastel contrasting palette 1: #faaf90  #fcc9b5 #d9e4ff #b3c7f7 #8babf1
c("#faaf90", "#fcc9b5", "#d9e4ff", "#b3c7f7", "#8babf1")



# Reorder grades into rank order
grade_order <- c("A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D+", "D", "F", "W", "I", "N", "NS", "NA")

# Ignore I, N, NS, NA course grades
# Define the desired order of the grades
grade_order <- c("A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D+", "D", "F")


# Grade distribution (bar plot) with color based on 'pes' column
Bio_PreAndPost |> 
  filter(survey_time == "Pre-BIOL 1009",   # Filter data to include only "Pre-BIOL 1009"
         !is.na(pes)) |>  # Exclude rows where 'pes' column is NA
  mutate(crse_grade_off = factor(crse_grade_off, levels = grade_order)) |>  # Reorder grades as a factor
  ggplot(aes(x = crse_grade_off, fill = pes)) +  # Fill color based on 'pes' column
  geom_bar(stat = "count", position = "dodge") +  # Compute counts automatically and dodge bars
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.8), vjust = -0.5, color = "black") +  # Add text labels above bars
  labs(title = "Distribution of Course Grades in BIOL1009\n(for students who took both the Pre and Post Course Survey)",
    x = "Final Grade", 
    y = "Number of Students\n(between X & Y dates,\nwho took both pre-post survey)",
    fill = "Affiliated with PES:") + 
  scale_fill_manual(values = c("Y" = "#0073e6", "N" = "#00bf7d")) +  # Manual color mapping for Y/N
  theme_minimal() +  
  theme(legend.position = "top", 
        plot.title = element_text(hjust = 0.5))

  

# START HERE NEXT^^^^^^^^^^^

unique(Bio_PreAndPost$crse_grade_off)

# Grade distribution (bar plot) with color based on 'trio' column
Bio_PreAndPost |>
  filter(survey_time == "Pre-BIOL 1009", # Filter data to include only "Pre-BIOL 1009" (otherwise the frequency of grades is double, pre + post survey data)
         !is.na(pes)) |>  # Exclude rows where 'pes' column is NA
  mutate(crse_grade_off = factor(crse_grade_off, levels = grade_order)) |>  # Reorder grades as a factor
  ggplot(aes(x = factor(crse_grade_off), fill = trio)) +  # Fill color based on 'pes' column
  geom_bar(stat = "count", position = "dodge") +  # Compute counts automatically and dodge bars
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.8), vjust = -0.5, color = "black") +  # Add text labels above bars
  labs(title = "Distribution of Course Grades in BIOL1009 
       (for students who took both the Pre and Post Course Survey)",
       x = "Final Grade", 
       y = "Number of Students
       (between X & Y dates, 
       who took both pre-post survey", 
       fill = "Affliated with TRIO:  ") + 
  scale_fill_manual(values = c("Y" = "#0073e6", "N" = "#00bf7d")) +  # Manual color mapping for Y/N
  theme_minimal() +  
  theme(legend.position = "top", 
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))  # Angle x-axis labels if needed
# Why so many NAs????

# Grade distribution (bar plot) with color based on IL Biology enrollment
# Ensure all combinations of grade and Enrollment_IL_Bio are present
Bio_PreAndPost |>
  filter(survey_time == "Pre-BIOL 1009", # Filter data to include only "Pre-BIOL 1009" (otherwise the frequency of grades is double, pre + post survey data)
         !is.na(pes)) |>  # Exclude rows where 'pes' column is NA
  mutate(crse_grade_off = factor(crse_grade_off, levels = grade_order)) |>  # Reorder grades as a factor
  ggplot(aes(x = factor(crse_grade_off), fill = factor(Enrollment_IL_Bio))) +  # Ensure Enrollment_IL_Bio is a factor
  geom_bar(stat = "count", position = position_dodge(width = 0.5), width = 0.7) +  # Dodge position and width adjustment
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.8), vjust = -0.5, color = "black") +  # Add text labels above bars
  labs(
    title = "Distribution of Course Grades in BIOL1009\n(for students who took both the Pre and Post Course Survey)",
    x = "Final Grade", 
    y = "Number of Students\n(between X & Y dates,\nwho took both pre-post survey)",
    fill = "Enrollment in IL Biology:"
  ) + 
  scale_fill_manual(values = c("1" = "#0073e6", "0" = "#00bf7d"), labels = c("No", "Yes")) +  # Map colors and change legend labels
  theme_minimal() +  
  theme(
    legend.position = "top", 
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1))  # Angle x-axis labels if needed
# Why so many NAs????







##############################
## ?? - Sandbox Area 
##############################

#Does PES "Y" predict Biol 1009 grade? 
# Multinomial regression is appropriate for categorical outcomes with more than 
# two levels. However, it is not directly available in lme4, so you would need 
# to use other packages like nnet::multinom() or brms for these types of outcomes.

# Subset data to make sure looking at clean df
#Bio_subset <- Bio_all[, c("act")]
#Bio_subset <- Bio_all |> 
  #select("deid", "term", "pes", "trio", "subject", "course", "Subject_Course", 
         #"crse_grade_off", ...) |>
  #dplyr::filter()

crse_grade_off # this is the official course grade in BIOL1009

# Does TRIO "Y" predict Biol 1009 grade? 
# Explore Odinal Logistic Regression
install.packages("MASS")
library(MASS)
# Subset data 
# <- Bio_PreAndPost|> 
  #select("deid", "term", "pes", "trio", "subject", "course", "Subject_Course", 
         #"crse_grade_off", ...) |>
  #dplyr::filter()

# Maybe need: 
#grade_order <- c("A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D+", "F")
#mutate(crse_grade_off = factor(crse_grade_off, levels = grade_order)) |>  # Reorder grades as a factor
  
# Fit an ordinal logistic regression model
m1 <- polr(crse_grade_off ~ 
             predictor1 + predictor2 + ..., 
           data = Bio_PreAndPost, 
           Hess = TRUE) #Calc Hessian matrix & provides SEs for the coefficients
# Summary of the model
summary(m1)

# LYDIA 
# # # # # # # # # # # # # # # # # # # #
# comparing +/- delta values for      #
# survey items according to pes/trio  #
# # # # # # # # # # # # # # # # # # # #
# create new data-frame that has student ID and changee in survey item response
# change in survey item responses 
survey_item_difference <- Bio_PreAndPost %>%
  # subgroup by ID
  group_by(deid) %>%
  arrange(deid, term) %>%
  # create occurrence column
  # there were students who took the class more than once
  mutate(occurrence = row_number()) %>%
  # create subgroup column
  mutate(
    # values are 1 or 2 (one sem or two sem aka 2 or 4 pre/post surveys)
    subgroup = (occurrence - 1) %/% 2 + 1
  ) %>%
  group_by(deid, subgroup) %>%
  # summarize all columns whose names begin w/ "all_"
  # taking difference post (2) minus pre (1)
  summarise(
    across(starts_with("all_"), ~ .[2] - .[1]),
    .groups = "drop"
  )

# got rid of subgroup column; wasn't important
survey_item_difference <- survey_item_difference %>%
  select(-subgroup)

# combine subgroup info from Bio_PreAndPost into survey_item_difference
survey_item_difference <- survey_item_difference %>%
  left_join(
    Bio_PreAndPost %>% select(deid, pes, trio, academic_program),
    # pull info by same deid
    by = 'deid'
  ) %>%
  mutate(
    # this way subgroups aren't exclusive
    is_pes = ifelse(pes == "Y", "PES", NA),
    is_trio = ifelse(trio == "Y", "Trio", NA),
    is_cdhd = ifelse(academic_program == 'Col of Educ/Human Development', 'CDHD', NA),
    is_cdhd_and_trio = ifelse(academic_program == 'Col of Educ/Human Development' & trio == 'Y', "CDHD and Trio", NA),
    is_neither = ifelse(pes != "Y" & trio != "Y", "Neither", NA)
  )

# get survey item column names
survey_items <- grep("^all_", names(survey_item_difference), value = TRUE)
# create a list to store plots
survey_plots <- list()

# yay for loop!
# loop thru each survey item
for (item in survey_items) {
  # prepare data for current survey item
  survey_plot_data <- survey_item_difference %>%
    select(deid, all_of(item), is_pes, is_trio, is_cdhd, is_cdhd_and_trio, is_neither) %>%
    pivot_longer(
      cols = starts_with("is_"),
      names_to = "subgroup",
      values_to = "membership",
      values_drop_na = TRUE
    ) %>%
    filter(!is.na(membership)) %>%
    group_by(membership) %>%
    summarise(
      # counts number of individuals in pes/trio/both/neither with pos and neg values, repectively
      # aka 8 groups/columns
      positive = sum(.data[[item]] > 0, na.rm = TRUE),
      negative = sum(.data[[item]] < 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = c(positive, negative),
      names_to = "Type",
      values_to = "Count"
    ) %>%
    # make negative count into negative value
    mutate(Count = ifelse(Type == "negative", -Count, Count))
  # plot diverging chart (for each survey item)
  survey_plot <- ggplot(survey_plot_data, aes(x = membership, y = Count, fill = Type)) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(
        label = abs(Count),
      size = 5,
      hjust = ifelse(Count > 0, -0.1, 1.1)) # Push text outside the bars
    ) +
    coord_flip() +
    scale_y_continuous(labels = abs) +
    labs(
      title = paste("Delta Values for", item),
      x = "Subgroup",
      y = "Count of Delta Values",
      fill = "Type"
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("positive" = "#0073e6", "negative" = "#00bf7d"))
  survey_plots[[item]] <- survey_plot
}

for (item in names(survey_plots)) {
  print(survey_plots[[item]])
}

# subgroup by trio, pes, and ref/normal pop
trio_preandpost <- Bio_PreAndPost[Bio_PreAndPost$trio == "Y", ]
pes_preandpost <- Bio_PreAndPost[Bio_PreAndPost$pes == "Y", ]
reference_preandpost <- Bio_PreAndPost[Bio_PreAndPost$trio != "Y" & Bio_PreAndPost$pes != "Y", ]
trio_and_pres_preandpost <- Bio_PreAndPost[Bio_PreAndPost$trio == "Y" & Bio_PreAndPost$pes == "Y", ]
# note that 250 students are in trio and 246 are in trio and pes
# aka almost all trio students are in pes

# # # # # # # # # # # # # #  
# +/- delta values as box #
# plot acc. to pes/trio   #
# # # # # # # # # # # # # # 

# create new list for boxplots
survey_boxplots <- list()
# create boxplots for each survey item
for (item in survey_items) {
  # prepare data
  survey_plot_data <- survey_item_difference %>%
    select(deid, all_of(item), is_pes, is_trio, is_cdhd, is_cdhd_and_trio, is_neither) %>%
    pivot_longer(
      cols = starts_with("is_"),
      names_to = "subgroup",
      values_to = "membership",
      values_drop_na = TRUE
    ) %>%
    filter(!is.na(membership)) %>%
    mutate(
      # convert subgroup names
      membership = factor(membership, levels = c("PES", 'Trio', "CDHD", 'CDHD and Trio', "Neither"))
    )
  # plot boxplot for current item
  survey_plot <- ggplot(survey_plot_data, aes(x = membership, y = .data[[item]], fill = membership)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7) + 
    labs(
      title = paste("Delta Value Distribution for", item),
      x = "Subgroup",
      y = "Delta Value",
      fill = "Subgroup"
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("PES" = "#0073e6", "Trio" = "#00bf7d", "CDHD" = "#00b4c5", 'CDHD and Trio' = '#9b8bf4', "Neither" = "#000000"))
  # add plot to list
  survey_boxplots[[item]] <- survey_plot
  print(survey_plot)
}

# # # # # # # # # # # # # # # # 
# +/- delta values as bubble  #
# plot acc. to pes/trio       #
# # # # # # # # # # # # # # # #
survey_bubble <- list()
# same for loop
for (item in survey_items) {
  # prepare data
  survey_plot_data <- survey_item_difference %>%
    select(deid, all_of(item), is_pes, is_trio, is_cdhd, is_cdhd_and_trio, is_neither) %>%
    pivot_longer(
      cols = starts_with('is_'),
      names_to = 'subgroup',
      values_to = 'membership',
      values_drop_na = TRUE
    ) %>%
    filter(!is.na(membership)) %>%
    group_by(membership, Value = .data[[item]]) %>%
    summarise(
      Count = n(),
      .groups = "drop"
    )
  # create plot
  survey_plot <- ggplot(survey_plot_data, aes(x = Value, y = membership, size = Count, color = membership)) +
    geom_point(alpha = 0.6) +
    scale_size_continuous(range = c(4,20)) +
    labs(
      title = paste('Bubble Plot for', item),
      x = "Delta Value",
      y = "Subgroup",
      size = "Number of Individuals",
      color = "Subgroup"
    ) +
    theme_minimal() +
    theme(legend.position = "right") +
    scale_fill_manual(values = c("PES" = "#0073e6", "Trio" = "#00bf7d", 
                                 "CDHD" = "#00b4c5", 'CDHD and Trio' = '#9b8bf4', "Neither" = "#000000")) +
    scale_color_manual(values = c("PES" = "#0073e6", "Trio" = "#00bf7d", 
                                  "CDHD" = "#00b4c5", 'CDHD and Trio' = '#9b8bf4', "Neither" = "#000000"))
  # save plot
  survey_bubble[[item]] <- survey_plot
  print(survey_plot)
}



# # # # # # # # # # # # # # # # # # # #
# % of each ethnic group in pes/trio  #
# # # # # # # # # # # # # # # # # # # #
# calculate counts for each ethnic group in program
ethnicity_counts <- data.frame(
  ethnicity = c("Black", "White", "American Indian", "Hawaiian", "Asian", "Hispanic"),
  pes = c(
    sum(Bio_PreAndPost$ethnicity == "Black" & Bio_PreAndPost$pes == "Y"),
    sum(Bio_PreAndPost$ethnicity == "White" & Bio_PreAndPost$pes == "Y"),
    sum(Bio_PreAndPost$ethnicity == "Am. Indian" & Bio_PreAndPost$pes == "Y"),
    sum(Bio_PreAndPost$ethnicity == "Hawaiian" & Bio_PreAndPost$pes == "Y"),
    sum(Bio_PreAndPost$ethnicity == "Asian" & Bio_PreAndPost$pes == "Y"),
    sum(Bio_PreAndPost$ethnicity == "Hispanic" & Bio_PreAndPost$pes == "Y")
  ),
  trio = c(
    sum(Bio_PreAndPost$ethnicity == "Black" & Bio_PreAndPost$trio == "Y"),
    sum(Bio_PreAndPost$ethnicity == "White" & Bio_PreAndPost$trio == "Y"),
    sum(Bio_PreAndPost$ethnicity == "Am. Indian" & Bio_PreAndPost$trio == "Y"),
    sum(Bio_PreAndPost$ethnicity == "Hawaiian" & Bio_PreAndPost$trio == "Y"),
    sum(Bio_PreAndPost$ethnicity == "Asian" & Bio_PreAndPost$trio == "Y"),
    sum(Bio_PreAndPost$ethnicity == "Hispanic" & Bio_PreAndPost$trio == "Y")
  ),
  neither = c(
    sum(Bio_PreAndPost$ethnicity == "Black" & Bio_PreAndPost$pes == "N" & Bio_PreAndPost$trio == "N"),
    sum(Bio_PreAndPost$ethnicity == "White" & Bio_PreAndPost$pes == "N" & Bio_PreAndPost$trio == "N"),
    sum(Bio_PreAndPost$ethnicity == "Am. Indian" & Bio_PreAndPost$pes == "N" & Bio_PreAndPost$trio == "N"),
    sum(Bio_PreAndPost$ethnicity == "Hawaiian" & Bio_PreAndPost$pes == "N" & Bio_PreAndPost$trio == "N"),
    sum(Bio_PreAndPost$ethnicity == "Asian" & Bio_PreAndPost$pes == "N" & Bio_PreAndPost$trio == "N"),
    sum(Bio_PreAndPost$ethnicity == "Hispanic" & Bio_PreAndPost$pes == "N" & Bio_PreAndPost$trio == "N")
  ),
  total = c(
    sum(Bio_PreAndPost$ethnicity == "Black"),
    sum(Bio_PreAndPost$ethnicity == "White"),
    sum(Bio_PreAndPost$ethnicity == "Am. Indian"),
    sum(Bio_PreAndPost$ethnicity == "Hawaiian"),
    sum(Bio_PreAndPost$ethnicity == "Asian"),
    sum(Bio_PreAndPost$ethnicity == "Hispanic")
  )
)

# add columns for %'s
ethnicity_counts <- ethnicity_counts %>%
  mutate(
    `% in PES` = pes / total * 100,
    `% in Trio` = trio / total * 100,
    `% in Neither` = neither / total * 100
  )
# reshape for plotting
ethnicity_percentages <- ethnicity_counts %>%
  select(ethnicity, `% in PES`, `% in Trio`, `% in Neither`) %>%
  pivot_longer(
    cols = starts_with("%"),
    names_to = "Subgroup",
    values_to = "Percentage"
  )

# plot
ggplot(ethnicity_percentages, aes(x = ethnicity, y = Percentage, fill = Subgroup)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    aes(label = round(Percentage, 1)),  # Round to 1 decimal place
    position = position_stack(vjust = 0.5),
    size = 4, color = "black"
  ) +
  coord_flip() +
  labs(
    title = "Distribution of Ethnicities by Program Subgroup (Percentage). NOTE: Most students in Trio are also in PES",
    x = "Ethnicity",
    y = "Percentage of Students",
    fill = "Subgroup"
  ) +
  theme_minimal()


# # # # # # # # # # # # # # # # # # # 
# ethnicity vs. grade vs. pes/trio  #
# # # # # # # # # # # # # # # # # # # 
# Create categories a/b, c/d, f/w/i
A_B <- c("A", "A-", "B+", "B", "B-")
C_D <- c("C+", "C", "C-", "D+", "D")
F_W <- c("F", "W")

# Add column to Bio_PreAndPost to classify grades
Bio_PreAndPost <- Bio_PreAndPost %>%
  mutate(
    grade_category = case_when(
      crse_grade_off %in% A_B ~ "AB",
      crse_grade_off %in% C_D ~ "CD",
      crse_grade_off %in% F_W ~ "FW",
      TRUE ~ "Other"
    )
  ) %>%
  filter(grade_category != "Other")

# Summarize grade counts
grade_summary <- Bio_PreAndPost %>%
  filter(
    !is.na(ethnicity) & ethnicity != "", 
    !is.na(pes) & pes != "", 
    !is.na(grade_category) & grade_category != ""
  ) %>%
  group_by(ethnicity, pes, grade_category) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  filter(grade_category != "Other")  
  # Exclude "Other" from grade_summary

# Pivot data and join to ethnicity_counts
grade_summary_wide <- grade_summary %>%
  pivot_wider(names_from = grade_category, values_from = Count, values_fill = 0)
# Rename the 'pes' column in grade_summary_wide to avoid confusion
grade_summary_wide <- grade_summary_wide %>%
  rename(pes_status = pes)

# Add % columns for each grade category, using new variable names (AB, CD, FW)
grade_summary_wide <- grade_summary_wide %>%
  mutate(
    total = AB + CD + FW,
    percent_AB = (AB / total) * 100,
    percent_CD = (CD / total) * 100,
    percent_FW = (FW / total) * 100
  )

grade_summary_long <- grade_summary_wide %>%
  pivot_longer(
    cols = c("percent_AB", "percent_CD", "percent_FW"),
    names_to = "grade_category",
    values_to = "percentage"
  )

# Plot
ggplot(grade_summary_long, aes(x = ethnicity, y = percentage, fill = grade_category)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ pes_status) +  # Faceting by pes_status (Y/N)
  labs(
    title = "Grade Distribution by Ethnicity and PES Status",
    x = "Ethnicity",
    y = "Percentage of Students",
    fill = "Grade Category"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(
    values = c("percent_AB" = "#00bf7d", "percent_CD" = "#0073e6", "percent_FW" = "#faaf90")
  ) +
  geom_text(
    aes(label = scales::percent(percentage / 100, accuracy = 0.1)),
    position = position_stack(vjust = 0.5),  # Position at the center of each segment
    color = "black",  # Color of the text
    size = 3  # Adjust text size if needed
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grade_ethnicity_summary <- grade_summary_long

# # # # # # # # # # # # # # # # # # 
# bar graph of grade distribution #
# # # # # # # # # # # # # # # # # #
#count occurrences of each grade
grade_counts <- table(Bio_PreAndPost$crse_grade_off)

# bar plot
barplot(
  grade_counts,
  main = "Grade Distribution",
  xlab = "Grades",
  ylab = "Number of Students",
  col = '#0073e6',
  border = 'black'
)

# want to show proportions of students in/not in edhd
grade_cdhd_table <- table(
  Bio_PreAndPost$crse_grade_off,
  Bio_PreAndPost$academic_program == "Col of Educ/Human Development"
)

filtered_data <- Bio_PreAndPost[
  Bio_PreAndPost$crse_grade_off != '' & !is.na(Bio_PreAndPost$crse_grade_off) &
    Bio_PreAndPost$ethnicity != '' & !is.na(Bio_PreAndPost$ethnicity),
]
# proportions of student's grades according to ethnicity
grade_ethnicity_cdhd_table <- table(
  filtered_data$crse_grade_off,
  filtered_data$ethnicity
)

# stacked bar plot for cdhd
barplot(
  t(grade_cdhd_table),
  main = 'Grade Distribution by CDHD Enrollment',
  xlab = "Grades",
  ylab = "Proportion of Students",
  col = c('#0073e6', '#00bf7d'),
  legend.text = c('Non-CDHD', 'CDHD'),
  args.legend = list(x = 'topright', bty = 'n')
)

# stacked bar plot for ethnicity
barplot(
  t(grade_ethnicity_cdhd_table),
  main = 'Grade Distribution by Ethnicity',
  xlab = 'Grades',
  ylab = 'Proportion of Students',
  col = c('#0073e6', '#00bf7d', '#00b4c5', '#8babf1', '#9b8bf4', '#d9e4ff'),
  legend.text = c('Am. Indian', 'Asian', 'Black', 'Hawaiian', 'Hispanic', 'White'),
  args.legend = list(
    x = 'topright', 
    inset = c(-0.05, -0.2)
  )
)

# proportions of students in edhd/not edhd both in trio
# add new column to classify based on trio involvement
# Filter out rows with blank TRIO or PES status
filtered_data <- Bio_PreAndPost[
  Bio_PreAndPost$trio %in% c('Y', 'N') &
    Bio_PreAndPost$pes %in% c('Y', 'N'), 
]

# Construct the table again
grade_cdhd_table <- table(
  Grade = filtered_data$crse_grade_off,
  CDHD_Status = filtered_data$academic_program == 'Col of Educ/Human Development',
  TRIO_status = filtered_data$trio,
  PES_status = filtered_data$pes
)

#install.packages('reshape2')
library(reshape2)
grade_cdhd_df <- melt(grade_cdhd_table)

# bar plot
ggplot(grade_cdhd_df, aes(
  x = Grade,
  y = value,
  fill = interaction(CDHD_Status, TRIO_status)
)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_fill_manual(
    values = c(
      'FALSE.Y' = "#0073e6",
      'TRUE.Y' = '#00bf7d',
      'FALSE.N' = '#000000',
      'TRUE.N' = '#00b4c5'
    ),
    name = 'CDHD and TRIO Status'
  ) +
  labs(
    title = 'Grade Distribution by CDHD and TRIO Membership',
    x = 'Grades',
    y = 'Number of Students',
    fill = 'CDHD & TRIO Status'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# CDHD AND PES
ggplot(grade_cdhd_df, aes(
  x = Grade,
  y = value,
  fill = interaction(CDHD_Status, PES_status)
)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_fill_manual(
    values = c(
      'FALSE.Y' = "#0073e6",
      'TRUE.Y' = '#00bf7d',
      'FALSE.N' = '#000000',
      'TRUE.N' = '#00b4c5'
    ),
    name = 'CDHD and PES Status'
  ) +
  labs(
    title = 'Grade Distribution by CDHD and PES Membership',
    x = 'Grades',
    y = 'Number of Students',
    fill = 'CDHD & PES Status'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###########################
# CDHD AND PES AND TRIO
ggplot(grade_cdhd_df, aes(
  x = Grade,
  y = value,
  fill = interaction(CDHD_Status, PES_status, TRIO_status)
)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_fill_manual(
    values = c(
      'FALSE.Y.N' = "#0073e6",
      'TRUE.Y.Y' = '#00bf7d',
      'FALSE.N.N' = '#000000',
      'TRUE.N.N' = '#00b4c5',
      'TRUE.Y.N' = '#8babf1',
      'FALSE.N.Y' = '#9b8bf4',
      'TRUE.N.Y' = '#faaf90',
      'FALSE.Y.Y' = '#d9e4ff'
    ),
    name = 'CDHD, PES, and TRIO Status'
  ) +
  labs(
    title = 'Grade Distribution by CDHD, TRIO, and PES Membership',
    x = 'Grades',
    y = 'Number of Students',
    fill = 'CDHD & TRIO & PES Status'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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

# apply new function to create a new column for binned grades
Bio_PreAndPost$Grade_bin <- sapply(Bio_PreAndPost$crse_grade_off, bin_grades)

# construct table w/ binned grades
grade_cdhd_table <- table(
  Grade_bin = Bio_PreAndPost$Grade_bin,  # Use binned grades
  CDHD_status = Bio_PreAndPost$academic_program == 'Col of Educ/Human Development',
  TRIO_status = Bio_PreAndPost$trio,
  PES_status = Bio_PreAndPost$pes
)

# Melt table for plotting
grade_cdhd_df <- melt(grade_cdhd_table)

# Bar plot for CDHD and TRIO
ggplot(grade_cdhd_df, aes(
  x = Grade_bin,
  y = value,
  fill = interaction(CDHD_status, TRIO_status)
)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_fill_manual(
    values = c(
      'FALSE.Y' = '#0073e6',
      'TRUE.Y' = '#00bf7d',
      'FALSE.N' = '#000000',
      'TRUE.N' = '#00b4c5'
    ),
    name = 'CDHD and TRIO Status'
  ) +
  labs(
    title = 'Grade Distribution by CDHD and TRIO Membership (Binned)',
    x = 'Letter Grades',
    y = 'Number of Students',
    fill = 'CDHD & TRIO Status'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar plot for CDHD and PES
ggplot(grade_cdhd_df, aes(
  x = Grade_bin,
  y = value,
  fill = interaction(CDHD_status, PES_status)
)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_fill_manual(
    values = c(
      'FALSE.Y' = '#0073e6',
      'TRUE.Y' = '#00bf7d',
      'FALSE.N' = '#000000',
      'TRUE.N' = '#00b4c5'
    ),
    name = 'CDHD and PES Status'
  ) +
  labs(
    title = 'Grade Distribution by CDHD and PES Membership (Binned)',
    x = 'Letter Grades',
    y = 'Number of Students',
    fill = 'CDHD & PES Status'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###########
# grades vs. IL enrollment 
# apply binning function 
# create a new column indicating IL entrollment
# Create the IL_enrollment column with explicit FALSE for non-IL enrollment
Bio_PreAndPost$IL_enrollment <- !is.na(Bio_PreAndPost$Subject_Course) & 
  Bio_PreAndPost$Subject_Course == 'EDHD_1620'

# create a table of binned grades by IL enrollment
grade_il_table <- table(
  Grade_bin = Bio_PreAndPost$Grade_bin,
  IL_enrollment = Bio_PreAndPost$IL_enrollment
)

# melt table for plotting
grade_il_df <- melt(grade_il_table)

# plot
ggplot(grade_il_df, aes(
  x = Grade_bin,
  y = value,
  fill = IL_enrollment
)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_fill_manual(
    values = c('FALSE' = '#0073e6', 'TRUE' = '#00bf7d'),
    name = 'IL Enrollment'
  ) +
  labs(
    title = 'Grade Distribution by IL Enrollment',
    x = 'Letter Grades',
    y = 'Number of Students',
    fill = 'IL Enrollment'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#########

# separate graphs for better insight
# Subset data for IL_enrollment = FALSE
il_false_df <- subset(Bio_PreAndPost, IL_enrollment == FALSE)

# Table for IL_enrollment = FALSE
grade_il_false_table <- table(Grade_bin = il_false_df$Grade_bin)
grade_il_false_df <- as.data.frame(grade_il_false_table)

# Plot for IL_enrollment = FALSE
ggplot(grade_il_false_df, aes(
  x = Grade_bin,
  y = Freq,
  fill = Grade_bin
)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_brewer(palette = "Set3", name = 'Letter Grades') +
  labs(
    title = 'Grade Distribution for Non-IL Enrollment',
    x = 'Letter Grades',
    y = 'Number of Students'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Subset data for IL_enrollment = TRUE
il_true_df <- subset(Bio_PreAndPost, IL_enrollment == TRUE)

# Table for IL_enrollment = TRUE
grade_il_true_table <- table(Grade_bin = il_true_df$Grade_bin)
grade_il_true_df <- as.data.frame(grade_il_true_table)

# Plot for IL_enrollment = TRUE
ggplot(grade_il_true_df, aes(
  x = Grade_bin,
  y = Freq,
  fill = Grade_bin
)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_brewer(palette = "Set3", name = 'Letter Grades') +
  labs(
    title = 'Grade Distribution for IL Enrollment',
    x = 'Letter Grades',
    y = 'Number of Students'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###########
# subset for people in TRIO
trio_df <- subset(Bio_PreAndPost, trio == 'Y')

# subset for TRIO members with il enroll
trio_il_false_df <- subset(trio_df, IL_enrollment == FALSE)

# table for TRIO and il = FALSE
grade_trio_il_false_table <- table(Grade_bin = trio_il_false_df$Grade_bin)
grade_trio_il_false_df <- as.data.frame(grade_trio_il_false_table)

# plot for trio w/ il = false
ggplot(grade_trio_il_false_df, aes(
  x = Grade_bin,
  y = Freq,
  fill = Grade_bin
)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_brewer(palette = 'Set3', name = 'Letter Grades') +
  labs(
    title = 'Grade Distribution for TRIO Members (Non-IL Enrollment)',
    x = 'Letter Grades',
    y = 'Number of Students'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# subset for trio members with il = true
trio_il_true_df <- subset(trio_df, IL_enrollment == TRUE)

# table for trio and il = TRUE
grade_trio_il_true_table <- table(Grade_bin = trio_il_true_df$Grade_bin)
grade_trio_il_true_df <- as.data.frame(grade_trio_il_true_table)

# plot
ggplot(grade_trio_il_true_df, aes(
  x = Grade_bin,
  y = Freq,
  fill = Grade_bin
)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_brewer(palette = 'Set3', name = 'Letter Grades') +
  labs(
    title = 'Grade Distribution for TRIO Members (IL Enrollment)',
    x = 'Letter Grades',
    y = 'Number of Students'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#########################
# stacked bar plot

# add IL enroll status
grade_il_false_df$IL_enrollment <- 'Non-IL Enrollment'
grade_il_true_df$IL_enrollment <- 'IL Enrollment'

# combine 
grade_il_combined_df <- bind_rows(grade_il_false_df, grade_il_true_df)

# plot
ggplot(grade_il_combined_df, aes(
  x = Grade_bin,
  y = Freq,
  fill = IL_enrollment
)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_fill_brewer(palette = 'Set2', name = 'IL Enrollment') +
  labs(
    title = 'Grade Distribution for TRIO Members by IL Enrollment',
    x = 'Letter Grades',
    y = 'Number of Students'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#############
# plotting individual #'s
# starting with science confidence

# define sc columns
sc_columns <- paste0('sc', sprintf('%02d', 1:11))
sc_long_df <- trio_df %>%
  select(IL_enrollment, all_of(sc_columns)) %>%
  pivot_longer(cols = all_of(sc_columns), names_to = 'SC_Question', values_to = 'Response')

# count occurrences of each response
sc_counts <- sc_long_df %>%
  group_by(IL_enrollment, Response) %>%
  summarise(Freq = n(), .groups = 'drop')

# convert il enrollment to label
sc_counts$IL_enrollment <- factor(sc_counts$IL_enrollment, labels = c('Not in IL Bio', 'IL Bio'))

# convert response to factor for ordering
sc_counts$Response <- factor(sc_counts$Response, levels = c(1, 2, 3, 4))

# plot
ggplot(sc_counts, aes(
  x = Response,
  y = Freq,
  fill = IL_enrollment
)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_fill_brewer(palette = 'Set2', name = 'IL Enrollment') +
  labs(
    title = 'Response Distribution for Science Confidence 01-11',
    x = 'Response Value (1-4)',
    y = 'Number of Occurrences'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


################################################
# new graph ideas !
# grade vs. proportion of group (by group membership)
# all graphs on same figure
# using Bio_All data frame
# 1009 NO pes NO trio NO il
# 1009 YES pes
# 1009 YES trio 
# 1009 YES il

Bio_All <- Bio_All %>%
  mutate(deid, as.character(deid))
# change so just one value per id
Bio_All_unique <- Bio_All %>% distinct(deid, .keep_all = TRUE)

# apply 'bin_grades' function to official course grades
Bio_All_unique$Grade_bin <- sapply(Bio_All_unique$crse_grade_off, bin_grades)
Bio_All_unique <- Bio_All_unique %>%
  filter(!is.na(Grade_bin))
Bio_All_unique <- Bio_All_unique %>%
  mutate(IL_bio = ifelse(is.na(Subject_Course), 'N', ifelse(Subject_Course == 'EDHD_1620', 'Y', 'N')))

# construct table w/ binned grades
Bio_all_grade <- table(
  Grade_bin = Bio_All_unique$Grade_bin,  
  IL_bio_status = Bio_All_unique$IL_bio,
  TRIO_status = Bio_All_unique$trio,
  PES_status = Bio_All_unique$pes,
  fgen_status = Bio_All_unique$fgen
)

# melt table for plotting
Bio_all_grade_df <- melt(Bio_all_grade)
Bio_all_grade_df <- Bio_all_grade_df %>%
  filter(!(is.na(PES_status) | PES_status == "") &
           !(is.na(TRIO_status) | TRIO_status == "")) 

# compute proportions within each group
Bio_all_grade_df <- Bio_all_grade_df %>%
  mutate(Group = case_when(
    PES_status == 'N' & TRIO_status == 'N' & IL_bio_status == 'N' ~ 'Reference \n n = 10,515',
    PES_status == 'Y' & TRIO_status == 'N' ~ 'Yes PES, No TRIO \n n = 948',
    PES_status == 'Y' & TRIO_status == 'Y' & IL_bio_status == 'N' ~ 'Yes PES, Yes TRIO, No IL Bio \n n = 137',
    PES_status == 'Y' & TRIO_status == 'Y' & IL_bio_status == 'Y' ~ 'Yes PES, Yes TRIO, Yes IL Bio \n n = 183'
  )) %>%
  filter(!is.na(Group)) %>%
  group_by(Group) %>%
  mutate(Proportion = value / sum(value)) %>%
  ungroup()

# Calculate the total number of people in each group
group_sizes <- Bio_all_grade_df %>%
  group_by(Group) %>%
  summarise(n = sum(value))

# Merge group sizes back to Bio_all_grade_df
Bio_all_grade_df <- Bio_all_grade_df %>%
  left_join(group_sizes, by = "Group")

grade_levels <- c("A", "B", "C", "D", "F", "W")
# Define the order for the Group variable
desired_order <- c('Reference \n n = 10,515', "Yes PES, No TRIO \n n = 948", "Yes PES, Yes TRIO, No IL Bio \n n = 137", "Yes PES, Yes TRIO, Yes IL Bio \n n = 183")
Bio_all_grade_df$Group <- factor(Bio_all_grade_df$Group, levels = desired_order)


# plot
ggplot(Bio_all_grade_df, aes(x = Grade_bin, y = Proportion, fill = Group)) +
  geom_bar(stat = 'identity', alpha = 0.8) +
  facet_wrap(~ Group, ncol = 4) +
  scale_fill_manual(values = c('Reference \n n = 10,515' = '#0073e6',
                               # n = 10515
                               'Yes PES, No TRIO \n n = 948' = '#00bf7d',
                               # n = 948
                               'Yes PES, Yes TRIO, No IL Bio \n n = 137' = '#00b4c5',
                               # n = 137
                               'Yes PES, Yes TRIO, Yes IL Bio \n n = 183'= '#9b8bf4')) +
                                # n = 183
  labs(
    title = 'Grade Distribution by Group',
    x = 'Grade Bin',
    y = 'Proportion',
    fill = 'Group'
  ) +
  theme_minimal() +
  scale_x_discrete(labels = grade_levels)

# View the sample sizes for each group
print(group_sizes)

############################
# GRAPH IDEA 2 

# edited code from before to add a column to Bio_All_unique
# for fgen status

# woah! only 126 students took IL bio!
# compared to 1715 who did NOT

# N 1st gen, Y 1st gen, 
# Y 1st + N trio, Y 1st + Y trio
# Y 1st + Y trio + N IL, Y 1st + Y trio + Y IL

#install.packages("purrr")
#install.packages("tidyr")

library(dplyr)
# make copy of Bio_all_grade_df to define new groups
Bio_all_grade_df_new <- melt(Bio_all_grade)
Bio_all_grade_df_new <- Bio_all_grade_df_new %>%
  filter(!(is.na(PES_status) | PES_status == "") &
           !(is.na(TRIO_status) | TRIO_status == ""))
Bio_all_grade_df_new <- Bio_all_grade_df_new %>%
  select(-PES_status)

# create individual df's, add proportion and group columns
first_gen_df <- subset(Bio_all_grade_df_new, Bio_all_grade_df_new$fgen_status == 'First generation')
first_gen_df <- first_gen_df %>%
  mutate(proportion = value / sum(value, na.rm = TRUE),
         group = 'First Generation \n n = 2829')

continuing_gen_df <- subset(Bio_all_grade_df_new, Bio_all_grade_df_new$fgen_status == 'Continuing generation')
continuing_gen_df <- continuing_gen_df %>%
  mutate(proportion = value / sum(value, na.rm = TRUE),
         group = 'Continuing Generation \n n = 8898')

first_gen_trio_df <- subset(Bio_all_grade_df_new, Bio_all_grade_df_new$fgen_status == 'First generation' & Bio_all_grade_df_new$TRIO_status == 'Y')
first_gen_trio_df <- first_gen_trio_df %>%
  mutate(proportion = value / sum(value, na.rm = TRUE),
         group = 'First Gen in TRIO \n n = 261')

first_gen_no_trio_df <- subset(Bio_all_grade_df_new, Bio_all_grade_df_new$fgen_status == 'First generation' & Bio_all_grade_df_new$TRIO_status == 'N')
first_gen_no_trio_df <- first_gen_no_trio_df %>%
  mutate(proportion = value / sum(value, na.rm = TRUE),
         group = 'First Gen no TRIO \n n = 2567')

first_gen_trio_no_ilbio_df <- subset(Bio_all_grade_df_new, Bio_all_grade_df_new$fgen_status == 'First generation' & Bio_all_grade_df_new$TRIO_status == 'Y' & Bio_all_grade_df_new$IL_bio_status == 'N')
first_gen_trio_no_ilbio_df <- first_gen_trio_no_ilbio_df %>%
  mutate(proportion = value / sum(value, na.rm = TRUE),
         group = 'First Gen TRIO No IL Biology \n n = 156')

first_gen_trio_ilbio_df <- subset(Bio_all_grade_df_new, Bio_all_grade_df_new$fgen_status == 'First generation' & Bio_all_grade_df_new$TRIO_status == 'Y' & Bio_all_grade_df_new$IL_bio_status == 'Y')
first_gen_trio_ilbio_df <- first_gen_trio_ilbio_df %>%
  mutate(proportion = value / sum(value, na.rm = TRUE),
         group = 'First Gen TRIO in IL Biology \n 105')

# merge subsets 
combined_df <- bind_rows(first_gen_df, continuing_gen_df, first_gen_trio_df, first_gen_no_trio_df, first_gen_trio_no_ilbio_df, first_gen_trio_ilbio_df)

desired_order_2 <- c('Continuing Generation \n n = 8898', 'First Generation \n n = 2829', 'First Gen no TRIO \n n = 2567', 'First Gen in TRIO \n n = 261', 'First Gen TRIO No IL Biology \n n = 156', 'First Gen TRIO in IL Biology \n 105')
combined_df$group <- factor(combined_df$group, levels = desired_order_2)

summarized_df <- combined_df %>%
  group_by(Grade_bin, group) %>%
  summarise(proportion = sum(proportion), .groups = 'drop')

# plot
ggplot(summarized_df, aes(x = Grade_bin, y = proportion, fill = group)) +
  geom_bar(stat = 'identity', alpha = 0.8) +
  facet_wrap(~ group, ncol = 6) +
  scale_fill_manual(values = c('Continuing Generation \n n = 8898' = '#0073e6',
                               'First Generation \n n = 2829' = '#00bf7d',
                               'First Gen no TRIO \n n = 2567' = '#9b8bf4', 
                               'First Gen in TRIO \n n = 261' = '#b2d3c2', 
                               'First Gen TRIO No IL Biology \n n = 156' = '#00b4c5',
                               'First Gen TRIO in IL Biology \n 105' = 'plum')) +
  labs(
    title = 'First Generation Grade Distribution by Group Membership',
    x = 'Grade Bin',
    y = 'Proportion',
    fill = 'Group'
  ) +
  theme_minimal() +
  scale_x_discrete(labels = grade_levels) +
  geom_text(aes(label = round(proportion, 2)), vjust = -0.5, size = 3)


##############################
## ?? - Rejected Ideas / Don't Use
##############################

# Does TRIO "Y" predict Biol 1009 grade? 
# Explore multinomial logistic regression model 
# Should I use regression? (QuantCrit)
# Fitting multinom regression is when response/dependent variable is multiclass and IS NOT ordinal
# course grade is a multiclass/multilevel categorical variable, but IS ordinal
# Don't use below.
#install.packages("nnet")
#library(nnet)
#m <- multinom(crse_grade_off ~ 
#                     Subject_Course + #FEs
#                     (1|deid), #REs
#                   data = Bio_all) #no family as running multinominal regression 
#summary(m)
# ^^ Possibly use proportional odds model
