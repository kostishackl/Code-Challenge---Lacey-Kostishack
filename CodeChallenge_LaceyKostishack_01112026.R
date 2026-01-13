#  PURPOSE: (1) Pull, score, and clean HAM data from participants of interest 
#           (2) Visualize total number of participants by Recruitment Source, Age, Gender, and Group
#  CREATOR: Lacey Kostishack
#  CREATED: 01/11/2026
#  LAST UPDATED: 01/12/2026

#load libraries
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(apyramid)

#load data
load("C:/Users/Kosti/OneDrive/Desktop/Current CV and Cover Letters/Interview Prep/12-15-2025 - Data Coordinator & Analyst - 7484823394 - UPMC Western Psych/CodeChallenge2025/CodeChallenge2024.RData")

old_ids <- read.table("C:/Users/Kosti/OneDrive/Desktop/Current CV and Cover Letters/Interview Prep/12-15-2025 - Data Coordinator & Analyst - 7484823394 - UPMC Western Psych/CodeChallenge2025/IDs.txt", header = FALSE)

#(1) Pull, score, and clean HAM data from participants of interest -------------------------------------------
#(1a) Explore and clean data
#explore data
str(consent_date)
str(HAM_protect)
str(HAM_sleep)
str(id_map)
str(old_ids)
str(recruitment_data)

#rename columns
old_ids <- old_ids %>% rename(old_id = V1)
consent_date <- consent_date %>% rename (new_id = ID)
HAM_sleep <- HAM_sleep %>% rename (new_id = ID)
HAM_protect <- HAM_protect %>% rename(old_id = ID)

#look at columns of character type that should be numeric
freq_HAM_protect <- lapply(HAM_protect, table)
print(freq_HAM_protect)

freq_id_map <- lapply(id_map, table)
print(freq_id_map)

#convert relevant character variables to numeric variables
HAM_protect <- HAM_protect %>%
  mutate(across(c(old_id, ham_9_ag, ham_11_soma, ham_12_gi, ham_13_gs, ham_14_sex, ham_15_hd, ham_17_weight), as.numeric))
  #NOTE: one entry for ham_17_weight entered as "NASK" converted to NA

id_map <- id_map %>%
  mutate(across(old_id, as.numeric))
  #NOTE: two entries for old_id entered as "N/A" converted to NA

#check variable types
str(HAM_protect)
str(id_map)

#convert all old ids to new ids and remove old ids
new_ids <- left_join(old_ids, id_map, by = "old_id")

HAM_protect <-  left_join(HAM_protect, id_map, by = "old_id")
HAM_protect <- subset(HAM_protect, select = -old_id)

#filter to only IDs of interest
consent_date_subset <- semi_join(consent_date, new_ids, by = "new_id") #n=836 to n=90
HAM_protect_subset <- semi_join(HAM_protect, new_ids, by = "new_id") #n=1759 to n=1738
HAM_sleep_subset <- semi_join(HAM_sleep, new_ids, by = "new_id") #n=149 to n=149

#(1b) Calculate and include the total HAM score of each participant, mean score of each participant, and latest score of each participant----------------------------------------------------
#append HAM tables and sort by participant and time point
HAM_subset <- rbind(HAM_sleep_subset, HAM_protect_subset) #n=1887
HAM_subset <- HAM_subset %>% arrange(new_id, bq_date, fug_date)
str(HAM_subset)

#calculate total HAM score per participant per time point
HAM_subset <- HAM_subset %>% mutate(HAM_total=ham_1_dm+ham_2_gf+ham_3_su+ham_4_ii+ham_5_im+ham_6_di+ham_7_wi+ham_8_re+ham_9_ag
                                    +ham_10_psya+ham_11_soma+ham_12_gi+ham_13_gs+ham_14_sex+ham_15_hd+ham_16_li+ham_17_weight)

#calculate mean HAM score 
HAM_mean <- HAM_subset %>% group_by(new_id) %>% summarize(HAM_mean = round(mean(HAM_total, na.rm = TRUE), 2)) #caluclate mean by ID

HAM_subset <- left_join(HAM_subset, HAM_mean, by = "new_id") #join mean HAM score to main data set

#pull latest HAM score
HAM_subset <- HAM_subset %>%
  mutate(date = coalesce(bq_date, fug_date)) #create one date variable to pull most recent date

HAM_latest <- HAM_subset %>% 
  group_by(new_id) %>%
  arrange(new_id, date) %>%
  filter(!is.na(HAM_total)) %>%
  slice_tail(n = 1) #filters out latest HAM score where HAM is not NA

HAM_subset <- HAM_subset %>%
  left_join(
    HAM_latest %>%
      select(new_id, HAM_total) %>%
      rename(HAM_latest = HAM_total),
    by = "new_id"
  ) #join latest HAM score to main data set

#(1c) Get the HAM score that is closest to 1 year after their first consent date
#find first consent date for each ID
consent_date_subset$first_consent <- as.Date(apply(consent_date_subset, MARGIN = 1, FUN = min, na.rm = TRUE))

#join first consent date to to HAM data
HAM_subset <- HAM_subset %>%
  left_join(
    consent_date_subset %>%
      select(new_id, first_consent),
    by = "new_id"
  )

#calculate difference between HAM date and target date (i.e., one year after first consent date) and find minimun
one_year <- HAM_subset %>%
  filter(!is.na(HAM_total)) %>% #filter out rows with NA
  group_by(new_id) %>%
  mutate(
    target_date = first_consent + years(1), #calculate one year after first consent date as target date
    days_diff = abs(as.integer(difftime(date, target_date, units = "days"))) #calculate days b/w HAM score date and consent date
  ) %>%
  slice_min(order_by = days_diff, n=1, with_ties = FALSE) #select minimum
  
#join HAM score closest to one year after first consent date to to HAM data
HAM_subset <- HAM_subset %>%
  left_join(
    one_year %>%
      select(new_id, HAM_total) %>%
      rename(HAM_one_year = HAM_total),
    by = "new_id"
  )

#(1d) Create final data set and export
final_df <- subset(HAM_subset, select = -c(date, first_consent))

write.csv(final_df, 
          file = "C:/Users/Kosti/OneDrive/Desktop/Current CV and Cover Letters/Interview Prep/12-15-2025 - Data Coordinator & Analyst - 7484823394 - UPMC Western Psych/CodeChallenge2025/OutputFiles_LaceyKostishack/final_df.csv", 
          row.names = FALSE)

#-------------------------------------------------------------------------------------------------------------

#(2) Visualize total number of participants by Recruitment Source, Age, Gender, and Group -------------------------------------------
#(2a) Visualize total number of participants by recruitment source
#clean data for better readability on visuals 
recruitment_data$RecruitSource_simple <- gsub("[0-9]", "", recruitment_data$RecruitSource) #remove numbers
recruitment_data$RecruitSource_simple <- gsub("^\\s+", "", recruitment_data$RecruitSource_simple) #remove leading spaces

#count number of participants by recruitment source
source_counts <- recruitment_data %>%
  group_by(RecruitSource_simple) %>%
  summarize(count = n()) %>%
  arrange(count)

#order so that most frequent is first and other is last
source_counts <- source_counts %>%
  mutate(is_other = ifelse(RecruitSource_simple == "Other", 0, 1)) %>%
  arrange(is_other, count) %>%
  mutate(RecruitSource_simple = factor(RecruitSource_simple, levels = RecruitSource_simple))

#create horizontal bar chart
by_recruit_source <- ggplot(source_counts, aes(x = count, y = RecruitSource_simple)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Participant Count by Recruitment Source",
    x = "Number of Participants",
    y = "Recruitment Source"
  ) +
  theme_minimal() +
  scale_y_discrete(limits = levels(source_counts$RecruitSource_simple))

ggsave("Participants by Recruitment Source.png", 
       plot = by_recruit_source,
       width = 8,
       height = 6,
       units = "in",
       dpi = 300)

#(2b) Visualize total number of participants by age
#define breaks for bins
my_breaks <- seq(min(recruitment_data$Age), max(recruitment_data$Age)+5, by = 5)

#create histogram
by_age <- ggplot(recruitment_data, aes(x = Age)) +
  geom_histogram(breaks = my_breaks, fill = "skyblue", color = "white") +
  scale_x_continuous(breaks = my_breaks, name = "Age Group") +
  scale_y_continuous(breaks = pretty_breaks(n = 6), labels = function(x) floor(x)) +
  labs(title = "Participant Count by Age", y = "Number of Participants") +
  theme_minimal()

ggsave("Participants by Recruitment Age.png", 
       plot = by_age,
       width = 8,
       height = 6,
       units = "in",
       dpi = 300)

#(2c) Visualize total number of participants by gender
#create bar chart
by_gender <- ggplot(recruitment_data, aes(x = Gender)) +
  geom_bar(fill = "skyblue") + 
  scale_x_discrete(labels = c("F" = "Female", "M" = "Male")) +
  labs(title = "Participant Count by Gender",
       x = "Gender",
       y = "Number of Participants") +
  theme_minimal()

ggsave("Participants by Recruitment Gender.png", 
       plot = by_gender,
       width = 8,
       height = 6,
       units = "in",
       dpi = 300)

#(2d) Visualize total number of participants by group
by_group <- ggplot(recruitment_data, aes(x = fct_infreq(Group))) +
  geom_bar(fill = "skyblue") + 
  labs(title = "Participant Count by Group",
       x = "Group",
       y = "Number of Participants") +
  theme_minimal()

ggsave("Participants by Recruitment Group.png", 
       plot = by_group,
       width = 8,
       height = 6,
       units = "in",
       dpi = 300)

#(2e) EXTRA VISUAL: Create population pyramid
#create age groups
recruitment_data <- recruitment_data %>%
  mutate(age_group = cut(Age, breaks = seq(30, 75, by = 5), right = FALSE, 
                         labels = c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74")))

#calculate population counts by age and gender
population_counts <- recruitment_data %>%
  count(Gender, age_group, name = "count") %>%
  mutate(
    gender_factor = factor(Gender),
    gender_full = case_when(
      gender_factor == 'M' ~ 'Male',
      gender_factor == 'F' ~ 'Female'
      ),
    pop_count = ifelse(gender_full == "Male", -count, count) # Assuming 'count' is your population variable
  )

#create population pyramid and save
pop_pyramid <- ggplot(population_counts, aes(x = age_group, y = pop_count, fill = gender_full)) +
  geom_bar(stat = "identity") +
  coord_flip() + # Flips axes
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "red")) + # Custom colors
  labs(title = "Population Pyramid", x = "Age Group", y = "Population") +
  scale_y_continuous(
    breaks = seq(-10, 10, by = 2), # Set tick locations, e.g., from -150 to 150 in steps of 50
    labels = abs(seq(-10, 10, by = 2)) # Display the absolute values (positive numbers)
  ) +
  labs(fill = "Gender") +
  theme_minimal()

ggsave("Population Pyramid.png", 
       plot = pop_pyramid,
       width = 8,
       height = 6,
       units = "in",
       dpi = 300)
