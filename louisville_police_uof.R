library(tidyverse)

#### Reading in UOF data
uof <- read_csv('uof_louisville.csv')

#### Creating a new column which is the hour of the time_of_occurrence column and finding the hour that is the most frequent for UOF reports
frequent_hour <- uof %>%
  mutate(hour = lubridate::hour(time_of_occurrence)) %>%
  count(hour, sort = T) %>%
  slice(1) %>%
  pull(hour)

#### Creating a new column which is the month of the date_of_occurrence column, and finding the month with the leasy amount of UOF incidents
least_frequent_month <- uof %>%
  mutate(month = lubridate::month(date_of_occurrence)) %>%
  count(month, sort = T) %>%
  slice(12) %>%
  pull(month)

#### Creating a new column named day which is the day of the date_of_occurrence column, and finding the day of the week that has the most number of UOF reports
most_frequent_day <- uof %>%
  mutate(day = lubridate::wday(date_of_occurrence, label = TRUE)) %>%
  count(day, sort = T) %>%
  slice(1) %>%
  pull(day)

#### Creating a table to see if there is any relationship as to when UOF incidents happen most in the month
day_distribution <- uof %>% count(day = lubridate::day(date_of_occurrence), sort = T)
day_distribution <- day_distribution %>% mutate(fraction = n / nrow(uof))
day_distribution <- janitor::adorn_totals(day_distribution)

#### Finding all unique categories of force_used_1 and force_used_2
force_used_1 <- uof %>% distinct(force_used_1) %>% pull()
force_used_2 <- uof %>% distinct(force_used_2) %>% pull()

#### Extracting all the unique types of forces used from force_used_1...force_used_8
all_force <- uof %>%
  distinct(force_used_1, force_used_2, force_used_3, force_used_4, force_used_5, force_used_6, force_used_7, force_used_8) %>%
  t() %>%
  c() %>%
  unique()

#### Defining a new vector which contains all the violent UOF tactics
violent_force <- c("take down", "hobble", "ecw cartridge deployed", "knee strike(s)",
                   "12 ga. sock round", "take-down", "impact weapon",
                   "kick", "deadly force used")

#### Creating a column which is a binary variable equal to 1 if force_used_1 contains any values from violent_force and 0 otherwise
uof <- uof %>%
  mutate(violent_uof_1 = ifelse(force_used_1 %in% violent_force ,1, 0))

#### Creating table to see count and proportions of violent force used 
violent_force_service_table <- uof %>%
  filter(violent_uof_1 == 1) %>%
  count(service_rendered, sort = T) %>%
  mutate(fraction = n/nrow(filter(uof, violent_uof_1 == 1))) %>%
  janitor::adorn_totals()

#### Filtering citizen_gender to only include males and females, excluding all NA values from citizen_race, and creating a new column which is binary 1 if force_used_1_effective is equal to 'yes' and 0 otherwise
uof_filtered <- uof %>%
  filter(citizen_gender == 'male'| citizen_gender == 'female') %>%
  filter(!is.na(citizen_race)) %>%
  mutate(force_used_1_effective_binary = ifelse(force_used_1_effective == 'yes', 1, 0))

#### Creating table to group by gender and race and see how many times violent force was used on them, how often it was effective, and what fraction of the time it was effective
uof_filtered_table <- uof_filtered %>%
  group_by(citizen_gender, citizen_race) %>%
  summarise(effective_1 = sum(force_used_1_effective_binary, na.rm = T), counts = n()) %>%
  janitor::adorn_totals() %>%
  mutate(fraction_effective = effective_1/counts)

#### Finding which officers are involved the most, which have the most 'violent' interactions, and which officers have the highest percentage of violent interactions
violent_officers <- uof %>%
  group_by(name_badge_number) %>%
  filter(force_used_1 != 'none' & !is.na(force_used_1)) %>%
  summarise(violent_amount = sum(violent_uof_1), counts = n()) %>%
  mutate(fraction_violent = violent_amount/counts) %>%
  arrange(desc(fraction_violent))

#### Finding which officers had the most violent interactions by race
violent_officers_prej <- uof %>%
  group_by(name_badge_number, citizen_race) %>%
  summarise(violent_force_race = sum(violent_uof_1)) %>%
  arrange(desc(violent_force_race))

#### Finding what reason cops were called that led to force
force_by_reason <- uof %>%
  group_by(service_rendered) %>%
  filter(force_used_1 != 'none' & !is.na(force_used_1)) %>%
  count(sort = T)

#### Finding what reason cops were called that led to violent interaction
violent_by_reason <- uof %>%
  group_by(service_rendered) %>%
  summarise(violent = sum(violent_uof_1)) %>%
  arrange(desc(violent))

force_failed_violent <- uof %>%
  filter(force_used_1_effective == 'no' & violent_uof_1 == 1) %>%
  nrow()

violent_interaction <- uof %>%
  filter(violent_uof_1 == 1) %>%
  nrow()

#### Finding how successful the violent interaction was
violent_failure_rate <- force_failed_violent/violent_interaction


#### Finding what the citizen influences were that led to force
citizen_influence_force <- uof %>%
  filter(force_used_1 != 'none' & !is.na(force_used_1)) %>%
  group_by(citizen_influence_assessment) %>%
  count() %>%
  arrange(desc(n))
citizen_influence_violent <- uof %>%
  filter(force_used_1 != 'none' & !is.na(force_used_1)) %>%
  group_by(citizen_influence_assessment) %>%
  summarise(violent_influnece = sum(violent_uof_1)) %>%
  arrange(desc(violent_influnece))






