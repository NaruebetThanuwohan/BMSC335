library(tidyverse)
library(ggplot2)
library(janitor)

data <- read_csv("C:/Users/Predator/OneDrive/Documents/University/BMSC339/EEG Data.csv") 

data <- clean_names(data)

data$q_seconds <- data$seconds_of_stage_3_seconds_before_q_asked 

data <- data %>% select(-seconds_of_stage_3_seconds_before_q_asked)

data %>% filter()

question_before <- data %>% group_by(stage, question) %>% 
  filter(stage == "before") %>% 
  summarise(count = mean(beta_wave_typical)) 

question_before$stage <- question_before$stage <- "Baseline"

question_read <- data %>% 
  filter(stage == "after" & q_seconds == 4) %>%
  rename(count = beta_wave_typical)

question_think_1 <- data %>% 
  filter(stage == "after" & q_seconds == 5) %>%
  rename(count = beta_wave_typical)

question_think_2 <- data %>% 
  filter(stage == "after" & q_seconds == 6) %>%
  rename(count = beta_wave_typical)

q_before_read <- rbind(question_before, question_read)
q_before_read_think_1 <- rbind(q_before_read, question_think_1)
q_before_read_think_1_2 <- rbind(q_before_read_think_1, question_think_2)

data_cleaned <- q_before_read_think_1_2 %>% mutate(stage = case_when(
  q_seconds == 4 ~ "1 second",
  q_seconds == 5 ~ "2 seconds",
  q_seconds == 6 ~ "3 seconds",
  TRUE ~ as.character(stage)
))

data_cleaned$stage <- factor(data_cleaned$stage, levels = c("Baseline", "1 second", "2 seconds", "3 seconds"))


data_cleaned <- data_cleaned %>% filter(question != "clear mind")
options(repr.plot.width = 2, repr.plot.height =100)

filename="C:/Users/Predator/OneDrive/Desktop/123.png"

ggplot(data_cleaned, aes(stage, count, group = 1)) +                                    
  geom_line(color = "red") +
  facet_wrap(~question, ncol = 3) + 
  theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust=1)) + 
  ggtitle("Count of Beta Waves Before and When Task Assigned") + 
  ylab("Average Number of Beta Waves")

main_aov <- aov(count ~ stage, data = data_cleaned)

data_mean_stage <- data_cleaned %>% group_by(stage) %>% summarise(mean_stage = mean(count))

ggplot(data_mean_stage, aes(stage, mean_stage)) +                                    
  geom_col() +
  ggtitle("Count of Beta Waves At Baseline and When Task Assigned") + 
  ylab("Average Number of Beta Waves") 

summary(main_aov)

questions_aov <- aov(count ~ stage + question, data = data_cleaned)

data_mean_stage_question <- data_cleaned %>% group_by(stage, question) %>% summarise(mean_stage_question = mean(count), .groups = "keep")


ggplot(data_mean_stage_question, aes(stage, mean_stage_question)) +                                    
  geom_col() +
  facet_wrap(~question, ncol = 3) + 
  theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust=1)) + 
  ggtitle("Count of Beta Waves Before and When Task Assigned") + 
  ylab("Average Number of Beta Waves")

summary(questions_aov)


