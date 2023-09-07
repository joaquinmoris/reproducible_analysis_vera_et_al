library(tidyverse)

full_data <- read_delim(file = "./input/Participants data.csv",
                        delim = ";")

summary(full_data)

# Change from a wide to a long format
tidy_data <- full_data %>%
  mutate(AT_pr = AT_pr * 100, ST_pr = ST_pr * 100) %>%
  rename(AT_pctR = AT_pr, ST_pctR = ST_pr) %>%
  mutate(max_AT_pctR = (AT_pctR == 100), max_ST_pctR = (ST_pctR == 100),
         max_pctR = factor(max_AT_pctR + max_ST_pctR * 10),
         ) %>%
  pivot_longer(cols = starts_with(c("AT","ST")), names_sep = "_",
               names_to = c("type", "measure"), values_to = "value")


# Table 1 of the paper
tidy_data %>%
  filter (measure != "rt") %>%
  group_by(Group, type, measure) %>%
  summarise(M = mean(value),
            SD = sd(value))

# Figure 1 of the paper
tidy_data %>%
  filter(measure == "contingency") %>%
  ggplot(aes(x = Group, color = Group, y = value)) +
  geom_boxplot(outlier.alpha = 0) +
  geom_jitter(height = 0, alpha = 0.2) +
  stat_summary(fun = mean, geom = "point", size = 3) + 
  facet_grid(measure~type)

# Figure 2 of the paper
tidy_data %>%
  filter(measure == "confidence") %>%
  ggplot(aes(x = t_order, y = value, color = type)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point", size = 3) +
  coord_cartesian(ylim = c(0,100))

# Table included in figure 2
tidy_data %>%
  filter(measure == "confidence") %>%
  group_by(t_order, type) %>%
  summarise(M = mean(value))


# Inferential analyses ----------------------------------------------------

# T-tests comparing each condition against 0

for (cur_condition in c("AT", "ST")){
  for (cur_group in c("Engineer", "Student")){
    print(paste("T TEST FOR THE CONDITION",cur_condition,
                "AND THE GROUP", cur_group))
    tidy_data %>%
      filter(measure == "contingency",
             type == cur_condition,
             Group == cur_group) %>%
      with(t.test(value, mu = 0)) %>% print()
  }
}

# ANOVAs

library(ez)

for (cur_measure in unique(tidy_data$measure)){
  print(paste("REPEATED MEASURES ANOVA FOR THE MEASURE",
              cur_measure))

  ezANOVA(tidy_data %>%
            filter(measure == cur_measure),
          wid = Participant,
          dv = value,
          within = type,
          between = c(Group, t_order)) %>% print()
}

# Correlations

cor.test(x = full_data$AT_pr, y = full_data$AT_contingency)
cor.test(x = full_data$ST_pr, y = full_data$ST_contingency)
cor.test(x = full_data$AT_contingency, y = full_data$ST_contingency)
cor.test(x = full_data$AT_risk, y = full_data$ST_risk)