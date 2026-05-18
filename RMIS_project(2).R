# install packages needed 
install.packages(c("tidyverse", "effsize", "rstatix", "ggplot2"))
# Checked em
library(tidyverse)
library(effsize)
library(rstatix)
library(ggplot2)
# This is to load the data, left blank in document as it reads my own personal folders
setwd("C:/Users/mulle/OneDrive/Documents")
#getwd()
# Data loading
data_raw <- read.csv("RMIS_May 16, 2026_11.18.csv", header = TRUE)

# Removing rows 1 to 10, is 2 to 11 in excel, though R doesn't count header
data_raw <- data_raw[-(1:10), ]

# Removes the first 18 columns as they are not necessary data.
data_raw <- data_raw[, -(1:18)]
colnames(data_raw)[1:10]
# Reset de row numbers
rownames(data_raw) <- NULL

# Rows are imported as character, not as numbers
columns <- c("Q1", "Q1.1", "Q2", "Q2.1", "Q3", "Q3.1",
             "Q4", "Q4.1", "Q5", "Q5.1", "Q6", "Q6.1",
             "Q7", "Q7.1", "Q8", "Q8.1", "Q9", "Q9.1",
             "Q10", "Q10.1", "Q11", "Q11.1", "Q12", "Q12.1")

data_raw <- data_raw %>%
  mutate(across(all_of(columns), as.numeric))

# Check of het gelukt is
str(data_raw %>% select(all_of(columns)))

# Advies values for each question
advies <- c(
  Q1  = 1658,      # AI label
  Q2  = 1942,       # AI label
  Q3  = 85,       # Mens label
  Q4  = 40800,      # Mens label
  Q5  = 584,   # Mens label
  Q6  = 6725,    # AI label
  Q7  = 213,      # Mens label
  Q8  = 1878,      # AI label
  Q9  = 31,  # AI label
  Q10 = 7525,      # Mens label
  Q11 = 1950,      # Mens label
  Q12 = 1899       # AI label
)
# Some people left it blank when they didn't want to change their answer, so make 
# Sure they are the same as initial guess, to remain true to the WOA formula
data_raw <- data_raw %>%
  mutate(
    Q1.1  = ifelse(is.na(Q1.1),  Q1,  Q1.1),
    Q2.1  = ifelse(is.na(Q2.1),  Q2,  Q2.1),
    Q3.1  = ifelse(is.na(Q3.1),  Q3,  Q3.1),
    Q4.1  = ifelse(is.na(Q4.1),  Q4,  Q4.1),
    Q5.1  = ifelse(is.na(Q5.1),  Q5,  Q5.1),
    Q6.1  = ifelse(is.na(Q6.1),  Q6,  Q6.1),
    Q7.1  = ifelse(is.na(Q7.1),  Q7,  Q7.1),
    Q8.1  = ifelse(is.na(Q8.1),  Q8,  Q8.1),
    Q9.1  = ifelse(is.na(Q9.1),  Q9,  Q9.1),
    Q10.1 = ifelse(is.na(Q10.1), Q10, Q10.1),
    Q11.1 = ifelse(is.na(Q11.1), Q11, Q11.1),
    Q12.1 = ifelse(is.na(Q12.1), Q12, Q12.1)
  )
# WOA per question
data <- data_raw %>%
  mutate(
    WOA_Q1  = (Q1.1  - Q1)  / (advies["Q1"]  - Q1),
    WOA_Q2  = (Q2.1  - Q2)  / (advies["Q2"]  - Q2),
    WOA_Q3  = (Q3.1  - Q3)  / (advies["Q3"]  - Q3),
    WOA_Q4  = (Q4.1  - Q4)  / (advies["Q4"]  - Q4),
    WOA_Q5  = (Q5.1  - Q5)  / (advies["Q5"]  - Q5),
    WOA_Q6  = (Q6.1  - Q6)  / (advies["Q6"]  - Q6),
    WOA_Q7  = (Q7.1  - Q7)  / (advies["Q7"]  - Q7),
    WOA_Q8  = (Q8.1  - Q8)  / (advies["Q8"]  - Q8),
    WOA_Q9  = (Q9.1  - Q9)  / (advies["Q9"]  - Q9),
    WOA_Q10 = (Q10.1 - Q10) / (advies["Q10"] - Q10),
    WOA_Q11 = (Q11.1 - Q11) / (advies["Q11"] - Q11),
    WOA_Q12 = (Q12.1 - Q12) / (advies["Q12"] - Q12)
  )

# Clip extreme values en removes NaN
# (happens when schatting = advies)
clip_woa <- function(x) {
  ifelse(is.nan(x) | is.infinite(x) | x < -1 | x > 2, NA, x)
}

data <- data %>%
  mutate(across(starts_with("WOA_"), clip_woa))

# Control to see if it works
data %>%
  select(starts_with("WOA_")) %>%
  summary()

# Dataframe per row: participant, question number, label, WOA
data <- data %>%
  mutate(ResponseID = row_number())
data_long <- data %>%
  select(ResponseID, starts_with("WOA_")) %>%
  pivot_longer(
    cols = starts_with("WOA_"),
    names_to = "Question",
    values_to = "WOA"
  ) %>%
  # Adds labels to the questions
  mutate(Label = case_when(
    Question %in% c("WOA_Q1", "WOA_Q2", "WOA_Q6",
                    "WOA_Q8", "WOA_Q9", "WOA_Q12") ~ "AI",
    Question %in% c("WOA_Q3", "WOA_Q4", "WOA_Q5",
                    "WOA_Q7", "WOA_Q10", "WOA_Q11") ~ "Mens",
    TRUE ~ NA_character_
  ))

# Calculate average WOA per condition
data_summary <- data_long %>%
  group_by(ResponseID, Label) %>%
  summarise(
    mean_WOA = mean(WOA, na.rm = TRUE),
    .groups = "drop"
  )
# Check
head(data_summary, 10)

# Descriptives
descriptives <- data_summary %>%
  group_by(Label) %>%
  summarise(
    N         = n(),
    Gemiddelde = mean(mean_WOA, na.rm = TRUE),
    SD        = sd(mean_WOA, na.rm = TRUE),
    Mediaan   = median(mean_WOA, na.rm = TRUE),
    Min       = min(mean_WOA, na.rm = TRUE),
    Max       = max(mean_WOA, na.rm = TRUE),
    SE        = SD / sqrt(N)
  )

print(descriptives)

# Normality tests, hope that its normally distributed! 
data_summary %>%
  group_by(Label) %>%
  shapiro_test(mean_WOA)
# Visual check
ggplot(data_summary, aes(x = mean_WOA, fill = Label)) +
  geom_histogram(bins = 10, alpha = 0.6, position = "identity") +
  facet_wrap(~Label) +
  labs(title = "Verdeling WOA per conditie",
       x = "Gemiddelde WOA",
       y = "Frequentie") +
  theme_minimal()

# Data back to wide 
data_wide <- data_summary %>%
  pivot_wider(names_from = Label, values_from = mean_WOA)
# Wilcoxon signed-rank tests, it is after all not normally distributed!
t_result <- t.test(data_wide$AI, data_wide$Mens,
                   paired = TRUE,
                   alternative = "greater")
print(t_result)

# Cohen's d voor paired t-toets
cohen_d <- cohen.d(data_wide$AI, data_wide$Mens, paired = TRUE)
print(cohen_d)
# outlier check
cat("Deelnemers met WOA_Mens > 1.0:\n")
print(data_wide %>% filter(Mens > 1.0) %>% select(ResponseID, AI, Mens))

# Run t-toets zonder outlier
data_wide_clean <- data_wide %>% filter(Mens <= 1.0)

t_zonder <- t.test(data_wide_clean$AI, data_wide_clean$Mens,
                   paired = TRUE,
                   alternative = "two.sided")

cohen_d_zonder <- cohen.d(data_wide_clean$AI, data_wide_clean$Mens,
                          paired = TRUE, na.rm = TRUE)
# Vergelijk beide resultaten
cat("\n--- Sensitiviteitsanalyse ---\n")
cat("Met outlier (n = 37):\n")
cat("  t =", round(t_result$statistic, 3),
    ", df =", t_result$parameter,
    ", p =", round(t_result$p.value, 3),
    ", d =", round(cohen_d$estimate, 3), "\n\n")

cat("Zonder outlier (n =", nrow(data_wide_clean), "):\n")
cat("  t =", round(t_zonder$statistic, 3),
    ", df =", t_zonder$parameter,
    ", p =", round(t_zonder$p.value, 3),
    ", d =", round(cohen_d_zonder$estimate, 3), "\n")
# Er is 1 echte outlier! Met 1.30 bij mens
# Als we die weghalen...
data_wide_clean2 <- data_wide %>% filter(Mens <= 1.25)

t_zonder2 <- t.test(data_wide_clean2$AI, data_wide_clean2$Mens,
                    paired = TRUE, alternative = "two.sided")
cohen_d_zonder2 <- cohen.d(data_wide_clean2$AI, data_wide_clean2$Mens,
                           paired = TRUE, na.rm = TRUE)
# ------------- DEGENE HIERONDER IS DE GEBRUIKTE RESULTAAT! ---------------
cat("Zonder alleen deelnemer 32 (n =", nrow(data_wide_clean2), "):\n")
cat("  t =", round(t_zonder2$statistic, 3),
    ", df =", t_zonder2$parameter,
    ", p =", round(t_zonder2$p.value, 3),
    ", d =", round(cohen_d_zonder2$estimate, 3), "\n")
# ------------- DEGENE HIERBOVEN IS DE GEBRUIKTE RESULTAAT! ---------------
# This is for the STIA questions
likert_naar_nummer <- function(x) {
  case_when(
    x == "Strongly disagree"          ~ 1,
    x == "Disagree"                   ~ 2,
    x == "Somewhat disagree"          ~ 3,
    x == "Neither agree nor disagree" ~ 4,
    x == "Somewhat agree"             ~ 5,
    x == "Agree"                      ~ 6,
    x == "Strongly agree"             ~ 7,
    TRUE ~ NA_real_
  )
}
data <- data %>%
  mutate(
    Q51_1 = likert_naar_nummer(as.character(Q51_1)),
    Q51_2 = likert_naar_nummer(as.character(Q51_2)),
    Q51_3 = likert_naar_nummer(as.character(Q51_3))
  ) %>%
  mutate(STIA_score = rowMeans(
    cbind(Q51_1, Q51_2, Q51_3), na.rm = TRUE
  ))

# Descriptives
data %>%
  summarise(
    N          = sum(!is.na(STIA_score)),
    Gemiddelde = mean(STIA_score, na.rm = TRUE),
    SD         = sd(STIA_score, na.rm = TRUE),
    Mediaan    = median(STIA_score, na.rm = TRUE),
    Min        = min(STIA_score, na.rm = TRUE),
    Max        = max(STIA_score, na.rm = TRUE)
  )

# Add WOA_Mens
data_wide <- data_summary %>%
  pivot_wider(names_from = Label, values_from = mean_WOA) %>%
  rename(WOA_AI_gem = AI, WOA_Mens_gem = Mens)
data <- data %>%
  left_join(data_wide, by = "ResponseID")
# correlaties
cor_AI <- cor.test(data$STIA_score, data$WOA_AI_gem,
                   method = "spearman", exact = FALSE)
cor_Mens <- cor.test(data$STIA_score, data$WOA_Mens_gem,
                     method = "spearman", exact = FALSE)

cat("\n--- Correlaties STIA met WOA ---\n")
cat("STIA x WOA_AI:   rho =", round(cor_AI$estimate, 3),
    ", p =", round(cor_AI$p.value, 3), "\n")
cat("STIA x WOA_Mens: rho =", round(cor_Mens$estimate, 3),
    ", p =", round(cor_Mens$p.value, 3), "\n")

# plots
data_summary_clean <- data_summary %>%
  filter(ResponseID != 32)

# Schone versie van data_long zonder outlier
data_long_clean <- data_long %>%
  filter(ResponseID != 32)
# Boxplot WOA per condition
ggplot(data_summary_clean, aes(x = Label, y = mean_WOA, fill = Label)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16) +
  geom_jitter(width = 0.1, alpha = 0.4) +
  scale_fill_manual(values = c("AI" = "#4C9BE8", "Mens" = "#E8824C")) +
  labs(
    title = "Gemiddelde WOA per adviesbron (zonder outlier)",
    subtitle = paste0("Gepaarde t-toets: t(", t_zonder2$parameter, ") = ",
                      round(t_zonder2$statistic, 3),
                      ", p = ", round(t_zonder2$p.value, 3)),
    x = "Label adviesbron",
    y = "Gemiddelde Weight of Advice (WOA)",
    caption = "Hogere WOA = meer adviesopvolging. n = 36."
  ) +
  theme_minimal() +
  theme(legend.position = "none")
# WOA per question, helps to see differences between questions
# Otherwise the plot doesn't show well :(
juiste_volgorde <- c("WOA_Q1", "WOA_Q2", "WOA_Q3", "WOA_Q4",
                     "WOA_Q5", "WOA_Q6", "WOA_Q7", "WOA_Q8",
                     "WOA_Q9", "WOA_Q10", "WOA_Q11", "WOA_Q12")

woa_per_vraag_clean <- data_long_clean %>%
  group_by(Question, Label) %>%
  summarise(
    mean_WOA = mean(WOA, na.rm = TRUE),
    se = sd(WOA, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(Question = factor(Question, levels = juiste_volgorde))

ggplot(woa_per_vraag_clean, aes(x = Question, y = mean_WOA, fill = Label)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_errorbar(
    aes(ymin = mean_WOA - se, ymax = mean_WOA + se),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  scale_fill_manual(values = c("AI" = "#4C9BE8", "Mens" = "#E8824C")) +
  labs(
    title = "Gemiddelde WOA per vraag (zonder outlier)",
    x = "Question",
    y = "Gemiddelde WOA",
    fill = "Label"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Histogram, like before with data normality check bu tnow with median line
medianen_clean <- data_summary_clean %>%
  group_by(Label) %>%
  summarise(mediaan = median(mean_WOA, na.rm = TRUE))

ggplot(data_summary_clean, aes(x = mean_WOA, fill = Label)) +
  geom_histogram(bins = 10, alpha = 0.6, position = "identity") +
  geom_vline(
    data = medianen_clean,
    aes(xintercept = mediaan, color = Label),
    linetype = "dashed",
    linewidth = 1
  ) +
  facet_wrap(~Label) +
  scale_fill_manual(values = c("AI" = "#4C9BE8", "Mens" = "#E8824C")) +
  scale_color_manual(values = c("AI" = "#4C9BE8", "Mens" = "#E8824C")) +
  labs(
    title = "Verdeling WOA per conditie (zonder outlier)",
    subtitle = "Gestippelde lijn = mediaan",
    x = "Gemiddelde WOA",
    y = "Frequentie"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
