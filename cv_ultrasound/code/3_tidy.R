#!/usr/bin/env Rscript

## Tidy the data {{{

# Add ID numbers to each subject, ids package used
num <- length(df_big$email)
id <- adjective_animal(n = num, style = "snake")
tmp <- add_column(df_big, id, .before = 1)
df <- tmp

# Break into demographic and question data frames from total
demo <- df[c(1, 4:13)]
quiz <- df[c(1, 14:53)]

# Create factor of career-plan to be either CV-ultrasound oriented or not
demo$proc_career <- "No"
demo$proc_career[df$career_plan == "Pulmonology, Critical Care" | df$career_plan == "Cardiology"] <- "Yes"

# Clean up the demographic data into factors
cols <- names(demo[c(2:length(names(demo)))])
demo <- 
  demo %>%
  mutate_at(cols, funs(factor(.)))


# Clean up quiz answers
cols <- names(quiz[c(2:41)])
quiz <-
  quiz %>%
  mutate_at(cols, funs(factor(.)))

# Clean up dataframes
rm(df, num, tmp, cols)

# }}}

## Scoring the quiz {{{

# Score the quiz
df_scores <- CTT::score(quiz[, questions], answers, ID = demo$id, output.scored = TRUE)

# Place into a tibble
df_scores <- 
  df_scores$scored %>%
  as.data.frame %>%
  rownames_to_column %>%
  as_tibble

# Rename the ID column
names(df_scores)[1] <- "id"

# Grading the quiz per entry
df_scores$total <- rowSums(df_scores[-1])
df_scores$percent <- df_scores$total/21 * 100

# }}}

## Creating the confidence levels {{{

# Confidence levels
df_conf <- 
  quiz[, -which(names(quiz) %in% questions)] %>%
  mutate_at(vars(2:21), funs(recode(., "High"=1, "Low"=0, "Medium"=1)))

# Rename columns to match
names(df_conf)[2:21] <- questions

# Scores are contained in a similar field, 21 columns long 
df <- df_scores[1:21]

# Looping through both tables
# Output is /df/ that has confidence scoring included
for(y in 2:21) {
  for(x in 1:89) {
    if (is.na(df[x,y]) | is.na(df_conf[x,y])) {
      df[x,y] <- NA
    } else if (df[x,y] == 0 & df_conf[x,y] == 1) {
      df[x, y] <- "overconfidence" 
    } else if (df_conf[x,y] == 0) {
      df[x,y] <- "knowledge_gap"
    } else if (df[x,y] == 1 & df_conf[x,y] == 1) {
      df[x,y] <- "appropriate" }
  }
}

# Restructure answers, make factors, and melt it
# Each ID and each question has a correct:confidence label
cols <- names(df[2:21])
df_overconfidence <- 
  df %>%
  mutate_at(cols, funs(factor(.))) %>%
  gather(., question, level, -id)

# Refactor
df_overconfidence <-
  mutate_at(df_overconfidence, c("question", "level"), funs(factor(.)))

# Clean up
rm(cols, df, x, y)

# }}}

## Confidence and answers correlation table {{{

# Create long data of questions and confidence
a <- gather(df_scores[1:21], question, correctness, -id)
b <- gather(df_conf, question, confidence, -id)

# Concordance table
df_concordance <- 
  full_join(a, b, by = c("id", "question"))

# Set up alternative agreement table for correct x confidence
# [2] = concurrence, [1] = discrepancy, [0] = overconfidence
df <- df_concordance
df$agreement[df$correctness == 1 & df$confidence == 1] <- 2
df$agreement[df$correctness == 0 & df$confidence == 1] <- 0
df$agreement[df$correctness == 1 & df$confidence == 0] <- 1
df$agreement[df$correctness == 0 & df$confidence == 0] <- 1
df_concordance <- df

# Clean up
rm(a, b)

# }}}
