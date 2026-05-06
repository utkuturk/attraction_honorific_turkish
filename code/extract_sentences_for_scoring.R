library(dplyr)
library(readxl)
library(janitor)
library(magrittr)

# Load data
df <- read_excel("./ibex/results/results_2021-02-09T14_54_08_538Z_SA_AAF.xlsx")
colnames(df) %<>% make_clean_names()
df %<>% subset(logged_in_as_experiment_owner_if_known != "yes")

# Filter out bad subjects
practice <- df %>% subset(type == "practice" & !is.na(question_null_if_none))
practice$whether_or_not_answer_was_correct_null_if_n_a[
    practice$whether_or_not_answer_was_correct_null_if_n_a == "NULL"
] <- 0
practice$whether_or_not_answer_was_correct_null_if_n_a %<>% as.integer()

bad_subjects <- practice %>%
    group_by(results_index) %>%
    summarize(p_yes = mean(whether_or_not_answer_was_correct_null_if_n_a)) %>%
    subset(p_yes <= 0.5) %>%
    .$results_index

df <- df %>% subset(!results_index %in% bad_subjects)

# Get formal register items with sentences
formal_items <- df %>%
    subset(grepl("filler_utku_", type)) %>%
    subset(!is.na(sentence_or_sentence_md5)) %>%
    mutate(
        register = stringr::str_split_fixed(type, "_", 5)[, 3],
        att_n = stringr::str_sub(type, -4, -3),
        v_n = stringr::str_sub(type, -2, -1)
    ) %>%
    filter(register == "formal", v_n == "pl") %>%
    select(group, type, att_n, sentence_or_sentence_md5) %>%
    distinct() %>%
    arrange(group, att_n)

cat("=== Sentence Extraction for Honorificness Scoring ===\n\n")
cat("Total unique sentence items:", nrow(formal_items), "\n\n")

# Create template for honorificness scoring
scoring_template <- formal_items %>%
    mutate(
        item_id = paste(group, att_n, sep = "_"),
        honorificness_score = NA, # To be filled in manually
        notes = "" # Optional notes about the subject
    ) %>%
    select(
        item_id,
        group,
        att_n,
        sentence_or_sentence_md5,
        honorificness_score,
        notes
    )

# Save template
write.csv(
    scoring_template,
    "honorificness_scoring_template.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
)

cat("=== Template saved ===\n")
cat("File: honorificness_scoring_template.csv\n")
cat("This file contains", nrow(scoring_template), "unique items to score\n\n")

cat("=== Instructions for manual scoring ===\n")
cat("1. Open honorificness_scoring_template.csv\n")
cat("2. For each sentence, identify the subject NP\n")
cat("3. Score the subject's honorificness on a 1-7 scale:\n")
cat("   1 = Very low status (child, student, informal peer)\n")
cat("   4 = Neutral/average status\n")
cat("   7 = Very high status (professor, doctor, elder, authority)\n")
cat("4. Fill in the 'honorificness_score' column\n")
cat("5. Optionally add notes about the subject in the 'notes' column\n")
cat("6. Save the file as 'honorificness_scores.csv'\n")
cat("7. Run: Rscript code/analyze_honorificness.R\n\n")

# Print sample of items for preview
cat("=== Sample of items to score ===\n")
print(head(scoring_template, 10), width = 200)
