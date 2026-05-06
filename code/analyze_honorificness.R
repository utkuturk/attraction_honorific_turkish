library(dplyr)
library(readxl)
library(janitor)
library(magrittr)
library(ggplot2)
library(lme4) # for mixed effects models

# This script analyzes whether subject honorificness predicts acceptability
# within Cluster 1 (low acceptors) only

cat("=== Honorificness Analysis: Cluster 1 (Low Acceptors) ===\n\n")

# 1. Load honorificness scores
if (!file.exists("honorificness_scores.csv")) {
    stop(
        "ERROR: honorificness_scores.csv not found!\n",
        "Please complete the scoring template first.\n",
        "See: honorificness_scoring_template.csv"
    )
}

honorificness <- read.csv("honorificness_scores.csv", fileEncoding = "UTF-8")
cat("Loaded honorificness scores for", nrow(honorificness), "items\n")

# Check for missing scores
missing <- sum(is.na(honorificness$honorificness_score))
if (missing > 0) {
    warning("WARNING:", missing, "items are missing honorificness scores")
}

# 2. Load participant data
df <- read_excel("./ibex/results/results_2021-02-09T14_54_08_538Z_SA_AAF.xlsx")
colnames(df) %<>% make_clean_names()
df %<>% subset(logged_in_as_experiment_owner_if_known != "yes")

# Filter bad subjects
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

# 3. Load cluster assignments
clusters <- read.csv("cluster_assignments.csv")
cluster1_participants <- clusters %>%
    filter(cluster == 1) %>%
    pull(results_index)

cat("Cluster 1 has", length(cluster1_participants), "participants\n\n")

# 4. Prepare response data
formal_data <- df %>%
    subset(grepl("filler_utku_", type)) %>%
    subset(!is.na(question_null_if_none)) %>%
    mutate(
        register = stringr::str_split_fixed(type, "_", 5)[, 3],
        att_n = stringr::str_sub(type, -4, -3),
        v_n = stringr::str_sub(type, -2, -1),
        response_yes = ifelse(
            grepl("P'ye", answer),
            TRUE,
            ifelse(grepl("Q'ya", answer), FALSE, NA)
        )
    ) %>%
    filter(
        register == "formal",
        v_n == "pl",
        !is.na(response_yes),
        results_index %in% cluster1_participants # CLUSTER 1 ONLY
    ) %>%
    mutate(item_id = paste(group, att_n, sep = "_"))

# 5. Merge with honorificness scores
data_with_honor <- formal_data %>%
    left_join(
        honorificness %>% select(item_id, honorificness_score),
        by = "item_id"
    ) %>%
    filter(!is.na(honorificness_score))

cat("=== Data Summary ===\n")
cat("Responses from Cluster 1 participants:", nrow(data_with_honor), "\n")
cat(
    "Unique items with honorificness scores:",
    n_distinct(data_with_honor$item_id),
    "\n"
)
cat("Unique participants:", n_distinct(data_with_honor$results_index), "\n\n")

# 6. Item-level analysis
cat("=== Item-Level Analysis ===\n")
item_summary <- data_with_honor %>%
    group_by(item_id, honorificness_score) %>%
    summarize(
        acceptance_rate = mean(response_yes),
        n_responses = n(),
        .groups = "drop"
    ) %>%
    arrange(honorificness_score)

print(item_summary)
cat("\n")

# 7. Correlation analysis
cat("=== Correlation: Honorificness × Acceptance ===\n")
cor_test <- cor.test(
    item_summary$honorificness_score,
    item_summary$acceptance_rate
)
cat("Pearson's r =", round(cor_test$estimate, 3), "\n")
cat("p-value =", format.pval(cor_test$p.value, digits = 4), "\n")
if (cor_test$p.value < 0.05) {
    cat("*** SIGNIFICANT correlation\n\n")
} else {
    cat("Not significant\n\n")
}

# 8. Mixed effects model
cat("=== Mixed Effects Model ===\n")
cat("Model: acceptance ~ honorificness + (1|participant) + (1|item)\n\n")

data_with_honor$response_numeric <- as.numeric(data_with_honor$response_yes)
model <- glmer(
    response_numeric ~ honorificness_score +
        (1 | results_index) +
        (1 | item_id),
    data = data_with_honor,
    family = binomial,
    control = glmerControl(optimizer = "bobyqa")
)

print(summary(model))
cat("\n")

# 9. Median split analysis
cat("=== Median Split: Low vs High Honorificness ===\n")
median_honor <- median(item_summary$honorificness_score)
cat("Median honorificness:", median_honor, "\n\n")

split_summary <- item_summary %>%
    mutate(
        honor_group = ifelse(honorificness_score <= median_honor, "Low", "High")
    ) %>%
    group_by(honor_group) %>%
    summarize(
        n_items = n(),
        mean_acceptance = mean(acceptance_rate),
        sd_acceptance = sd(acceptance_rate),
        .groups = "drop"
    )

print(split_summary)

# T-test
low_accept <- item_summary %>%
    filter(honorificness_score <= median_honor) %>%
    pull(acceptance_rate)
high_accept <- item_summary %>%
    filter(honorificness_score > median_honor) %>%
    pull(acceptance_rate)
t_result <- t.test(low_accept, high_accept)
cat(
    "\nt-test: t =",
    round(t_result$statistic, 3),
    ", p =",
    format.pval(t_result$p.value, digits = 4),
    "\n\n"
)

# 10. Visualization (B&W)
p1 <- ggplot(item_summary, aes(x = honorificness_score, y = acceptance_rate)) +
    geom_point(size = 3, shape = 21, fill = "gray70", color = "black") +
    geom_smooth(
        method = "lm",
        color = "black",
        linewidth = 1,
        linetype = "solid"
    ) +
    theme_classic() +
    theme(text = element_text(size = 14)) +
    labs(
        title = "Honorificness × Acceptability (Cluster 1: Low Acceptors Only)",
        subtitle = paste0(
            "r = ",
            round(cor_test$estimate, 3),
            ", p = ",
            format.pval(cor_test$p.value, digits = 3)
        ),
        x = "Subject Honorificness Score (1-7)",
        y = "Acceptance Rate",
        caption = paste0(
            "N = ",
            nrow(item_summary),
            " items, ",
            n_distinct(data_with_honor$results_index),
            " participants"
        )
    ) +
    scale_x_continuous(breaks = 1:7) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))

print(p1)

ggsave(
    "figures/honorificness_cluster1_BW.png",
    p1,
    width = 10,
    height = 7,
    dpi = 300
)
ggsave("figures/honorificness_cluster1_BW.pdf", p1, width = 10, height = 7)

cat("=== Plot saved ===\n")
cat("honorificness_cluster1_BW.png/pdf\n")
