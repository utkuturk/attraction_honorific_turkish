library(dplyr)
library(readxl)
library(janitor)
library(magrittr)
library(ggplot2)
library(lme4)

# Honorificness analysis for ALL PARTICIPANTS (not just Cluster 1)

cat("=== Honorificness Analysis: ALL PARTICIPANTS ===\n\n")

# Load honorificness scores
honorificness <- read.csv("honorificness_scores.csv", fileEncoding = "UTF-8")
cat("Loaded honorificness scores for", nrow(honorificness), "items\n")

# Load participant data
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

cat("Total participants:", n_distinct(df$results_index), "\n\n")

# Prepare response data (ALL PARTICIPANTS)
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
        !is.na(response_yes)
    ) %>%
    mutate(item_id = paste(group, att_n, sep = "_"))

# Merge with honorificness scores
data_with_honor <- formal_data %>%
    left_join(
        honorificness %>% select(item_id, honorificness_score),
        by = "item_id"
    ) %>%
    filter(!is.na(honorificness_score))

cat("=== Data Summary ===\n")
cat("Total responses:", nrow(data_with_honor), "\n")
cat("Unique items:", n_distinct(data_with_honor$item_id), "\n")
cat("Unique participants:", n_distinct(data_with_honor$results_index), "\n\n")

# Item-level analysis
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

# Correlation
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

# Mixed effects model
cat("=== Mixed Effects Model (All Participants) ===\n")
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

# By honorificness level
cat("=== Acceptance Rate by Honorificness Level ===\n")
by_honor <- item_summary %>%
    group_by(honorificness_score) %>%
    summarize(
        n_items = n(),
        mean_acceptance = mean(acceptance_rate),
        sd_acceptance = sd(acceptance_rate),
        .groups = "drop"
    )
print(by_honor)
cat("\n")

# Visualization (B&W)
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
        title = "Subject Honorificness × Acceptability (ALL Participants)",
        subtitle = paste0(
            "r = ",
            round(cor_test$estimate, 3),
            ", p = ",
            format.pval(cor_test$p.value, digits = 3)
        ),
        x = "Subject Honorificness Score (1-5)",
        y = "Acceptance Rate",
        caption = paste0(
            "N = ",
            nrow(item_summary),
            " items, ",
            n_distinct(data_with_honor$results_index),
            " participants"
        )
    ) +
    scale_x_continuous(breaks = 1:5) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))

print(p1)

ggsave(
    "figures/honorificness_all_participants_BW.png",
    p1,
    width = 10,
    height = 7,
    dpi = 300
)
ggsave(
    "figures/honorificness_all_participants_BW.pdf",
    p1,
    width = 10,
    height = 7
)

# By-cluster comparison (if clusters available)
if (file.exists("cluster_assignments.csv")) {
    clusters <- read.csv("cluster_assignments.csv")

    data_with_cluster <- data_with_honor %>%
        left_join(
            clusters %>% select(results_index, cluster),
            by = "results_index"
        )

    cat("=== By-Cluster Analysis ===\n")

    cluster_item_summary <- data_with_cluster %>%
        filter(!is.na(cluster)) %>%
        group_by(cluster, item_id, honorificness_score) %>%
        summarize(acceptance_rate = mean(response_yes), .groups = "drop")

    # Correlation by cluster
    for (c in sort(unique(cluster_item_summary$cluster))) {
        cluster_data <- cluster_item_summary %>% filter(cluster == c)
        cluster_cor <- cor.test(
            cluster_data$honorificness_score,
            cluster_data$acceptance_rate
        )
        cat(
            "Cluster",
            c,
            ": r =",
            round(cluster_cor$estimate, 3),
            ", p =",
            format.pval(cluster_cor$p.value, digits = 4),
            "\n"
        )
    }

    # Faceted plot by cluster
    p2 <- ggplot(
        cluster_item_summary,
        aes(x = honorificness_score, y = acceptance_rate)
    ) +
        geom_point(size = 2, shape = 21, fill = "gray70", color = "black") +
        geom_smooth(method = "lm", color = "black", linewidth = 1) +
        facet_wrap(~ paste("Cluster", cluster), ncol = 2) +
        theme_classic() +
        theme(text = element_text(size = 12)) +
        labs(
            title = "Honorificness × Acceptability by Cluster",
            x = "Subject Honorificness Score (1-5)",
            y = "Acceptance Rate"
        ) +
        scale_x_continuous(breaks = 1:5) +
        scale_y_continuous(limits = c(0, 1))

    print(p2)

    ggsave(
        "figures/honorificness_by_cluster_BW.png",
        p2,
        width = 10,
        height = 5,
        dpi = 300
    )
    ggsave(
        "figures/honorificness_by_cluster_BW.pdf",
        p2,
        width = 10,
        height = 5
    )
}

cat("\n=== Plots saved ===\n")
cat("- honorificness_all_participants_BW.png/pdf\n")
if (file.exists("cluster_assignments.csv")) {
    cat("- honorificness_by_cluster_BW.png/pdf\n")
}
