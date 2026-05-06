library(dplyr)
library(readxl)
library(janitor)
library(magrittr)
library(ggplot2)
library(tidyr)

# Load and prepare data
df <- read_excel("./ibex/results/results_2021-02-09T14_54_08_538Z_SA_AAF.xlsx")
colnames(df) %<>% make_clean_names()
df %<>% subset(logged_in_as_experiment_owner_if_known != "yes")

# Get practice data and filter bad subjects
practice <- df %>% subset(type == "practice" & !is.na(question_null_if_none))
practice$whether_or_not_answer_was_correct_null_if_n_a[
    practice$whether_or_not_answer_was_correct_null_if_n_a == "NULL"
] <- 0
practice$whether_or_not_answer_was_correct_null_if_n_a %<>% as.integer()

bad_subjects_by_practice <- practice %>%
    group_by(results_index) %>%
    summarize(p_yes = mean(whether_or_not_answer_was_correct_null_if_n_a)) %>%
    subset(p_yes <= 0.5) %>%
    .$results_index

# Filter out bad subjects
df <- df %>% subset(!results_index %in% bad_subjects_by_practice)

# Only selects the types that start with "filler_utku_"
df <- df %>%
    subset(grepl("filler_utku_", type)) %>%
    subset(!is.na(question_null_if_none))

df %<>%
    select(
        -time,
        -counter,
        -hash,
        -logged_in_as_experiment_owner_if_known,
        -element_number,
        -field_name,
        -field_value,
        -sentence_or_sentence_md5,
        -question_null_if_none,
        -whether_or_not_answer_was_correct_null_if_n_a
    )

df$response_yes <- ifelse(
    grepl("P'ye", df$answer),
    T,
    ifelse(grepl("Q'ya", df$answer), F, NA)
)

df %<>% select(-answer)
df %<>% subset(!is.na(response_yes))

# Parse condition information
df <- df %>%
    mutate(
        register = stringr::str_split_fixed(type, "_", 5)[, 3],
        att_n = stringr::str_sub(type, -4, -3),
        v_n = stringr::str_sub(type, -2, -1)
    )

# Filter for FORMAL conditions with PLURAL verb
formal_pl_verb <- df %>%
    filter(
        register == "formal",
        v_n == "pl"
    )

cat("\n=== Item × Participant Interaction Analysis ===\n")
cat("Formal register, plural verb acceptability\n\n")

# ==============================================================================
# 1. ITEM DIFFICULTY ANALYSIS
# ==============================================================================

cat("=== 1. Item Difficulty (By-Item Acceptance Rates) ===\n\n")

item_difficulty <- formal_pl_verb %>%
    group_by(group, att_n) %>%
    summarize(
        n_responses = n(),
        acceptance_rate = mean(response_yes),
        sd = sd(response_yes),
        .groups = "drop"
    ) %>%
    arrange(acceptance_rate)

cat("Items ordered by acceptance rate:\n")
print(item_difficulty, n = Inf)
cat("\n")

# Categorize items by difficulty
item_difficulty <- item_difficulty %>%
    mutate(
        difficulty_category = case_when(
            acceptance_rate < 0.2 ~ "Very Low",
            acceptance_rate < 0.4 ~ "Low",
            acceptance_rate < 0.6 ~ "Medium",
            acceptance_rate < 0.8 ~ "High",
            TRUE ~ "Very High"
        ),
        difficulty_category = factor(
            difficulty_category,
            levels = c("Very Low", "Low", "Medium", "High", "Very High")
        )
    )

cat("Item difficulty distribution:\n")
print(table(item_difficulty$difficulty_category))
cat("\n")

# Plot item difficulty (B&W)
p1 <- ggplot(
    item_difficulty,
    aes(x = reorder(group, acceptance_rate), y = acceptance_rate)
) +
    geom_point(size = 3, shape = 21, fill = "white", color = "black") +
    geom_errorbar(
        aes(
            ymin = acceptance_rate - sd / sqrt(n_responses),
            ymax = acceptance_rate + sd / sqrt(n_responses)
        ),
        width = 0.3
    ) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray40") +
    theme_classic() +
    theme(
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8)
    ) +
    labs(
        title = "Item Difficulty: Acceptance Rate by Item",
        subtitle = "Formal register, plural verb",
        x = "Item (Group)",
        y = "Acceptance Rate",
        caption = "Error bars = ±SE; Dashed line = 50%"
    )

print(p1)

ggsave("figures/item_difficulty_BW.png", p1, width = 12, height = 6, dpi = 300)
ggsave("figures/item_difficulty_BW.pdf", p1, width = 12, height = 6)

# ==============================================================================
# 2. ITEM DISCRIMINATION (Between-Participant Variance)
# ==============================================================================

cat("=== 2. Item Discrimination (Between-Participant Variance) ===\n\n")

cat("High discrimination = high variance = participants disagree\n")
cat("Low discrimination = low variance = participants agree\n\n")

item_discrimination <- item_difficulty %>%
    mutate(
        discrimination = sd,
        discrimination_category = case_when(
            sd > 0.45 ~ "High",
            sd > 0.35 ~ "Medium",
            TRUE ~ "Low"
        )
    )

cat("Item discrimination summary:\n")
print(
    item_discrimination %>%
        select(
            group,
            att_n,
            acceptance_rate,
            discrimination,
            discrimination_category
        ) %>%
        arrange(desc(discrimination)),
    n = Inf
)
cat("\n")

# Plot: Difficulty vs Discrimination
p2 <- ggplot(
    item_discrimination,
    aes(x = acceptance_rate, y = discrimination)
) +
    geom_point(size = 4, shape = 21, fill = "gray70", color = "black") +
    geom_text(aes(label = group), hjust = -0.2, vjust = 0, size = 2.5) +
    theme_classic() +
    theme(text = element_text(size = 14)) +
    labs(
        title = "Item Difficulty vs Discrimination",
        subtitle = "Do controversial items (high SD) fall in middle difficulty range?",
        x = "Acceptance Rate (Difficulty)",
        y = "SD (Discrimination)",
        caption = "Higher SD = more disagreement among participants"
    )

print(p2)

ggsave(
    "figures/item_difficulty_vs_discrimination_BW.png",
    p2,
    width = 10,
    height = 7,
    dpi = 300
)
ggsave(
    "figures/item_difficulty_vs_discrimination_BW.pdf",
    p2,
    width = 10,
    height = 7
)

# ==============================================================================
# 3. PARTICIPANT × ITEM HEATMAP
# ==============================================================================

cat("=== 3. Participant × Item Response Matrix ===\n\n")

# Create response matrix
response_matrix <- formal_pl_verb %>%
    mutate(item_id = paste(group, att_n, sep = "_")) %>%
    group_by(results_index, item_id) %>%
    summarize(response = mean(response_yes), .groups = "drop")

# Sort participants by mean acceptance
participant_order <- response_matrix %>%
    group_by(results_index) %>%
    summarize(mean_acceptance = mean(response), .groups = "drop") %>%
    arrange(mean_acceptance) %>%
    pull(results_index)

# Sort items by acceptance rate
item_order <- item_difficulty %>%
    mutate(item_id = paste(group, att_n, sep = "_")) %>%
    arrange(acceptance_rate) %>%
    pull(item_id)

# Create heatmap with ordered rows/columns
response_matrix_ordered <- response_matrix %>%
    mutate(
        results_index = factor(results_index, levels = participant_order),
        item_id = factor(item_id, levels = item_order)
    )

# Sample for visualization if too large (take every nth participant)
n_participants <- length(unique(response_matrix_ordered$results_index))
if (n_participants > 100) {
    sample_ids <- participant_order[seq(
        1,
        length(participant_order),
        length.out = 100
    )]
    response_matrix_plot <- response_matrix_ordered %>%
        filter(results_index %in% sample_ids)
    cat("Sampling 100 participants for visualization\n\n")
} else {
    response_matrix_plot <- response_matrix_ordered
}

p3 <- ggplot(
    response_matrix_plot,
    aes(x = item_id, y = results_index, fill = response)
) +
    geom_tile(color = "white", linewidth = 0.1) +
    scale_fill_gradient(low = "white", high = "black", name = "Acceptance") +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        text = element_text(size = 12)
    ) +
    labs(
        title = "Participant × Item Response Matrix",
        subtitle = "Rows = participants (ordered by mean), Columns = items (ordered by difficulty)",
        x = "Item (Group_Attractor)",
        y = "Participant",
        caption = "Black = Accept; White = Reject"
    )

print(p3)

ggsave(
    "figures/participant_item_heatmap_BW.png",
    p3,
    width = 12,
    height = 10,
    dpi = 300
)
ggsave("figures/participant_item_heatmap_BW.pdf", p3, width = 12, height = 10)

# ==============================================================================
# 4. SUMMARY STATISTICS
# ==============================================================================

cat("=== Summary Statistics ===\n\n")

cat("ITEM-LEVEL:\n")
cat("- Total items:", nrow(item_difficulty), "\n")
cat(
    "- Mean acceptance across items:",
    round(mean(item_difficulty$acceptance_rate), 3),
    "\n"
)
cat(
    "- Range:",
    round(min(item_difficulty$acceptance_rate), 3),
    "to",
    round(max(item_difficulty$acceptance_rate), 3),
    "\n"
)
cat(
    "- High discrimination items (SD > 0.45):",
    sum(item_difficulty$sd > 0.45),
    "\n\n"
)

cat("PARTICIPANT-LEVEL:\n")
participant_means <- response_matrix %>%
    group_by(results_index) %>%
    summarize(mean_acceptance = mean(response), .groups = "drop")
cat("- Total participants:", nrow(participant_means), "\n")
cat(
    "- Mean acceptance across participants:",
    round(mean(participant_means$mean_acceptance), 3),
    "\n"
)
cat(
    "- Range:",
    round(min(participant_means$mean_acceptance), 3),
    "to",
    round(max(participant_means$mean_acceptance), 3),
    "\n\n"
)

cat("=== Analysis Complete ===\n")
cat("Generated plots:\n")
cat("1. item_difficulty_BW.png/pdf - Acceptance rate by item\n")
cat(
    "2. item_difficulty_vs_discrimination_BW.png/pdf - Difficulty vs variance\n"
)
cat("3. participant_item_heatmap_BW.png/pdf - Full response matrix\n\n")

# Save item characteristics
write.csv(item_discrimination, "item_characteristics.csv", row.names = FALSE)
cat("Item characteristics saved to: item_characteristics.csv\n")
