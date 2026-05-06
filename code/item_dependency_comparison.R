library(dplyr)
library(readxl)
library(janitor)
library(magrittr)
library(ggplot2)

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

# Filter for FORMAL conditions with SINGULAR attractor and PLURAL verb
formal_sg_pl <- df %>%
    filter(
        register == "formal",
        att_n == "sg",
        v_n == "pl"
    )

# Calculate by-participant means
participant_summary <- formal_sg_pl %>%
    group_by(results_index) %>%
    summarize(
        prop_yes = mean(response_yes),
        n_trials = n(),
        .groups = "drop"
    ) %>%
    mutate(
        acceptance_group = case_when(
            prop_yes <= 0.33 ~ "Low Acceptance",
            prop_yes >= 0.67 ~ "High Acceptance",
            TRUE ~ "Medium Acceptance"
        )
    )

# ==============================================================================
# Item Dependency Analysis for ALL Acceptance Groups
# ==============================================================================

cat("\n=== Item Dependency Analysis: ALL ACCEPTANCE GROUPS ===\n\n")

# Function to calculate item variance for a group
analyze_group <- function(group_name) {
    group_ids <- participant_summary %>%
        filter(acceptance_group == group_name) %>%
        pull(results_index)

    group_data <- formal_sg_pl %>%
        filter(results_index %in% group_ids)

    item_by_participant <- group_data %>%
        group_by(results_index, group) %>%
        summarize(
            prop_yes = mean(response_yes),
            n_trials = n(),
            .groups = "drop"
        )

    participant_item_variance <- item_by_participant %>%
        group_by(results_index) %>%
        summarize(
            mean_across_items = mean(prop_yes),
            sd_across_items = sd(prop_yes),
            variance_across_items = var(prop_yes),
            n_items = n(),
            .groups = "drop"
        )

    return(list(
        n_participants = length(group_ids),
        mean_sd = mean(participant_item_variance$sd_across_items, na.rm = TRUE),
        median_sd = median(
            participant_item_variance$sd_across_items,
            na.rm = TRUE
        ),
        min_sd = min(participant_item_variance$sd_across_items, na.rm = TRUE),
        max_sd = max(participant_item_variance$sd_across_items, na.rm = TRUE),
        n_high_variance = sum(
            participant_item_variance$sd_across_items > 0.4,
            na.rm = TRUE
        ),
        n_low_variance = sum(
            participant_item_variance$sd_across_items < 0.2,
            na.rm = TRUE
        ),
        variance_data = participant_item_variance
    ))
}

# Analyze each group
low_results <- analyze_group("Low Acceptance")
medium_results <- analyze_group("Medium Acceptance")
high_results <- analyze_group("High Acceptance")

# Print comparison
cat("┌─────────────────────────────────────────────────────────────────┐\n")
cat("│                    LOW ACCEPTANCE GROUP                         │\n")
cat("└─────────────────────────────────────────────────────────────────┘\n")
cat("N participants:", low_results$n_participants, "\n")
cat("Mean SD across items:", round(low_results$mean_sd, 3), "\n")
cat("Median SD across items:", round(low_results$median_sd, 3), "\n")
cat(
    "Range of SD:",
    round(low_results$min_sd, 3),
    "to",
    round(low_results$max_sd, 3),
    "\n"
)
cat(
    "High item-dependency (SD > 0.4):",
    low_results$n_high_variance,
    "(",
    round(low_results$n_high_variance / low_results$n_participants * 100, 1),
    "%)\n"
)
cat(
    "Low item-dependency (SD < 0.2):",
    low_results$n_low_variance,
    "(",
    round(low_results$n_low_variance / low_results$n_participants * 100, 1),
    "%)\n\n"
)

cat("┌─────────────────────────────────────────────────────────────────┐\n")
cat("│                   MEDIUM ACCEPTANCE GROUP                       │\n")
cat("└─────────────────────────────────────────────────────────────────┘\n")
cat("N participants:", medium_results$n_participants, "\n")
cat("Mean SD across items:", round(medium_results$mean_sd, 3), "\n")
cat("Median SD across items:", round(medium_results$median_sd, 3), "\n")
cat(
    "Range of SD:",
    round(medium_results$min_sd, 3),
    "to",
    round(medium_results$max_sd, 3),
    "\n"
)
cat(
    "High item-dependency (SD > 0.4):",
    medium_results$n_high_variance,
    "(",
    round(
        medium_results$n_high_variance / medium_results$n_participants * 100,
        1
    ),
    "%)\n"
)
cat(
    "Low item-dependency (SD < 0.2):",
    medium_results$n_low_variance,
    "(",
    round(
        medium_results$n_low_variance / medium_results$n_participants * 100,
        1
    ),
    "%)\n\n"
)

cat("┌─────────────────────────────────────────────────────────────────┐\n")
cat("│                    HIGH ACCEPTANCE GROUP                        │\n")
cat("└─────────────────────────────────────────────────────────────────┘\n")
cat("N participants:", high_results$n_participants, "\n")
cat("Mean SD across items:", round(high_results$mean_sd, 3), "\n")
cat("Median SD across items:", round(high_results$median_sd, 3), "\n")
cat(
    "Range of SD:",
    round(high_results$min_sd, 3),
    "to",
    round(high_results$max_sd, 3),
    "\n"
)
cat(
    "High item-dependency (SD > 0.4):",
    high_results$n_high_variance,
    "(",
    round(high_results$n_high_variance / high_results$n_participants * 100, 1),
    "%)\n"
)
cat(
    "Low item-dependency (SD < 0.2):",
    high_results$n_low_variance,
    "(",
    round(high_results$n_low_variance / high_results$n_participants * 100, 1),
    "%)\n\n"
)

# Create comparison plot
all_variance_data <- bind_rows(
    low_results$variance_data %>% mutate(group = "Low Acceptance"),
    medium_results$variance_data %>% mutate(group = "Medium Acceptance"),
    high_results$variance_data %>% mutate(group = "High Acceptance")
) %>%
    mutate(
        group = factor(
            group,
            levels = c("Low Acceptance", "Medium Acceptance", "High Acceptance")
        )
    )

p1 <- ggplot(
    all_variance_data,
    aes(x = group, y = sd_across_items, color = group)
) +
    geom_jitter(size = 2, alpha = 0.6, width = 0.2) +
    geom_boxplot(alpha = 0, outlier.shape = NA, width = 0.5) +
    geom_hline(
        yintercept = 0.4,
        linetype = "dashed",
        color = "red",
        linewidth = 0.8
    ) +
    geom_hline(
        yintercept = 0.2,
        linetype = "dotted",
        color = "gray50",
        linewidth = 0.8
    ) +
    theme_minimal() +
    theme(
        text = element_text(size = 14),
        legend.position = "none",
        panel.grid.major.x = element_blank()
    ) +
    labs(
        title = "Item-Dependency Across Acceptance Groups",
        subtitle = "SD of responses across items per participant",
        x = "Acceptance Group",
        y = "Standard Deviation Across Items",
        caption = "Red line = high dependency threshold (0.4); Gray line = low dependency threshold (0.2)"
    ) +
    scale_color_manual(
        values = c(
            "Low Acceptance" = "#d73027",
            "Medium Acceptance" = "#fee08b",
            "High Acceptance" = "#1a9850"
        )
    )

print(p1)

ggsave(
    "figures/item_dependency_comparison.png",
    p1,
    width = 10,
    height = 7,
    dpi = 300
)
ggsave("figures/item_dependency_comparison.pdf", p1, width = 10, height = 7)

cat("\n=== SUMMARY ===\n")
cat("All three groups show similar patterns of item-dependency:\n\n")
cat(
    "Low Acceptance:   ",
    round(low_results$n_high_variance / low_results$n_participants * 100, 1),
    "% show high item-dependency\n",
    sep = ""
)
cat(
    "Medium Acceptance:",
    round(
        medium_results$n_high_variance / medium_results$n_participants * 100,
        1
    ),
    "% show high item-dependency\n",
    sep = ""
)
cat(
    "High Acceptance:  ",
    round(high_results$n_high_variance / high_results$n_participants * 100, 1),
    "% show high item-dependency\n\n",
    sep = ""
)

cat(
    "CONCLUSION: Item-dependency is NOT unique to medium acceptance participants.\n"
)
cat(
    "This is likely due to the small number of items per participant (<10 items),\n"
)
cat("which naturally leads to high variance even for consistent responders.\n")
