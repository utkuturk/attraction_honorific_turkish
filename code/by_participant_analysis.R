library(dplyr)
library(readxl)
library(janitor)
library(magrittr)
library(ggplot2)

# Load and prepare data (same as in analysis.R)
df <- read_excel("./ibex/results/results_2021-02-09T14_54_08_538Z_SA_AAF.xlsx")
colnames(df) %<>% make_clean_names()
df %<>% subset(logged_in_as_experiment_owner_if_known != "yes")

# Get practice data and filter bad subjects
practice <- df %>% subset(type == "practice" & !is.na(question_null_if_none))
practice %>% subset(whether_or_not_answer_was_correct_null_if_n_a == "NULL")
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

# Filter for FORMAL conditions with SINGULAR attractor (head) and PLURAL verb
formal_sg_pl <- df %>%
    filter(
        register == "formal",
        # att_n == "sg", # Singular head/attractor
        v_n == "pl" # Plural verb
    )

# Calculate by-participant means for this condition
participant_summary <- formal_sg_pl %>%
    group_by(results_index) %>%
    summarize(
        prop_yes = mean(response_yes),
        n_trials = n(),
        .groups = "drop"
    ) %>%
    arrange(prop_yes) %>%
    mutate(participant_ordered = factor(row_number()))

# Categorize participants into acceptance groups
participant_summary <- participant_summary %>%
    mutate(
        acceptance_group = case_when(
            prop_yes <= 0.33 ~ "Low Acceptance",
            prop_yes >= 0.67 ~ "High Acceptance",
            TRUE ~ "Medium Acceptance"
        ),
        acceptance_group = factor(
            acceptance_group,
            levels = c("Low Acceptance", "Medium Acceptance", "High Acceptance")
        )
    )

# Print summary statistics
cat(
    "=== By-Participant Analysis: Formal Register, Singular Head, Plural Verb ===\n\n"
)
cat("Number of participants:", nrow(participant_summary), "\n")
cat(
    "Mean proportion of 'yes' responses across participants:",
    round(mean(participant_summary$prop_yes), 3),
    "\n"
)
cat("SD:", round(sd(participant_summary$prop_yes), 3), "\n")
cat(
    "Range:",
    round(min(participant_summary$prop_yes), 3),
    "-",
    round(max(participant_summary$prop_yes), 3),
    "\n\n"
)

# Print acceptance group breakdown
cat("=== Participant Acceptance Groups ===\n")
cat("(Low: 0-33%, Medium: 34-66%, High: 67-100%)\n\n")
acceptance_counts <- participant_summary %>%
    group_by(acceptance_group) %>%
    summarize(
        n_participants = n(),
        pct_participants = round(n() / nrow(participant_summary) * 100, 1),
        mean_prop = round(mean(prop_yes), 3),
        sd_prop = round(sd(prop_yes), 3),
        .groups = "drop"
    )
print(acceptance_counts)
cat("\n")

# Create jitter plot showing each participant with clustering
p1 <- ggplot(
    participant_summary,
    aes(x = acceptance_group, y = prop_yes, color = acceptance_group)
) +
    geom_jitter(size = 3, alpha = 0.7, width = 0.2) +
    geom_violin(aes(fill = acceptance_group), alpha = 0.2, color = NA) +
    geom_boxplot(width = 0.3, alpha = 0, outlier.shape = NA) +
    geom_hline(
        yintercept = mean(participant_summary$prop_yes),
        linetype = "dashed",
        color = "black",
        linewidth = 1
    ) +
    geom_hline(
        yintercept = 0.33,
        linetype = "dotted",
        color = "gray50",
        linewidth = 0.5
    ) +
    geom_hline(
        yintercept = 0.67,
        linetype = "dotted",
        color = "gray50",
        linewidth = 0.5
    ) +
    theme_minimal() +
    theme(
        text = element_text(size = 14),
        legend.position = "none",
        panel.grid.major.x = element_blank()
    ) +
    labs(
        title = "By-Participant Acceptance Rates: Formal, Singular Head + Plural Verb",
        subtitle = paste0("N = ", nrow(participant_summary), " participants"),
        x = "Acceptance Group",
        y = "Proportion of 'Yes' Responses",
        caption = paste0(
            "Black dashed line = Overall mean (",
            round(mean(participant_summary$prop_yes), 2),
            "); Each point = one participant"
        )
    ) +
    scale_y_continuous(limits = c(-0.05, 1.05), breaks = seq(0, 1, 0.2)) +
    scale_color_manual(
        values = c(
            "Low Acceptance" = "#d73027",
            "Medium Acceptance" = "#fee08b",
            "High Acceptance" = "#1a9850"
        )
    ) +
    scale_fill_manual(
        values = c(
            "Low Acceptance" = "#d73027",
            "Medium Acceptance" = "#fee08b",
            "High Acceptance" = "#1a9850"
        )
    )

print(p1)

# Save plot
ggsave(
    "figures/by_participant_jitter.png",
    p1,
    width = 10,
    height = 7,
    dpi = 300
)
ggsave("figures/by_participant_jitter.pdf", p1, width = 10, height = 7)

# Alternative: Single-axis jitter plot showing all participants
p2 <- ggplot(
    participant_summary,
    aes(x = 0, y = prop_yes, color = acceptance_group)
) +
    geom_jitter(size = 3, alpha = 0.7, width = 0.15) +
    geom_hline(
        yintercept = mean(participant_summary$prop_yes),
        linetype = "dashed",
        color = "black",
        linewidth = 1
    ) +
    geom_hline(
        yintercept = 0.33,
        linetype = "dotted",
        color = "gray50",
        linewidth = 0.5
    ) +
    geom_hline(
        yintercept = 0.67,
        linetype = "dotted",
        color = "gray50",
        linewidth = 0.5
    ) +
    # Add density curve on the side
    stat_density(
        aes(y = prop_yes, x = after_stat(scaled) * 0.3 + 0.5),
        geom = "line",
        linewidth = 1.5,
        color = "gray30"
    ) +
    theme_minimal() +
    theme(
        text = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "right"
    ) +
    labs(
        title = "Distribution of Participant Acceptance Rates",
        subtitle = "Formal, Singular Head + Plural Verb",
        x = "",
        y = "Proportion of 'Yes' Responses",
        color = "Acceptance Group",
        caption = paste0(
            "Each point = one participant (N = ",
            nrow(participant_summary),
            "); Black line = overall mean"
        )
    ) +
    scale_y_continuous(limits = c(-0.05, 1.05), breaks = seq(0, 1, 0.2)) +
    scale_x_continuous(limits = c(-0.3, 0.8)) +
    scale_color_manual(
        values = c(
            "Low Acceptance" = "#d73027",
            "Medium Acceptance" = "#fee08b",
            "High Acceptance" = "#1a9850"
        )
    )

print(p2)

# Save plot 2
ggsave(
    "figures/by_participant_density_jitter.png",
    p2,
    width = 10,
    height = 7,
    dpi = 300
)
ggsave("figures/by_participant_density_jitter.pdf", p2, width = 10, height = 7)

cat("\n=== Plots saved to figures/ directory ===\n")
cat("- by_participant_jitter.png/pdf (grouped by acceptance level)\n")
cat(
    "- by_participant_density_jitter.png/pdf (all participants with density curve)\n\n"
)

# Print individual participant data
cat("\n=== Individual Participant Data ===\n")
print(
    participant_summary %>%
        select(results_index, acceptance_group, prop_yes, n_trials),
    n = Inf
)

# Optional: Show raw trial-level data for inspection
cat("\n=== Sample of Trial-Level Data ===\n")
print(head(formal_sg_pl %>% select(results_index, type, response_yes), 20))
