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

# Filter for FORMAL conditions with PLURAL verb (BOTH singular and plural attractors)
formal_pl_verb <- df %>%
    filter(
        register == "formal",
        v_n == "pl" # Plural verb - includes BOTH sg and pl attractors
    )

cat("\n=== Analysis: Formal register, Plural verb, ALL attractors ===\n\n")

# Calculate by-participant means and item variability
participant_analysis <- formal_pl_verb %>%
    group_by(results_index, group, att_n) %>%
    summarize(
        prop_yes = mean(response_yes),
        .groups = "drop"
    ) %>%
    group_by(results_index) %>%
    summarize(
        mean_acceptance = mean(prop_yes),
        sd_across_items = sd(prop_yes),
        n_items = n(),
        .groups = "drop"
    )

cat("Total participants:", nrow(participant_analysis), "\n")
cat(
    "Mean acceptance:",
    round(mean(participant_analysis$mean_acceptance), 3),
    "\n"
)
cat(
    "Mean SD across items:",
    round(mean(participant_analysis$sd_across_items, na.rm = TRUE), 3),
    "\n\n"
)

# Create the key plot: Mean acceptance vs Item variability
# This shows if being "in the middle" correlates with higher item variability

p1 <- ggplot(
    participant_analysis,
    aes(x = mean_acceptance, y = sd_across_items)
) +
    geom_point(
        size = 2.5,
        alpha = 0.6,
        shape = 21,
        fill = "gray50",
        color = "black"
    ) +
    geom_smooth(
        method = "loess",
        color = "black",
        linewidth = 1.2,
        linetype = "solid"
    ) +
    geom_vline(
        xintercept = c(0.33, 0.67),
        linetype = "dotted",
        color = "gray40",
        linewidth = 0.8
    ) +
    theme_classic() +
    theme(
        text = element_text(size = 14),
        panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
        legend.position = "none"
    ) +
    labs(
        title = "Item Variability by Mean Acceptance Rate",
        subtitle = "Formal register, plural verb, all attractors",
        x = "Mean Acceptance Rate",
        y = "SD Across Items (Item Variability)",
        caption = "Dotted lines mark low/medium/high acceptance boundaries (0.33, 0.67)"
    ) +
    scale_x_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
    scale_y_continuous(breaks = seq(0, 0.6, 0.1))

print(p1)

ggsave(
    "figures/mean_acceptance_vs_item_variability_BW.png",
    p1,
    width = 10,
    height = 7,
    dpi = 300
)
ggsave(
    "figures/mean_acceptance_vs_item_variability_BW.pdf",
    p1,
    width = 10,
    height = 7
)

# Create binned analysis to show clearer pattern
participant_analysis_binned <- participant_analysis %>%
    mutate(
        acceptance_bin = cut(
            mean_acceptance,
            breaks = seq(0, 1, 0.1),
            labels = c(
                "0-10%",
                "10-20%",
                "20-30%",
                "30-40%",
                "40-50%",
                "50-60%",
                "60-70%",
                "70-80%",
                "80-90%",
                "90-100%"
            ),
            include.lowest = TRUE
        )
    ) %>%
    filter(!is.na(acceptance_bin))

# Aggregate by bin
binned_summary <- participant_analysis_binned %>%
    group_by(acceptance_bin) %>%
    summarize(
        mean_sd = mean(sd_across_items, na.rm = TRUE),
        se_sd = sd(sd_across_items, na.rm = TRUE) / sqrt(n()),
        n = n(),
        .groups = "drop"
    ) %>%
    filter(n >= 3) # Only show bins with at least 3 participants

p2 <- ggplot(binned_summary, aes(x = acceptance_bin, y = mean_sd)) +
    geom_point(size = 4, shape = 18) +
    geom_errorbar(
        aes(ymin = mean_sd - se_sd, ymax = mean_sd + se_sd),
        width = 0.3,
        linewidth = 0.8
    ) +
    geom_hline(
        yintercept = 0.4,
        linetype = "dashed",
        color = "gray40",
        linewidth = 0.8
    ) +
    theme_classic() +
    theme(
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3)
    ) +
    labs(
        title = "Item Variability Peaks in Middle Acceptance Range",
        subtitle = "Mean SD across items by acceptance rate bins",
        x = "Acceptance Rate Bin",
        y = "Mean SD Across Items",
        caption = "Error bars = ±SE; Dashed line = high variability threshold (0.4)"
    )

print(p2)

ggsave(
    "figures/binned_item_variability_BW.png",
    p2,
    width = 10,
    height = 7,
    dpi = 300
)
ggsave("figures/binned_item_variability_BW.pdf", p2, width = 10, height = 7)

# Recreate by-participant jitter plot in B&W
participant_summary <- participant_analysis %>%
    mutate(
        acceptance_group = case_when(
            mean_acceptance <= 0.33 ~ "Low",
            mean_acceptance >= 0.67 ~ "High",
            TRUE ~ "Medium"
        ),
        acceptance_group = factor(
            acceptance_group,
            levels = c("Low", "Medium", "High")
        )
    )

p3 <- ggplot(
    participant_summary,
    aes(x = acceptance_group, y = mean_acceptance, shape = acceptance_group)
) +
    geom_jitter(size = 2.5, alpha = 0.6, width = 0.2) +
    geom_violin(
        alpha = 0.1,
        color = "black",
        fill = "gray80",
        linewidth = 0.8
    ) +
    geom_boxplot(width = 0.3, alpha = 0, outlier.shape = NA, linewidth = 0.8) +
    geom_hline(
        yintercept = mean(participant_summary$mean_acceptance),
        linetype = "dashed",
        color = "black",
        linewidth = 1
    ) +
    geom_hline(
        yintercept = c(0.33, 0.67),
        linetype = "dotted",
        color = "gray50",
        linewidth = 0.5
    ) +
    theme_classic() +
    theme(
        text = element_text(size = 14),
        legend.position = "none",
        panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3)
    ) +
    labs(
        title = "By-Participant Acceptance Rates: Formal, Plural Verb, All Attractors",
        subtitle = paste0("N = ", nrow(participant_summary), " participants"),
        x = "Acceptance Group",
        y = "Mean Acceptance Rate",
        caption = paste0(
            "Black dashed line = Overall mean (",
            round(mean(participant_summary$mean_acceptance), 2),
            ")"
        )
    ) +
    scale_y_continuous(limits = c(-0.05, 1.05), breaks = seq(0, 1, 0.2)) +
    scale_shape_manual(values = c("Low" = 1, "Medium" = 2, "High" = 0)) # Circle, Triangle, Square

print(p3)

ggsave(
    "figures/by_participant_jitter_BW.png",
    p3,
    width = 10,
    height = 7,
    dpi = 300
)
ggsave("figures/by_participant_jitter_BW.pdf", p3, width = 10, height = 7)

# Print summary statistics
cat("\n=== Summary by Acceptance Group ===\n")
group_summary <- participant_summary %>%
    group_by(acceptance_group) %>%
    summarize(
        n = n(),
        mean_sd = mean(sd_across_items, na.rm = TRUE),
        median_sd = median(sd_across_items, na.rm = TRUE),
        .groups = "drop"
    )
print(group_summary)

cat("\n=== Plots saved (Black & White) ===\n")
cat("1. mean_acceptance_vs_item_variability_BW.png/pdf\n")
cat(
    "   - Scatterplot showing relationship between mean acceptance and item variability\n"
)
cat("2. binned_item_variability_BW.png/pdf\n")
cat("   - Binned analysis showing variability peaks in middle range\n")
cat("3. by_participant_jitter_BW.png/pdf\n")
cat("   - By-participant acceptance rates with shape coding\n\n")
