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
# Item Dependency Analysis for Medium Acceptance Participants
# ==============================================================================

cat("\n=== Item Dependency Analysis: Medium Acceptance Participants ===\n\n")

# Get medium acceptance participants
medium_acceptance_ids <- participant_summary %>%
    filter(acceptance_group == "Medium Acceptance") %>%
    pull(results_index)

# Filter data for medium acceptance participants only
medium_data <- formal_sg_pl %>%
    filter(results_index %in% medium_acceptance_ids)

# Calculate by-participant, by-item acceptance rates
item_by_participant <- medium_data %>%
    group_by(results_index, group) %>% # 'group' represents item
    summarize(
        prop_yes = mean(response_yes),
        n_trials = n(),
        .groups = "drop"
    )

# Calculate variance in responses across items for each participant
participant_item_variance <- item_by_participant %>%
    group_by(results_index) %>%
    summarize(
        mean_across_items = mean(prop_yes),
        sd_across_items = sd(prop_yes),
        variance_across_items = var(prop_yes),
        n_items = n(),
        .groups = "drop"
    ) %>%
    arrange(desc(variance_across_items))

cat(
    "Medium acceptance participants (N =",
    length(medium_acceptance_ids),
    ")\n\n"
)
cat("Variance in acceptance across items:\n")
cat(
    "Mean SD across items:",
    round(mean(participant_item_variance$sd_across_items, na.rm = TRUE), 3),
    "\n"
)
cat(
    "Median SD across items:",
    round(median(participant_item_variance$sd_across_items, na.rm = TRUE), 3),
    "\n"
)
cat(
    "Range of SD:",
    round(min(participant_item_variance$sd_across_items, na.rm = TRUE), 3),
    "to",
    round(max(participant_item_variance$sd_across_items, na.rm = TRUE), 3),
    "\n\n"
)

# Participants with high item-dependency (high variance across items)
cat("Participants with HIGH item-dependency (SD > 0.4):\n")
high_variance_participants <- participant_item_variance %>%
    filter(sd_across_items > 0.4, !is.na(sd_across_items))
cat("N =", nrow(high_variance_participants), "\n")
print(high_variance_participants)
cat("\n")

# Participants with low item-dependency (consistent across items)
cat("Participants with LOW item-dependency (SD < 0.2):\n")
low_variance_participants <- participant_item_variance %>%
    filter(sd_across_items < 0.2, !is.na(sd_across_items))
cat("N =", nrow(low_variance_participants), "\n")
print(low_variance_participants)
cat("\n")

# Calculate by-item consistency
item_consistency <- medium_data %>%
    group_by(group) %>%
    summarize(
        item_mean = mean(response_yes),
        item_sd = sd(response_yes),
        n_responses = n(),
        .groups = "drop"
    ) %>%
    arrange(desc(item_sd))

cat("By-Item consistency for medium acceptance participants:\n")
cat("Items with highest variability (most dependent on participant):\n")
print(item_consistency)
cat("\n")

# Create heatmap showing participant x item responses
if (nrow(item_by_participant) > 0) {
    # Reshape for heatmap
    heatmap_data <- item_by_participant %>%
        mutate(
            participant_id = as.factor(results_index),
            item_id = as.factor(group)
        )

    p1 <- ggplot(
        heatmap_data,
        aes(x = item_id, y = participant_id, fill = prop_yes)
    ) +
        geom_tile(color = "white", linewidth = 0.5) +
        scale_fill_gradient2(
            low = "#d73027",
            mid = "#fee08b",
            high = "#1a9850",
            midpoint = 0.5,
            limits = c(0, 1),
            name = "Prop. Yes"
        ) +
        theme_minimal() +
        theme(
            text = element_text(size = 12),
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(size = 8)
        ) +
        labs(
            title = "Item-Dependency Heatmap: Medium Acceptance Participants",
            subtitle = "Each cell = one participant's mean response to one item",
            x = "Item (Group)",
            y = "Participant ID"
        )

    print(p1)

    ggsave(
        "figures/medium_acceptance_item_dependency.png",
        p1,
        width = 12,
        height = 10,
        dpi = 300
    )
    ggsave(
        "figures/medium_acceptance_item_dependency.pdf",
        p1,
        width = 12,
        height = 10
    )

    cat("\n=== Item dependency heatmap saved ===\n")
    cat("- medium_acceptance_item_dependency.png/pdf\n\n")
}

# Summary interpretation
cat("\n=== INTERPRETATION ===\n")
cat("If SD across items is HIGH → participant's responses are ITEM-DEPENDENT\n")
cat("If SD across items is LOW → participant is CONSISTENT across items\n\n")

cat("Results:\n")
cat(
    "- ",
    nrow(high_variance_participants),
    " participants (",
    round(
        nrow(high_variance_participants) /
            nrow(participant_item_variance) *
            100,
        1
    ),
    "%) show HIGH item-dependency (SD > 0.4)\n",
    sep = ""
)
cat(
    "- ",
    nrow(low_variance_participants),
    " participants (",
    round(
        nrow(low_variance_participants) / nrow(participant_item_variance) * 100,
        1
    ),
    "%) show LOW item-dependency (SD < 0.2)\n",
    sep = ""
)
cat(
    "\nThis suggests that medium acceptance participants are ",
    ifelse(
        nrow(high_variance_participants) > nrow(participant_item_variance) / 2,
        "MOSTLY item-dependent",
        "MIXED (some item-dependent, some consistent)"
    ),
    "\n"
)
