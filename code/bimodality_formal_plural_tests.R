library(dplyr)
library(readxl)
library(janitor)
library(magrittr)

ensure_package <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        stop(
            paste0(
                "Package '",
                pkg,
                "' is required but not installed. Install with install.packages('",
                pkg,
                "')."
            )
        )
    }
}

ensure_package("diptest")
ensure_package("multimode")

data_path <- "./ibex/results/results_2021-02-09T14_54_08_538Z_SA_AAF.xlsx"

df <- read_excel(data_path)
colnames(df) %<>% make_clean_names()
df %<>% subset(logged_in_as_experiment_owner_if_known != "yes")

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

df <- df %>% subset(!results_index %in% bad_subjects_by_practice)

df <- df %>%
    subset(grepl("filler_utku_", type)) %>%
    subset(!is.na(question_null_if_none))

df$response_yes <- ifelse(
    grepl("P'ye", df$answer),
    TRUE,
    ifelse(grepl("Q'ya", df$answer), FALSE, NA)
)

df <- df %>% subset(!is.na(response_yes))

df <- df %>%
    mutate(
        register = stringr::str_split_fixed(type, "_", 5)[, 3],
        att_n = stringr::str_sub(type, -4, -3),
        v_n = stringr::str_sub(type, -2, -1)
    )

# User request: plural verb + formal conditions, collapsing across attractor number
# (att_n intentionally NOT filtered)
formal_plural <- df %>%
    subset(register == "formal") %>%
    subset(v_n == "pl")

participant_acceptance <- formal_plural %>%
    group_by(results_index) %>%
    summarize(
        acceptance_rate = mean(as.numeric(response_yes)),
        n_trials = n(),
        .groups = "drop"
    )

x <- participant_acceptance$acceptance_rate

if (length(x) < 8) {
    stop("Not enough participant-level points for bimodality tests (need at least 8).")
}

dip_res <- diptest::dip.test(x)

# Silverman test via multimode::modetest(method='SI')
set.seed(123)
silverman_res <- multimode::modetest(
    x,
    mod0 = 1,
    method = "SI",
    B = 1000
)

# Excess mass style test via ACR method
set.seed(123)
excess_mass_res <- multimode::modetest(
    x,
    mod0 = 1,
    method = "ACR",
    B = 1000
)

# Bimodality coefficient (heuristic; not a formal hypothesis test)
mu <- mean(x)
sigma <- sd(x)
skewness <- mean((x - mu)^3) / (sigma^3)
kurtosis <- mean((x - mu)^4) / (sigma^4)
bimodality_coefficient <- (skewness^2 + 1) / kurtosis

results <- data.frame(
    subset = "formal_register_plural_verb_collapsed_attractor",
    n_participants = length(x),
    mean_acceptance = mean(x),
    sd_acceptance = sd(x),
    dip_statistic = as.numeric(dip_res$statistic),
    dip_p_value = as.numeric(dip_res$p.value),
    silverman_p_value = as.numeric(silverman_res$p.value),
    excess_mass_p_value = as.numeric(excess_mass_res$p.value),
    skewness = skewness,
    kurtosis = kurtosis,
    bimodality_coefficient = bimodality_coefficient
)

dir.create("results", showWarnings = FALSE)

write.csv(
    participant_acceptance,
    "results/formal_plural_participant_acceptance_rates.csv",
    row.names = FALSE
)

write.csv(
    results,
    "results/bimodality_tests_formal_plural.csv",
    row.names = FALSE
)

cat("=== Bimodality tests: formal + plural verb (collapsed across attractor) ===\n")
print(results)
cat("\nSaved:\n")
cat("- results/formal_plural_participant_acceptance_rates.csv\n")
cat("- results/bimodality_tests_formal_plural.csv\n")