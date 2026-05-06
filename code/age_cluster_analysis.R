library(dplyr)
library(readxl)
library(janitor)
library(magrittr)
library(ggplot2)

# Load cluster assignments
clusters <- read.csv('cluster_assignments.csv')

# Load age data
df <- read_excel('./ibex/results/results_2021-02-09T14_54_08_538Z_SA_AAF.xlsx')
colnames(df) %<>% make_clean_names()
df %<>% subset(logged_in_as_experiment_owner_if_known != 'yes')

age_data <- df %>%
    subset(field_name == 'age') %>%
    mutate(age = as.numeric(field_value)) %>%
    subset(!is.na(age)) %>%
    select(results_index, age) %>%
    distinct()

# Merge with clusters
cluster_age <- clusters %>%
    left_join(age_data, by = 'results_index') %>%
    filter(!is.na(age))

cat('=== AGE × CLUSTER ANALYSIS ===\n\n')
cat('Total participants with age data:', nrow(cluster_age), '\n\n')

# Age by cluster
cat('Age by cluster:\n')
age_summary <- cluster_age %>%
    group_by(cluster) %>%
    summarize(
        n = n(),
        mean_age = mean(age),
        sd_age = sd(age),
        median_age = median(age),
        range_min = min(age),
        range_max = max(age),
        .groups = "drop"
    )
print(age_summary)

# T-test
cat('\n=== T-test: Age difference between clusters ===\n')
c1_age <- cluster_age %>% filter(cluster == 1) %>% pull(age)
c2_age <- cluster_age %>% filter(cluster == 2) %>% pull(age)

t_result <- t.test(c1_age, c2_age)
cat('Cluster 1 (Low Acceptors) mean age:', round(mean(c1_age), 2), 'years\n')
cat('Cluster 2 (High Acceptors) mean age:', round(mean(c2_age), 2), 'years\n')
cat('Difference:', round(mean(c1_age) - mean(c2_age), 2), 'years\n')
cat('t-statistic:', round(t_result$statistic, 3), '\n')
cat('df:', round(t_result$parameter, 1), '\n')
cat('p-value:', format.pval(t_result$p.value, digits = 4), '\n')

if (t_result$p.value < 0.05) {
    cat('*** SIGNIFICANT difference (p < .05)\n')
} else {
    cat('Not significant (p >= .05)\n')
}

# Effect size
pooled_sd <- sqrt((sd(c1_age)^2 + sd(c2_age)^2) / 2)
cohens_d <- (mean(c1_age) - mean(c2_age)) / pooled_sd
cat("Cohen's d:", round(cohens_d, 3), '\n\n')

# Interpretation
cat('Effect size interpretation:\n')
if (abs(cohens_d) < 0.2) {
    cat('  Negligible effect\n')
} else if (abs(cohens_d) < 0.5) {
    cat('  Small effect\n')
} else if (abs(cohens_d) < 0.8) {
    cat('  Medium effect\n')
} else {
    cat('  Large effect\n')
}

# Boxplot (B&W)
p1 <- ggplot(
    cluster_age,
    aes(x = factor(cluster), y = age, shape = factor(cluster))
) +
    geom_jitter(size = 2.5, alpha = 0.6, width = 0.2) +
    geom_boxplot(alpha = 0, outlier.shape = NA, width = 0.5) +
    theme_classic() +
    theme(
        text = element_text(size = 14),
        legend.position = "none"
    ) +
    labs(
        title = "Age Distribution by Cluster",
        x = "Cluster (1=Low Acceptors, 2=High Acceptors)",
        y = "Age (years)",
        caption = paste0(
            "t = ",
            round(t_result$statistic, 2),
            ", p = ",
            format.pval(t_result$p.value, digits = 3)
        )
    ) +
    scale_shape_manual(values = c(1, 2))

print(p1)

ggsave("figures/age_by_cluster_BW.png", p1, width = 8, height = 6, dpi = 300)
ggsave("figures/age_by_cluster_BW.pdf", p1, width = 8, height = 6)

cat('\n=== Plot saved ===\n')
cat('age_by_cluster_BW.png/pdf\n')
