library(dplyr)
library(readxl)
library(janitor)
library(magrittr)
library(ggplot2)
library(tidyr)
library(tibble)
library(cluster) # for clustering
library(factoextra) # for cluster visualization

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

# Filter for FORMAL conditions with PLURAL verb (all attractors)
formal_pl_verb <- df %>%
    filter(
        register == "formal",
        v_n == "pl"
    )

cat("\n=== Participant Clustering Analysis ===\n")
cat("Formal register, plural verb acceptability\n\n")

# Create participant × item response matrix
response_matrix <- formal_pl_verb %>%
    select(results_index, group, response_yes) %>%
    mutate(response_numeric = as.numeric(response_yes)) %>%
    group_by(results_index, group) %>%
    summarize(response = mean(response_numeric), .groups = "drop") %>%
    pivot_wider(
        names_from = group,
        values_from = response,
        values_fill = NA
    )

# Remove participants with too many missing values
participant_matrix <- response_matrix %>%
    column_to_rownames("results_index")

# Count non-NA values per participant
completeness <- rowSums(!is.na(participant_matrix))
cat("Participants with at least 5 items:", sum(completeness >= 5), "\n")

# Keep only participants with at least 5 items
participant_matrix_clean <- participant_matrix[completeness >= 5, ]

# Impute missing values with participant mean (for clustering)
participant_matrix_imputed <- t(apply(
    participant_matrix_clean,
    1,
    function(row) {
        ifelse(is.na(row), mean(row, na.rm = TRUE), row)
    }
))

cat(
    "Final matrix:",
    nrow(participant_matrix_imputed),
    "participants ×",
    ncol(participant_matrix_imputed),
    "items\n\n"
)

# ==============================================================================
# 1. HIERARCHICAL CLUSTERING
# ==============================================================================

cat("=== Hierarchical Clustering ===\n\n")

# Calculate distance matrix
dist_matrix <- dist(participant_matrix_imputed, method = "euclidean")

# Hierarchical clustering
hclust_result <- hclust(dist_matrix, method = "ward.D2")

# Cut tree into k clusters (try k=3 first)
k <- 3
clusters <- cutree(hclust_result, k = k)

# Add cluster assignments to participant data
participant_clusters <- data.frame(
    results_index = as.numeric(rownames(participant_matrix_imputed)),
    cluster = as.factor(clusters)
)

# Calculate cluster characteristics
cluster_summary <- participant_clusters %>%
    left_join(
        formal_pl_verb %>%
            group_by(results_index) %>%
            summarize(mean_acceptance = mean(response_yes), .groups = "drop"),
        by = "results_index"
    ) %>%
    group_by(cluster) %>%
    summarize(
        n_participants = n(),
        mean_acceptance = mean(mean_acceptance),
        sd_acceptance = sd(mean_acceptance),
        .groups = "drop"
    )

cat("Cluster summary (k =", k, "):\n")
print(cluster_summary)
cat("\n")

# Dendrogram plot (B&W)
p1 <- fviz_dend(
    hclust_result,
    k = k,
    cex = 0.5,
    color_labels_by_k = FALSE,
    rect = TRUE,
    rect_border = "black",
    rect_fill = FALSE,
    main = "Hierarchical Clustering of Participants",
    xlab = "Participant",
    ylab = "Height"
) +
    theme_classic() +
    theme(text = element_text(size = 12))

print(p1)

ggsave(
    "figures/participant_dendrogram_BW.png",
    p1,
    width = 12,
    height = 7,
    dpi = 300
)
ggsave("figures/participant_dendrogram_BW.pdf", p1, width = 12, height = 7)

# ==============================================================================
# 2. K-MEANS CLUSTERING
# ==============================================================================

cat("=== K-Means Clustering ===\n\n")

# Determine optimal number of clusters using elbow method
set.seed(123)
wss <- sapply(1:8, function(k) {
    kmeans(participant_matrix_imputed, centers = k, nstart = 25)$tot.withinss
})

# Silhouette analysis
sil_width <- sapply(2:8, function(k) {
    km <- kmeans(participant_matrix_imputed, centers = k, nstart = 25)
    ss <- silhouette(km$cluster, dist_matrix)
    mean(ss[, 3])
})

cat("Silhouette scores for k=2 to k=8:\n")
print(data.frame(k = 2:8, silhouette = round(sil_width, 3)))
cat("\n")

# Use k with highest silhouette score
best_k <- which.max(sil_width) + 1
cat("Optimal k =", best_k, "(based on silhouette)\n\n")

# Final k-means with optimal k
kmeans_result <- kmeans(
    participant_matrix_imputed,
    centers = best_k,
    nstart = 25
)

participant_kmeans <- data.frame(
    results_index = as.numeric(rownames(participant_matrix_imputed)),
    cluster = as.factor(kmeans_result$cluster)
)

# Cluster characteristics
kmeans_summary <- participant_kmeans %>%
    left_join(
        formal_pl_verb %>%
            group_by(results_index) %>%
            summarize(mean_acceptance = mean(response_yes), .groups = "drop"),
        by = "results_index"
    ) %>%
    group_by(cluster) %>%
    summarize(
        n_participants = n(),
        mean_acceptance = mean(mean_acceptance),
        sd_acceptance = sd(mean_acceptance),
        .groups = "drop"
    ) %>%
    arrange(mean_acceptance)

cat("K-means cluster summary (k =", best_k, "):\n")
print(kmeans_summary)
cat("\n")

# Elbow plot (B&W)
p2 <- ggplot(data.frame(k = 1:8, wss = wss), aes(x = k, y = wss)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3, shape = 21, fill = "white", color = "black") +
    theme_classic() +
    theme(text = element_text(size = 14)) +
    labs(
        title = "Elbow Method for Optimal k",
        x = "Number of Clusters (k)",
        y = "Total Within-Cluster Sum of Squares"
    )

print(p2)

ggsave("figures/kmeans_elbow_BW.png", p2, width = 8, height = 6, dpi = 300)
ggsave("figures/kmeans_elbow_BW.pdf", p2, width = 8, height = 6)

# Cluster distribution plot (B&W)
cluster_dist_data <- participant_kmeans %>%
    left_join(
        formal_pl_verb %>%
            group_by(results_index) %>%
            summarize(mean_acceptance = mean(response_yes), .groups = "drop"),
        by = "results_index"
    )

p3 <- ggplot(
    cluster_dist_data,
    aes(x = cluster, y = mean_acceptance, shape = cluster)
) +
    geom_jitter(size = 2.5, alpha = 0.6, width = 0.2) +
    geom_boxplot(alpha = 0, outlier.shape = NA, width = 0.5) +
    theme_classic() +
    theme(
        text = element_text(size = 14),
        legend.position = "none"
    ) +
    labs(
        title = paste0("Participant Clusters (k = ", best_k, ")"),
        subtitle = "Based on response patterns across items",
        x = "Cluster",
        y = "Mean Acceptance Rate"
    ) +
    scale_shape_manual(values = 1:best_k)

print(p3)

ggsave(
    "figures/cluster_distribution_BW.png",
    p3,
    width = 8,
    height = 6,
    dpi = 300
)
ggsave("figures/cluster_distribution_BW.pdf", p3, width = 8, height = 6)

cat("\n=== Clustering Analysis Complete ===\n")
cat("Generated plots:\n")
cat("1. participant_dendrogram_BW.png/pdf\n")
cat("2. kmeans_elbow_BW.png/pdf\n")
cat("3. cluster_distribution_BW.png/pdf\n\n")

# Save cluster assignments for further analysis
cluster_assignments <- participant_kmeans %>%
    left_join(
        formal_pl_verb %>%
            group_by(results_index) %>%
            summarize(mean_acceptance = mean(response_yes), .groups = "drop"),
        by = "results_index"
    )

write.csv(cluster_assignments, "cluster_assignments.csv", row.names = FALSE)
cat("Cluster assignments saved to: cluster_assignments.csv\n")
