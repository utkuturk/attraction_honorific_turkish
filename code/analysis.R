
library(dplyr)
library(readxl)
library(janitor)
library(magrittr)

df <- read_excel("./ibex/results/results_2021-02-09T14_54_08_538Z_SA_AAF.xlsx")
colnames(df) %<>% make_clean_names()
df %<>% subset(logged_in_as_experiment_owner_if_known != "yes")

nsubj = df %>% select(results_index) %>% distinct() %>% nrow()

age <- df %>%
  subset(field_name == "age") %>%
  mutate(field_value = as.numeric(field_value)) %>%
  subset(!is.na(field_value)) %>%
  select(field_value) %>%
  droplevels() %>%
  summarize(
    mean = mean(field_value),
    sd = sd(field_value),
    max = max(field_value),
    min = min(field_value)
  )

practice <- df %>% subset(type == "practice" & !is.na(question_null_if_none))

practice %>% subset(whether_or_not_answer_was_correct_null_if_n_a == "NULL")
practice$whether_or_not_answer_was_correct_null_if_n_a[practice$whether_or_not_answer_was_correct_null_if_n_a == "NULL"] <- 0
practice$whether_or_not_answer_was_correct_null_if_n_a %<>% as.integer()
bad_subjects_by_practice <- practice %>%
  group_by(results_index) %>%
  summarize(p_yes = mean(whether_or_not_answer_was_correct_null_if_n_a)) %>%
  subset(p_yes <= 0.5) %>%
  .$results_index


# filter out bad subjects
df <- df %>% subset(!results_index %in% bad_subjects_by_practice)

# only selects the types that start with "filler_utku_"
df <- df %>%
  subset(grepl("filler_utku_", type)) %>%
    subset(!is.na(question_null_if_none))

df %<>% select(
  -time, -counter, -hash, -logged_in_as_experiment_owner_if_known,
  -element_number, -field_name, -field_value, -sentence_or_sentence_md5,
  -question_null_if_none, -whether_or_not_answer_was_correct_null_if_n_a
)

df$response_yes <- ifelse(grepl("P'ye", df$answer), T,
  ifelse(grepl("Q'ya", df$answer), F, NA)
)

df %<>% select(-answer)
df %<>% subset(!is.na(response_yes))

# If response_yes is TRUE, and type includes "formal" or ends with "sg", then make the ResponseCorret 1


# summarize data with mean, n(), sd, and se, and ci
avgs <- df %>% group_by(type) %>% summarize(
  mean = mean(response_yes),
  n = n(),
  sd = sd(response_yes),
  se = sd(response_yes) / sqrt(n),
  ci = qt(0.975, n - 1) * se
) %>% arrange(mean)

{
  (condition_stats <- avgs %>%
    group_by(register) %>%
    summarise(
      overall_mean = mean(mean),
      overall_se = sd(mean) / sqrt(n()), # Standard error
      .groups = "drop"
    ))

}

{
  (mean_combinations <- avgs %>%
    group_by(att_n, v_n) %>%
    summarise(
      overall_mean = mean(mean), # Calculate mean
      overall_se = sd(mean) / sqrt(n()), # Calculate standard error
      .groups = "drop"
    ))
}

# separate the type into 5 columns. the first column is filler, then, utku, and then either formal or informal. then it is the first two characters after the last _, and the last two characters of the entire type column
avgs$register <- stringr::str_split_fixed(avgs$type, "_", 5)[, 3]

avgs$att_n <- stringr::str_sub(avgs$type, -4, -3)
avgs$v_n <- stringr::str_sub(avgs$type, -2, -1)

# plot avgs, use att_n as a linetype, use v_n as a x axis, and use register as a facet_wrap
library(ggplot2)
pd = position_dodge(0.1)
ggplot(avgs, aes(x = v_n, y = mean, group = att_n, linetype = att_n)) +
geom_point(position = pd) +
   geom_errorbar(aes(ymin = mean-ci, ymax = mean+ci, width = 0.3), position = pd) +
   geom_line(aes(linetype = att_n)) +
   facet_wrap(~register, labeller = labeller(register = c(formal = "Formal", informal = "Informal"))) +
   theme_minimal() +
   theme(legend.position = "top", text = element_text(size = 14)) +
   labs(
     x = "Verb Number",
     y = "Yes responses [%]",
     linetype = "Attractor Number",
     color = "Attractor Number"
   ) +
  #  theme(axis.text = element_text(size = 14)) +
   scale_x_discrete(labels = c("Plural", "Singular")) +
   scale_linetype(labels = c("Plural", "Singular")) +
   theme(strip.text = element_text(color = "white", face = "bold"), strip.background = element_rect(fill = "black"))

  # ggplot(avgs, aes(x = v_n, y = ifelse(v_n == "pl", 1 - mean, mean), group = att_n, linetype = att_n)) +
  #   geom_point() +
  #   geom_errorbar(aes(ymin = ifelse(v_n == "pl", 1 - (mean + se), mean - se), ymax = ifelse(v_n == "pl", 1 - (mean - se), mean + se)), width = 0.15) +
  #   geom_line(aes(linetype = att_n)) +
  #   facet_wrap(~register, labeller = labeller(register = c(formal = "Formal", informal = "Informal"))) +
  #   theme_minimal() +
  #   theme(legend.position = "right", text = element_text(size = 18)) +
  #   labs(
  #     x = "Verb Number",
  #     y = "Correct responses [%]",
  #     linetype = "Attractor\n Number",
  #     color = "Attractor\n Number"
  #   ) +
  #   theme(axis.text = element_text(size = 14)) +
  #   scale_x_discrete(labels = c("Plural", "Singular")) +
  #   scale_linetype(labels = c("Plural", "Singular")) +
  #   theme(strip.text = element_text(color = "white", face = "bold"), strip.background = element_rect(fill = "black"))

df <- df %>%
mutate(
  register =  stringr::str_split_fixed(type, "_", 5)[, 3],
  att_n = stringr::str_sub(type, -4, -3),
  v_n = stringr::str_sub(type, -2, -1),
)


df <- df %>%
  mutate(
    att_n = as.factor(att_n), # Ensure match is a factor
    v_n = as.factor(v_n), # Ensure delay is a factor
    register = as.factor(register),
    results_index = as.factor(results_index),
    group = as.factor(group),
    v_n = recode(v_n, "pl" = 0.5, "sg" = -0.5), # Recode match to 0.5 and -0.5
    att_n = recode(att_n, "sg" = -0.5, "pl" = 0.5), # Recode delay to 0.5 and -0.5,
    register = recode(register, "formal" = 0.5, "informal" = -0.5)
  )


library(brms)
# Define the formula
formula <- bf(response_yes ~ v_n * att_n * register + (1 + v_n + att_n + register | results_index) + (1 + v_n + att_n + register | group))

fit <- brm(
  formula = formula,
  data = df,
  family = bernoulli("logit"),
  chains = 4,
  iter = 2000,
  warmup = 1000,
  control = list(adapt_delta = 0.95),
  cores = 4,
  file = "full_model"
)


summary(fit)



{
library(bayesplot)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggtext)
# Create a mapping of original effect names to concise labels
effect_labels <- c(
  "b_v_n" = "Verb Number",
  "b_att_n" = "Attractor Number",
  "b_register" = "Register",
  "b_v_n:att_n" = "Verb x Attractor Number",
  "b_v_n:att_n:register" = "3-way interaction"
)

# Assuming 'fit' is your Bayesian model fit object
post_samples <- as_draws_df(fit)

# Filter the relevant parameters
post_samples_filtered <- post_samples[, c("b_v_n", "b_att_n", "b_register", "b_v_n:att_n", "b_v_n:att_n:register")]

# Calculate the means and credible intervals
post_samples_summary <- post_samples_filtered %>%
  summarise(across(everything(),
    list(
      mean = ~ mean(.),
      lower = ~ quantile(., 0.025),
      upper = ~ quantile(., 0.975)
    ),
    .names = "{.col}_{.fn}"
  ))

# Reshape the summary data using pivot_longer()
post_samples_summary_long <- post_samples_summary %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Effect", "stat"),
    names_pattern = "(.+)_(mean|lower|upper)$", # Regex to capture all before last underscore and the stat
    values_to = "value"
  ) %>%
  dplyr::filter(stat %in% c("mean", "lower", "upper")) %>%
  pivot_wider(names_from = stat, values_from = value)

# Replace Effect names with concise labels
post_samples_summary_long <- post_samples_summary_long %>%
  mutate(Effect = recode(Effect, !!!effect_labels))

# Calculate probabilities of effects being >0 and <0
probabilities <- post_samples_filtered %>%
  summarise(across(everything(),
    list(
      prob_gt = ~ mean(. > 0),
      prob_lt = ~ mean(. < 0)
    ),
    .names = "{.col}_prob_{.fn}"
  )) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Effect", "prob_type"),
    names_pattern = "(.+)_prob_(.+)",
    values_to = "probability"
  )

# delete the last 5 characters from Effect colum
probabilities$Effect <- substr(probabilities$Effect, 1, nchar(probabilities$Effect) - 5)


# Clean Effect names for the probabilities
probabilities <- probabilities %>%
  mutate(Effect = recode(Effect, !!!effect_labels))


# Create a combined dataset for plotting
combined_data <- post_samples_summary_long %>%
  left_join(probabilities, by = "Effect")
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(ggtext) # Add ggtext for enhanced text formatting

# Prepare combined_data with labels that include probabilities
combined_data <- combined_data %>%
  group_by(Effect) %>%
  summarize(
    mean = first(mean),
    lower = first(lower),
    upper = first(upper),
    prob_gt = first(probability[prob_type == "gt"]),
    prob_lt = first(probability[prob_type == "lt"]),
    .groups = "drop"
  ) %>%
  mutate(
    # Concatenate the probability label
    label = case_when(
      round(prob_gt, 2) == 1 ~ "P(> 0): >0.99",
      round(prob_gt, 2) == 0 ~ "P(< 0): >0.99",
      prob_gt > 0 ~ paste("P(> 0):", round(prob_gt, 2)),
      prob_lt > 0 ~ paste("P(< 0):", round(prob_lt, 2)),
      TRUE ~ NA_character_ # Ensure NA for any unmatched cases
    )
  )

# Specify the order of effects for the y-axis
effect_order <- c(
  "3-way interaction", "Verb x Attractor Number", "Register", "Attractor Number", "Verb Number"
)

# Create the plot with ROPE and annotations
ggplot(combined_data, aes(x = mean, y = factor(Effect, levels = effect_order))) + # Set y-axis order
  geom_point(size = 3) + # Plot means as points
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) + # Error bars for credible intervals
  # geom_text(aes(label = label), hjust = 0.8, vjust = -1.5, size = 3.5, check_overlap = TRUE, family = "mono") + # Annotate probabilities with monospaced font
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1.2, alpha = 0.4) + # Bold vertical line at zero
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) + # Horizontal line just above x-axis
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 12, color = "black"), # Default font for effect names
    axis.text.x = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    panel.grid.major.x = element_line(color = "gray80", size = 0.5),
    panel.grid.minor.x = element_line(color = "gray90", size = 0.5),
    panel.grid.minor = element_blank()
  ) +
  labs(x = "Estimate (logit)", y = "", title = "") +
  scale_y_discrete(labels = function(x) {
    labels_with_probs <- paste0(x, "\n ", combined_data$label[match(x, combined_data$Effect)], "")
    labels_with_probs[is.na(labels_with_probs)] <- ""
    return(labels_with_probs)
  }) +
  theme(axis.text.y = element_text(size = 12))

}
