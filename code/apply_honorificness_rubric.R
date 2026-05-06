library(dplyr)
library(stringr)

# Load template
template <- read.csv(
    "honorificness_scoring_template.csv",
    fileEncoding = "UTF-8"
)

# User's scoring rubric (1-5 scale)
scoring_rubric <- list(
    `1` = c("hizmetçi", "çaycı", "işçi", "kapıcı"),
    `2` = c(
        "aşçı",
        "çiçekçi",
        "terzi",
        "bakıcı",
        "dadı",
        "sürücü",
        "taksici",
        "çalgıcı",
        "hasta",
        "yardımcı",
        "nakliyeci",
        "falcı",
        "bekçi"
    ),
    `3` = c(
        "abla",
        "abi",
        "emlakçı",
        "hala",
        "dayı",
        "enişte",
        "sunucu",
        "müşteri",
        "koruma",
        "izleyici",
        "yolcu",
        "teyze",
        "amca",
        "komşu"
    ),
    `4` = c("eczacı", "hemşire", "dişçi", "anne", "eğitimci", "modacı", "nine"),
    `5` = c("hoca", "müdire")
)

# Function to extract subject NP and score it
score_honorificness <- function(sentence) {
    # Extract potential subject (usually after possessive and before verb)
    # Pattern: look for common noun suffixes before "efendim"

    for (score in names(scoring_rubric)) {
        for (np in scoring_rubric[[score]]) {
            # Check if NP appears in sentence (with various suffixes)
            if (
                grepl(paste0(np, "[sı|si|sü|su|ı|i|u|ü]"), sentence) |
                    grepl(np, sentence)
            ) {
                return(as.numeric(score))
            }
        }
    }

    # If not found in rubric, return NA
    return(NA)
}

# Apply scoring
cat("=== Applying honorificness scoring rubric ===\n\n")

template$honorificness_score <- sapply(
    template$sentence_or_sentence_md5,
    score_honorificness
)

# Check coverage
scored <- sum(!is.na(template$honorificness_score))
cat("Scored:", scored, "/", nrow(template), "items\n")

if (scored < nrow(template)) {
    cat("\nItems without scores:\n")
    unscored <- template %>% filter(is.na(honorificness_score))
    print(unscored %>% select(item_id, sentence_or_sentence_md5))
}

# Distribution
cat("\n=== Honorificness score distribution ===\n")
print(table(template$honorificness_score, useNA = "ifany"))

# Save
write.csv(
    template,
    "honorificness_scores.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
)

cat("\n=== Saved ===\n")
cat("File: honorificness_scores.csv\n")
cat("Ready to run analysis: Rscript code/analyze_honorificness.R\n")
