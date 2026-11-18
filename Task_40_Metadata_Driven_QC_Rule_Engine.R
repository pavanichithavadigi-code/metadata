# TASK 40: METADATA-DRIVEN QC RULE ENGINE
# Synthetic clinical programming prototype using data.table.
# Supported rules: UNIQUE, NOT_MISSING, RANGE, ALLOWED_VALUES, TYPE.
# This is a training prototype, not a validated production QC system.

if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
library(data.table)
set.seed(4040)

# 1. Project folders and logging ------------------------------------------------
root <- getwd()
dirs <- c(
  data = file.path(root, "data"),
  config = file.path(root, "config"),
  reports = file.path(root, "reports"),
  exceptions = file.path(root, "reports", "exceptions"),
  logs = file.path(root, "logs")
)
for (p in dirs) dir.create(p, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(dirs[["logs"]],
                      paste0("qc_engine_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))

write_log <- function(message) {
  line <- paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "|", message)
  cat(line, "\n")
  cat(line, "\n", file = log_file, append = TRUE)
}
write_log("Task 40 started.")

# 2. Synthetic clinical datasets -----------------------------------------------
n <- 80L
ids <- sprintf("SUBJ%03d", seq_len(n))

adsl <- data.table(
  STUDYID = "STUDY40",
  USUBJID = ids,
  AGE = sample(18:80, n, TRUE),
  SEX = sample(c("Male", "Female"), n, TRUE),
  TRT01A = sample(c("Placebo", "Drug A", "Drug B"), n, TRUE)
)
adae <- data.table(
  STUDYID = "STUDY40",
  AESEQ = seq_len(140),
  USUBJID = sample(ids, 140, TRUE),
  AEDECOD = sample(c("Headache", "Nausea", "Fatigue", "Dizziness"), 140, TRUE),
  AESEV = sample(c("MILD", "MODERATE", "SEVERE"), 140, TRUE),
  TRTEMFL = sample(c("Y", "N"), 140, TRUE)
)
adlb <- CJ(
  USUBJID = ids,
  PARAMCD = c("ALT", "AST", "HGB"),
  AVISIT = c("BASELINE", "WEEK 4")
)
adlb[, `:=`(
  STUDYID = "STUDY40",
  AVAL = round(runif(.N, 5, 160), 1),
  AVALU = fifelse(PARAMCD == "HGB", "g/dL", "U/L")
)]
setcolorder(adlb, c("STUDYID", "USUBJID", "PARAMCD", "AVISIT", "AVAL", "AVALU"))

# Introduce controlled issues so that the QC engine produces exceptions.
adsl[1, AGE := NA_integer_]
adsl[2, AGE := 12L]
adsl[3, SEX := "Unknown category"]
adsl <- rbindlist(list(adsl, copy(adsl[4])), use.names = TRUE)
adae[1, AEDECOD := NA_character_]
adae[2, TRTEMFL := "MAYBE"]
adae[, AESEV := as.factor(AESEV)] # Deliberately wrong storage type for TYPE rule.
adlb[1, AVAL := NA_real_]
adlb[2, AVAL := 2500]

datasets <- list(ADSL = adsl, ADAE = adae, ADLB = adlb)
fwrite(adsl, file.path(dirs[["data"]], "synthetic_adsl.csv"))
fwrite(adae, file.path(dirs[["data"]], "synthetic_adae.csv"))
fwrite(adlb, file.path(dirs[["data"]], "synthetic_adlb.csv"))
write_log("Synthetic ADSL, ADAE and ADLB datasets created.")

# 3. QC metadata: rules are stored as rows, not hardcoded calls ----------------
qc_rules <- data.table(
  RULE_ID = c("ADSL001", "ADSL002", "ADSL003", "ADSL004",
              "ADAE001", "ADAE002", "ADAE003",
              "ADLB001", "ADLB002", "ADLB003"),
  DATASET = c("ADSL", "ADSL", "ADSL", "ADSL",
              "ADAE", "ADAE", "ADAE",
              "ADLB", "ADLB", "ADLB"),
  VARIABLE = c("USUBJID", "AGE", "AGE", "SEX",
               "AEDECOD", "TRTEMFL", "AESEV",
               "AVAL", "AVAL", "AVAL"),
  RULE_TYPE = c("UNIQUE", "NOT_MISSING", "RANGE", "ALLOWED_VALUES",
                "NOT_MISSING", "ALLOWED_VALUES", "TYPE",
                "NOT_MISSING", "RANGE", "TYPE"),
  KEY_COLUMNS = c("USUBJID", NA, NA, NA, NA, NA, NA, NA, NA, NA),
  MIN_VALUE = c(NA, NA, 18, NA, NA, NA, NA, NA, 0, NA),
  MAX_VALUE = c(NA, NA, 100, NA, NA, NA, NA, NA, 2000, NA),
  ALLOWED_VALUES = c(NA, NA, NA, "Male|Female", NA, "Y|N", NA, NA, NA, NA),
  EXPECTED_TYPE = c(NA, NA, NA, NA, NA, NA, "CHARACTER", NA, NA, "NUMERIC"),
  SEVERITY = "ERROR",
  DESCRIPTION = c(
    "USUBJID must be unique in ADSL.",
    "AGE must not be missing.",
    "AGE must be between 18 and 100.",
    "SEX must be Male or Female.",
    "AEDECOD must not be missing.",
    "TRTEMFL must be Y or N.",
    "AESEV must be stored as character.",
    "ADLB AVAL must not be missing.",
    "ADLB AVAL must be between 0 and 2000.",
    "ADLB AVAL must be numeric."
  )
)
fwrite(qc_rules, file.path(dirs[["config"]], "qc_rule_metadata.csv"))

# 4. Validate metadata before running it ---------------------------------------
validate_metadata <- function(rules, dataset_names) {
  required <- c("RULE_ID", "DATASET", "VARIABLE", "RULE_TYPE", "KEY_COLUMNS",
                "MIN_VALUE", "MAX_VALUE", "ALLOWED_VALUES", "EXPECTED_TYPE",
                "SEVERITY", "DESCRIPTION")
  missing_cols <- setdiff(required, names(rules))
  if (length(missing_cols)) stop(paste("Missing metadata columns:", paste(missing_cols, collapse = ", ")))
  if (anyNA(rules$RULE_ID) || any(rules$RULE_ID == "") || anyDuplicated(rules$RULE_ID)) {
    stop("RULE_ID must be populated and unique.")
  }
  if (any(!rules$DATASET %chin% dataset_names)) stop("Metadata refers to an unknown dataset.")
  supported <- c("UNIQUE", "NOT_MISSING", "RANGE", "ALLOWED_VALUES", "TYPE")
  if (any(!rules$RULE_TYPE %chin% supported)) stop("Metadata contains an unsupported rule type.")

  rr <- rules[RULE_TYPE == "RANGE"]
  if (nrow(rr) && any(is.na(rr$MIN_VALUE) | is.na(rr$MAX_VALUE) |
                      rr$MIN_VALUE > rr$MAX_VALUE)) {
    stop("RANGE rules require valid MIN_VALUE and MAX_VALUE.")
  }
  av <- rules[RULE_TYPE == "ALLOWED_VALUES"]
  if (nrow(av) && anyNA(av$ALLOWED_VALUES)) stop("ALLOWED_VALUES rules need configured values.")
  tr <- rules[RULE_TYPE == "TYPE"]
  valid_types <- c("CHARACTER", "NUMERIC", "INTEGER", "LOGICAL")
  if (nrow(tr) && any(is.na(tr$EXPECTED_TYPE) | !tr$EXPECTED_TYPE %chin% valid_types)) {
    stop("TYPE must be CHARACTER, NUMERIC, INTEGER or LOGICAL.")
  }
  ur <- rules[RULE_TYPE == "UNIQUE"]
  if (nrow(ur) && anyNA(ur$KEY_COLUMNS)) stop("UNIQUE rules need KEY_COLUMNS.")
  invisible(TRUE)
}
validate_metadata(qc_rules, names(datasets))
write_log("Metadata validation passed.")

# 5. Standardize exception records ---------------------------------------------
make_exceptions <- function(data, row_ids, rule) {
  if (!length(row_ids)) return(data.table())
  out <- copy(data[row_ids])
  out[, `:=`(
    QC_RULE_ID = rule$RULE_ID,
    QC_DATASET = rule$DATASET,
    QC_VARIABLE = rule$VARIABLE,
    QC_RULE_TYPE = rule$RULE_TYPE,
    QC_SEVERITY = rule$SEVERITY,
    QC_ROW_NUMBER = row_ids,
    QC_DESCRIPTION = rule$DESCRIPTION
  )]
  setcolorder(out, c(
    "QC_RULE_ID", "QC_DATASET", "QC_VARIABLE", "QC_RULE_TYPE",
    "QC_SEVERITY", "QC_ROW_NUMBER", "QC_DESCRIPTION",
    setdiff(names(out), c("QC_RULE_ID", "QC_DATASET", "QC_VARIABLE",
                          "QC_RULE_TYPE", "QC_SEVERITY", "QC_ROW_NUMBER",
                          "QC_DESCRIPTION"))
  ))
  out
}

# 6. One evaluator: behavior is selected by RULE_TYPE --------------------------
evaluate_one_rule <- function(data, rule) {
  rule_id <- rule$RULE_ID
  type <- rule$RULE_TYPE
  variable <- rule$VARIABLE

  if (type == "UNIQUE") {
    keys <- strsplit(rule$KEY_COLUMNS, "|", fixed = TRUE)[[1]]
    missing_cols <- setdiff(keys, names(data))
  } else {
    keys <- character()
    missing_cols <- setdiff(variable, names(data))
  }

  if (length(missing_cols)) {
    return(list(
      summary = data.table(
        RULE_ID = rule_id, DATASET = rule$DATASET, VARIABLE = variable,
        RULE_TYPE = type, SEVERITY = rule$SEVERITY, STATUS = "ERROR",
        EXCEPTION_COUNT = NA_integer_,
        DETAILS = paste("Missing column(s):", paste(missing_cols, collapse = ", "))
      ),
      exceptions = data.table()
    ))
  }

  bad_rows <- integer()
  details <- ""

  if (type == "UNIQUE") {
    duplicate_keys <- data[, .N, by = keys][N > 1L]
    if (nrow(duplicate_keys)) {
      row_key <- do.call(paste, c(data[, ..keys], sep = "\r"))
      duplicate_key <- do.call(paste, c(duplicate_keys[, ..keys], sep = "\r"))
      bad_rows <- which(row_key %chin% duplicate_key)
    }
    details <- paste("Duplicate key groups:", nrow(duplicate_keys),
                     "| Key:", paste(keys, collapse = " + "))

  } else if (type == "NOT_MISSING") {
    bad_rows <- which(is.na(data[[variable]]))
    details <- paste("Missing values:", length(bad_rows))

  } else if (type == "RANGE") {
    values <- data[[variable]]
    if (!is.numeric(values)) {
      return(list(
        summary = data.table(
          RULE_ID = rule_id, DATASET = rule$DATASET, VARIABLE = variable,
          RULE_TYPE = type, SEVERITY = rule$SEVERITY, STATUS = "ERROR",
          EXCEPTION_COUNT = NA_integer_,
          DETAILS = "RANGE rule requires a numeric column."
        ),
        exceptions = data.table()
      ))
    }
    bad_rows <- which(!is.na(values) &
                        (values < rule$MIN_VALUE | values > rule$MAX_VALUE))
    details <- paste("Allowed range:", rule$MIN_VALUE, "to", rule$MAX_VALUE,
                     "| Out of range:", length(bad_rows))

  } else if (type == "ALLOWED_VALUES") {
    values <- as.character(data[[variable]])
    allowed <- strsplit(rule$ALLOWED_VALUES, "|", fixed = TRUE)[[1]]
    bad_rows <- which(!is.na(values) & !(values %chin% allowed))
    details <- paste("Allowed:", paste(allowed, collapse = ", "),
                     "| Invalid:", length(bad_rows))

  } else if (type == "TYPE") {
    values <- data[[variable]]
    expected <- rule$EXPECTED_TYPE
    matches <- switch(
      expected,
      CHARACTER = is.character(values),
      NUMERIC = is.numeric(values),
      INTEGER = is.integer(values),
      LOGICAL = is.logical(values),
      FALSE
    )
    if (!matches) bad_rows <- seq_len(nrow(data))
    details <- paste("Expected:", expected,
                     "| Actual:", paste(class(values), collapse = "/"))
  }

  n_bad <- length(bad_rows)
  list(
    summary = data.table(
      RULE_ID = rule_id,
      DATASET = rule$DATASET,
      VARIABLE = variable,
      RULE_TYPE = type,
      SEVERITY = rule$SEVERITY,
      STATUS = if (n_bad == 0L) "PASS" else "FAIL",
      EXCEPTION_COUNT = as.integer(n_bad),
      DETAILS = details
    ),
    exceptions = make_exceptions(data, bad_rows, rule)
  )
}

# 7. Master engine: apply every configured rule -------------------------------
run_qc_engine <- function(datasets, rules) {
  validate_metadata(rules, names(datasets))
  summaries <- vector("list", nrow(rules))
  exceptions <- vector("list", nrow(rules))

  for (i in seq_len(nrow(rules))) {
    rule <- rules[i]
    write_log(paste("Evaluating", rule$RULE_ID, "on", rule$DATASET))
    result <- evaluate_one_rule(datasets[[rule$DATASET]], rule)
    summaries[[i]] <- result$summary
    exceptions[[i]] <- result$exceptions
  }

  list(
    summary = rbindlist(summaries, use.names = TRUE, fill = TRUE),
    exceptions = rbindlist(exceptions, use.names = TRUE, fill = TRUE)
  )
}

# 8. Run the engine -------------------------------------------------------------
write_log("Starting metadata-driven evaluation.")
qc <- run_qc_engine(datasets, qc_rules)
qc_summary <- qc$summary
qc_exceptions <- qc$exceptions
setorder(qc_summary, DATASET, RULE_ID)

dataset_summary <- qc_summary[, .(
  TOTAL_RULES = .N,
  PASSED = sum(STATUS == "PASS"),
  FAILED = sum(STATUS == "FAIL"),
  ERRORS = sum(STATUS == "ERROR"),
  TOTAL_EXCEPTIONS = sum(EXCEPTION_COUNT, na.rm = TRUE)
), by = DATASET]

overall_summary <- qc_summary[, .(
  TOTAL_RULES = .N,
  PASSED = sum(STATUS == "PASS"),
  FAILED = sum(STATUS == "FAIL"),
  ERRORS = sum(STATUS == "ERROR"),
  TOTAL_EXCEPTIONS = sum(EXCEPTION_COUNT, na.rm = TRUE)
)]

fwrite(qc_summary, file.path(dirs[["reports"]], "qc_rule_summary.csv"))
fwrite(dataset_summary, file.path(dirs[["reports"]], "qc_dataset_summary.csv"))
fwrite(overall_summary, file.path(dirs[["reports"]], "qc_overall_summary.csv"))

if (nrow(qc_exceptions)) {
  fwrite(qc_exceptions, file.path(dirs[["exceptions"]], "all_qc_exceptions.csv"))
  for (id in unique(qc_exceptions$QC_RULE_ID)) {
    fwrite(qc_exceptions[QC_RULE_ID == id],
           file.path(dirs[["exceptions"]], paste0(id, "_exceptions.csv")))
  }
}

# 9. Independent reference checks: compare selected rules ----------------------
# These checks are separately coded; they do not call the metadata evaluator.
reference <- data.table(
  RULE_ID = c("ADSL001", "ADSL002", "ADSL003", "ADAE002", "ADAE003", "ADLB002"),
  REFERENCE_EXCEPTION_COUNT = c(
    adsl[USUBJID %chin% adsl[, .N, by = USUBJID][N > 1L, USUBJID], .N],
    adsl[is.na(AGE), .N],
    adsl[!is.na(AGE) & (AGE < 18 | AGE > 100), .N],
    adae[!is.na(TRTEMFL) &
           !(as.character(TRTEMFL) %chin% c("Y", "N")), .N],
    if (is.character(adae$AESEV)) 0L else nrow(adae),
    adlb[!is.na(AVAL) & (AVAL < 0 | AVAL > 2000), .N]
  )
)

engine_counts <- qc_summary[
  RULE_ID %chin% reference$RULE_ID,
  .(RULE_ID, ENGINE_EXCEPTION_COUNT = as.integer(EXCEPTION_COUNT))
]

comparison <- merge(reference, engine_counts, by = "RULE_ID", all = TRUE)
comparison[, COMPARISON_STATUS := fifelse(
  REFERENCE_EXCEPTION_COUNT == ENGINE_EXCEPTION_COUNT, "MATCH", "MISMATCH"
)]
comparison[, DETAILS := fifelse(
  COMPARISON_STATUS == "MATCH",
  "Engine agrees with independent reference check.",
  "Investigate the difference."
)]
fwrite(comparison, file.path(dirs[["reports"]], "engine_vs_reference_comparison.csv"))

# 10. Demonstrate adding a rule through metadata only ---------------------------
# No new evaluator function is required because ALLOWED_VALUES is supported.
new_rule <- data.table(
  RULE_ID = "ADSL005",
  DATASET = "ADSL",
  VARIABLE = "TRT01A",
  RULE_TYPE = "ALLOWED_VALUES",
  KEY_COLUMNS = NA_character_,
  MIN_VALUE = NA_real_,
  MAX_VALUE = NA_real_,
  ALLOWED_VALUES = "Placebo|Drug A|Drug B",
  EXPECTED_TYPE = NA_character_,
  SEVERITY = "ERROR",
  DESCRIPTION = "TRT01A must be Placebo, Drug A or Drug B."
)
rules_plus <- rbindlist(list(qc_rules, new_rule), use.names = TRUE, fill = TRUE)
new_rule_result <- run_qc_engine(datasets, rules_plus[RULE_ID == "ADSL005"])
fwrite(new_rule_result$summary,
       file.path(dirs[["reports"]], "new_rule_metadata_demo.csv"))

write_log(paste(
  "Task 40 completed. PASS:", qc_summary[STATUS == "PASS", .N],
  "| FAIL:", qc_summary[STATUS == "FAIL", .N],
  "| ERROR:", qc_summary[STATUS == "ERROR", .N]
))
