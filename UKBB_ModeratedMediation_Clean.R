# UKBB_ModeratedMediation.R
# --------------------------
# Packages
# --------------------------
packages <- c("dplyr", "lavaan")
lapply(packages, library, character.only = TRUE)

# --------------------------
# Data path using setwd(Downloads) 
# --------------------------
downloads_dir <- path.expand("~/Downloads")
if (!dir.exists(downloads_dir)) stop("Downloads folder not found at: ", downloads_dir)
setwd(downloads_dir)

data_file <- "ukbb_full_matched_hitop_clean_0825.csv"
if (!file.exists(data_file)) stop("Data file '", data_file, "' not found in ", downloads_dir,
                                 ". Place the file there or change data_file/path in the script.")
message("Loading data from: ", normalizePath(file.path(downloads_dir, data_file)))
ukb_mediation <- read.csv(data_file, header = TRUE, sep = ",", stringsAsFactors = FALSE)

# --------------------------
# Helper: save lavaan outputs safely
# --------------------------
save_lavaan_outputs <- function(fit, name, outdir = "output") {
  if (is.null(fit)) stop("fit is NULL for ", name)
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  pe <- parameterEstimates(fit, boot.ci.type = "perc")
  keep_cols <- intersect(c("lhs","op","rhs","label","est","se","z","pvalue","ci.lower","ci.upper"), names(pe))
  write.csv(pe[, keep_cols], file = file.path(outdir, paste0(name, "_parameterEstimates.csv")), row.names = FALSE)
  ind <- pe[pe$op == ":=" | grepl("ind", pe$label, ignore.case = TRUE), , drop = FALSE]
  if (nrow(ind) > 0) {
    write.csv(ind[, keep_cols], file = file.path(outdir, paste0(name, "_indirects.csv")), row.names = FALSE)
  }
  saveRDS(fit, file = file.path(outdir, paste0(name, "_fit.rds")))
  message("Saved outputs for ", name, " -> ", outdir)
}

# --------------------------
# Create interaction terms (same as original)
# --------------------------
ukb_mediation <- ukb_mediation %>%
  mutate(
    pal_dx = pal * dx,
    anhedonia_dx = freq_unenthusiasm_disinterest * dx,
    
    pal_sex = pal * sex,
    anhedonia_sex = freq_unenthusiasm_disinterest * sex,
    
    pal_int = pal * int,
    anhedonia_int = freq_unenthusiasm_disinterest * int,
    
    pal_ext = pal * ext,
    anhedonia_ext = freq_unenthusiasm_disinterest * ext,
    
    pal_psy = pal * psy,
    anhedonia_psy = freq_unenthusiasm_disinterest * psy
  )

### Anhedonia Mediation

# No Moderation
model_anhedonia_simple <- "
  # Mediation paths
  freq_unenthusiasm_disinterest ~ a1*pal 
  freq_social_visits ~ b1*freq_unenthusiasm_disinterest + c_prime*pal

  # Indirect effect
  ind_effect := a1 * b1
"

fit_anhedonia_simple <- sem(model_anhedonia_simple, data = ukb_mediation, se = "bootstrap", 
                  bootstrap = 2000)
estimates_anhedonia_simple <- parameterEstimates(fit_anhedonia_simple, boot.ci.type = "perc")
# optional interactive view: View(estimates_anhedonia_simple)
save_lavaan_outputs(fit_anhedonia_simple, "anhedonia_simple")

# Moderated Anhedonia Mediation - Diagnosis
model_anhedonia_mod_dx <- "
  freq_unenthusiasm_disinterest ~ a1*pal + a2*dx + a3*pal_dx
  freq_social_visits ~ b1*freq_unenthusiasm_disinterest + b2*dx + 
  b3*anhedonia_dx + c_prime*pal

  ind_effect := a1 * b1
  ind_effect_mod := (a1 + a3*1) * (b1 + b3*1)
"

fit_anhedonia_mod_dx <- sem(model_anhedonia_mod_dx, data = ukb_mediation, se = "bootstrap", 
              bootstrap = 2000)
estimates_anhedonia_mod_dx <- parameterEstimates(fit_anhedonia_mod_dx, boot.ci.type = "perc")
save_lavaan_outputs(fit_anhedonia_mod_dx, "anhedonia_mod_dx")

# Moderator: Sex
model_anhedonia_mod_sex <- "
  freq_unenthusiasm_disinterest ~ a1*pal + a2*sex + a3*pal_sex
  freq_social_visits ~ b1*freq_unenthusiasm_disinterest + b2*sex + 
  b3*anhedonia_sex + c_prime*pal

  ind_effect := a1*b1
  ind_effect_mod := (a1 + a3*1)*(b1 + b3*1)
"

fit_anhedonia_mod_sex <- sem(model_anhedonia_mod_sex, data = ukb_mediation, se = 'bootstrap', 
               bootstrap = 2000)
estimates_anhedonia_mod_sex <- parameterEstimates(fit_anhedonia_mod_sex, boot.ci.type = 'perc')
save_lavaan_outputs(fit_anhedonia_mod_sex, "anhedonia_mod_sex")

# Moderator: Internalizing
model_anhedonia_mod_int <- "
  freq_unenthusiasm_disinterest ~ a1*pal + a2*int + a3*pal_int
  freq_social_visits ~ b1*freq_unenthusiasm_disinterest + b2*int + 
  b3*anhedonia_int + c_prime*pal

  ind_effect := a1*b1
  ind_effect_mod := (a1 + a3*1)*(b1 + b3*1)
"

fit_anhedonia_mod_int <- sem(model_anhedonia_mod_int, data = ukb_mediation, se = 'bootstrap', 
               bootstrap = 2000)
estimates_anhedonia_mod_int <- parameterEstimates(fit_anhedonia_mod_int, boot.ci.type = 'perc')
save_lavaan_outputs(fit_anhedonia_mod_int, "anhedonia_mod_int")

# Moderator: Externalizing
model_anhedonia_mod_ext <- "
  freq_unenthusiasm_disinterest ~ a1*pal + a2*ext + a3*pal_ext
  freq_social_visits ~ b1*freq_unenthusiasm_disinterest + b2*ext + 
  b3*anhedonia_ext + c_prime*pal

  ind_effect := a1*b1
  ind_effect_mod := (a1 + a3*1)*(b1 + b3*1)
"

fit_anhedonia_mod_ext <- sem(model_anhedonia_mod_ext, data = ukb_mediation, se = 'bootstrap', 
               bootstrap = 2000)
estimates_anhedonia_mod_ext <- parameterEstimates(fit_anhedonia_mod_ext, boot.ci.type = 'perc')
save_lavaan_outputs(fit_anhedonia_mod_ext, "anhedonia_mod_ext")

# Moderator: Psychosis
model_anhedonia_mod_psy <- "
  freq_unenthusiasm_disinterest ~ a1*pal + a2*psy + a3*pal_psy
  freq_social_visits ~ b1*freq_unenthusiasm_disinterest + b2*psy + 
  b3*anhedonia_psy + c_prime*pal

  ind_effect := a1*b1
  ind_effect_mod := (a1 + a3*1)*(b1 + b3*1)
"

fit_anhedonia_mod_psy <- sem(model_anhedonia_mod_psy, data = ukb_mediation, se = 'bootstrap', 
               bootstrap = 2000)
estimates_anhedonia_mod_psy <- parameterEstimates(fit_anhedonia_mod_psy, boot.ci.type = 'perc')
save_lavaan_outputs(fit_anhedonia_mod_psy, "anhedonia_mod_psy")

### Verbal Memory Mediation (pal and freq_unenthusiasm_disinterest are swapped)
model_verbal_simple <- "
  # Mediation paths
  pal ~ a1*freq_unenthusiasm_disinterest
  freq_social_visits ~ b1*pal + c_prime*freq_unenthusiasm_disinterest

  # Indirect effect
  ind_effect := a1 * b1
"

fit_verbal_simple <- sem(model_verbal_simple, data = ukb_mediation, se = "bootstrap", 
                  bootstrap = 2000)
estimates_verbal_simple <- parameterEstimates(fit_verbal_simple, boot.ci.type = "perc")
save_lavaan_outputs(fit_verbal_simple, "verbal_simple")

# Save session info (package versions) for reproducibility record
if (!dir.exists("output")) dir.create("output")
writeLines(capture.output(sessionInfo()), "output/sessionInfo.txt")
message("Wrote sessionInfo to output/sessionInfo.txt")

message("All done. Parameter tables and indirects (if any) saved in 'output/' (no raw data included).")