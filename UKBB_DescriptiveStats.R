#ukbb ch1 descriptive statistics (making sure pre submisison 11/18/25)

setwd('/Users/isabelladimatteo/Downloads')

df <- read.csv('ukbb_full_matched_hitop_clean_0825.csv', header = TRUE, sep = ",")

View(df)

library(dplyr)
library(tidyr)

# Example categorical variables
vars <- c("sex", "int", "ext", "psy")

# Create patients and controls dataframes from df
patients <- df %>% filter(dx == 1)
controls <- df %>% filter(dx != 1)

# Function to get n(%)
get_counts <- function(df, var) {
  df %>%
    count(!!sym(var)) %>%
    mutate(percent = n / sum(n) * 100,
           label = paste0(n, " (", round(percent, 2), "%)")) %>%
    select(!!sym(var), label)
}

# Apply function for each variable
summary_list <- lapply(vars, function(v) {
  pat_counts <- get_counts(patients, v) %>% rename(patients = label)
  con_counts <- get_counts(controls, v) %>% rename(controls = label)
  
  full_join(pat_counts, con_counts, by = v)
})

# Name the list elements
names(summary_list) <- vars

# Each element in summary_list is a table for one variable
summary_list$sex
summary_list$int
summary_list$ext
summary_list$psy


# Continuous variables
cont_vars <- c("age", "tdep", "pal", "freq_unenthusiasm_disinterest", "freq_social_visits")

# Function to calculate mean (SD)
get_mean_sd <- function(df, vars) {
  df %>%
    summarise(across(all_of(vars),
                     ~ paste0(round(mean(.x, na.rm = TRUE), 2),
                              " (", round(sd(.x, na.rm = TRUE), 2), ")"))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "mean_sd")
}

# Calculate summaries
patients_summary <- get_mean_sd(patients, cont_vars) %>% rename(patients = mean_sd)
controls_summary <- get_mean_sd(controls, cont_vars) %>% rename(controls = mean_sd)

# Combine into one table
summary_continuous <- left_join(patients_summary, controls_summary, by = "variable")

# View
summary_continuous

library(ggplot2)

# Histogram + density for age
ggplot(df, aes(x = age)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black", alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Age", x = "Age", y = "Density")

# QQ-plot for age
qqnorm(df$age)
qqline(df$age, col = "red", lwd = 2)

# Histogram + density for tdep
ggplot(df, aes(x = tdep)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgreen", color = "black", alpha = 0.6) +
  geom_density(color = "blue", size = 1) +
  labs(title = "Distribution of tdep", x = "tdep", y = "Density")

# QQ-plot for tdep
qqnorm(df$tdep)
qqline(df$tdep, col = "blue", lwd = 2)

#mann-whitney U tests
# Continuous variables
cont_vars <- c("age", "tdep", "pal", "freq_unenthusiasm_disinterest", "freq_social_visits")

# Function to perform Mann-Whitney U and calculate rank-biserial r
mann_whitney_with_r <- function(x, group) {
  wt <- wilcox.test(x ~ group, exact = FALSE)  # Mann-Whitney U test
  U <- wt$statistic                            # U statistic
  n1 <- sum(group == 1)
  n2 <- sum(group == 0)
  r <- 1 - (2*U)/(n1*n2)                       # rank-biserial correlation
  return(c(U = as.numeric(U), p = wt$p.value, r = r))
}

# Run for all variables
results <- sapply(cont_vars, function(v) mann_whitney_with_r(df[[v]], df$dx))
results_df <- as.data.frame(t(results))
results_df$p <- round(results_df$p, 3)
results_df$r <- round(results_df$r, 3)
results_df

#chi-square tests
# Install lsr if needed
install.packages("lsr")
library(lsr)

# Categorical variables
cat_vars <- c("sex", "int", "ext", "psy")

# Empty list to store results
chi_results <- list()

for (v in cat_vars) {
  tbl <- table(df[[v]], df$dx)        # cross-tab
  chi <- chisq.test(tbl, correct = FALSE)
  cramer <- cramersV(tbl)             # effect size
  chi_results[[v]] <- data.frame(
    variable = v,
    chi2 = round(chi$statistic, 2),
    p = round(chi$p.value, 3),
    cramers_v = round(cramer, 3)
  )
}

# Combine into one dataframe
chi_results_df <- do.call(rbind, chi_results)
chi_results_df

