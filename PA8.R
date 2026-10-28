# Name: Lyle O'Dell
# Course: COP2073C
# Assignment: Module 8 Programming Assignment (PA8)
# Date: October 28, 2026

# Create the recipe data frame, including missing control observations.
df <- data.frame(
  RecipeA = c(0, 4, 4, 5, 4, 1, 2, 3, 3, 5),
  RecipeB = c(7, 6, 4, 2, 3, 0, 8, 6, 2, 3),
  RecipeC = c(1, 0, 1, 0, NA, 1, 1, NA, 1, 0)
)

# Stack the recipe groups and exclude missing observations.
newdf <- na.omit(stack(df))
cat("stacked data frame:\n")
print(newdf)

# Summarize the response (values) by predictor group (ind).
initial_summary <- summary(newdf)
cat("\ninitial summary:\n")
print(initial_summary)

# Response variable: values (zombie transformation significance).
# Predictor variable: ind (cookie recipe A, B, or C).
# H0: All three population mean transformation scores are equal.
# H1: At least one population mean transformation score differs.
anova_model <- aov(values ~ ind, data = newdf)

# Summarize the one-way ANOVA and obtain its p-value.
anova_summary <- summary(anova_model)
cat("\nanova summary:\n")
print(anova_summary)

pval <- anova_summary[[1]]$`Pr(>F)`[1]
cat("\np_value is", pval, "\n")

# Reject H0 when the p-value is below the 0.05 significance level.
if (pval < 0.05) {
  cat("Sufficient evidence to reject the null hypothesis.\n")
  cat("Ingredient X may have an effect on the results.\n")
} else {
  cat("Insufficient evidence to reject the null hypothesis.\n")
}
