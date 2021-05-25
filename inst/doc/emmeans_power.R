## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  # collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library("Superpower")
library("emmeans")

## -----------------------------------------------------------------------------
# Set up a within design with 2 factors, each with 2 and 3 levels
design_result <- ANOVA_design(
  design = "2w*3w",
  n = 40,
  mu = c(0.3, 0, 0.5, 0.3, 0, 0),
  sd = 2,
  r = 0.8, 
  labelnames = c("condition", "cheerful", "sad", "voice", "human", "robot", "cartoon")
)

exact_result <- ANOVA_exact(
  design_result,
  alpha_level = 0.05,
  verbose = FALSE,
  emm = TRUE,
  contrast_type = "pairwise"
)

## -----------------------------------------------------------------------------
exact_result$main_results
head(exact_result$emm_results)

## -----------------------------------------------------------------------------
knitr::kable(exact_result$emmeans$emmeans)
knitr::kable(exact_result$emmeans$contrasts)

## -----------------------------------------------------------------------------
head(emmeans_power(exact_result$emmeans$contrasts))

## -----------------------------------------------------------------------------
simple_condition_effects <- emmeans(
  exact_result$emmeans$emmeans,
  specs = ~ condition | voice
)

emmeans_power(pairs(simple_condition_effects))

## -----------------------------------------------------------------------------
emmeans_power(test(simple_condition_effects, null = 0.5))

## -----------------------------------------------------------------------------
custom_contrast <- contrast(
  exact_result$emmeans$emmeans,
  list(robot_vs_sad_human = c(0, 0, 0, 1, -0.5, -0.5))
)

emmeans_power(custom_contrast)

## -----------------------------------------------------------------------------
n_contrasts <- nrow(as.data.frame(simple_condition_effects))

emmeans_power(
  pairs(simple_condition_effects),
  alpha_level = 0.05 / n_contrasts
)

## -----------------------------------------------------------------------------
emmeans_power(
  pairs(simple_condition_effects)[1],
  alpha_level = 2 * 0.05
)

## -----------------------------------------------------------------------------
emmeans_power(
  pairs(simple_condition_effects, side = "equivalence", delta = 0.3)[2]
)

## -----------------------------------------------------------------------------
voice_by_condition <- joint_tests(
  exact_result$emmeans$emmeans,
  by = "condition"
)

emmeans_power(voice_by_condition, alpha_level = 0.05 / 2)

