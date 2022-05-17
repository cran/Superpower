## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  # collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library("Superpower")

## -----------------------------------------------------------------------------
power_oneway_ancova(
  mu = c(400,450,500),
  n_cov = 3,
  sd = 100,
  r2 = .25,
  alpha_level = .05,
  #n = c(17,17,17),
  beta_level = .2,
  round_up = TRUE,
  type = "approx"
)

## -----------------------------------------------------------------------------
power_oneway_ancova(
  mu = c(400,450,500),
  n_cov = 3,
  sd = 100,
  r2 = .25,
  alpha_level = .05,
  #n = c(17,17,17),
  beta_level = .2,
  round_up = TRUE,
  type = "exact"
)

## -----------------------------------------------------------------------------
# Run function
res1 = ANCOVA_analytic(
  design = "2b*3b",
  mu = c(400, 450, 500,
         400, 500, 600),
  n_cov = 3,
  sd = 100,
  r2 = .25,
  alpha_level = .05,
  #n = 17,
  beta_level = .2,
  round_up = TRUE
)

# Print main results
res1


## -----------------------------------------------------------------------------
res1$aov_list$a

res1$aov_list$b

res1$aov_list$ab

## -----------------------------------------------------------------------------
plot(res1)

## -----------------------------------------------------------------------------
des1 = ANOVA_design(  design = "2b*3b",
  mu = c(400, 450, 500,
         400, 500, 600),
  n = 17,
  sd = 100)

res2 = ANCOVA_analytic(
  design_result = des1,
  n_cov = 3,
  r2 = .25,
  alpha_level = .05,
  round_up = TRUE
)

res2

## -----------------------------------------------------------------------------

ANCOVA_analytic(design = "2b",
                mu = c(0,1),
                n = 15,
                cmats = list(test = matrix(c(-1,1),
                                              nrow = 1)),
                sd = 1,
                r2 = .2,
                n_cov = 1)$con_list$test

# Same result
ANCOVA_contrast(cmat = c(-1,1),
                n = 15,
                mu = c(0,1),
                sd = 1,
                r2 = .2,
                n_cov = 1)

