## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(Superpower)

## ----fig.height=5, fig.width=6------------------------------------------------
res <- optimal_alpha(power_function = "power.t.test(delta = .5, sd = 1, n=64, 
                     sig.level = x, type='two.sample', 
                     alternative='two.sided')$power")

print(res)

## ----fig.height=5, fig.width=6------------------------------------------------
res2 <- optimal_alpha(power_function = "power.t.test(delta = .5, sd = 1, n=64, 
                      sig.level = x, type='two.sample', 
                      alternative='two.sided')$power", error = "balance")

print(res2)

## ----fig.height=5, fig.width=6------------------------------------------------
res3 <- optimal_alpha(power_function = "power.t.test(delta = .5, sd = 1, n=100, 
                      sig.level = x, type='two.sample', 
                      alternative='two.sided')$power",
                      error = "minimal", costT1T2 = 4)

print(res3)

## ----fig.height=5, fig.width=6------------------------------------------------
res4 <- optimal_alpha(power_function = "power.t.test(delta = .5, sd = 1, n=100, 
                      sig.level = x, type='two.sample', 
                      alternative='two.sided')$power", 
                      error = "minimal", priorH1H0 = 2)

print(res4)

## -----------------------------------------------------------------------------
power.ftest(
  num_df = 1,
  den_df = 128,
  cohen_f = .22,
  alpha_level = .045,
  beta_level = NULL,
  liberal_lambda = FALSE
)

## ----fig.width=5, fig.height=4------------------------------------------------
design_result <- ANOVA_design(design = "2b*2w",
                   n = 40, 
                   mu = c(1.03, 1.41, 0.98, 1.01), 
                   sd = 1.03, 
                   r = 0.8, 
                   labelnames = c("voice", "human", "robot", 
                                  "emotion", "cheerful", "sad"),
                   plot = TRUE)

## -----------------------------------------------------------------------------
exact2_res = ANOVA_exact2(design_result, verbose = FALSE)

knitr::kable(exact2_res$main_results)

## -----------------------------------------------------------------------------
power.ftest(num_df = exact2_res$anova_table$num_df[3],
            den_df = exact2_res$anova_table$den_df[3],
            cohen_f = exact2_res$main_results$cohen_f[3],
            alpha_level = NULL,
            beta_level = .2)

## ----fig.height=5, fig.width=6------------------------------------------------
res5 = optimal_alpha("power.ftest(num_df = exact2_res$anova_table$num_df[3],
            den_df = exact2_res$anova_table$den_df[3],
            cohen_f = exact2_res$main_results$cohen_f[3],
            alpha_level = x)$power/100",
            error = "minimal")

print(res5)

## ----ANOVA_compromise---------------------------------------------------------
comp_res = ANOVA_compromise(design_result,
                            emm = TRUE)

## -----------------------------------------------------------------------------
#ANOVA results
knitr::kable(comp_res$aov_comp)

#MANOVA results
knitr::kable(comp_res$manova_comp)

#emmeans results
knitr::kable(comp_res$emmeans_comp)

## ----comp_plots, fig.height=5, fig.width=6------------------------------------
comp_res$aov_plotlist

