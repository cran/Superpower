## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(Superpower)
nsims = 250

## -----------------------------------------------------------------------------
Superpower_options()

## -----------------------------------------------------------------------------
Superpower_options("verbose")
Superpower_options("verbose" = FALSE) 
Superpower_options("verbose")

## ---- fig.width=7, fig.height=4, echo=FALSE, message=FALSE, warning=FALSE-----
design_result <- ANOVA_design(design = "2b*2w*2b",
                              n = 10, 
                              mu = c(1, 2, 3, 4, 5, 6, 7, 8), 
                              sd = 1, 
                              r = 0.9,
                              plot = FALSE)

design_result

plot(design_result)


## -----------------------------------------------------------------------------
(((2*2)^2)-(2*2))/2

## -----------------------------------------------------------------------------
(((2*2*4)^2)-(2*2*4))/2

## ---- fig.width=5, fig.height=4-----------------------------------------------
design_result <- ANOVA_design(design = "2w*2w",
                              n = 80,
                              mu = c(1.1, 1.2, 1.3, 1.4),
                              sd = 2,
                              r <- c(0.91, 0.92, 0.93, 0.94, 0.95, 0.96),
                              plot = FALSE)

design_result

plot(design_result)

## -----------------------------------------------------------------------------
design_result$cor_mat

## ---- fig.width=7, fig.height=4-----------------------------------------------
design_result <- ANOVA_design(design = "2b*2w",
                   n = 40, 
                   mu = c(1.03, 1.41, 0.98, 1.01), 
                   sd = 1.03, 
                   r = 0.8, 
                   label_list = list("voice"  = c("human", "robot"),
                                     "emotion" = c( "cheerful", "sad")),
                   plot = TRUE)

## -----------------------------------------------------------------------------
power_result_vig_1 <- ANOVA_power(design_result, 
                                  alpha = 0.05, 
                                  nsims = nsims, 
                                  seed = 1234)


## -----------------------------------------------------------------------------
knitr::kable(confint(power_result_vig_1, level = .98))

## ---- fig.width=7, fig.height=4-----------------------------------------------
design <- "2b"
n <- 100
mu <- c(24, 26.2)
sd <- 6.4
label_list = list("condition" = c("control", "pet")) #

design_result <- ANOVA_design(design = design,
                              n = n,
                              mu = mu, 
                              sd = sd, 
                              label_list = label_list)

## -----------------------------------------------------------------------------
power_result_vig_2 <- ANOVA_power(design_result, 
                                  nsims = nsims, 
                                  seed = 1234)
#Note we do not specify any correlation in the ANOVA_design function (default r = 0), nor do we specify an alpha in the ANOVA_power function (default is 0.05)

knitr::kable(confint(power_result_vig_2, level = .98))

## -----------------------------------------------------------------------------
library(pwr)
pwr.t.test(d = 2.2/6.4,
           n = 100,
           sig.level = 0.05,
           type = "two.sample",
           alternative = "two.sided")$power


## -----------------------------------------------------------------------------
pwr.anova.test(n = 100,
               k = 2,
               f = 0.171875,
               sig.level = 0.05)$power

## ---- fig.width=7, fig.height=4-----------------------------------------------
design_result <- ANOVA_design(design = "2b",
                   n = 100, 
                   mu = c(24, 26.2), 
                   sd = 6.4, 
                   label_list = list("condition" = c("control", "pet")),
                   plot = TRUE)

ANOVA_exact(design_result,
            verbose = FALSE)$main_results$power
# power of 67.7 is a bit low. Let's increase it a bit to n = 150 to see if we are closer to our goal of 90% power.

design_result <- ANOVA_design(design = "2b",
                   n = 150, 
                   mu = c(24, 26.2), 
                   sd = 6.4, 
                   label_list = list("condition" = c("control", "pet")),
                   plot = FALSE)

ANOVA_exact(design_result,
            verbose = FALSE)$main_results$power
# Close, but not there yet. Let's try n = 175 

design_result <- ANOVA_design(design = "2b",
                   n = 175, 
                   mu = c(24, 26.2), 
                   sd = 6.4, 
                   label_list = list("condition" = c("control", "pet")),
                   plot = FALSE)

ANOVA_exact(design_result,
            verbose = FALSE)$main_results$power
#Very close. Let's add a few more and try n = 180

design_result <- ANOVA_design(design = "2b",
                   n = 180, 
                   mu = c(24, 26.2), 
                   sd = 6.4, 
                   label_list = list("condition" = c("control", "pet")),
                   plot = FALSE)

ANOVA_exact(design_result,
            verbose = FALSE)$main_results$power


## ---- fig.width=7, fig.height=4-----------------------------------------------
plot_power(design_result, min_n = 10, max_n = 250)

## ----morey1, fig.width=7, fig.height=4----------------------------------------
morey_plot.ttest(
  es = seq(.1, .5, .01),
  n = c(10, 20),
  alpha_level = c(.05, .075),
  type = "paired",
  alternative = "one.sided"
)

## ----morey2, fig.width=7, fig.height=6----------------------------------------
morey_plot.ftest(
  es = seq(.1, .5, .01),
  num_df = c(1, 2),
  den_df = c(20,30),
  alpha_level = c(.05, .075),
  liberal_lambda = FALSE
)

