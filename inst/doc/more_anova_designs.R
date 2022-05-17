## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE
)

## -----------------------------------------------------------------------------
library(Superpower)
Superpower_options(verbose = FALSE,
                   plot = FALSE)
nsims = 250

## ---- fig.width=7, fig.height=4-----------------------------------------------
design <- "3b"
n <- 50
mu <- c(24, 26.2, 26.6)
sd <- 6.4
label_list = list("condition" = c("control", "cat", "dog"))

design_result <- ANOVA_design(design = design,
                   n = n, 
                   mu = mu, 
                   sd = sd, 
                   label_list = label_list,
                   plot = TRUE)

## -----------------------------------------------------------------------------
library(pwr)
pwr.t.test(d = 2.2/6.4,
           n = 50,
           sig.level = 0.05,
           type="two.sample",
           alternative="two.sided")$power

pwr.t.test(d = 2.6/6.4,
           n = 50,
           sig.level = 0.05,
           type="two.sample",
           alternative="two.sided")$power

pwr.t.test(d = 0.4/6.4,
           n = 50,
           sig.level = 0.05,
           type="two.sample",
           alternative="two.sided")$power


## ----eval=FALSE---------------------------------------------------------------
#  plot_power(design_result, min_n = 10, max_n = 250)

## ---- fig.width=7, fig.height=4-----------------------------------------------
design <- "3b"
n <- 134
mu <- c(24, 26.2, 26.6)
sd <- 6.4
label_list = list("condition" = c("control", "cat", "dog"))

design_result <- ANOVA_design(design = design,
                   n = n, 
                   mu = mu, 
                   sd = sd, 
                   label_list = label_list,
                   plot = FALSE) #do not print the plot (same as above)

ANOVA_exact2(design_result,
             emm = TRUE)

n <- 180
design_result <- ANOVA_design(design = design,
                   n = n, 
                   mu = mu, 
                   sd = sd, 
                   label_list = label_list,
                   plot = FALSE) #do not print the plot (same as above)

ANOVA_exact2(design_result,
             emm = TRUE)

n <- 5380
design_result <- ANOVA_design(design = design,
                   n = n, 
                   mu = mu, 
                   sd = sd, 
                   label_list = label_list,
                   plot = FALSE) #do not print the plot (same as above)

ANOVA_exact2(design_result,
             emm = TRUE)


## ---- out.width = "600px", echo=FALSE-----------------------------------------
knitr::include_graphics("screenshots/gpower_9.png")

## ---- out.width = "600px", echo=FALSE-----------------------------------------
knitr::include_graphics("screenshots/gpower_1.png")

## ---- out.width = "600px", echo=FALSE-----------------------------------------
knitr::include_graphics("screenshots/gpower_11.png")

## ---- out.width = "600px", echo=FALSE-----------------------------------------
knitr::include_graphics("screenshots/gpower_10.png")

## ---- fig.width=7, fig.height=4-----------------------------------------------
K <- 2
n <- 34
sd <- 1
r <- 0.5
alpha = 0.05
f <- 0.25
f2 <- f^2
ES <- f2/(f2+1)
ES
mu <- mu_from_ES(K = K, ES = ES)
design = paste(K,"w",sep="")
label_list <- list("speed" =c("fast", "slow"))

design_result <- ANOVA_design(design = design,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r, 
                              label_list = label_list,
                              plot = TRUE)

ANOVA_exact2(design_result,
            emm = TRUE)

## ---- fig.width=7, fig.height=4-----------------------------------------------
K <- 2
n <- 34
sd <- 1
r <- 0.7
alpha = 0.05
f <- 0.25
f2 <- f^2
ES <- f2/(f2+1)
ES
mu <- mu_from_ES(K = K, ES = ES)
design = paste(K,"w",sep="")
label_list <- list("speed" = c("fast", "slow"))

design_result <- ANOVA_design(design = design,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r, 
                              label_list = label_list,
                              plot = TRUE)

ANOVA_exact2(design_result,
            emm = TRUE)

## -----------------------------------------------------------------------------
mu <- c(3.8, 4.2, 4.3)
sd <- 0.9
f <- sqrt(sum((mu-mean(mu))^2)/length(mu))/sd #Cohen, 1988, formula 8.2.1 and 8.2.2
f

r <- 0.7
(4.2-3.8)/0.9/sqrt(2*(1-r))
(4.3-3.8)/0.9/sqrt(2*(1-r))
(4.3-4.2)/0.9/sqrt(2*(1-r))


## ---- fig.width=7, fig.height=4-----------------------------------------------
K <- 3
n <- 20
sd <- 1
r <- 0.8
alpha = 0.05
f <- 0.25
f2 <- f^2
ES <- f2/(f2+1)
ES
mu <- mu_from_ES(K = K, ES = ES)
sqrt(sum((mu-mean(mu))^2)/length(mu))/sd #Cohen, 1988, formula 8.2.1 and 8.2.2
design = paste(K,"w",sep="")
label_list <- list("speed" = c("fast", "medium", "slow"))

design_result <- ANOVA_design(design = design,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r, 
                              label_list = label_list,
                              plot = TRUE)
power_result = ANOVA_power(design_result,
                           nsims = 250)

confint(power_result,
        param = "main_results",
        level = .98)


## ---- out.width = "600px", echo=FALSE-----------------------------------------
knitr::include_graphics("screenshots/gpower_12.png")

## ---- out.width = "600px", echo=FALSE-----------------------------------------
knitr::include_graphics("screenshots/gpower_14.png")

## ---- out.width = "600px", echo=FALSE-----------------------------------------
knitr::include_graphics("screenshots/gpower_5.png")

## ---- fig.width=7, fig.height=4-----------------------------------------------
mu <- c(-0.25, 0.25, 0.25, -0.25)
n <- 23
sd <- 1
r <- 0.5
design = "2w*2b"
design_result <- ANOVA_design(design = design,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r,
                              plot = TRUE)

ANOVA_exact2(design_result,
             emm = TRUE)


## ---- out.width = "600px", echo=FALSE-----------------------------------------
knitr::include_graphics("screenshots/gpower_6.png")

## ---- fig.width=7, fig.height=4-----------------------------------------------
mu <- c(-0.25, 0.25, 0.25, -0.25)
n <- 23
sd <- 1
r <- 0.7
design = "2w*2b"
design_result <- ANOVA_design(design = design,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r,
                              plot = TRUE)

ANOVA_exact2(design_result,
            emm = TRUE)

## ----mean-plot, fig.width=7, fig.height=4, echo=FALSE, message=FALSE, warning=FALSE, results='hide', fig.cap="Vizualization for the expected means and confidence intervals for a crossover interaction."----

# To save time compiling this vignette, we ran these simulations once, and stored them
design_result_cross_80 <- ANOVA_design(design = "2b*2b",
                                       n = 80, 
                                       mu = c(1, 0, 0, 1), 
                                       sd = 2, 
                                       label_list = list("condition" = c("cheerful","sad"), 
                                                      "voice" = c("human", "robot")))

plot(design_result_cross_80)
# 
power_result_cross_80 <- ANOVA_exact(design_result_cross_80, alpha_level = 0.05, verbose = FALSE)


# To save time compiling this vignette, we ran these simulations once, and stored them
design_result_cross_40 <- ANOVA_design(design = "2b*2b",
                                       n = 40, 
                                       mu = c(1, 0, 0, 1), 
                                       sd = 2, 
                                       label_list = list("condition" = c("cheerful", "sad"), 
                                                                                                            "voice" = c("human", "robot")), plot = FALSE)
# 
power_result_cross_40 <- ANOVA_exact(design_result_cross_40, alpha_level = 0.05, verbose = FALSE)

#Analytic solution
power_twoway_between(design_result_cross_40)$power_A
power_twoway_between(design_result_cross_40)$power_B
power_twoway_between(design_result_cross_40)$power_AB

## ----sim-interaction-2,  message=FALSE, warning=FALSE-------------------------

# To save time compiling this vignette, we ran these simulations once, and stored them
design_result_ordinal <- ANOVA_design(design = "2b*2b", 
                                      n = 160, 
                                      mu = c(1, 0, 0, 0), 
                                      sd = 2, 
                                      label_list = list("condition" = c("cheerful", "sad"),
                                                     "voice" = c("human", "robot")))
# 
power_result_ordinal <- ANOVA_power(design_result_ordinal, 
                                    alpha_level = 0.05, 
                                    p_adjust = "none", 
                                    seed = 2019, 
                                    nsims = nsims)

knitr::kable(confint(power_result_ordinal, 
                     parm = "main_results",
                     level = .98))

knitr::kable(confint(power_result_ordinal, 
                     parm = "pc_results",
                     level = .98))

#Analytic solution
power_twoway_between(design_result_ordinal)$power_A
power_twoway_between(design_result_ordinal)$power_B
power_twoway_between(design_result_ordinal)$power_AB
power_twoway_between(design_result_ordinal)$Cohen_f_AB
power_twoway_between(design_result_ordinal)$eta_p_2_AB

## ----sim-interaction-3, echo=FALSE, message=FALSE, warning=FALSE, include=FALSE----
design_result <-ANOVA_design(design = "2b*2b",
                             n = 40,
                             mu = c(2, 0, 0, 0),
                             sd = 2,
                             label_list = list("condition" = c("cheerful", "sad"),
                                            "voice" = c( "human", "robot")))
#Analytic solution
power_twoway_between(design_result)$power_A
power_twoway_between(design_result)$power_B
power_twoway_between(design_result)$power_AB
power_twoway_between(design_result)$Cohen_f_AB
power_twoway_between(design_result)$eta_p_2_AB

design_result <- ANOVA_design(design = "2b*2b*2b", 
                              n = 20, 
                              mu = c(4, 0, 0, 0, 0, 0, 0, 0), 
                              sd = 2,
                              label_list = list(
                                "condition" = c("cheerful", "sad"),
                                "voice" = c("human", "robot"),
                                "factor_c" = c("c1", "c2")
                              ))
power_threeway_between(design_result)$power_ABC
power_threeway_between(design_result)$Cohen_f_ABC


## ---- fig.width=7, fig.height=4-----------------------------------------------
design <- "3b*3b"
n <- 20
mu <- c(20, 20, 20, 20, 20, 20, 20, 20, 25) 
# Enter means in the order that matches the labels below.
sd <- 5
label_list <- list("Factor_A" = c("a1", "a2", "a3"), 
                "Factor_B" = c("b1", "b2", "b3")) #
# the label names should be in the order of the means specified above.

design_result <- ANOVA_design(design = design,
                   n = n, 
                   mu = mu, 
                   sd = sd, 
                   label_list = label_list)
plot(design_result)

res = ANOVA_exact2(design_result, 
            alpha_level = 0.05)

print(res)


## -----------------------------------------------------------------------------

power_res <- power_twoway_between(design_result) #using default alpha level of .05

power_res$power_A
power_res$power_B
power_res$power_AB


## ---- eval=FALSE--------------------------------------------------------------
#  # Not Run
#  plot_power(design_result, min_n = 20, max_n = 100)

## ---- out.width = "600px", echo=FALSE-----------------------------------------
knitr::include_graphics("screenshots/PS2000.gif")

## ---- fig.width=7, fig.height=4-----------------------------------------------
mu = c(2,1,4,2) 
n <- 20
sd <- 5
r <- c(
  0.8, 0.5, 0.4,
       0.4, 0.5,
            0.8
  )

design = "2w*2w"
label_list = list("A" =c( "a1", "a2"),
               "B" = c("b1", "b2"))
design_result <- ANOVA_design(design = design,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r, 
                              label_list = label_list,
                              plot = TRUE)

power_result_vig_4 = ANOVA_power(design_result,
                                 nsims = nsims)

knitr::kable(confint(power_result_vig_4, level = .98))

## ----eval=FALSE---------------------------------------------------------------
#  # Not Run
#  ANOVA_exact2(design_result = design_result)

