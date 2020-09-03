## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(Superpower)
nsims = 100000

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
                   labelnames = c("voice", "human", "robot", "emotion", "cheerful", "sad"),
                   plot = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  power_result_vig_1 <- ANOVA_power(design_result,
#                                    alpha = 0.05,
#                                    nsims = nsims,
#                                    seed = 1234)
#  
#  

## ---- fig.width=7, fig.height=4-----------------------------------------------
design <- "2b"
n <- 100
mu <- c(24, 26.2)
sd <- 6.4
labelnames <- c("condition", "control", "pet") #

design_result <- ANOVA_design(design = design,
                              n = n,
                              mu = mu, 
                              sd = sd, 
                              labelnames = labelnames)

## ---- eval=FALSE--------------------------------------------------------------
#  power_result_vig_2 <- ANOVA_power(design_result, nsims = nsims, seed = 1234)
#  #Note we do not specify any correlation in the ANOVA_design function (default r = 0), nor do we specify an alpha in the ANOVA_power function (default is 0.05)

## ---- include=FALSE-----------------------------------------------------------
power_result_vig_2 <- readRDS(file = "sim_data/power_result_vig_2.rds")



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
                   labelnames = c("condition", "control", "pet"),
                   plot = TRUE)

ANOVA_exact(design_result,
            verbose = FALSE)$main_results$power
# power of 67.7 is a bit low. Let's increase it a bit to n = 150 to see if we are closer to our goal of 90% power.

design_result <- ANOVA_design(design = "2b",
                   n = 150, 
                   mu = c(24, 26.2), 
                   sd = 6.4, 
                   labelnames = c("condition", "control", "pet"),
                   plot = FALSE)

ANOVA_exact(design_result,
            verbose = FALSE)$main_results$power
# Close, but not there yet. Let's try n = 175 

design_result <- ANOVA_design(design = "2b",
                   n = 175, 
                   mu = c(24, 26.2), 
                   sd = 6.4, 
                   labelnames = c("condition", "control", "pet"),
                   plot = FALSE)

ANOVA_exact(design_result,
            verbose = FALSE)$main_results$power
#Very close. Let's add a few more and try n = 180

design_result <- ANOVA_design(design = "2b",
                   n = 180, 
                   mu = c(24, 26.2), 
                   sd = 6.4, 
                   labelnames = c("condition", "control", "pet"),
                   plot = FALSE)

ANOVA_exact(design_result,
            verbose = FALSE)$main_results$power


## ---- fig.width=7, fig.height=4-----------------------------------------------
plot_power(design_result, min_n = 10, max_n = 250)

## ---- fig.width=7, fig.height=4-----------------------------------------------
design_result <- ANOVA_design(design = "2b",
                   n = 180, 
                   mu = c(24, 26), 
                   sd = 6.4, 
                   labelnames = c("condition", "control", "pet"),
                   plot = FALSE)

plot_power(design_result, max_n = 250)

## ---- fig.width=7, fig.height=4-----------------------------------------------
design_result <- ANOVA_design(design = "2b",
                   n = 180, 
                   mu = c(24, 26), 
                   sd = 6.8, 
                   labelnames = c("condition", "control", "pet"),
                   plot = FALSE)

plot_power(design_result, min_n = 10, max_n = 250)

## ---- fig.width=7, fig.height=4-----------------------------------------------
design <- "3b"
n <- 50
mu <- c(24, 26.2, 26.6)
sd <- 6.4
labelnames <- c("condition", "control", "cat", "dog") #

design_result <- ANOVA_design(design = design,
                   n = n, 
                   mu = mu, 
                   sd = sd, 
                   labelnames = labelnames,
                   plot = TRUE)

ANOVA_exact(design_result,
            emm = TRUE) # Pairwise emmeans will automatically run

## -----------------------------------------------------------------------------
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


## ---- fig.width=7, fig.height=4-----------------------------------------------
plot_power(design_result, min_n = 10, max_n = 250)

## ---- fig.width=7, fig.height=4-----------------------------------------------
design <- "3b"
n <- 134
mu <- c(24, 26.2, 26.6)
sd <- 6.4
labelnames <- c("condition", "control", "cat", "dog") #

design_result <- ANOVA_design(design = design,
                   n = n, 
                   mu = mu, 
                   sd = sd, 
                   labelnames = labelnames,
                   plot = FALSE) #do not print the plot (same as above)

ANOVA_exact(design_result,
            emm = TRUE)

n <- 180
design_result <- ANOVA_design(design = design,
                   n = n, 
                   mu = mu, 
                   sd = sd, 
                   labelnames = labelnames,
                   plot = FALSE) #do not print the plot (same as above)

ANOVA_exact(design_result,
            emm = TRUE)

n <- 5380
design_result <- ANOVA_design(design = design,
                   n = n, 
                   mu = mu, 
                   sd = sd, 
                   labelnames = labelnames,
                   plot = FALSE) #do not print the plot (same as above)

ANOVA_exact(design_result,
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
labelnames <- c("speed", "fast", "slow")

design_result <- ANOVA_design(design = design,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r, 
                              labelnames = labelnames,
                              plot = TRUE)

ANOVA_exact(design_result,
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
labelnames <- c("speed", "fast", "slow")

design_result <- ANOVA_design(design = design,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r, 
                              labelnames = labelnames,
                              plot = TRUE)

ANOVA_exact(design_result,
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
labelnames <- c("speed", "fast", "medium", "slow")

design_result <- ANOVA_design(design = design,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r, 
                              labelnames = labelnames,
                              plot = TRUE)

# power_result_vig_3 <- ANOVA_power(design_result, nsims = nsims)
# saveRDS(power_result_vig_3, file = "sim_data/power_result_vig_3.rds")
power_result_vig_3 <- readRDS(file = "sim_data/power_result_vig_3.rds")


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

ANOVA_exact(design_result,
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

ANOVA_exact(design_result,
            emm = TRUE)

## ----mean-plot, fig.width=7, fig.height=4, echo=FALSE, message=FALSE, warning=FALSE, results='hide', fig.cap="Vizualization for the expected means and confidence intervals for a crossover interaction."----

# To save time compiling this vignette, we ran these simulations once, and stored them
design_result_cross_80 <- ANOVA_design(design = "2b*2b", n = 80, mu = c(1, 0, 0, 1), sd = 2, labelnames = c("condition", "cheerful", "sad", "voice", "human", "robot"), plot = TRUE)
# 
# power_result_cross_80 <- ANOVA_exact(design_result_cross_80, alpha_level = 0.05, verbose = FALSE)
power_result_cross_80 <- readRDS(file = "sim_data/power_result_cross_80.rds")

# To save time compiling this vignette, we ran these simulations once, and stored them
design_result_cross_40 <- ANOVA_design(design = "2b*2b", n = 40, mu = c(1, 0, 0, 1), sd = 2, labelnames = c("condition", "cheerful", "sad", "voice", "human", "robot"), plot = FALSE)
# 
# power_result_cross_40 <- ANOVA_exact(design_result_cross_40, alpha_level = 0.05, verbose = FALSE)
power_result_cross_40 <- readRDS(file = "sim_data/power_result_cross_40.rds")

#Analytic solution
power_twoway_between(design_result_cross_40)$power_A
power_twoway_between(design_result_cross_40)$power_B
power_twoway_between(design_result_cross_40)$power_AB

## ----sim-interaction-2, echo=FALSE, message=FALSE, warning=FALSE, include=FALSE----

# To save time compiling this vignette, we ran these simulations once, and stored them
design_result_ordinal <- ANOVA_design(design = "2b*2b", n = 160, mu = c(1, 0, 0, 0), sd = 2, labelnames = c("condition", "cheerful", "sad", "voice", "human", "robot"))
# 
# power_result_ordinal <- ANOVA_power(design_result_ordinal, alpha_level = 0.05, p_adjust = "none", seed = 2019, nsims = nsims)
power_result_ordinal <- readRDS(file = "sim_data/power_result_ordinal.rds")
power_result_ordinal

#Analytic solution
power_twoway_between(design_result_ordinal)$power_A
power_twoway_between(design_result_ordinal)$power_B
power_twoway_between(design_result_ordinal)$power_AB
power_twoway_between(design_result_ordinal)$Cohen_f_AB
power_twoway_between(design_result_ordinal)$eta_p_2_AB

## ----sim-interaction-3, echo=FALSE, message=FALSE, warning=FALSE, include=FALSE----
design_result <- ANOVA_design(design = "2b*2b", n = 40, mu = c(2, 0, 0, 0), sd = 2, labelnames = c("condition", "cheerful", "sad", "voice", "human", "robot"))
#Analytic solution
power_twoway_between(design_result)$power_A
power_twoway_between(design_result)$power_B
power_twoway_between(design_result)$power_AB
power_twoway_between(design_result)$Cohen_f_AB
power_twoway_between(design_result)$eta_p_2_AB

design_result <- ANOVA_design(design = "2b*2b*2b", n = 20, mu = c(4, 0, 0, 0, 0, 0, 0, 0), sd = 2, labelnames = c("condition", "cheerful", "sad", "voice", "human", "robot", "factor_c", "c1", "c2"))
power_threeway_between(design_result)$power_ABC
power_threeway_between(design_result)$Cohen_f_ABC


## ---- fig.width=7, fig.height=4-----------------------------------------------
design <- "3b*3b"
n <- 20
mu <- c(20, 20, 20, 20, 20, 20, 20, 20, 25) 
# Enter means in the order that matches the labels below.
sd <- 5
labelnames <- c("Factor_A", "a1", "a2", "a3", "Factor_B", "b1", "b2", "b3") #
# the label names should be in the order of the means specified above.

design_result <- ANOVA_design(design = design,
                   n = n, 
                   mu = mu, 
                   sd = sd, 
                   labelnames = labelnames,
                   plot = TRUE)

ANOVA_exact(design_result, alpha_level = 0.05)


## -----------------------------------------------------------------------------

power_res <- power_twoway_between(design_result) #using default alpha level of .05

power_res$power_A
power_res$power_B
power_res$power_AB


## ---- fig.width=7, fig.height=8-----------------------------------------------
plot_power(design_result, min_n = 20, max_n = 100)

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
labelnames = c("A", "a1", "a2", "B", "b1", "b2")
design_result <- ANOVA_design(design = design,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r, 
                              labelnames = labelnames,
                              plot = TRUE)

# power_result_vig_4 <- ANOVA_power(design_result, nsims = nsims)
# saveRDS(power_result_vig_4, file = "sim_data/power_result_vig_4.rds")
power_result_vig_4 <- readRDS(file = "sim_data/power_result_vig_4.rds")
power_result_vig_4$main_results

## -----------------------------------------------------------------------------
ANOVA_exact(design_result = design_result)

