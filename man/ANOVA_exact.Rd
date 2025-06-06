% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ANOVA_exact.R
\name{ANOVA_exact}
\alias{ANOVA_exact}
\alias{ANOVA_exact2}
\title{Simulates an exact dataset (mu, sd, and r represent empirical, not population, mean and covariance matrix) from the design to calculate power}
\usage{
ANOVA_exact(
  design_result,
  correction = Superpower_options("correction"),
  alpha_level = Superpower_options("alpha_level"),
  verbose = Superpower_options("verbose"),
  emm = Superpower_options("emm"),
  emm_model = Superpower_options("emm_model"),
  contrast_type = Superpower_options("contrast_type"),
  liberal_lambda = Superpower_options("liberal_lambda"),
  emm_comp
)

ANOVA_exact2(
  design_result,
  correction = Superpower_options("correction"),
  alpha_level = Superpower_options("alpha_level"),
  verbose = Superpower_options("verbose"),
  emm = Superpower_options("emm"),
  emm_model = Superpower_options("emm_model"),
  contrast_type = Superpower_options("contrast_type"),
  emm_comp,
  liberal_lambda = Superpower_options("liberal_lambda")
)
}
\arguments{
\item{design_result}{Output from the ANOVA_design function}

\item{correction}{Set a correction of violations of sphericity. This can be set to "none", "GG" Greenhouse-Geisser, and "HF" Huynh-Feldt}

\item{alpha_level}{Alpha level used to determine statistical significance}

\item{verbose}{Set to FALSE to not print results (default = TRUE)}

\item{emm}{Set to FALSE to not perform analysis of estimated marginal means}

\item{emm_model}{Set model type ("multivariate", or "univariate") for estimated marginal means}

\item{contrast_type}{Select the type of comparison for the estimated marginal means. Default is pairwise. See the emmeans package on "contrast-methods" for more details on acceptable methods.}

\item{liberal_lambda}{Logical indicator of whether to use the liberal (cohen_f^2\*(num_df+den_df)) or conservative (cohen_f^2\*den_df) calculation of the noncentrality (lambda) parameter estimate. Default is FALSE.}

\item{emm_comp}{Set the comparisons for estimated marginal means comparisons. This is a factor name (a), combination of factor names (a+b), or for simple effects a | sign is needed (a|b)}
}
\value{
Returns dataframe with simulation data (power and effect sizes!), anova results and simple effect results, plot of exact data, and alpha_level. Note: Cohen's f = sqrt(pes/1-pes) and the noncentrality parameter is = f^2*df(error)

\describe{
  \item{\code{"dataframe"}}{A dataframe of the simulation result.}
  \item{\code{"aov_result"}}{\code{aov} object returned from \link[afex]{aov_car}.}
  \item{\code{"main_result"}}{The power analysis results for ANOVA level effects.}
  \item{\code{"pc_results"}}{The power analysis results for the pairwise (t-test) comparisons.}
  \item{\code{"emm_results"}}{The power analysis results of the pairwise comparison results.}
  \item{\code{"manova_results"}}{Default is "NULL". If a within-subjects factor is included, then the power of the multivariate (i.e. MANOVA) analyses will be provided.}
  \item{\code{"alpha_level"}}{The alpha level, significance cut-off, used for the power analysis.}
  \item{\code{"method"}}{Record of the function used to produce the simulation}
  \item{\code{"plot"}}{A plot of the dataframe from the simulation; should closely match the meansplot in \code{\link{ANOVA_design}}}

}
}
\description{
Simulates an exact dataset (mu, sd, and r represent empirical, not population, mean and covariance matrix) from the design to calculate power
}
\section{Functions}{
\itemize{
\item \code{ANOVA_exact2()}: An extension of ANOVA_exact that uses the effect sizes calculated from very large sample size empirical simulation. This allows for small sample sizes, where ANOVA_exact cannot, while still accurately estimating power. However, model objects (emmeans and aov) are not included as output, and pairwise (t-test) results are not currently supported.

}}
\section{Warnings}{

Varying the sd or r (e.g., entering multiple values) violates assumptions of homoscedascity and sphericity respectively
}

\examples{
## Set up a within design with 2 factors, each with 2 levels,
## with correlation between observations of 0.8,
## 40 participants (who do all conditions), and standard deviation of 2
## with a mean pattern of 1, 0, 1, 0, conditions labeled 'condition' and
## 'voice', with names for levels of "cheerful", "sad", amd "human", "robot"
design_result <- ANOVA_design(design = "2w*2w", n = 40, mu = c(1, 0, 1, 0),
      sd = 2, r = 0.8, labelnames = c("condition", "cheerful",
      "sad", "voice", "human", "robot"))
exact_result <- ANOVA_exact(design_result, alpha_level = 0.05)
}
