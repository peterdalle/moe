#' Calculate margin of error for simple probability samples
#'
#' \code{moe} calculates margin of error and confidence intervals for simple probability
#' samples, as well as results formatted in accordance with American Psychological Association standards (APA6).
#'
#' @param proportion value between 0 and 1 indicating the proportion, such as 0.30 for 30 percent.
#' @param n sample size.
#' @param conf.level confidence level (defaults to \code{0.95}).
#' @param digits number of decimal digits used when formatting the results as APA and human-readable messages (defaults to \code{2}).
#' @param population.correction whether or not results should be corrected by population size (defaults to \code{FALSE}), using the Finite Population Correction technique.
#' @param population.size population size used by the population correction (defaults to \code{NULL}). Only used if \code{population.correction} is set to \code{TRUE}.
#' @return a list with \code{margin.of.error} (margin of error), \code{conf.level} (confidence level), \code{conf.lower} (confidence interval lower bound), \code{conf.upper} (confidence interval upper bound), \code{proportion} (proportion), \code{percentage} (percentage), \code{z.value} (z-value from normal distribution), \code{digits} (number of digits used to format APA confidence intervals), \code{n} (sample size), \code{population.corrected} (whether or not the margin of error is corrected for population size), \code{population.size} (population size), \code{fpc} (finite population correction, between 0 and 1), \code{sampling.fraction} (sampling fraction, ratio of sample size to population size, between 0 and 1), \code{error.uncorrected} (margin of error before it is corrected for population size), and \code{apa} (APA6 style formatted confidence intervals).
#' @section Assumptions:
#' \code{moe} assumes a normal distribution by calculating the z-value from \code{qnorm}, as well as simple random sampling (i.e., all observations have an equal probability of inclusion).
#' @section Finite population correction:
#' When the sampling fraction (ratio of sample size to population size) is large, approximately 5 percent or more, the estimate of the standard error can be corrected by multiplying a \href{https://en.wikipedia.org/wiki/Standard_error#Correction_for_finite_population}{Finite Population Correction}. To use this correction, set the \code{population.correction} argument to \code{TRUE} and set the sample size for the population using the \code{population.size} argument.
#' @section References:
#' Bondy, W. & Zlot, W. (1976). The Standard Error of the Mean and the Difference Between Means for Finite Populations. The American Statistician, 30, 96–97. doi:10.2307/2683803
#' @seealso \link[stats]{qnorm()}
#' @examples
#' \dontrun{
#' # Margin of error for party with 30%
#' # of voters in a sample of 1,200.
#' moe(proportion=0.30, n=1200)
#'
#' # Correct for population size (N=300,000),
#' # using 99% confidence level.
#' moe(proportion=0.30, n=1200, conf.level=0.99,
#'     population.correction=TRUE, population.size=300000)
#'
#' # Get confidence interval.
#' m <- moe(proportion=0.30, n=1200)
#' m$conf.lower
#' m$conf.upper
#'
#' # Show all information.
#' summary(m, digits=2)
#'
#' # APA6 style confidence intervals.
#' as.character(m, digits=2)
#'
#' # Print margin of error.
#' print(m, digits=2)
#'
#' # 2-sample test for equality of proportions
#' # (Chi-square) using the minus operator.
#' m1 <- moe(proportion=0.33, n=1200)
#' m2 <- moe(proportion=0.40, n=1200)
#' m1 - m2
#' }
#' @export
moe <- function(proportion, n, conf.level=.95, digits=2, population.correction=FALSE, population.size=NULL) {
  # Failchecks.
  if(proportion < 0 | proportion > 1) { stop("proportion must be a value beteen 0 and 1, such as 0.30 for 30%.") }
  if(conf.level < 0 | conf.level > 1) { stop("conf.level must be a value beteen 0 and 1, such as 0.95 for 95%.") }
  if(n < 0) { stop("n cannot be negative.") }
  if(digits < 0) { stop("digits cannot be negative.") }
  if(population.correction) {
    if(is.null(population.size)) { stop("If population.correction is TRUE, a population.size must be set.") }
    if(population.size < 0) { stop("population.size cannot be negative.") }
    if(population.size < n) { stop("population.size cannot be smaller than sample size n.") }
  }

  # Convert confidence level to two-sided alpha and its z-value.
  alpha <- (1 - conf.level) / 2
  z <- stats::qnorm(alpha, lower.tail=FALSE)

  # Calculate margin of error.
  error <- z * (sqrt(proportion * (1 - proportion) / n)) * 100

  sampling.fraction <- NULL
  if(!is.null(population.size)) {
    # Sampling fraction (ratio of sample size to population size).
    sampling.fraction <- n / population.size
  }

  # Use Finite Population Correction (FPC) for margin of error?
  fpc <- NULL
  error.uncorrected <- NULL
  if(population.correction) {
    fpc <- (1 - (n / population.size))
    error.uncorrected <- error
    error <- error * fpc
  }

  # Calculate confidence lower and upper bounds.
  conf.lower <- proportion * 100 - error
  conf.upper <- proportion * 100 + error

  # Save parameters to list.
  moeobj <- list(margin.of.error = error,                        # Margin of error (in percentage points).
                  conf.level = round(conf.level * 100, 0),       # Confidence level (in percentage points).
                  conf.lower = conf.lower,                       # Confidence interval lower bound (in percentage points).
                  conf.upper = conf.upper,                       # Confidence interval upper bound (in percentage points).
                  proportion = proportion,                       # Proportion.
                  percentage = proportion * 100,                 # Proportion expressed as a percentage.
                  z.value = z,                                   # Z-value from confidence level.
                  digits = digits,                               # Number of digits when formatting the APA and human-readable results.
                  n = n,                                         # Sample size.
                  population.corrected = population.correction,  # Whether or not population correction method was applied.
                  population.size = population.size,             # The population size used in the population correction method (only used when population.corrected is TRUE).
                  fpc = fpc,                                     # Finite population correction (only used when population.corrected is TRUE).
                  sampling.fraction = sampling.fraction,         # Sampling fraction (only used when population.corrected is TRUE).
                  error.uncorrected = error.uncorrected,         # Margin of error without finite population correction (only used when population.corrected is TRUE).

                  # APA6 style format.
                  apa = paste0(round(proportion * 100, digits), "%, ",
                                     conf.level, "% CI [",
                                     conf.lower, digits, ", ",
                                     conf.upper, digits, "]"))

  # Set S3 class.
  class(moeobj) <- "moe"
  return(moeobj)
}


# Check whether object is a "moe" object.
#' @export
is.moe <- function(obj) {
  return(class(obj) == "moe")
}


# Print moe object with human-readable interpretation of the margin of error.
#' @export
print.moe <- function(obj, digits=NULL) {
  if(!is.moe(obj)) {
    stop("obj must be a 'moe' object.")
  }
  if(!is.null(digits)) {
    cat(round(obj$margin.of.error, digits), "\n")
  } else {
    cat(obj$margin.of.error, "\n")
  }
}


# Test if two proportions are significant using 2-sample test for equality of proportions (Pearson Chi-squared).
#' @export
`-.moe` <- function(obj1, obj2) {
  if(!is.moe(obj1)) { stop("obj1 must be a 'moe' object.") }
  if(!is.moe(obj2)) { stop("obj2 must be a 'moe' object.") }

  # Use the confidence level from the first object.
  conf.level <- obj1$conf.level
  cat("Note: Using the ", conf.level, "% confidence level from '", deparse(substitute(obj1)), "'.\n", sep="")

  proportion1 <- ceiling(obj1$proportion * obj1$n)
  proportion2 <- ceiling(obj2$proportion * obj2$n)
  n1 <- obj1$n
  n2 <- obj2$n
  test <- stats::prop.test(x = c(proportion1, proportion2),
                           n = c(n1, n2),
                           conf.level = conf.level / 100,
                           alternative = "two.sided")
  return(test)
}


# Inform user that this method is not allowed.
#' @export
`+.moe` <- function(obj1, obj2) {
  stop(paste0("The plus operator is not meaningful for moe objects. Please use '", deparse(substitute(obj1)), " - ", deparse(substitute(obj2)), "' instead."))
}


# Indent text and wrap long lines.
cat_wrap <- function(text, width=80, indent="") {
  cat(indent, strwrap(text, width=width), sep=paste0("\n", indent))
}


# Print all available information from a moe object.
#' @export
summary.moe <- function(obj, digits=2, interpretation=TRUE) {
  if(!is.moe(obj)) {
    stop("obj must be a 'moe' object.")
  }

  # Human-readable interpretation.
  interpretation.text = paste("A share of ", round(obj$percentage, digits), "% with a sample size of ",
                              format(obj$n, scientific = FALSE), " has a ", round(obj$conf.level, 0),
                              "% confidence interval between ", round(obj$conf.lower, digits), " and ",
                              round(obj$conf.upper, digits), " percentage points, and the margin of error is plus/minus ",
                              round(obj$margin.of.error, digits), " percentage points.",
                              ifelse(obj$population.corrected,
                                     paste0(" These percentage points are corrected for the population size of ",
                                            format(obj$population.size, scientific = FALSE), "."), ""), sep="")

  cat("Parameters\n\n")
  cat(format("  Margin of error:", justify="left", width=32), obj$margin.of.error, "\n")
  cat(format("  Proportion:", justify="left", width=32), obj$proportion, paste0("(", obj$percentage, "%)"), "\n")
  cat(format("  Confidence level:", justify="left", width=32), paste0(obj$conf.level, "%"), "\n")
  cat(format("  Confidence interval:", justify="left", width=32), paste0("[", round(obj$conf.lower, digits), ", ", round(obj$conf.upper, digits), "]"), "\n")
  cat(format("  Sample size:", justify="left", width=32), paste0("n = ", obj$n), "\n")
  cat(format("  z-value:", justify="left", width=32), obj$z, "\n")
  cat(format("  APA6 style format:", justify="left", width=32), as.character(obj, digits=digits), "\n")
  if(obj$population.corrected) {
    cat("\n  Note: Margin of error and confidence intervals are corrected for population size.\n\n")
    cat("Population correction\n\n")
    cat(format("  Population size:", justify="left", width=32), paste0("N = ", format(obj$population.size, scientific = FALSE)), "\n")
    cat(format("  Sampling fraction:", justify="left", width=32), obj$sampling.fraction, "\n")
    cat(format("  Finite population correction:", justify="left", width=32), obj$fpc, "\n")
    err.diff <- obj$error.uncorrected - obj$margin.of.error
    cat(format("  Uncorrected margin of error:", justify="left", width=32), format(obj$error.uncorrected, scientific=FALSE), paste0("(difference: ", format(err.diff, scientific=FALSE), ")"), "\n")
  }
  if(interpretation) {
    cat(format("\nInterpretation\n"))
    cat_wrap(interpretation.text, indent="  ", width=80)
  }
}


# Return character string with APA6 style formatting from moe object.
#' @export
as.character.moe <- function(obj, digits=2) {
  if(!is.moe(obj)) {
    stop("obj must be a 'moe' object.")
  }
  return(paste0(round(obj$proportion * 100, digits), "%, ",
               obj$conf.level, "% CI [",
               round(obj$conf.lower, digits), ", ",
               round(obj$conf.upper, digits), "]"))
}


# Return margin of error as a double from moe object.
#' @export
as.double.moe <- function(obj) {
  if(!is.moe(obj)) {
    stop("obj must be a 'moe' object.")
  }
  return(obj$margin.of.error)
}


# Return margin of error as an integer from moe object.
#' @export
as.integer.moe <- function(obj) {
  if(!is.moe(obj)) {
    stop("obj must be a 'moe' object.")
  }
  return(as.integer(obj$margin.of.error))
}
