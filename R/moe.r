#' Calculate margin of error for simple probability samples
#'
#' \code{moe} calculates margin of error and confidence intervals for simple probability
#' samples, as well as results formatted in accordance with American Psychological Association standards (APA6) and
#' human-readable interpretations of the results.
#'
#' @param proportion value between 0 and 1 indicating the proportion, such as 0.30 for 30 percent.
#' @param n sample size.
#' @param conf.level confidence level (defaults to \code{0.95}).
#' @param digits number of decimal digits used when formatting the results as APA and human-readable messages (defaults to \code{2}).
#' @param population.correction whether or not results should be corrected by population size (defaults to \code{FALSE}).
#' @param population.size population size used by the population correction (defaults to \code{NULL}). Only used if \code{population.correction} is set to \code{TRUE}.
#' @return a list with margin of error, confidence level, confidence interval (lower and upper bound), z-value,
#' results formatted in accordance with APA6 standards, and human-readable interpretation of results.
#' @section Assumptions:
#' \code{moe} assumes a normal distribution by calculating the z-value from \code{qnorm}.
#' @seealso \link[stats]{qnorm()}
#' @examples
#' \dontrun{
#' # Margin of error for party with 30% of voters in a sample of 1,200.
#' moe(proportion=0.30, n=1200)
#'
#' # Margin of error for party with 30% of voters in a sample of 50,000,
#' # corrected for population size (300,000) and 99% confidence level.
#' moe(proportion=0.30, n=50000, conf.level=0.99,
#'     population.correction=TRUE, population.size=300000)
#'
#' # Extract confidence interval.
#' m <- moe(proportion=0.30, n=1200)
#' m$conf.lower
#' m$conf.upper
#'
#' # Extract confidence interval in APA6 format.
#' m$apa
#' }
#' @export
moe <- function(proportion, n, conf.level=.95, digits=2, population.correction=FALSE, population.size=NULL) {
  # Failchecks.
  if(proportion < 0 | proportion > 1) { stop("proportion must be a value beteen 0 and 1, such as 0.30 for 30%.") }
  if(conf.level < 0 | conf.level > 1) { stop("conf.level must be a value beteen 0 and 1, such as 0.95 for 95%.") }
  if(population.correction) { if(is.null(population.size)) { stop("If population.correction is TRUE, a population.size must be set.") } }
  if(!is.null(population.size)) { if(population.size < 0) { stop("population.size cannot be negative.") } }
  if(digits < 0) { stop("digits cannot be negative.") }
  if(n < 0) { stop("n cannot be negative.") }

  # Convert confidence level to two-sided alpha and its z-value.
  alpha <- (1 - conf.level) / 2
  z <- stats::qnorm(alpha, lower.tail=FALSE)

  # Calculate margin of error.
  error <- z * (sqrt(proportion * (1 - proportion) / n)) * 100

  # Use population correction for margin of error?
  if(population.correction) {
    error <- error * (1 - (n / population.size))
  }

  # Calculate confidence lower and upper bounds.
  conf.lower <- proportion * 100 - error
  conf.upper <- proportion * 100 + error

  return(list(margin.of.error = error,                       # Margin of error (in percentage points).
              conf.level = round(conf.level * 100, 0),       # Confidence level.
              conf.lower = conf.lower,                       # Confidence interval lower bound.
              conf.upper = conf.upper,                       # Confidence interval upper bound.
              proportion = proportion,                       # Proportion.
              percentage = round(proportion * 100, digits),  # Proportion expressed as a percentage.
              z.value = z,                                   # Z-value from confidence level.
              digits = digits,                               # Number of digits when formatting the APA and human-readable results.
              n = n,                                         # Sample size.
              population.corrected = population.correction,  # Whether or not population correction method was applied.
              population.size = population.size,             # The population size used in the population correction method.

              # APA formatted message.
              apa = paste(round(proportion * 100, digits), "%, ", round(conf.level * 100, 0), "% CI [",
                          round(conf.lower, digits), ", ", round(conf.upper, digits), "]", sep=""),

              # Human-readable interpretation.
              interpretation = paste("A share of ", round(proportion * 100, digits), "% with a sample size of ",
                                     format(n, scientific = FALSE), " has a ", round(conf.level * 100, 0),
                                     "% confidence interval between ", round(conf.lower, digits), " and ",
                                     round(conf.upper, digits), " percentage points, and the margin of error is plus/minus ",
                                     round(error, digits), " percentage points.", ifelse(population.correction,
                                            paste0(" Note that these percentage points are corrected for the population size of ",
                                                   format(population.size, scientific = FALSE), "."), ""), sep="")
  ))
}
