# moe: Calculate Margin of Error

R package **moe** calculates margin of error for simple probability samples.

## Install

```
devtools::install_github("peterdalle/moe")
library(moe)
```

## Usage

```r
moe(proportion, n, conf.level = 0.95, digits = 2,
    population.correction = FALSE, population.size = NULL)
```

## Arguments

- `proportion` ... value between 0 and 1 indicating the proportion, such as 0.30 for 30 percent.
- `n` ... sample size.
- `conf.level` ... confidence level (defaults to 0.95).
- `digits` ... number of decimal digits used when formatting the results as APA and human-readable messages (defaults to 2).
- `population.correction` ... whether or not results should be corrected by population size (defaults to FALSE).
- `population.size` ... population size used by the population correction (defaults to NULL). Only used if population.correction is set to TRUE.

## Returns

a list with margin of error, confidence level, confidence interval (lower and upper bound), z-value, results formatted in accordance to APA standards, and human-readable interpretation of results.

## Examples

In this case, a political party got 30% in a sample of 1,200 voters.

```r
moe(proportion=0.30, n=1200)
```

Which outputs:

```r
$`margin.of.error`
[1] 2.592789

$conf.level
[1] 95

$conf.lower
[1] 27.40721

$conf.upper
[1] 32.59279

$proportion
[1] 0.3

$percentage
[1] 30

$z.value
[1] 1.959964

$digits
[1] 2

$n
[1] 1200

$population.corrected
[1] FALSE

$population.size
NULL

$apa
[1] "30%, 95% CI [27.41, 32.59]"

$interpretation
[1] "A share of 30% and a sample size of 1200 has a 95% confidence interval between 27.41 and 32.59 percentage points, and the margin of error is plus/minus 2.59 percentage points."
```

As you can see above, the output includes (among other things) margin of error, confidence intervals, results formatted in accordance with APA6 standards, as well as human-readable interpretation.

You can also extract specific data:

```r
m <- moe(proportion=0.3, n=1200)
m$margin.of.error
```

Which outputs:

```r
[1] 2.592789
```

You can also correct the results for population size. In essence, the closer the sample size is to the population size, the smaller the margin of error will be. In everyday survey research (where n = 1,000), however, the effect of population correction is trivial.

In this example, the sample is 50,000. We correct for population size (in this fictional country with 300,000 voters) and increase the confidence level to 99%.

```r
moe(proportion=0.355, n=50000, conf.level=0.99, population.correction=TRUE, population.size=300000)
```

Which outputs:

```r
$`margin.of.error`
[1] 0.4593512

$conf.level
[1] 99

$conf.lower
[1] 35.04065

$conf.upper
[1] 35.95935

$proportion
[1] 0.355

$percentage
[1] 35.5

$z.value
[1] 2.575829

$digits
[1] 2

$n
[1] 50000

$population.corrected
[1] TRUE

$population.size
[1] 3e+05

$apa
[1] "35.5%, 99% CI [35.04, 35.96]"

$interpretation
[1] "A share of 35.5% with a sample size of 50000 has a 99% confidence interval between 35.04 and 35.96 percentage points, and the margin of error is plus/minus 0.46 percentage points. Note that these percentage points are corrected for the population size of 300000."
```

You can also compare margin of errors with and without population correction:

```
moe(proportion=0.355, n=50000, conf.level=0.99, population.correction=TRUE, population.size=300000)$margin.of.error
moe(proportion=0.355, n=50000, conf.level=0.99)$margin.of.error
```

Which outputs:

```r
[1] 0.4593512
[1] 0.5512215
```

As you can see above, the corrected method (first line) give somewhat smaller margin of error, in this particular case.

# History

- 2018-08-14 Version 0.9.0 First release.