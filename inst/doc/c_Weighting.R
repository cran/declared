## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(declared)

n <- 1234
set.seed(n)
dfm <- data.frame(
  Area = declared(
    sample(1:2, n, replace = TRUE, prob = c(0.45, 0.55)),
    labels = c("Rural" = 1, "Urban" = 2)
  ),
  Gender = declared(
    sample(1:2, n, replace = TRUE, prob = c(0.55, 0.45)),
    labels = c("Males" = 1, "Females" = 2)
  ),
  Opinion = declared(
    sample(c(1:5, NA, -91), n, replace = TRUE),
    labels = c(
      "Very bad" = 1, "Bad" = 2, "Neither" = 3,
      "Good" = 4, "Very good" = 5, "Don't know" = -91
    ),
    na_values = -91
  ),
  Age = sample(18:90, n, replace = TRUE),
  Children = sample(0:5, n, replace = TRUE)
)

## -----------------------------------------------------------------------------
table(dfm$Opinion, useNA = "ifany")

## -----------------------------------------------------------------------------
table(as.factor(undeclare(dfm$Opinion)), useNA = "ifany")

## -----------------------------------------------------------------------------
w_table(dfm$Opinion, values = TRUE)

## -----------------------------------------------------------------------------
# Observed proportions
op <- with(dfm, proportions(table(Gender, Area)))

# Theoretical / population proportions:
# 53% Rural, and 50% Females
weights <- rep(c(0.53, 0.47), each = 2) * rep(0.5, 4) / op

dfm$fweight <- weights[
  match(10 * dfm$Area + dfm$Gender, c(11, 12, 21, 22))
]

## -----------------------------------------------------------------------------
with(dfm, w_table(Opinion, wt = fweight, values = TRUE))

