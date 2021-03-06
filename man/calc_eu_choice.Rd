% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_eu_choice.R
\name{calc_eu_choice}
\alias{calc_eu_choice}
\title{Helper function that returns TRUE if expected utility suggests the same lottery choice that was choosen by the survey respondent.}
\usage{
calc_eu_choice(
  r,
  lottery_probs_1,
  initial_wealth,
  lottery_payoffs_1,
  lottery_probs_2,
  lottery_payoffs_2,
  lottery_choice
)
}
\arguments{
\item{r}{a numerical value representing the risk preference parameter to test}

\item{lottery_probs_1}{a numerical vector representing the probabilities of the first outcome occurring in each lottery}

\item{initial_wealth}{a numerical value representing the initial wealth to use for each respondent}

\item{lottery_payoffs_1}{a numerical vector representing the payoffs if the first outcome occurs in each lottery}

\item{lottery_probs_2}{a numerical vector representing the probabilities of the second outcome occurring in each lottery}

\item{lottery_payoffs_2}{a numerical vector representing the payoffs if the second outcome occurs in each lottery}

\item{lottery_choice}{a numeric value representing the observed lottery choice of the survey respondent}
}
\description{
Helper function that returns TRUE if expected utility suggests the same lottery choice that was choosen by the survey respondent.
}
