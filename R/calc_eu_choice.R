#' Helper function that returns TRUE if expected utility suggests the same lottery choice that was choosen by the survey respondent.
#'
#' @param r a numerical value representing the risk preference parameter to test
#' @param lottery_probs_1 a numerical vector representing the probabilities of the first outcome occurring in each lottery 
#' @param lottery_probs_2 a numerical vector representing the probabilities of the second outcome occurring in each lottery 
#' @param lottery_payoffs_1 a numerical vector representing the payoffs if the first outcome occurs in each lottery 
#' @param lottery_payoffs_2 a numerical vector representing the payoffs if the second outcome occurs in each lottery 
#' @param initial_wealth a numerical value representing the initial wealth to use for each respondent
#' @param lottery_choice a numeric value representing the observed lottery choice of the survey respondent

calc_eu_choice <- function(r,lottery_probs_1,initial_wealth,lottery_payoffs_1,lottery_probs_2, lottery_payoffs_2,lottery_choice){
  # calculate expected utilities for a given risk preference and lottery characteristics
  eu <- data.frame(lot = rep(NA,length(lottery_probs_1)), eu = NA)
  for(k in 1:length(lottery_probs_1)){
    eu$eu[k] <- lottery_probs_1[k] * crra_u(
      +lottery_payoffs_1[k],r) +
      lottery_probs_2[k] * crra_u(initial_wealth+lottery_payoffs_2[k],r)
    eu$lot[k] <- paste0("eu_",k)
  }
  
  # figure out which lottery would have been chosen with the risk preferences currently being used
  choice_temp <- as.numeric(gsub("eu_","",eu$lot[which(eu$eu == max(eu$eu))]))
  
  #return(choice_temp == lottery_choice)
  return(choice_temp)
}


