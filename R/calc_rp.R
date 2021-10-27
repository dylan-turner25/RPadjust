#' Helper function that calculates risk preferences from individuals observed lottery choice.
#'
#' @param utility_function a character vector representing the utility function to use for deriving the risk preferences. The only current option is "crra".
#' @param lottery_choice a numeric value representing the observed lottery choice of the survey respondent
#' @param lottery_probs_1 a numerical vector representing the probabilities of the first outcome occurring in each lottery 
#' @param lottery_probs_2 a numerical vector representing the probabilities of the second outcome occurring in each lottery 
#' @param lottery_payoffs_1 a numerical vector representing the payoffs if the first outcome occurs in each lottery 
#' @param lottery_payoffs_2 a numerical vector representing the payoffs if the second outcome occurs in each lottery 
#' @param rp_lb a numerical value representing the upper bound to consider for the risk preference coefficient. 
#' @param rp_ub a numerical value representing the lower bound to consider for the risk preference coefficient.
#' @param rp_resolution a numeric value representing the resolution to use for identifying risk preferences. 
#' For example, a value of .01, will search the risk preference parameter space in 0.01 increments. This value can
#' be adjusted to speed up computation if fine resolution is not required.#' 
#' @param initial_wealth a numerical value representing the initial wealth to use for each respondent

#' @return returns adjusted risk preferences
#' @export
#'
#' @examples
#' calc_rp(utility_function = "crra",
#'        lottery_choice = 1,
#'        lottery_probs_1 = c(1,.5,.225,.125,.025),
#'        lottery_probs_2 = c(0,.5,.775,.875,.975),
#'        lottery_payoffs_1 = c(5,8,22,60,325),
#'        lottery_payoffs_2 = c(0,3,2,0,0),
#'        rp_ub = 2,
#'        rp_lb = -2,
#'        initial_wealth = .000001)
#'        
calc_rp <- function(utility_function, lottery_choice, lottery_probs_1, lottery_probs_2,
                    lottery_payoffs_1, lottery_payoffs_2, rp_ub, rp_lb, 
                    rp_resolution ,initial_wealth){

  # stop if something other than crra is input
  if(utility_function != "crra"){
    stop("crra utility is currently the only supported utility function")
  }

  if(utility_function == "crra"){


    # generate possible crra values to search over
    crra_vals <- data.frame(crra = seq(rp_lb,rp_ub,rp_resolution), possible_value = NA)


    # returns TRUE if expected utility suggests the lottery choice that was choosen by the survey respondent.
    possible_value <- function(r){
      # calculate expected utilities for a given risk preference and lottery characteristics
      eu <- data.frame(lot = rep(NA,length(lottery_probs_1)), eu = NA)
      for(k in 1:length(lottery_probs_1)){
        eu$eu[k] <- lottery_probs_1[k] * crra_u(initial_wealth+lottery_payoffs_1[k],r) +
                            lottery_probs_2[k] * crra_u(initial_wealth+lottery_payoffs_2[k],r)
        eu$lot[k] <- paste0("eu_",k)
      }

      # figure out which lottery would have been chosen with the risk preferences currently being used
      choice_temp <- as.numeric(gsub("eu_","",eu$lot[which(eu$eu == max(eu$eu))]))

      return(choice_temp == lottery_choice)
    }

    # apply the possible values function to each crra in crra_vals
    crra_vals$possible_value <-  sapply(crra_vals$crra, possible_value)


    # remove rows that correspond to crra values than aren't possible values
    crra_vals <- crra_vals[which(crra_vals$possible_value == T),]

    # possible range of crra values
    crra_range <- c(min(crra_vals$crra),max(crra_vals$crra))

    # return the range
    return(crra_range)

  }

}

