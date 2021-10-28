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
#'        lottery_payoffs_1 = c(5,8,22,60,300),
#'        lottery_payoffs_2 = c(0,3,2,0,0),
#'        rp_ub = 2,
#'        rp_lb = -2,
#'        initial_wealth = .00000000000000001,
#'        rp_resolution = .05)
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
    crra_vals$crra <- replace(crra_vals$crra, crra_vals$crra == 1, 1.0001)

    # apply the possible values function to each crra in crra_vals
    eu_implied_choice <- sapply(crra_vals$crra, calc_eu_choice, 
                                        lottery_probs_1 = lottery_probs_1,
                                        initial_wealth = initial_wealth,
                                        lottery_payoffs_1 = lottery_payoffs_1,
                                        lottery_probs_2 = lottery_probs_2, 
                                        lottery_payoffs_2 = lottery_payoffs_2,
                                        lottery_choice = lottery_choice)
    
    eu_implied_choice <- t(array(as.numeric(unlist(eu_implied_choice)), dim=c(1,length(crra_vals$crra))))
    crra_vals$eu_implied_choice <- eu_implied_choice
    crra_vals$possible_value <- c(crra_vals$eu_implied_choice == lottery_choice)

    # remove rows that correspond to crra values than aren't possible values
    crra_vals <- crra_vals[which(crra_vals$possible_value == T),]
    
    # possible range of crra values
    if(nrow(crra_vals) > 0){
      crra_range <- c(min(crra_vals$crra),max(crra_vals$crra))
    }
    
    # deal with discontinuity
    if(nrow(crra_vals) == 0){
      pos_val <- eu_implied_choice
      for(k in 1:(length(pos_val)-1)){
        if(pos_val[k] > lottery_choice & pos_val[k+1] < lottery_choice){
          pos_val[k] <- T
        } 
        if(pos_val[k] < lottery_choice & pos_val[k+1] > lottery_choice){
          pos_val[k] <- T
        } 
        if(pos_val[k] < lottery_choice & pos_val[k+1] < lottery_choice){
          pos_val[k] <- F
        }
        if(pos_val[k] > lottery_choice & pos_val[k+1] > lottery_choice){
          pos_val[k] <- F
        }
        if(k == (length(pos_val)-1) ){
          pos_val[k+1] <- F
        }
      }
      pos_val <- as.logical(pos_val)
      
      crra_vals <- data.frame(crra = seq(rp_lb,rp_ub,rp_resolution), possible_value = NA)
      crra_vals$crra <- replace(crra_vals$crra, crra_vals$crra == 1, 1.0001)
      crra_vals$possible_value <- pos_val
      
      # remove rows that correspond to crra values than aren't possible values
      crra_vals <- crra_vals[which(crra_vals$possible_value == T),]
      crra_range <- c(crra_vals$crra-rp_resolution,crra_vals$crra+rp_resolution)
      
    }

 
    # return the range
    return(crra_range)

  }

}




