# mc_reps <- 100
# large_adjustment <- .10
# small_adjustment <- .05
# rp_lb <- -2
# rp_ub <- 2
# lottery_probs_1 <- c(1,.5,.225,.125,.025)
# lottery_probs_2 <- c(0,.5,.775,.875,.975)
# lottery_payoffs_1 <- c(5,8,22,60,325)
# lottery_payoffs_2 <- c(0,3,2,0,0)
# sub_beliefs <- c(1,4,3,4)
# lottery_choice <- 1
# utility_function <- "crra"
# initial_wealth <- .0000001
# returned_obj <- "midpoint"
# rp_resolution <- .01
# 
# obs <- 1
# mc_reps = 100 # the number of monte-carlo reps to use
# large_adjustment = .10  # the upper bound on the large adjustment interval
# small_adjustment = .05 # the upper bound on the small adjustment interval
# rp_lb = -2 # the lower bound on the range of CRRA values to consider
# rp_ub = 2 # the upper bound on the range of CRRA values to consider
# rp_resolution = .01 # the resolution of the CRRA value (controls how finely we search the parameter space)
# lottery_probs_1 = df$lottery_probs_1[obs][[1]] # probabilities that the first outcome in each lottery occurs
# lottery_probs_2 = df$lottery_probs_2[obs][[1]] # probabilities that the second outcome in each lottery occurs
# lottery_payoffs_1 = df$lottery_payoffs_1[obs][[1]] # payoffs for the first outcome in each lottery
# lottery_payoffs_2 = df$lottery_payoffs_2[obs][[1]] # payoffs for the second outcome in each lottery
# sub_beliefs = c(3,3,3,3) # likert responses to the subjective probability debriefing questions
# # setting all equal to 3's will produce unadjusted risk preference parameters
# lottery_choice = df$lottery_choice[obs] # the observed lottery choice of the respondent
# utility_function = "crra" # the utility function to use in the risk preference calculation (crra is the only choice right now)
# initial_wealth = 0 # initial wealth to assume. Assuming a positive but
# # arbitrarily close to zero initial wealth. I.e. assuming the agent is playing the lottery in isolation
# returned_obj = "range"


# adjust_rp(mc_reps = 100, large_adjustment = .10, small_adjustment = .05,
# rp_lb = -2,rp_ub = 2,rp_resolution = .05,lottery_probs_1 = c(1,.5,.225,.125,.025),
# lottery_probs_2 = c(0,.5,.775,.875,.975),lottery_payoffs_1 = c(5,8,22,60,325),
# lottery_payoffs_2 = c(0,3,2,0,0),sub_beliefs = c(1,2,3,4),lottery_choice = 3,
# utility_function = "crra",initial_wealth = .0000001,returned_obj = "midpoint")



#' Adjusts experimentally elicited risk preferences using the methodology of Turner and Landry, 2021
#'
#' @param mc_reps numerical value representing the number of monte-carlo replications to use.
#' @param large_adjustment numerical value representing the upper bound of the large adjustment as described by Turner and Landry, 2021
#' @param small_adjustment numerical value representing the upper bound of the small adjustment as described by Turner and Landry, 2021
#' @param lottery_probs_1 a numerical vector representing the probabilities of the first outcome occurring in each lottery 
#' @param lottery_probs_2 a numerical vector representing the probabilities of the second outcome occurring in each lottery 
#' @param lottery_payoffs_1 a numerical vector representing the payoffs if the first outcome occurs in each lottery 
#' @param lottery_payoffs_2 a numerical vector representing the payoffs if the second outcome occurs in each lottery 
#' @param rp_lb a numerical value representing the upper bound to consider for the risk preference coefficient. 
#' @param rp_ub a numerical value representing the lower bound to consider for the risk preference coefficient.
#' @param initial_wealth a numerical value representing the initial wealth to use for each respondent
#' @param sub_beliefs a vector representing likert scale responses to each of the subjective probability belief elicitation questions in Turner and Landry, 2021
#' @param utility_function a character vector representing the utility function to use for deriving the risk prefrences. The only current option is "crra".
#' @param lottery_choice a numeric value representing the observed lottery choice of the survey respondent
#' @param returned_obj a character vector representing how adjusted risk preferences should be returned. 
#' Option are "range" or "midpoint". "range" will return two values representing the lower and upper bounds on the risk preference
#' coefficeients that are compatiable with the respondent's observed choices and survey responses. "midpoint" will return a single
#' value that is the midpoing of the upper and lower bounds.
#' @param rp_resolution a numeric value representing the resolution to use for identifying risk preferences. 
#' For example, a value of .01, will search the risk preference parameter space in 0.01 increments. This value can
#' be adjusted to speed up computation if fine resolution is not required.
#'
#' @return returns either a range or midpoint (depending on the value of returned_obj) representing the adjusted risk preferences
#' @export
#' @importFrom stats runif
#'
#' @examples
#' adjust_rp(mc_reps = 100, large_adjustment = .10, small_adjustment = .05,
#' rp_lb = -2,rp_ub = 2,rp_resolution = .01,lottery_probs_1 = c(1,.5,.225,.125,.025),
#' lottery_probs_2 = c(0,.5,.775,.875,.975),lottery_payoffs_1 = c(5,8,22,60,325),
#' lottery_payoffs_2 = c(0,3,2,0,0),sub_beliefs = c(1,2,3,4),lottery_choice = 3,
#' utility_function = "crra",initial_wealth = .0000001,returned_obj = "midpoint")
#' 
adjust_rp <- function(mc_reps, large_adjustment, small_adjustment, lottery_probs_1, lottery_probs_2,
                      lottery_payoffs_1, lottery_payoffs_2, rp_lb, rp_ub, initial_wealth, sub_beliefs,
                      utility_function, lottery_choice, returned_obj = "midpoint", rp_resolution = .01){
 
  #lapply over mc_reps
  # check to make sure the large and small adjustment vectors are the same length
  # if(length(small_adjustments) != length(large_adjustments)){
  #   stop(paste0("The small and large adjustment vectors passed to the function must be the same length. small_adjustments has length ",
  #               length(small_adjustments), " while large_adjustments has length ", length(large_adjustments)))
  # }
  #
  # # check to make sure the large adjustment values are all bigger than the small adjustment values
  # if(F %in% (large_adjustments > small_adjustments)){
  #   stop("Each element in large_adjustments must be greater than the corresponding element in small_adjustments.")
  # }


  # If survey respondent thought all reported probabilities were correct
  # there is no need for an adjustment. If this is the case, set the number
  # of reps to 1 and then proceed with the rest of the loop. Only 1 rep
  # is needed since no perturbation will be made on each rep.
  # if(!(F %in% (sub_beliefs == 3))){
  #   mc_reps = 1
  # }


  # this loop repeatedly perturbs the decision probabilities used in the
  # lotteries according to repondents belief about the accuracy of the
  # probabilities stated in the survey question. After each perturbation,
  # the implied CRRA bounds are recorded. After a large number of reps, the
  # largest possible interval (using the minimum lower bound and maximum upper bound)
  # is constructed which maximizes the likelihood that the respondent's true
  # CRRA coefficient is in the bound we construct.
  
  
  
   if(!(F %in% (sub_beliefs == 3))){
     reps = 1
   } else {
     reps <- mc_reps
   }
  

  # get crra ranges, wrapping in sapply to vectorize over monte-carlo reps
  crra_ranges <- sapply(seq(1,reps,1), function(...){
  # create a data frame of lottery probs as they were in the survey
   # for(k in 1:length(lottery_probs_1)){
   #   assign(paste0("lot",k,"_p"), rep(lottery_probs_1[k],1)   )
   # }


  # lottery adjustments
  small_adj_temp = runif(1,.01,small_adjustment) # small adjustment
  large_adj_temp = runif(1,small_adjustment,large_adjustment) # large adjustment
  
  
  # adjust lottery probabilities in accordance with subjective beliefs
  lot_probs_adjusted <- c(lottery_probs_1[1])
  for(k in 2: length(lottery_probs_1)){
      
      if(sub_beliefs[k-1]  == 1){
        #assign(paste0("lot",k,"_p"),  eval(parse(text = paste0("lot",k,"_p"))) * (1+small_adj_temp) )
        lot_probs_adjusted <- c(lot_probs_adjusted, lottery_probs_1[k] * (1-large_adj_temp))
      }
    
      if(sub_beliefs[k-1]  == 2){
        #assign(paste0("lot",k,"_p"),  eval(parse(text = paste0("lot",k,"_p"))) * (1+small_adj_temp) )
        lot_probs_adjusted <- c(lot_probs_adjusted, lottery_probs_1[k] * (1-small_adj_temp))
      }
      
      if(sub_beliefs[k-1]  == 3){
        #assign(paste0("lot",k,"_p"),  eval(parse(text = paste0("lot",k,"_p"))) * (1+small_adj_temp) )
        lot_probs_adjusted <- c(lot_probs_adjusted, lottery_probs_1[k])
      }
    
      if(sub_beliefs[k-1]  == 4){
        #assign(paste0("lot",k,"_p"),  eval(parse(text = paste0("lot",k,"_p"))) * (1+small_adj_temp) )
        lot_probs_adjusted <- c(lot_probs_adjusted, lottery_probs_1[k] * (1+small_adj_temp))
      }
      
      if(sub_beliefs[k-1]  == 5){
        #assign(paste0("lot",k,"_p"),  eval(parse(text = paste0("lot",k,"_p"))) * (1+small_adj_temp) )
        lot_probs_adjusted <- c(lot_probs_adjusted, lottery_probs_1[k] * (1+large_adj_temp))
      }

   
  }

  # calculate risk preferences using the calc_rp function
  rp_range <- calc_rp(utility_function = "crra",
          lottery_choice = lottery_choice,
          lottery_probs_1 = lot_probs_adjusted,
          lottery_probs_2 = (1-lot_probs_adjusted),
          lottery_payoffs_1 = lottery_payoffs_1,
          lottery_payoffs_2 = lottery_payoffs_2,
          rp_ub = rp_ub,
          rp_lb = rp_lb,
          initial_wealth = initial_wealth,
          rp_resolution = rp_resolution)



  return(rp_range)
  } )


  # calculate max possible crra value
  max <- max(crra_ranges[2,])

  # calculate min possible crra value
  min <- min(crra_ranges[1,])

  # return the range
  if(returned_obj == "range"){
    return(c(min,max))
  }

  # return the midpoint
  if(returned_obj == "midpoint"){
    return((min+max)/2)
  }


  # TODO: implement the inf handling into the calc_rp function.


}


# adjust_rp(mc_reps = 100, large_adjustment = .10, small_adjustment = .05,
# rp_lb = -2,rp_ub = 2,rp_resolution = .01,lottery_probs_1 = c(1,.5,.225,.125,.025),
# lottery_probs_2 = c(0,.5,.775,.875,.975),lottery_payoffs_1 = c(5,8,22,60,325),
# lottery_payoffs_2 = c(0,3,2,0,0),sub_beliefs = c(1,2,3,4),lottery_choice = 3,
# utility_function = "crra",initial_wealth = .0000001,returned_obj = "midpoint")








