# df <- data %>% select(contains())
#     # need subjective beliefs on weather probabilities
#     # need lottery choices
#     # lottery payoffs
#     # lottery payoff probabilities
#
#
#
#
mc_reps <- 100
large_adjustment <- .10
small_adjustment <- .05
rp_lb <- -2
rp_ub <- 2
lottery_probs_1 <- c(1,.5,.225,.125,.025)
lottery_probs_2 <- c(0,.5,.775,.875,.975)
lottery_payoffs_1 <- c(5,8,22,60,325)
lottery_payoffs_2 <- c(0,3,2,0,0)
sub_beliefs <- c(3,3,4,1)
lottery_choice <- 3
utility_function <- "crra"
initial_wealth <- .0000001
returned_obj <- "midpoint"


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

  # get crra ranges, wrapping in sapply to vectorize over monte-carlo reps
  crra_ranges <- sapply(seq(1,mc_reps,1), function(...){
  # create a data frame of lottery probs as they were in the survey
   for(k in 1:length(lottery_probs_1)){
     assign(paste0("lot",k,"_p"), rep(lottery_probs_1[k],1)   )
   }


  # lottery adjustments
  small_adj_temp = runif(1,.01,small_adjustment) # small adjustment
  large_adj_temp = runif(1,small_adjustment,large_adjustment) # large adjustment

  # adjust lottery probabilities in accordance with subjective beliefs
  for(k in 2: length(lottery_probs_1)){
      # adjust if a small adjustment is warranted
      if(sub_beliefs[k-1] %in% c(2,4)){
        assign(paste0("lot",k,"_p"),  eval(parse(text = paste0("lot",k,"_p"))) * (1+small_adj_temp) )
      }

      # adjust if a large adjustment is warranted
      if(sub_beliefs[k-1] %in% c(1,5)){
        assign(paste0("lot",k,"_p"),  eval(parse(text = paste0("lot",k,"_p"))) * (1+large_adj_temp) )
      }
    }

  # calculate risk preferences using the calc_rp function
  rp_range <- RPadjust::calc_rp(utility_function = "crra",
          lottery_choice = lottery_choice,
          lottery_probs_1 = c(lot1_p, lot2_p, lot3_p, lot4_p, lot5_p),
          lottery_probs_2 = c(1-lot1_p, 1-lot2_p, 1-lot3_p, 1-lot4_p, 1-lot5_p),
          lottery_payoffs_1 = lottery_payoffs_1,
          lottery_payoffs_2 = lottery_payoffs_2,
          rp_ub = rp_ub,
          rp_lb = rp_lb,
          initial_wealth = initial_wealth)



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




adjust_rp(mc_reps = 100,
          large_adjustment = .10,
          small_adjustment = .05,
          rp_lb = -2,
          rp_ub = 2,
          rp_resolution = .01,
          lottery_probs_1 = c(1,.5,.225,.125,.025),
          lottery_probs_2 = c(0,.5,.775,.875,.975),
          lottery_payoffs_1 = c(5,8,22,60,325),
          lottery_payoffs_2 = c(0,3,2,0,0),
          sub_beliefs = c(1,2,3,4),
          lottery_choice = 3,
          utility_function = "crra",
          initial_wealth = .0000001,
          returned_obj = "midpoint")


