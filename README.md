
-   [RPadjust (Risk Preference
    Adjustment)](#rpadjust-risk-preference-adjustment)
-   [Introduction](#introduction)
-   [Working Example](#working-example)

## RPadjust (Risk Preference Adjustment)

![R-CMD-check](https://github.com/ropensci/ijtiff/workflows/R-CMD-check/badge.svg)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
<!-- [![Codecov test coverage](https://codecov.io/gh/dylan-turner25/rfema/branch/main/graph/badge.svg)](https://codecov.io/gh/dylan-turner25/rfema?branch=main) -->

<!-- badges: start -->

<!-- [![R-CMD-check](https://github.com/dylan-turner25/rfema/workflows/R-CMD-check/badge.svg)](https://github.com/dylan-turner25/rfema/actions) -->
<!-- badges: end -->

## Introduction

The `RPadjust` package contains functions for impementing the risk
preference adjustment procedure outlined in Turner and Landry, 2021.

Turner, D. and Landry, C. (2021). Accounting for uncertainty in decision
weights for experimental elicitation of risk preferences. SSRN Working
Paper. Avaliable at SSRN: <https://ssrn.com/abstract=3882694>.

## Working Example

First, we will simulate some data to work with. To calculate adjusted
risk preferences we need the following:

-   `lottery_probs_1`: In a series of binary lotteries that an
    individual faces, this is a vector containing the **probabilities**
    that the **first** outcome in each of those lottery occurs.
-   `lottery_probs_2`: In a series of binary lotteries that an
    individual faces, this is a vector containing the **probabilities**
    that the **second** outcome in each of those lottery occurs.
-   `lottery_payoffs_1`: In a series of binary lotteries that an
    individual faces, this is a vector containing the **payoffs**
    associated with the **first** outcome in each of those lottery
    occurs.
-   `lottery_payoffs_2`: In a series of binary lotteries that an
    individual faces, this is a vector containing the **payoffs**
    associated with the **second** outcome in each of those lottery
    occurs.
-   `subjective_beliefs_1`: This is a vector of likert scale responses
    in which respondents indicate if they thought the probabilities
    associated with each lottery were correct or too low/high.
    Specifically, in Turner and Landry 2021 the values of 1-5 correspond
    to the following responses:
    -   1
    -   2
-   `lottery_choice`: In a series of binary lotteries, this is a numeric
    value corresponding to the lottery choosen by the respondent.

``` r
n <- 10 # number of observations for fake data

# initialize a data frame to hold the fake data
df <- data.frame(lottery_choice = sample(seq(1,5,1),n,replace = T),
           lottery_probs_1 = NA,
           lottery_probs_2 = NA,
           lottery_payoffs_1 = NA,
           lottery_payoffs_2 = NA,
           subjective_beliefs = NA)

# populate the df with fake data
df$lottery_probs_1 <- list( c(1,.5,.225,.125,.025) ) 
df$lottery_probs_2 <- list( c(0,.5,.775,.875,.975) )
df$lottery_payoffs_1 <- list( c(5,8,22,60,325) )
df$lottery_payoffs_2 <- list( c(0,3,2,0,0) )
for(k in 1:nrow(df)){
  df$subjective_beliefs[k] <- list( c(sample(seq(1,5,1),1,replace = T),sample(seq(1,5,1),1,replace = T) ,sample(seq(1,5,1),1,replace = T),sample(seq(1,5,1),1,replace = T) ))
}

kable(head(df))
```

| lottery\_choice | lottery\_probs\_1                 | lottery\_probs\_2                 | lottery\_payoffs\_1 | lottery\_payoffs\_2 | subjective\_beliefs |
|----------------:|:----------------------------------|:----------------------------------|:--------------------|:--------------------|:--------------------|
|               2 | 1.000, 0.500, 0.225, 0.125, 0.025 | 0.000, 0.500, 0.775, 0.875, 0.975 | 5, 8, 22, 60, 325   | 0, 3, 2, 0, 0       | 5, 2, 2, 5          |
|               1 | 1.000, 0.500, 0.225, 0.125, 0.025 | 0.000, 0.500, 0.775, 0.875, 0.975 | 5, 8, 22, 60, 325   | 0, 3, 2, 0, 0       | 3, 2, 5, 3          |
|               2 | 1.000, 0.500, 0.225, 0.125, 0.025 | 0.000, 0.500, 0.775, 0.875, 0.975 | 5, 8, 22, 60, 325   | 0, 3, 2, 0, 0       | 1, 5, 5, 4          |
|               1 | 1.000, 0.500, 0.225, 0.125, 0.025 | 0.000, 0.500, 0.775, 0.875, 0.975 | 5, 8, 22, 60, 325   | 0, 3, 2, 0, 0       | 5, 3, 4, 3          |
|               1 | 1.000, 0.500, 0.225, 0.125, 0.025 | 0.000, 0.500, 0.775, 0.875, 0.975 | 5, 8, 22, 60, 325   | 0, 3, 2, 0, 0       | 5, 2, 3, 5          |
|               5 | 1.000, 0.500, 0.225, 0.125, 0.025 | 0.000, 0.500, 0.775, 0.875, 0.975 | 5, 8, 22, 60, 325   | 0, 3, 2, 0, 0       | 4, 1, 4, 1          |

First we can calculate the risk preference implied by the respondents’
observed lottery choices using the `adjust_rp()` function

``` r
# calculate the risk preferences of the first respondent (i.e., the first row in the data frame)
obs <- 1 # calculating for the first observation
for(obs in 1:nrow(df)){
risk_pref_unadjusted <- adjust_rp(mc_reps = 100, # the number of monte-carlo reps to use
                       large_adjustment = .10,  # the upper bound on the large adjustment interval
                       small_adjustment = .05, # the upper bound on the small adjustment interval
                       rp_lb = -2, # the lower bound on the range of CRRA values to consider
                       rp_ub = 2, # the upper bound on the range of CRRA values to consider
                       rp_resolution = .01, # the resolution of the CRRA value (controls how finely we search the parameter space)
                       lottery_probs_1 = df$lottery_probs_1[obs][[1]], # probabilities that the first outcome in each lottery occurs
                       lottery_probs_2 = df$lottery_probs_2[obs][[1]], # probabilities that the second outcome in each lottery occurs
                       lottery_payoffs_1 = df$lottery_payoffs_1[obs][[1]], # payoffs for the first outcome in each lottery
                       lottery_payoffs_2 = df$lottery_probs_1[obs][[1]], # payoffs for the second outcome in each lottery
                       sub_beliefs = c(3,3,3,3), # likert responses to the subjective probability debriefing questions, 
                                                 # setting all equal to 3's will produce unadjusted risk preference parameters
                       lottery_choice = df$lottery_choice[obs], # the observed lottery choice of the respondent
                       utility_function = "crra", # the utility function to use in the risk preference calculation (crra is the only choice right now)
                       initial_wealth = .0000001, # initial wealth to assume. Assuming a positive, but 
                                                  # arbitrarily close to zero initial wealth. I.e., assuming the agent is playing the lottery in isolation
                       returned_obj = "range") # will returning the range of the implied crra interval (can set to "midpoint" to get the midpoint of the implied range)

print(risk_pref_unadjusted)
}
```

    ## [1] -2.00  0.03
    ## [1] -2.00  0.03
    ## [1] 0.19 2.00

    ## Warning in min(crra_vals$crra): no non-missing arguments to min; returning Inf

    ## Warning in max(crra_vals$crra): no non-missing arguments to max; returning -Inf

    ## [1]  Inf -Inf
    ## [1] 0.04 0.18
    ## [1] 0.04 0.18

    ## Warning in min(crra_vals$crra): no non-missing arguments to min; returning Inf

    ## Warning in min(crra_vals$crra): no non-missing arguments to max; returning -Inf

    ## [1]  Inf -Inf

    ## Warning in min(crra_vals$crra): no non-missing arguments to min; returning Inf

    ## Warning in min(crra_vals$crra): no non-missing arguments to max; returning -Inf

    ## [1]  Inf -Inf
    ## [1] -2.00  0.03
    ## [1] 0.19 2.00

<!-- `rfema` allows users to access The Federal Emergency Management Agency's (FEMA) publicly available data through the open FEMA API. The package provides a set of functions to easily navigate and access all data sets provided by FEMA, including (but not limited to) data from the National Flood Insurance Program and FEMA's various disaster aid programs. -->
<!-- FEMA data is publicly available at the open FEMA website (https://www.fema.gov/about/openfema/data-sets) and is avaliable for bulk download, however, the files are sometimes very large (multiple gigabytes) and many times users do not need all records for a data series (for example: many users may only want records for a single state for several years). Using FEMA's API is a good option to circumvent working with the bulk data files, but can be inaccessible for those without prior API experience. This package contains a set of functions that allows users to easily identify and retrieve data from FEMA's API without needing any technical knowledge of APIs. -->
<!-- In accordance with the open_fema terms and conditions: This product uses the Federal Emergency Management Agency’s Open FEMA API, but is not endorsed by FEMA. The Federal Government or FEMA cannot vouch for the data or analyses derived from these data after the data have been retrieved from the Agency's website(s). Guidance on FEMA's preffered citation for Open FEMA data can be found at: https://www.fema.gov/about/openfema/terms-conditions -->
<!-- ## Why rfema? -->
<!-- What are the advantages of accessing the FEMA API through the `rfema` package as compared to accessing the API directly? In short, the `rfema` package handles much of the grunt work associated with constructing API queries, dealing with API limits, and applying filters or other parameters. Suppose one wants to obtain data on all of the flood insurance claims in Broward County, FL between 2010 and 2012. The following code obtains that data without the use of the `rfema` package. As you can see it requires many lines of code with significant hardcoding in the url strings. The hardcoding could be avoided, but that would require even more lines of code. -->
<!-- ```{r} -->
<!-- # define the url for the appropriate api end point -->
<!-- base_url <- "https://www.fema.gov/api/open/v1/FimaNfipClaims" -->
<!-- # append the base_url to apply filters -->
<!-- filters <- "?$inlinecount=allpages&$top=1000&$filter=(countyCode%20eq%20'12011')%20and%20(yearOfLoss%20ge%20'2010')%20and%20(yearOfLoss%20le%20'2012')" -->
<!-- api_query <- paste0(base_url, filters) -->
<!-- # run a query setting the top_n parameter to 1 to check how many records match the filters -->
<!-- record_check_query <- "https://www.fema.gov/api/open/v1/FimaNfipClaims?$inlinecount=allpages&$top=1&$select=id&$filter=(countyCode%20eq%20'12011')%20and%20(yearOfLoss%20ge%20'2010')%20and%20(yearOfLoss%20le%20'2012')" -->
<!-- # run the api call and determine the number of matching records -->
<!-- result <- httr::GET(record_check_query) -->
<!-- jsonData <- httr::content(result)         -->
<!-- n_records <- jsonData$metadata$count # there are 2119 records meaning we will need three seperate API calls to get all the data -->
<!-- # calculate number of calls neccesary to get all records using the  -->
<!-- # 1000 records/ call max limit defined by FEMA -->
<!-- itterations <- ceiling(n_records / 1000) -->
<!-- for(i in seq(from=1, to=itterations, by=1)){ -->
<!--   # As above, if you have filters, specific fields, or are sorting, add that to the base URL  -->
<!--   #   or make sure it gets concatenated here. -->
<!--   result <- httr::GET(paste0(api_query,"&$skip=",(i-1) * 1000)) -->
<!--   jsonData <- httr::content(result)          -->
<!--   if(i == 1){ -->
<!--     data <- dplyr::bind_rows(jsonData[[2]]) -->
<!--   } else { -->
<!--     data <- dplyr::bind_rows(data, dplyr::bind_rows(jsonData[[2]])) -->
<!--   } -->
<!-- } -->
<!-- # remove the html line breaks from returned data frame (if there are any)   -->
<!-- data <- as.data.frame(lapply(data, function(data) gsub("\n", "", data))) -->
<!-- ``` -->
<!-- Compare the above block of code to the following code which obtains the same data using the `rfema` package. The `rfema` package allowed the same request to be made with two lines of code. Notably, the `open_fema()` function handled checking the number of records and implementation an iterative loop to get around the 1000 records/call limit. -->
<!-- ```{r} -->
<!-- # define a list of filters to apply -->
<!-- filterList <- list(countyCode = "= 12011",yearOfLoss = ">= 2010", yearOfLoss = "<= 2012") -->
<!-- # make the API call using the `open_fema` function. -->
<!-- data <- rfema::open_fema(data_set = "fimaNfipClaims",ask_before_call = F, filters = filterList ) -->
<!-- ``` -->
<!-- ## Installation -->
<!-- Right now, the best way to install and use the `rfema` package is by installing directly from GitHub using `devtools::install_github("dylan-turner25/rfema")`. The FEMA API does not require an API key, meaning no further steps need be taken to start using the package -->
<!-- ## Getting Started -->
