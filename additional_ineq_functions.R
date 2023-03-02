#' Compute the Headcount Ratio
#' 
#' @description 
#' Compute the Headcount Ratio of a vector `v` at a specified poverty threshold `z`, 
#' using weights given by a weight vector `w`.
#' 
#' Adapted from: https://github.com/JosepER/Inequality.jl/
#' 
#' @param v A vector of a income source
#' @param z A numeric income value representing the poverty line
#' @param w (optional) A vector of sample weights
#' 
#' @return  A numeric value
headcount <- function(v, z, w){

    if(missing(w)){
        w = rep(1, length(x))
    }else{
        assertthat::assert_that(length(v) == length(w), 
        msg = "Vectors in 'v' and 'w' must have the same length.")
    }

    return(sum(w[v < z]) / sum(w))
}


#' Compute the Poverty Gap of a vector `v` at a specified poverty threshold `z`.
#'
#' Adapted from: https://github.com/JosepER/Inequality.jl/
#'
#' @param v A vector of a income source
#' @param z A numeric income value representing the poverty line
#' @param w (optional) A vector of sample weights
#'
#' @return  A numeric value
poverty_gap <- function(v, z, w){

    if(missing(w)){
        w <- rep(1, length(x))
    }else{
        assertthat::assert_that(length(v) == length(w),
        msg = "Vectors in 'v' and 'w' must have the same length.")
    }

    return(sum((z - v[v < z])/z * w[v < z])/sum(w))
}


#' Calculates the Foster-Greer-Thorbecke (FGT) family of poverty measures
#' 
#' Adapted from: https://github.com/antrologos/inequalityTools/blob/master/R/poverty_fgt.R
#'
#' @param x A vector of a income source
#' @param z A numeric income value representing the poverty line
#' @param w (optional) A vector of sample weights
#' @param alpha An arbitrary inequality aversion parameter: alpha = 0 returns the poverty rate; alpha = 1 returns the average income gap. Default to 0.
#' 
#' @return Returns the numeric value of the FGT measure of poverty
#' 
#' @export
poverty_fgt <- function(x, z, w = NULL, alpha = 0){
    if(missing(w)){
        w <- rep(1, length(x))
        }
    if(length(z) == 1){
        w <- rep(z, length(x))
    }

    data <- tibble::tibble(x, w, z) %>% 
        dplyr::filter(complete.cases(.))

    rm(x, w, z)

    data <- data %>%
        dplyr::mutate(g   = ifelse(x < z, ((z - x)/z), 0),
            fgt = ifelse(x < z, g^alpha, 0)) %>%
        dplyr::summarise(fgt = sum(w*fgt),
            n = sum(w))

    return(data$fgt/data$n)
}


#' Compute the Watts Poverty Index
#'
#' @description 
#' Adapted from the Julia version in: https://github.com/JosepER/Inequality.jl/blob/master/src/Watts.jl
#'
#' @param v A vector with the income variable.
#' @param w A vector with the weights.
#' @param alpha A numeric with the poverty line.
#'
#' @return A numeric with the Poverty Index.
watts <- function(v, alpha, w = NULL){

    if(missing(w)){
        w <- rep(1, length(v))
    }else{
        assertthat::assert_that(length(v) == length(w), 
        msg = "Vectors in 'v' and 'w' must have the same length.")
    }

    return(sum((log(alpha/(v[v < alpha]))) * w[v < alpha]) / sum(w))

}

#' Produce bootstrap resamples
#' 
#' @description 
#' Produces a list with m resamples of df.
#' 
#' Similar to how the tidyverse 'bootstrap' function works (https://modelr.tidyverse.org/reference/bootstrap.html).
#' 
#' @param df A data.frame type of object.
#' @param m An integer with the number of resamples
#' 
#' @return A list of length m, containing tibbles with the resamples.
bootstrap <- function(df, m){

    purrr::map(1:m, .f = function(x, df_){
    dplyr::sample_n(df_, nrow(df_), replace = TRUE)}, 
    df)

}