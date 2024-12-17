#' @title calculate_kbit2_score
#' @description Calculates intelligence quotient and age equivalents from the
#'     KBIT-2 score from look-up tables using the age of the participant and
#'     their verbal and nonverbal scores.
#' @param age Participant's age
#' @param verbal_score Raw verbal score
#' @param nonverbal_score Raw nonverbal score
#' @param print_kbit Print the KBIT-2 results to the console, Default: TRUE
#' @return A list containing the verbal and nonverbal standard scores, intelligent quotients,
#' @details Calculates intelligence quotient and age equivalents from the
#'     KBIT-2 score from look-up tables using the age of the participant and
#'     their verbal and nonverbal scores.
#' @seealso
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{select}}
#' @rdname calculate_kbit2_score
#' @export
#' @importFrom dplyr filter select

calculate_kbit2_score <- function(age, verbal_score, nonverbal_score, print_kbit = TRUE){

  files <- list.files(path = system.file("extdata", package = "abcds"), pattern = "kbit2", full.names = TRUE)

  for(f in files) load(f)

  # Calculate sum of standard scores

  vrblStnd <-
    verbal %>%
    dplyr::filter(
      min_age <= age &
        max_age >= age &
        raw_score == verbal_score) %>%
    dplyr::select(-c(min_age, max_age)) %>%
    as.list()

  nonvrblStnd <-
    nonverbal %>%
    dplyr::filter(
      min_age <= age &
        max_age >= age &
        raw_score == nonverbal_score) %>%
    dplyr::select(-c(min_age, max_age)) %>%
    as.list()

  sumStnd <- vrblStnd$standard_score + nonvrblStnd$standard_score

  # Calculate IQ

  iq_score <-
    expand_lookup(
      data = subset(iq, min_age <= age & max_age >= age),
      variable = "sum_of_standard_scores"
      ) %>%
    dplyr::filter(sum_of_standard_scores == sumStnd) %>%
    dplyr::select(-c(min_age, max_age)) %>%
    as.list()

  # Calculate age equivalent

  aev <-
    ae %>%
    expand_lookup(variable = "verbal_raw_score") %>%
    dplyr::filter(verbal_raw_score == verbal_score) %>%
    {.[, "age_equivalent", drop = TRUE]}

  aenv <-
    ae %>%
    expand_lookup(variable = "nonverbal_raw_score") %>%
    dplyr::filter(nonverbal_raw_score == nonverbal_score) %>%
    {.[, "age_equivalent", drop = TRUE]}


  kbit_results <-
    list(age = age,
         verbal = vrblStnd,
         nonverbal = nonvrblStnd,
         iq = iq_score,
         verbal_age_equivalent = aev,
         nonverbal_age_equivalent = aenv)

  if(print_kbit) print_kbit2_results(kbit_results)

  return(kbit_results)

}

#' @title print_kbit2_results
#' @description Prints out the KBIT-2 results from the `calculate_kbit2_score` function.
#' @param res Results from the `calculate_kbit2_score` function
#' @return NULL
#' @details Prints out the KBIT-2 results from the `calculate_kbit2_score` function.
#' @rdname print_kbit2_results
#' @export

print_kbit2_results <- function(res){

  message(
    sprintf(
      "KBIT-2 results for a %i-year old participant with a verbal score of %i and nonverbal score of %i.",
      res$age, res$verbal$raw_score, res$nonverbal$raw_score
      ), "\n\n",
    sprintf(
      "Verbal Standard Score: %i (90%% CI: %s); Percentile Rank: %s",
      res$verbal$standard_score, res$verbal$confidence_interval, res$verbal$percentile_rank
      ), "\n",
    sprintf(
      "Nonverbal Standard Score: %i (90%% CI: %s); Percentile Rank: %s",
      res$nonverbal$standard_score, res$nonverbal$confidence_interval, res$nonverbal$percentile_rank
    ), "\n",
    sprintf(
      "Intelligent Quotient %i (90%% CI: %s); Percentile Rank: %s",
      res$iq$standard_score, res$iq$confidence_interval, res$iq$percentile_rank
    ), "\n\n",
    sprintf(
      "Verbal Age Equivalent: %s; Nonverbal Age Equivalent: %s",
      res$verbal_age_equivalent, res$nonverbal_age_equivalent
    )
  )


}


