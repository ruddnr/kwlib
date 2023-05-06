#' partial root mean square error
#'
#' @param data A `data.frame` containing the columns specified by the `truth`
#' and `estimate` arguments.
#'
#' @param truth The column identifier for the true results
#' (that is `numeric`). This should be an unquoted column name although
#' this argument is passed by expression and supports
#' [quasiquotation][rlang::quasiquotation] (you can unquote column
#' names). For `_vec()` functions, a `numeric` vector.
#'
#' @param estimate The column identifier for the predicted
#' results (that is also `numeric`). As with `truth` this can be
#' specified different ways but the primary method is to use an
#' unquoted variable name. For `_vec()` functions, a `numeric` vector.
#'
#' @param frac Proportion of data to use in calculating rmse
#'
#' @param na_rm A `logical` value indicating whether `NA`
#' values should be stripped before the computation proceeds.
#'
#' @param case_weights The optional column identifier for case weights. This
#' should be an unquoted column name that evaluates to a numeric column in
#' `data`. For `_vec()` functions, a numeric vector.
#'
#' @param ... Not currently used.
#'
#' @export
frac_rmse <- function(data, ...) {
  UseMethod("frac_rmse")
}

frac_rmse <- new_numeric_metric(frac_rmse, direction = "minimize")

#' @rdname frac_rmse
#' @export
frac_rmse.data.frame <- function(data, truth, estimate, frac = 0.1, na_rm = TRUE, case_weights = NULL, ...) {

  numeric_metric_summarizer(
    name = "frac_rmse",
    fn = frac_rmse_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights),
    fn_options = list(frac = frac)
  )
}

#' @export
#' @rdname frac_rmse
frac_rmse_vec <- function(truth, estimate, frac = 0.1, na_rm = TRUE, case_weights = NULL, ...) {
  check_numeric_metric(truth, estimate, case_weights)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  frac_rmse_impl(truth, estimate, frac = frac, case_weights = case_weights)
}

frac_rmse_impl <- function(truth, estimate, frac, case_weights = NULL) {
  idx <- which(percent_rank(desc(truth)) <= frac)
  errors <- (truth[idx] - estimate[idx]) ^ 2
  sqrt(yardstick_mean(errors, case_weights = case_weights))
}

yardstick_mean <- function(x, ..., case_weights = NULL, na_remove = FALSE) {
  check_dots_empty()

  if (is.null(case_weights)) {
    mean(x, na.rm = na_remove)
  } else {
    case_weights <- vec_cast(case_weights, to = double())
    stats::weighted.mean(x, w = case_weights, na.rm = na_remove)
  }
}
# data("solubility_test")
#
# frac_rmse_vec(
#   truth = solubility_test$solubility,
#   estimate = solubility_test$prediction,
#   frac = 0.1
# )
#
# set.seed(1234)
# size <- 100
# times <- 10
#
# # create 10 resamples
# solubility_resampled <- bind_rows(
#   replicate(
#     n = times,
#     expr = sample_n(solubility_test, size, replace = TRUE),
#     simplify = FALSE
#   ),
#   .id = "resample"
# )
#
# solubility_resampled %>%
#   group_by(resample) %>%
#   frac_rmse(solubility, prediction, frac = 0.1)
#
# solubility_resampled %>%
#   group_by(resample) %>%
#   rmse(solubility, prediction, frac = 0.1)
