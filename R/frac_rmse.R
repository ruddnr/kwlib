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
#' @param frac Proportion of data to use in calculating rmse. If the value is less then 1,
#' then it is considered as a proportion. If the value is greater than or equal to 1,
#' then it is considered as a number of data points used in calculation.
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

frac_rmse <- yardstick::new_numeric_metric(frac_rmse, direction = "minimize")

#' @rdname frac_rmse
#' @export
frac_rmse.data.frame <- function(data, truth, estimate, frac = 0.1, ans_order = "desc",
                                 na_rm = TRUE, case_weights = NULL, ...) {

  yardstick::numeric_metric_summarizer(
    name = "frac_rmse",
    fn = frac_rmse_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    na_rm = na_rm,
    case_weights = !!rlang::enquo(case_weights),
    fn_options = list(frac = frac, ans_order = ans_order)
  )
}

#' @export
#' @rdname frac_rmse
frac_rmse_vec <- function(truth, estimate, frac = 0.1, ans_order = "desc", na_rm = TRUE, case_weights = NULL, ...) {
  yardstick::check_numeric_metric(truth, estimate, case_weights)

  if (na_rm) {
    result <- yardstick::yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick::yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  frac_rmse_impl(truth, estimate, frac = frac, ans_order = ans_order, case_weights = case_weights)
}

frac_rmse_impl <- function(truth, estimate, frac, ans_order = c("desc", "asc"), case_weights = NULL) {
  if (ans_order == "desc") {
    estimate2 <- dplyr::desc(estimate)
  } else {
    estimate2 <- estimate
  }
  if (frac < 1) {
    idx <- which(dplyr::percent_rank(estimate2) <= frac)
  } else {
    idx <- which(dplyr::row_number(estimate2) <= frac)
  }

  errors <- (truth[idx] - estimate[idx]) ^ 2
  sqrt(yardstick_mean(errors, case_weights = case_weights))
}

yardstick_mean <- function(x, ..., case_weights = NULL, na_remove = FALSE) {
  rlang::check_dots_empty()

  if (is.null(case_weights)) {
    mean(x, na.rm = na_remove)
  } else {
    case_weights <- vctrs::vec_cast(case_weights, to = double())
    stats::weighted.mean(x, w = case_weights, na.rm = na_remove)
  }
}
# data("solubility_test")
#
frac_rmse_vec(
  truth = solubility_test$solubility,
  estimate = solubility_test$prediction,
  frac = 0.1
)
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
solubility_resampled %>%
  group_by(resample) %>%
  frac_rmse(solubility, prediction, frac = 0.1)

solubility_resampled %>%
  group_by(resample) %>%
  slice_max(prediction, n = 10) %>%
  rmse(solubility, prediction)
