#' winsorize 레시피 스텝
#'
#' tidy modeling시 사용할 레시피 스텝
#' @param recipe recipe object
#' @param role
#'
#' @export
step_winsorize <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           probs = c(0.05, 0.95),
           min_vals = NULL,
           max_vals = NULL,
           na_rm = TRUE,
           skip = FALSE,
           id = rand_id("winsorize")) {
    add_step(
      recipe,
      step_winsorize_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        probs = probs,
        min_vals,
        max_vals,
        na_rm = na_rm,
        skip = skip,
        id = id,
        case_weights = NULL
      )
    )
  }

step_winsorize_new <-
  function(terms, role, trained, probs, min_vals, max_vals, na_rm, skip, id, case_weights) {
    step(
      subclass = "winsorize",
      terms = terms,
      role = role,
      trained = trained,
      probs = probs,
      min_vals = min_vals,
      max_vals = max_vals,
      na_rm = na_rm,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }

calc_prob_vals <- function(dat, col_names, prob, na.rm){
  dat[, col_names] %>%
    map_dfr(quantile, prob, na.rm) %>%
    as_vector() %>%
    set_names(col_names)
}

#' @export
prep.step_winsorize <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names])

  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts, unsupervised = TRUE)
  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  min_vals <- calc_prob_vals(training, col_names, prob = x$probs[1], na.rm = x$na_rm)
  max_vals <- calc_prob_vals(training, col_names, prob = x$probs[2], na.rm = x$na_rm)

  step_winsorize_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    probs = x$probs,
    min_vals = min_vals,
    max_vals = max_vals,
    na_rm = x$na_rm,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

#' @export
bake.step_winsorize <- function(object, new_data, ...) {
  check_new_data(names(object$min_vals), object, new_data)

  for (column in names(object$min_vals)) {
    min_val <- object$min_vals[column]
    max_val <- object$max_vals[column]
    new_data[[column]] <- DescTools::Winsorize(new_data[[column]], minval = min_val, maxval = max_val)
  }
  new_data
}

#' @export
print.step_winsorize <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Winsorizing for "
    print_step(names(x$min_vals), x$terms, x$trained, title, width,
               case_weights = x$case_weights)
    invisible(x)
  }

#'
#' #' @rdname tidy.recipe
#' #' @export
#' tidy.step_normalize <- function(x, ...) {
#'   if (is_trained(x)) {
#'     res <- tibble(
#'       terms = c(names(x$means), names(x$sds)),
#'       statistic = rep(c("mean", "sd"), each = length(x$sds)),
#'       value = unname(c(x$means, x$sds))
#'     )
#'   } else {
#'     term_names <- sel2char(x$terms)
#'     res <- tibble(
#'       terms = term_names,
#'       statistic = na_chr,
#'       value = na_dbl
#'     )
#'   }
#'   res$id <- x$id
#'   res
#' }
#'
#
# modeldata::attrition
#
# dat_split <- initial_split(attrition)
# train <- training(dat_split)
# test <- testing(dat_split)
#
# model_rec <- attrition %>%
#   recipe(Attrition ~ ., data = train) %>%
#   step_winsorize(all_numeric(), probs = c(0.1, 0.9)) %>%
#   step_dummy(all_nominal_predictors())
#
# model_spec <- logistic_reg(
#   mode = "classification", engine = "glmnet", penalty = 0.001, mixture = 0.5
# )
#
# wflow <-
#   workflow() %>%
#   add_model(model_spec) %>%
#   add_recipe(model_rec)
#
# model_fit <- fit(wflow, train)
#
# dat_folds <- vfold_cv(train)
#
# library(doParallel)
#
# set.seed(2021)
# cl <- makePSOCKcluster(12)
# registerDoParallel(cl)
# model_res <- fit_resamples(wflow, dat_folds)
# stopCluster(cl)

