# 0. 포트폴리오 시뮬레이션 함수 ----
# 포트폴리오 동일가중 수익률 계산
# 티커명 - ticker. 파일에 따라 code인 경우도 있으니 주의.
get_port_return <- function(port_weight, rtn_tbl, trd_cost = 0.003) {
  
  term_tbl <- tibble(
    td = unique(rtn_tbl$td)
  ) %>%
    filter(td >= min(port_weight$td)) %>% 
    left_join(port_weight %>% distinct(td) %>% mutate(term = td), by = "td") %>% 
    fill(term, .direction = "down") %>% 
    mutate(rebal = td == term) %>% 
    mutate(td = lead(td, 1)) %>% 
    na.omit()
  
  port_return <- rtn_tbl %>% 
    filter(td >= min(term_tbl$td), td <= max(term_tbl$td)) %>% 
    left_join(term_tbl, by = "td") %>% 
    left_join(
      port_weight %>% rename(term = td), 
      by = c("term", "ticker")
    ) %>% 
    arrange(td) %>% 
    
    # Drifting weight 계산
    group_by(term, ticker) %>% 
    # fill(weight, .direction = "down") %>% 
    mutate(weight = replace_na(weight, 0)) %>%
    mutate(weight2 = cumprod(1 + rtn) * weight) %>%
    mutate(weight2 = lag(weight2)) %>% 
    mutate(weight = if_else(is.na(weight2), weight, weight2)) %>% 
    ungroup() %>% 
    select(-weight2) %>% 
    
    # 비중 노멀라이즈
    group_by(td) %>% 
    mutate(weight = weight / sum(weight)) %>% 
    ungroup() %>% 
    
    group_by(ticker) %>% 
    mutate(diff = if_else(rebal == TRUE, weight - lag(weight), 0)) %>% 
    # 분석초기 weight_chg 0으로 취급
    mutate(diff = replace_na(diff, 0)) %>% 
    ungroup() %>% 
    
    group_by(td) %>% 
    summarise(
      rtn = sum(rtn * weight, na.rm = TRUE),
      diff = sum(abs(diff), na.rm  = TRUE) / 2
    ) %>% 
    mutate(rtn_with_cost = rtn - diff * trd_cost) %>% 
    ungroup() 
  
  return(port_return)
}

get_port_sim <- function(port_return) {
  bind_cols(
    tq_performance(port_return, rtn_with_cost, performance_fun = table.AnnualizedReturns),
    tq_performance(port_return, rtn_with_cost, performance_fun = maxDrawdown),
    port_return %>% summarise(avg_turnover = sum(diff)/(nrow(.)/250))
  )
}

get_rtn_cum <- function(dat, rtn_var) {
  dat %>%
    mutate(rtn_cum = cumprod({{rtn_var}} + 1) - 1) %>%
    select(td, rtn = {{rtn_var}}, rtn_cum)
}

# 0.2. 포트폴리오 시각화 함수 ----
get_plot_dat <- function(port_return, bm_return) {
  start_date <- max(min(port_return$td), min(bm_return$td))
  end_date <- min(max(port_return$td), max(bm_return$td))
  
  port_return <- port_return %>% filter(td >= start_date, td <= end_date)
  bm_return <- bm_return %>% filter(td >= start_date, td <= end_date)
  
  dat <- bind_rows(
    get_rtn_cum(port_return, rtn_with_cost) %>% mutate(type = "port"),
    get_rtn_cum(bm_return, rtn) %>% mutate(type = "bm")
  )
  
  return(dat)
}

plot_cum_return <- function(dat, title = NULL) {
  
  g1 <- dat %>% 
    ggplot(aes(td, rtn_cum, color = type)) +
    geom_line() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::percent_format()) + 
    labs(x = "", y = "", title = title)
  
  return(g1)
}

plot_monthly_return <- function(dat) {
  # 월별 수익률
  dat2 <- dat %>% 
    mutate(yearmon = as.yearmon(td)) %>% 
    group_by(type, yearmon) %>% 
    summarise(rtn = prod(rtn + 1) -1) %>% 
    ungroup()
  
  dat2 <- dat2 %>% 
    pivot_wider(names_from = type, values_from = rtn) %>% 
    mutate(rtn = port - bm) %>% 
    select(yearmon, rtn) %>% 
    mutate(type = "diff")
  
  
  g2 <- dat2 %>% 
    ggplot(aes(yearmon, rtn, fill = type)) +
    geom_col(position = "dodge") + 
    scale_fill_tq() + 
    scale_y_continuous(labels = scales::percent_format()) + 
    labs(x = "", y = "", title = "월별 초과수익률")
  
  return(g2)
}

plot_weight_ts <- function(port_weight) {
  g <- port_weight %>% 
    group_by(td, theme) %>% 
    summarise(theme_weight = sum(weight)) %>% 
    ungroup() %>% 
    mutate(weight = str_glue("{td}
                                 {theme} : {percent(theme_weight, accuracy = .01)}")) %>% 
    ggplot(aes(td, theme_weight, fill = theme, label = weight)) + 
    geom_area() + 
    scale_fill_tq() +
    scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 0.01)) +
    theme(legend.position = "right") + 
    labs(x = "", y = "")
  ggplotly(g, tooltip = "label")
}

# 모델링 ----

## 병렬연산 초기화 ----
unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

get_model_res <- function(model_rec, model_spec, resamples, control, metrics) {
  model_wf <- workflow() %>% 
    add_recipe(model_rec) %>% 
    add_model(model_spec)
  
  set.seed(2021)
  cl <- makePSOCKcluster(12)
  registerDoParallel(cl)
  model_res <- fit_resamples(
    model_wf,
    resamples = resamples,
    control = control,
    metrics = metrics
  )
  stopCluster(cl)
  
  unregister_dopar()
  
  return(model_res)
}

# get_model_score <- function(model_res, model_dat, pred_col = .pred) {
#   model_predictions <- model_res %>% 
#     collect_predictions() %>% 
#     select(model = {{pred_col}}, .row) 
#   
#   model_score <- model_dat %>% 
#     mutate(.row = row_number()) %>% 
#     left_join(model_predictions) %>% 
#     filter(!is.na(model)) %>% 
#     select(td, ticker, rtn_nxt, model) 
#   
#   model_score
# }

get_model_score <- function(model_res, model_dat, pred_col = .pred) {
  model_predictions <- model_res %>% 
    collect_predictions() %>% 
    select(model_name = .config, model = {{pred_col}}, .row) 
  
  model_score <- model_dat %>% 
    mutate(.row = row_number()) %>% 
    left_join(model_predictions) %>% 
    filter(!is.na(model)) %>% 
    select(td, ticker, rtn_nxt, model, model_name) 
  
  model_score
}



get_model_rank <- function(model_score) {
  model_score %>% 
    group_by(td) %>% 
    mutate(rank = row_number(-model)) %>% 
    ungroup() %>% 
    
    group_by(ticker) %>% 
    mutate(rank_prev = lag(rank)) %>% 
    mutate(rank_prev = if_else(is.na(rank_prev), rank, rank_prev)) %>% 
    ungroup()
}

# 인덱스 모델 랭크 변화에 따른 편출입 제한
gen_port <- function(margin, n_index) { 
  
  force(margin)
  force(n_index)
  
  prev_selected <- NULL
  
  type_info <- read_xlsx("us_factor.xlsx", sheet = "ticker_info") %>% 
    select(type, ticker)
  
  # type threshold는 선택 인덱스의 절반 이상이라고 가정 
  type_threshold <- floor(n_index * 0.5)
  
  function(port) {
    
    port <- port %>% 
      left_join(type_info, by = "ticker") %>% 
      group_by(type) %>% 
      mutate(rank_type = row_number(-model)) %>% 
      ungroup()
    
    if(is.null(prev_selected)) {
      prev_selected <<- port %>% 
        filter(rank_type <= type_threshold) %>% 
        slice_min(rank, n = n_index) %>% 
        pull(ticker) 
    }
    
    cur_port <- port %>% 
      mutate(prev_selected = ticker %in% prev_selected)
    
    # 랭크 비교 결과 잔존 종목
    remain <- cur_port %>%  
      filter(prev_selected == TRUE & rank <= n_index + margin) %>% 
      mutate(selected = TRUE)
    
    remain_type_cnt <- remain %>% 
      count(type, name = "type_count") %>% 
      mutate(type_resid = type_threshold - type_count) %>% 
      select(type, type_resid)
    
    if(nrow(remain) == n_index) {
      res <- remain
    } else {
      addition <- cur_port %>% 
        anti_join(remain, by = "ticker") %>% 
        left_join(remain_type_cnt, by = "type") %>% 
        mutate(type_resid = replace_na(type_resid, type_threshold)) %>% 
        
        group_by(type) %>% 
        mutate(rank_type = row_number(-model)) %>% 
        ungroup() %>% 
        filter(rank_type <= type_resid) %>% 
        
        arrange(rank) %>% 
        dplyr::slice(1:(n_index - nrow(remain))) %>% 
        mutate(selected = TRUE)
      
      res <- bind_rows(remain, addition)
    }
    
    prev_selected <<- res %>% pull(ticker)
    
    return(res)
  }
}



calc_weight <- function(dat, type = "eq") {
  if(type == "eq") {
    dat <- dat %>% 
      group_by(td) %>% 
      mutate(weight = 1 / n()) %>% 
      ungroup()
  }
  else if(type == "score") {
    dat <- dat %>% 
      group_by(td) %>% 
      mutate(weight = model / sum(model)) %>% 
      ungroup()
  }
  
  return(dat)
}

gen_port_helper <- function(model_score, margin, n_index, weight_type = "eq") { 
  model_rank <- model_score %>% 
    get_model_rank()
  
  res <- model_rank %>% 
    nest(port = -td) %>% 
    mutate(final_port = map(port, gen_port(margin, n_index))) %>% 
    select(td, final_port) %>% 
    unnest(final_port) %>% 
    calc_weight(type = weight_type)
  
  return(res)
}

calc_mean_score <- function(...) {
  model_list <- list(...)
  res <- reduce(model_list, left_join, by = c("td", "ticker", "rtn_nxt")) %>%
    pivot_longer(contains("model"), names_to = "name", values_to = "model") %>% 
    group_by(td, ticker, rtn_nxt) %>% 
    summarise(model = mean(model)) %>% 
    ungroup()
  
  return(res)
}

# 나중에 지우기 ..
plot_model_score <- function(model_score, 
                             margin = 3, n_index = 3,
                             model_name = NULL, save = FALSE) {
  port_weight <- gen_port_helper(model_score, margin = margin, n_index = n_index)
  port_return <- get_port_return(port_weight, daily_rtn)
  port_sim <- get_port_sim(port_return)
  
  plot_dat <- get_plot_dat(port_return, bm_rtn)
  g <- plot_cum_return(plot_dat) + 
    ggtitle(model_name)
  
  if(save & !is.null(model_name)) {
    ggsave(str_c("model_res_png/",model_name,".png"), g, width = 7, height = 5)
  }
  
  g
}

plot_model_weight <- function(port_weight, daily_rtn, bm_rtn,
                             model_name = NULL, save = FALSE) {
  port_return <- get_port_return(port_weight, daily_rtn)
  port_sim <- get_port_sim(port_return)
  
  plot_dat <- get_plot_dat(port_return, bm_rtn)
  g <- plot_cum_return(plot_dat) + 
    ggtitle(model_name)
  
  if(save & !is.null(model_name)) {
    ggsave(str_c("model_res_png/",model_name,".png"), g, width = 7, height = 5)
  }
  
  g
}


# tune 결과 시뮬레이션
get_tune_sim <- function(tune_dat, grid_dat, model_dat, pred_col, daily_rtn, bm_rtn,
                         margin = 3, n_index = 3) {
  plan(multisession)
  res <- tune_dat %>% 
    get_model_score(model_dat = model_dat, pred_col = .pred) %>% 
    nest(data = -model_name) %>% 
    rename(model_score = data) %>% 
    mutate(port_weight = future_map(model_score, gen_port_helper, margin = margin, n_index = n_index),
           port_return = future_map(port_weight, get_port_return, daily_rtn),
           port_sim = future_map(port_return, get_port_sim),
           plot_dat = future_map(port_return, get_plot_dat, bm_rtn)) %>% 
    bind_cols(grid_dat)
  plan(sequential)
  
  return(res)
}

copy_tune_sim <- function(tune_sim) {
  tune_sim %>% 
    select(-c(model_score, port_weight, port_return, plot_dat)) %>%
    unnest(port_sim) %>%
    arrange(desc(`AnnualizedSharpe(Rf=0%)`)) %>% 
    write_clip()
}

plot_tune_sim_boxplot <- function(tune_sim) {
  tune_sim %>% 
    unnest(port_sim) %>% 
    select(-c(model_name, model_score, port_weight, port_return, plot_dat)) %>% 
    rename(sharpe = `AnnualizedSharpe(Rf=0%)`) %>% 
    pivot_longer(-c(1:5)) %>% 
    mutate(value = factor(value)) %>% 
    ggplot(aes(value, sharpe)) + 
    geom_violin(width = 0.7) +
    geom_boxplot(width = 0.3) +
    facet_wrap(~name, ncol = 2, scales = "free") +
    theme_tq() +
    theme(legend.position = "none")
}

# indv model ----

# index port 에 있는 종목만 추출, size 컬럼 추가
modify_model_score_indv <- function(model_score, etf_mv, index_port) {
  
  model_score %>% 
    left_join(index_port, by = c("td", "theme")) %>% 
    filter(!is.na(theme_weight)) %>% 
    left_join(etf_mv %>% select(theme, td, ticker, size), by = c("theme", "td", "ticker")) %>% 
    arrange(td, theme)
}

# sector ETF만
modify_model_score_indv2 <- function(model_score, etf_mv) {
  sector_vec <- c("XLE", "XLF", "XLU", "XLI", "XLK", 
                  "XLV", "XLB", "XLP", "XLY", "XTL", "XLC")

  model_score2 <- model_score %>% 
    filter(theme %in% sector_vec) %>% 
    left_join(etf_mv %>% 
                filter(theme == "SPY") %>% 
                select(td, ticker, size),
              by = c("td", "ticker")) %>% 
    filter(!is.na(size))
  
  sector_weight <- model_score2 %>% 
    group_by(td, theme) %>% 
    summarise(size = sum(size), .groups = "drop") %>% 
    group_by(td) %>% 
    mutate(theme_weight = size / sum(size), .keep = "unused") %>% 
    ungroup()
    
  model_score2 <- model_score2 %>% 
    left_join(sector_weight)
  
  model_score2
}

# 개별종목 포트 시총, 모델 랭크순 추출 함수
make_indv_port <- function(model_score, n_mcap, n_model, weight_type = "eq") {
  model_score <- model_score %>% 
    group_by(theme, td)
  
  if(n_mcap > 0) {
    port_mcap <- model_score %>% 
      slice_max(size, n = n_mcap)
  } else {
    port_mcap <- tibble()
  }
  
  if(n_model > 0){
    # 시총으로 뽑힌 종목은 제외
    if(nrow(port_mcap) > 0) {
      model_score <- model_score %>% 
        anti_join(port_mcap, by = c("td", "theme", "ticker"))
    }
    
    port_model <- model_score %>% 
      slice_max(model, n = n_model)
  } else {
    port_model <- tibble()
  }
  
  
  res <- bind_rows(port_mcap, port_model) %>% 
    group_by(theme, td)
  
  if(weight_type == "eq") {
    res <- res %>% 
      mutate(weight = theme_weight / n())
  } else if(weight_type == "model") {
    res <- res %>% 
      mutate(weight = theme_weight * model / sum(model))
  } else if(weight_type == "size") {
    res <- res %>% 
      mutate(weight = theme_weight * size / sum(size))
  }
  
  res <- res %>% 
    ungroup() %>% 
    arrange(td, theme)   
  
  return(res)
}

# 개별종목 포트 시총, 모델 랭크순 추출 함수
make_indv_port2 <- function(model_score, n_mcap, n_model, weight_type = "eq") {
  model_score <- model_score %>% 
    group_by(theme, td)
  
  if(n_mcap > 0) {
    port_mcap <- model_score %>% 
      slice_max(size, n = n_mcap)
  } else {
    port_mcap <- tibble()
  }
  
  if(n_model > 0){
    # 시총으로 뽑힌 종목은 제외
    if(nrow(port_mcap) > 0) {
      model_score <- model_score %>% 
        anti_join(port_mcap, by = c("td", "theme", "ticker"))
    }
    
    port_model <- model_score %>% 
      slice_max(model, n = n_model)
  } else {
    port_model <- tibble()
  }
  
  
  res <- bind_rows(port_mcap, port_model) %>% 
    group_by(theme, td)
  
  if(weight_type == "eq") {
    res <- res %>% 
      mutate(weight = theme_weight / n())
  } else if(weight_type == "model") {
    res <- res %>% 
      mutate(weight = theme_weight * model / sum(model))
  }
  
  res <- res %>% 
    ungroup() %>% 
    arrange(td, theme)   
  
  return(res)
}

sum_indv_port_weight <- function(port_weight) {
  port_weight %>% 
    group_by(td, ticker, name) %>% 
    summarise(weight = sum(weight)) %>% 
    ungroup()
}

# theme 칼럼만 추가 선택
get_model_score_indv <- function(model_res, model_dat, pred_col = .pred, index_port) {
  model_predictions <- model_res %>% 
    collect_predictions() %>% 
    select(model_name = .config, model = {{pred_col}}, .row) 
  
  model_score <- model_dat %>% 
    mutate(.row = row_number()) %>% 
    left_join(model_predictions, by = ".row") %>% 
    filter(!is.na(model)) %>% 
    select(theme, td, ticker, name, rtn_nxt, model, model_name) 
  
  model_score <- model_score %>% 
    modify_model_score_indv(model_dat, index_port) 
    
  model_score
}

# indv tune 결과 시뮬레이션
get_indv_tune_sim <- function(tune_dat, grid_dat, model_dat, index_port,
                              pred_col, daily_rtn, bm_rtn,
                         n_mcap = 2, n_model = 1) {
  plan(multisession)
  res <- tune_dat %>% 
    get_model_score_indv(model_dat = model_dat, pred_col = .pred, index_port) %>% 
    nest(data = -model_name) %>% 
    rename(model_score = data) %>% 
    mutate(port_weight = future_map(model_score, make_indv_port, n_mcap = n_mcap, n_model = n_model),
           port_weight = map(port_weight, sum_indv_port_weight),
           port_return = future_map(port_weight, get_port_return, daily_rtn),
           port_sim = future_map(port_return, get_port_sim),
           plot_dat = future_map(port_return, get_plot_dat, bm_rtn)) %>% 
    bind_cols(grid_dat)
  plan(sequential)
  
  return(res)
}


