#' 포트폴리오 일간 수익률 계산 함수
#'
#' 포트폴리오 비중에 대해 거래수수료를 감안한 일간 수익률 계산
#' 포트비중의 날짜 종가로
#'
#' @param port_weight 포트비중 시계열. td(날짜), ticker(티커명), weight(비중) 열을 가지고 있어야 함.
#' @param rtn_tbl 종목 일간 수익률. 종목에 대한 일간수익률 데이터는
#' td, ticker, rtn(수익률) 열을 가지고 있어야 함.
#' @param trd_cost 거래비용. 디폴트는 30bp
#' @return 일별 포트 수익률과 비용감안 포트 수익률률
#' @export
get_port_return <- function(port_weight, rtn_tbl, trd_cost = 0.003) {

  port_weight <- port_weight %>% sum_port_weight()

  term_tbl <- get_term_tbl(port_weight, rtn_tbl)

  port_return <- rtn_tbl %>%
    filter(td >= min(term_tbl$td), td <= max(term_tbl$td)) %>%
    left_join(term_tbl, by = "td") %>%
    left_join(
      port_weight %>% rename(term = td),
      by = c("term", "ticker")
    ) %>%
    arrange(td) %>%

    calc_drifting_weight() %>%

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

get_term_tbl <- function(port_weight, rtn_tbl) {

  term_tbl <- tibble(
    td = unique(rtn_tbl$td)
  ) %>%
    filter(td >= min(port_weight$td)) %>%
    left_join(port_weight %>% distinct(td) %>% mutate(term = td), by = "td") %>%
    fill(term, .direction = "down") %>%
    mutate(rebal = td == term) %>%
    mutate(td = lead(td, 1)) %>%
    na.omit()

  return(term_tbl)
}

sum_port_weight <- function(port_weight) {
  port_weight %>%
    group_by(td, ticker) %>%
    summarise(weight = sum(weight)) %>%
    ungroup()
}

calc_drifting_weight <- function(dat) {
  dat %>%
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
}
