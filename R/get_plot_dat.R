#' 포트폴리오 시각화를 위한 데이터 생성
#'
#' 포트 일간 수익률 데이터로부터 누적 수익률 데이터 생성
#' 포트 수익률은
#' 포트/bm 수익률 데이터가 모두 존재하는 기간에 대해서 수익률 생성
#'
#' @param port_return 포트폴리오 일간 수익률. rtn_with_cost 변수가 열에 있어야 함.
#' @param bm_return 벤치마크 일간 수익률
#' @return type에 포트/bm이 표시된 long format 누적 수익률
#' @export
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

get_rtn_cum <- function(dat, rtn_var) {
  dat %>%
    mutate(rtn_cum = cumprod({{rtn_var}} + 1) - 1) %>%
    select(td, rtn = {{rtn_var}}, rtn_cum)
}
