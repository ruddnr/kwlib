#' 포트폴리오 시뮬 summary statistic 생성
#'
#' port_return으로부터 시뮬레이션 결과 생성
#' 연간수익률, 최대낙폭, 평균 턴오버 계산
#'
#' @param port_return get_port_return 함수에서 반환된 포트폴리오 수익률
#' @return 시뮬레이션 결과 요약 tbl
#' @export
get_port_sim <- function(port_return) {
  bind_cols(
    tidyquant::tq_performance(port_return, rtn_with_cost, performance_fun = table.AnnualizedReturns),
    tudyquant::tq_performance(port_return, rtn_with_cost, performance_fun = maxDrawdown),
    tudyquant::tq_performance(port_return, rtn_with_cost, performance_fun = DownsideDeviation),
    port_return %>% summarise(avg_turnover = sum(diff)/(nrow(.)/250))
  )
}
