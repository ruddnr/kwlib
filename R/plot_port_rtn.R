#' 누적수익률 계산
#'
#'
#' @param dat get_plot_dat로 생성된 누적수익률
#' @export
plot_cum_return <- function(dat, title = NULL) {

  g1 <- dat %>%
    ggplot(aes(td, rtn_cum, color = type)) +
    geom_line() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = "", y = "", title = title)

  return(g1)
}

#' 포트폴리오 월별 수익률 시계열
#'
#' @param dat get_plot_dat에서 생성된 포트폴리오 수익률
#' @export
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
