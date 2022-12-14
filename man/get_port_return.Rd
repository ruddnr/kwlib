% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_port_return.R
\name{get_port_return}
\alias{get_port_return}
\title{포트폴리오 일간 수익률 계산 함수}
\usage{
get_port_return(port_weight, rtn_tbl, trd_cost = 0.003, adjust_rebal_date = 1)
}
\arguments{
\item{port_weight}{포트비중 시계열. td(날짜), ticker(티커명), weight(비중) 열을
가지고 있어야 함.}

\item{rtn_tbl}{종목 일간 수익률. 종목에 대한 일간수익률 데이터는
td, ticker, rtn(수익률) 열을 가지고 있어야 함.}

\item{trd_cost}{거래비용. 디폴트는 30bp}

\item{adjust_rebal_date}{리밸런싱 일자 조정. 디폴트 값 1일 경우, 포트 비중에
적힌 날짜의 종가로 매수하여, 그 다음날 수익률이 적용된다고 가정.
예를들면 비중 데이터의 날짜가 10월 31일이라면 해당 비중으로 11월 1일 수익률부터 적용됨.
데이터 가용시점을 고려한 보수적인 리밸런싱을 가정할 경우 날짜를 더 미룰 수 있음.}
}
\value{
일별 포트 수익률과 비용감안 포트 수익률
}
\description{
포트폴리오 비중에 대해 거래수수료를 감안한 일간 수익률 계산
포트비중의 날짜 종가로
}
