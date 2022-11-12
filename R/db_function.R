#' DB 연결 함수
#'
#' QPMS DB에 연결하여 데이터 반환
#'
#' @param dbo_name dbo 이름
#' @param table_name 테이블 명
#' @return collect 전의 tbl
#' @export
tbl_qpms <- function(dbo_name, table_name) {
  tbl(con_qpms, in_schema(sql(str_glue("{dbo_name}.dbo")), sql(table_name)))
}

#' @rdname tbl_qpms
#' @export
connect_qpms_db <- function(dsn = "QPMS") {
  con_qpms <<- odbc::dbConnect(odbc::odbc(),
                         dsn = dsn,
                         uid = "quant",
                         pwd = "mirae",
                         encoding = "EUC-KR")
}

#' @rdname tbl_qpms
#' @export
disconnect_qpms_db <- function() {
  odbc::dbDisconnect(con_qpms)
  rm(con_qpms, envir = globalenv())
}
