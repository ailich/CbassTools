#' Load database table into memory as dataframe (tibble)
#'
#' Load database table into memory as dataframe (tibble)
#' @param conn A DBIConnection object, as returned by dbConnect().
#' @param table a character string with the name of the remote table.
#' @importFrom dplyr tbl
#' @importFrom dplyr as_tibble
#' @export

import_db_table<- function(conn, table){
  out<- dplyr::tbl(conn, table) %>% dplyr::as_tibble()
  return(out)
}
