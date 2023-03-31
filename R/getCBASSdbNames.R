#' Returns names of CBASS databases
#'
#' Returns names of CBASS databases
#' @export

getCBASSdbNames<- function(){
  return(c("information_schema", "cLayback", "cbass1", "cbass2_nuc1", "cbass2_nuc2", "cbass2_nuc3", "mysql", "performance_schema", "sys"))}
