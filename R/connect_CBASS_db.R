#' Connect to a CBASS database
#'
#' Connect to a CBASS database. Names of databases can be accessed with getCBASSdbNames(). Must be connected through sars-tunnel.
#' @param dbname character string with name of database. Names of databases can be accessed with getCBASSdbNames(). Database names are "information_schema", "cLayback", "cbass1", "cbass2_nuc1", "cbass2_nuc2", "cbass2_nuc3", "mysql", "performance_schema", "sys".
#' @param user username
#' @param password password
#' @importFrom RMariaDB dbConnect
#' @importFrom RMariaDB MariaDB
#' @export

connect_CBASS_db<- function(dbname, user= "dataReader", password= "12345"){
  return(RMariaDB::dbConnect(RMariaDB::MariaDB(), user=user, password=password, dbname=dbname))
}
