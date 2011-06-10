
#
# database utils
#
readFromDB <- function(dbFile, tablename, where="") {
    m <- dbDriver("SQLite")
    con <- dbConnect(m,dbname=dbFile)
    sql <- paste("select * from ", tablename, where)
    print(paste(dbFile, ": ", sql))
    data <- dbGetQuery(con, sql)
    sqliteCloseConnection(con)
    sqliteCloseDriver(m)
    if ("ACTIVITY_TS" %in% colnames(data)) 
       data$ACTIVITY_TS <- as.Date(data$ACTIVITY_TS,'%Y-%m-%d')
    data
}

writeToDb <- function(dbFile, tablename, df) {
  # Write dataframe to database table
  m <- dbDriver("SQLite")
	conn <- dbConnect(m, dbname=dbFile)
	dbWriteTable(conn, tablename, df, overwrite=T)

	# cleanup sql connection
	sqliteCloseConnection(conn)
	sqliteCloseDriver(m)

}
