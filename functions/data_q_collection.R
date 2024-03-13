## ----------------------------------------------
## Function: applies all q-data-loading-functions in one batch to create common sql-database
##
## Output:  NULL, but creates a SQL-db with all daily measurements for each station 
## Input:   stations_tbl - list of selected gauging stations for download
## ----------------------------------------------
load_q_in_db <- function(stations_tbl){
  
  # define db directory
  db_path <- "data/discharge/q.db"
  log_file_path <- "logfiles/log_q_measurements.log"
  
  #Check q db existence
  if (file.exists(db_path)) {
    #Delete file if it exists
    file.remove(db_path)
  }
  
  #Check logfle existence
  if (file.exists(log_file_path)) {
    #Delete file if it exists
    file.remove(log_file_path)
  }
  
  # load q measurements
  load_q_FRA(stations_tbl = stations_tbl, 
             date_start = "2017-01-01", date_end = "2024-02-29",
             db_path, log_file_path)
  
  return(db_path)
}



## ----------------------------------------------
## Function: supporting function to load measurements from sql-table
##
## Output:  tibble with all discharge measurements for a spcific station 
## Input:   station_name - specific gauging station for which the data should be loaded 
##          db_path - file path of sql-database with discharge measurements
## ----------------------------------------------
load_qdata_sql <- function(station_name, db_path) {
  
  require(RSQLite)
  
  # Connect to the SQLite database
  sqlconnection <-
    dbConnect(SQLite(), dbname = db_path)
  
  data_station <- NULL  # Define data_station here
  
  tryCatch({
    # load data from database
    query <-dbSendQuery(sqlconnection, paste("SELECT * FROM ", station_name, sep = ""))
    
    data_station <- dbFetch(query) |> as_tibble()
    
    # clear results query
    dbClearResult(query)
    
  }, error = function(e) {
    # Handle the error (e.g., print a message or log it)
    cat(paste("Error loading data for station:", station_name, "\n"))
  })
  
  # disconnect from sql database
  dbDisconnect(sqlconnection)
  
  return(data_station)
}


## ----------------------------------------------
## Function: download of daily discharge data for french stations in sql-table 
##
## Output:  NULL, but creates a SQL-db with all daily measurements for each station 
## Input:   stations_tbl - list of selected gauging stations for download
##          date_start - first date of measurements
##          date_end - last date of measurements
## ----------------------------------------------
load_q_FRA <- function(stations_tbl, date_start, date_end, db_path, logfile_path) {
  
  require(hubeau)   # retrieve discharge measurements via hubeau
  require(RSQLite)  # store in sql-db
  require(logging)  # log messages/errors
  
  # set up logging: bootstrap logging package, configurate parameters, start logging
  basicConfig() 
  addHandler(writeToFile, logger = "log_discharge_download", file = logfile_path)
  loginfo("Script started.") 
  
  # Connect to the SQLite database (adjust the file path)
  sqlconnection <- dbConnect(SQLite(), dbname = db_path)
  
  # loop to save discharge data from all selected stations in a database
  for (gauge_i in unique(stations_tbl$ID_station)) {
    
    tryCatch({
      # get data for each station accd. to date_start, date_end, select daily values
      data_i <- get_hydrometrie_obs_elab(
        code_entite = gauge_i,
        date_debut_obs_elab = date_start,
        date_fin_obs_elab = date_end,
        grandeur_hydro_elab = "QmJ" 
      ) |> 
        mutate(date = date_obs_elab,
               q_m3_s = resultat_obs_elab/1000)|>
        filter(libelle_qualification == "Bonne")|>
        select(code_site, date, q_m3_s) |> 
        mutate(date = as.character(date)) |> 
        filter(!is.na(q_m3_s))
      
      loginfo(paste("API request completed for station ", gauge_i),
              logger = "log_discharge_download")
      
      # append measurements to table
      dbWriteTable(sqlconnection, gauge_i, data_i, append = TRUE)
      
      # Log messages to track progress
      loginfo(paste("Data written to file: ", gauge_i), logger = "log_discharge_download")
    }, error = function(e) {
      logerror(paste("!!! Error occurred: ", e$message), logger = "log_discharge_download")
    })
    
  }
  
  # Close the database connection when done
  dbDisconnect(sqlconnection)
  
  return()
}