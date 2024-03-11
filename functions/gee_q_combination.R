## ----------------------------------------------
## Function: loads results from GEE analysis and combines it in common table
##
## Output:    list with combined discharge and satellite data for each reach-gauge combination
## Input:     GEE-Satellite observations (Landsat or Sentinel-2), reaches-gauges over view table, path of db with discharge measurements
## ----------------------------------------------
combine_q_gee_data <- function(station_tbl, gee_data_tbl, q_db_path, scale){
  
  require(RSQLite)
  
  # create empty list to store all data in
  data_combined_tbl <- list()
  
  # loop through each station-reach combi and extract necessary data
  for (combi in station_tbl$ID_combi) {
    
    gauge_id <- station_tbl[station_tbl$ID_combi==combi,]$ID_station
    length_reach <- station_tbl[station_tbl$ID_combi==combi,]$length
    
    # set back to NULL
    combi_qdata <- NULL
    # Get q-data from specific gauge
    combi_qdata <- load_qdata_sql(gauge_id, q_db_path)
    
    # Check if combi_qdata is not NULL or has > zero rows
    if (!is.null(combi_qdata)) {
      
      # Proceed with the rest of the operations
      combi_qdata <- combi_qdata |>
        mutate(date = as.Date(date)) |> 
        filter(!is.na(q_m3_s))
      
      # load gee data  
      combi_gee_data <- gee_data_tbl |>
        filter(DGO_FID == station_tbl[station_tbl$ID_combi==combi,]$DGO_FID) |> 
        # select only quality-validated entries with low cloud score and complete coverage score
        filter(COVERAGE_SCORE == 100) |> 
        filter(CLOUD_SCORE < 5 )
      
      # combine data and save in list
      data_combined_tbl[[combi]] <- 
        inner_join(combi_gee_data, combi_qdata, by = join_by(DATE == date)) |> 
        mutate(width_m = case_when(!is.na(WATER_AREA/length) ~ WATER_AREA/length*(scale*scale),
                                   is.na(WATER_AREA/length) ~ 0)) |>
        relocate(DATE, width_m, WATER_AREA, q_m3_s) |> 
        
        # filter out 0-values for width
        filter(width_m > 0) |> 
        # Summarize each group of unique DGO_FID-DATE combinations by calculating the mean of all other variables, adhering to the new syntax
        group_by(DGO_FID, DATE) |> 
        summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = "drop") 
      
    } 
    else next # Skip to the next iteration
    
  }
  
  return(data_combined_tbl)
} 
