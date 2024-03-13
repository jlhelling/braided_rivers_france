## ----------------------------------------------
#| Function: loads results from GEE analysis and combines it with q data in common list-object
#|
#| Output:  list with combined discharge and satellite data for each reach-gauge combination
#| Input:   station_tbl:    reaches-gauges over view table,
#|          gee_data_tbl:   GEE-Satellite observations from GloUrbEE-workflow, 
#|          q_db_path:      path of db with discharge measurements, 
#|          scale:          image resolution in meters, 
#|          satellite_type: either Planet or something else, 
#|          clear_score:    proportion of pixels in dgo which should at least be free from clouds (Sentinel-2 or 
#|                          Landsat data) or from clouds, haze, snow, shadow (Planet)
## ----------------------------------------------
combine_q_gee_data <- function(station_tbl, gee_data_tbl, q_db_path, scale, satellite_type, clear_score){
  
  require(RSQLite)
  
  # create empty list to store all data in
  data_combined_list <- list()
  
  # loop through each station-reach combi and extract necessary data
  for (combi in station_tbl$ID_combi) {
    
    gauge_id <- station_tbl[station_tbl$ID_combi==combi,]$ID_station
    length_reach <- station_tbl[station_tbl$ID_combi==combi,]$length
    
    # set back to NULL
    combi_qdata <- NULL
    # Get q-data from specific gauge
    combi_qdata <- load_qdata_sql(gauge_id, q_db_path) 
    
    # load gee data  
    combi_gee_data <- gee_data_tbl |>
      filter(DGO_FID == station_tbl[station_tbl$ID_combi==combi,]$DGO_FID) |> 
      # select only quality-validated entries with defined clearage score and complete coverage score
      filter(COVERAGE_SCORE > 99) |> 
      filter(if (satellite_type == "Planet") CLEAR_SCORE > clear_score
             else CLOUD_SCORE < (100-clear_score))
    
    # Check if combi_qdata is not NULL or has > zero rows
    if (!is.null(combi_qdata)) {
      
      # Proceed with the rest of the operations
      combi_qdata <- combi_qdata |>
        mutate(date = as.Date(date)) |> 
        filter(!is.na(q_m3_s)) |> 
        group_by(code_site, date) |> 
        summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = "drop")
      
      # combine data 
      data_combined_tbl <- combi_gee_data |> 
        # Summarize each group of unique DGO_FID-DATE combinations by calculating the mean of all other variables, adhering to the new syntax
        group_by(DGO_FID, DATE) |> 
        summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = "drop") |>   
        inner_join(combi_qdata, by = join_by(DATE == date)) |> 
        mutate(width_m = case_when(!is.na(WATER_AREA/length_reach) ~ WATER_AREA/length_reach*(scale*scale),
                                   is.na(WATER_AREA/length_reach) ~ 0)) |> 
        # mark widths < 2xscale
        mutate(valid = as.factor(case_when((width_m >= scale*2) ~ TRUE,
                                           (width_m < scale*2) ~ FALSE))) |> 
        
        relocate(DATE, width_m, q_m3_s, WATER_AREA, valid)
      
    } 
    else next # Skip to the next iteration
    
    # finally add to list if data available
    if(count(data_combined_tbl)$n > 0) { data_combined_list[[combi]] <- data_combined_tbl}
    else next # Skip to the next iteration
    
  }
  
  return(data_combined_list)
} 
