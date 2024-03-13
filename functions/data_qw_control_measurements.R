
get_control_measurements <- function(station_tbl, q_db_path, bdortho_path, morel_path){
  
  # load BDOrtho data
  bdortho_measurements <- load_bdortho_measurements(station_tbl, q_db_path, bdortho_path)
  
  morel_measurements_all <- read_csv(morel_path)
  
  # load measurements from morel dataset
  morel_measurements_filtered <- 
    morel_measurements_all |> 
    filter(station %in% station_tbl$ID_POI) |> 
    select(id_reach = station, width_m = L1, q_m3_s = Q1) |>
    add_row(
      morel_measurements_all |> 
        filter(station %in% station_tbl$ID_POI) |> 
        select(id_reach = station, width_m = L2, q_m3_s = Q2)) |> 
    mutate(control = "Morel et al.", date = NA, gauge = NA)
  
  
   # join both datasets
  combined_control_tbl <- bdortho_measurements |> 
    add_row(morel_measurements_filtered) |> 
    filter(!is.na(width_m) & !is.na(q_m3_s))
  
  return(combined_control_tbl)
}

load_bdortho_measurements <- function(station_tbl, q_db_path, bdortho_path){
  
  # load control measurements from bdortho-campaigns
  bdortho_measurements <- 
    st_read(bdortho_path) |> 
    select(!id) |> 
    st_drop_geometry() |> 
    group_by(id_reach, date) |> 
    summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE)), .groups = "drop") |> 
    left_join(station_tbl |> select(ID_POI, length), by = join_by(id_reach == ID_POI), multiple = "first") |> 
    mutate(control = "BDORTHO") # paste("BDORTHO", year(date), sep = "_")
  
  q_data <- tibble(id_reach = character(), gauge = character(), date = character(), q_m3_s = numeric())
  
  for (row in 1:count(bdortho_measurements)$n) {
    
    reach_i = bdortho_measurements[row,]$id_reach
    date_i = bdortho_measurements[row,]$date
    
    # loop through multiple gauges for each reach
    for (gauge_i in station_tbl[station_tbl$ID_POI==reach_i,]$ID_station) {
      
      q_i <- NA
      
      # get q value of specific date for each reach-station combination
      q_i <- 
        load_qdata_sql(gauge_i, q_db_path) |> 
        mutate(date = as.Date(date)) |> 
        filter(!is.na(q_m3_s)) |> 
        group_by(code_site, date) |> 
        summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = "drop") |> 
        filter(date==date_i) |> 
        pull(var = q_m3_s)
      
      # add data to tibble
      q_data_i <- tibble(id_reach = reach_i, gauge = gauge_i, date = date_i, q_m3_s = q_i)
      q_data <- q_data |> add_row(q_data_i)
    }
  }
  
  # join q data, calculate width
  bdortho_measurements_q <- left_join(bdortho_measurements, q_data, by = join_by(id_reach, date), multiple = "all") |> 
    mutate(width_m = wextent_m2/length) |> 
    relocate(id_reach, width_m, q_m3_s, control) |> 
    select(!c(wextent_m2, length))
  
  return(bdortho_measurements_q)
}
