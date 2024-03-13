## ----------------------------------------------
#| Function: extract b-values for each reach based on the realtionship W=aQ^b
#|
#| Output:  table with b-value for each reach
#| Input:   name_combi: list with all collected data from reaches (discharge and width)
#|          morel_path:  path of csv-table from Morel et al. study
## ----------------------------------------------
get_b_values <- function(combined_list, morel_path){
  
  # load b values from morel dataset
  morel_measurements_all <- read_csv(morel_path) |> 
    select(id_reach = station, b_morel = b) 
  
  model_tbl <- tibble(id = character(), b_model = numeric(),a_model = numeric(), fstat_model = numeric(), pstat_model = numeric())
  
  for (combi in names(combined_list)) {
    
    # error handling
    tryCatch({
      
      # extract only valid values (those which are greater than 2*image resolution)
      table <- 
        combined_list[[combi]] |> 
        filter(!is.na(q_m3_s) & valid==TRUE)
      
      # save widths and q in vectors
      width_vector <- table$width_m
      q_vector <- table$q_m3_s
      
      # run linear model
      model <- lm(log(width_vector)~ log(q_vector))
      
      # add as entry in output-table with corresponding p-value
      model_tbl <- add_row(model_tbl, id = combi, 
                            b_model = model$coefficients[[2]],
                            a_model = exp(model$coefficients[[1]]),
                            fstat_model = summary.lm(model)$fstatistic[[1]],
                            pstat_model = summary.lm(model)$coefficients[[8]])
      
    }, error = function(e) {
      print(paste("!!! Error occurred: ", e$message))
    })
  }
  
  # join b-values from morel study
  output_tbl <- model_tbl |> 
    mutate(id_reach = sub("([^_]+_[^_]+).*", "\\1", id)) |> 
    left_join(morel_measurements_all, by = join_by(id_reach)) 
  
  
  return(output_tbl)
}
