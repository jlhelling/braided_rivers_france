
## ----------------------------------------------
## Function: extract b-values for each reach based on the realtionship W=aQ^b
##
## Output:  table with b-value for each reach
## Input:   list with all collected data from reaches (discharge and width)
## ----------------------------------------------
get_b_values <- function(combined_list, morel_tbl){
  
  output_tbl <- tibble(id = character(), b_model = numeric(), fstat_model = numeric(), pstat_model = numeric())
  
  for (combi in names(combined_list)) {
    
    # error handling
    tryCatch({
      
      # extract non-zero and non-NA values
      table <- 
        combined_list[[combi]] |> 
        filter(!is.na(width_m) & !is.na(q_m3_d))
      
      # save widths and q in vectors
      width_vector <- table$width_m
      q_vector <- table$q_m3_d
      
      # run linear model
      model <- lm(log(width_vector)~ log(q_vector))
      
      # add as entry in output-table with corresponding p-value
      output_tbl <- add_row(output_tbl, id = combi, 
                            b_model = model$coefficients[[2]],
                            fstat_model = summary.lm(model)$fstatistic[[1]],
                            pstat_model = summary.lm(model)$coefficients[[8]])
      
    }, error = function(e) {
      print(paste("!!! Error occurred: ", e$message))
    })
  }
  
  # join b-values from morel study
  output_tbl <- output_tbl |> 
    mutate(id_reach = sub("([^_]+_[^_]+).*", "\\1", id)) |> 
    left_join(morel_tbl |> select(station, b_morel = b), by = join_by(id_reach == station)) 
  
  
  return(output_tbl)
}


# combined_list <- combined_s2_list
# combi <- names(combined_list)[1]

# 
# 
# data_Eygue_6_V5354010 <- combined_s2_list$Eygue_6_V5354010 |> filter(width_m>0 & q_m3_d>0 & width_m<130)
# 
# 
# 
# 
# summary(model)
# 
# b <- model$coefficients[[2]]

