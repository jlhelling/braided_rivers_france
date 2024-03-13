## ----------------------------------------------
#| Function: save graphs of q-waterwidth relationships
#|
#| Output:  saving outputs of create_width_plot()-function for each reach-station combi
#| Input:   qw_combined_list: list-object with qw-tables for each reach-station-combination, 
#|          save_path:        path of archive to store the plots,
#|          satellite_info:   Description of datasource from satellite images, 
#|          qw_control_tbl:   table of all control measurements, 
#|          bvalue_tbl:       table of all b-values
## ----------------------------------------------
save_width_plots <- function(qw_combined_list, save_path, satellite_info, qw_control_tbl, bvalue_tbl){
  
  for (combi in names(qw_combined_list)) {
    
    # create plot via custom function
    plot <- create_width_plot(name_combi = combi, qw_table = qw_combined_list[[combi]], 
                              satellite_info, qw_control_tbl, bvalue_tbl)
    # save plots in defined location  
    ggsave(paste(save_path, combi, "_width.png", sep=""), width = 10, height = 10, scale = 0.6)
  }
  
  return(NULL)
}


## ----------------------------------------------
#| Function: create graph of q-waterwidth relationships for one reach-station combi
#|
#| Output:  point-geometry graph, including line of W=aQ^b relationship and control measurements from Morel study and BDORTHO
#| Input:   name_combi:     name of reach-station-combination, 
#|          qw_table:       table of q-w values from gee and discharge measurements,
#|          satellite_info: Description of datasource from satellite images, 
#|          qw_control_tbl: table of all control measurements, 
#|          bvalue_tbl:     table of all b-valuues
## ----------------------------------------------
create_width_plot <- function(name_combi, qw_table, satellite_info, qw_control_tbl, bvalue_tbl) {
  
  # set name of reach from reach_station combination
  reach <- sub("([^_]+_[^_]+).*", "\\1", name_combi)  
  
  # get observed data and classify the time period of the year
  data <- qw_table |>
    mutate(
      period = case_when(
        valid == FALSE ~ "unvalid",
        str_sub(DATE, 6, 7) %in% c("01","02","03") ~ "Jan-Mar",
        str_sub(DATE, 6, 7) %in% c("04","05","06") ~ "Apr-Jun",
        str_sub(DATE, 6, 7) %in% c("07","08","09") ~ "Jul-Sep",
        str_sub(DATE, 6, 7) %in% c("10","11","12") ~ "Oct-Dec"
      )
    )
  
  # get control data
  data_control <- qw_control_tbl |>
    filter(id_reach == reach) 
  
  # get modelled b-values (and those from Morel et al.)
  model_values <- bvalue_tbl |>
    filter(id == name_combi)
  
  # function of W=aQ^b relationship
  f <- function(x) model_values$a_model[1] * x^(model_values$b_model[1])
  
  # create color palette for observations
  cols_observations = c("Jan-Mar" = "#03045e",
                        "Apr-Jun" = "#0077b6",
                        "Jul-Sep" = "#00b4d8",
                        "Oct-Dec" = "#023e8a",
                        "unvalid" = "#ced4da")
  
  # final plot creation
  plot_combi <-
    ggplot() +
    geom_point(data = data, aes(x = q_m3_s, y = width_m, colour = period), alpha = 0.7, stroke = 0, size = 3) +
    scale_colour_manual(name = "period", values = cols_observations) +
    geom_function(fun = f, color = "black") +
    geom_point(data = data_control, aes(x = q_m3_s, y = width_m, shape = control), color = "red", alpha = 1, stroke = 0, size = 3) +
    theme_light() +
    labs(
      title = name_combi,
      subtitle = satellite_info,
      x = "Daily discharge [m³/s]",
      y = "Water width [m]",
      tag = expression(paste("\n               (line based on W=", aQ^{b}, " relationship)", sep=""))) +
    ylim(0, NA) +
    theme(plot.tag.position = "bottom",
          plot.tag = element_text(face = "italic", vjust = 0.3, hjust = 0, size = 9))
  
  return(plot_combi)
}