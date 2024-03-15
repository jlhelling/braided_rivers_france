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
  
  # create individual plots
  for (combi in names(qw_combined_list)) {
    
    # create plot via custom function
    plot <- create_width_plot(name_combi = combi, qw_table = qw_combined_list[[combi]], 
                              satellite_info, qw_control_tbl, bvalue_tbl)
    # save plots in defined location  
    ggsave(paste(save_path, combi, "_width.png", sep=""), plot = plot, width = 10, height = 10, scale = 0.6)
  }
  
  # create plot with all rivers
  plot_all <- create_width_plot_all(qw_combined_list, satellite_info, qw_control_tbl)
  ggsave(paste(save_path, "qw_", satellite_info, "_all.png", sep=""), plot = plot_all, width = 10, height = 10, scale = 0.6)
  
  return(NULL)
}

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
save_width_plots_combined <- function(qw_combined_list_s2, qw_combined_list_planet, save_path, qw_control_tbl){
  
  # create individual plots
  for (combi in names(qw_combined_list_planet)) {
    
    # create plot via custom function
    plot <- create_width_plot_combined(name_combi = combi, qw_table_s2 = qw_combined_list_s2[[combi]], 
                                       qw_table_planet = qw_combined_list_planet[[combi]], qw_control_tbl)
    # save plots in defined location  
    ggsave(paste(save_path, combi, "_width_combined.png", sep=""), plot = plot, width = 10, height = 10, scale = 0.6)
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


## ----------------------------------------------
#| Function: saves all q-waterwidth relationships together in one plot
#|
#| Output:  point-geometry graph, including control measurements from Morel study and BDORTHO
#| Input:   name_combi:     name of reach-station-combination, 
#|          qw_table:       table of q-w values from gee and discharge measurements,
#|          satellite_info: Description of datasource from satellite images, 
#|          qw_control_tbl: table of all control measurements, 
#|          bvalue_tbl:     table of all b-valuues
## ----------------------------------------------
create_width_plot_all <- function(qw_combined_list, satellite_info, qw_control_tbl){
  
  library(scales)
  
  # Combine all data and create the 'river' variable
  data <- bind_rows(lapply(names(qw_combined_list), function(combi) {
    qw_combined_list[[combi]] |> 
      select(q_m3_s, width_m, valid) |> 
      mutate(river = if_else(valid == FALSE, "unvalid", as.factor(sub("_.*", "", combi))),
             id_reach = sub("([^_]+_[^_]+).*", "\\1", combi))
  })) 
  
  # eytract available control observations
  data_control <- qw_control_tbl |> 
    filter(id_reach %in% data$id_reach) |> 
    mutate(river = as.factor(sub("_.*", "", id_reach)))
  

  # number of rivers
  n <- as.numeric(length(levels(data$river)))
  
  # create color palette
  cols = c("Roubion" = "darkorange",
           "Eygue" = "darkgreen",
           "Drac" = "red",
           "Asse" = "purple",
           "Sasse" = "blue",
           "unvalid" = "#ced4da")
  
  # myColors <- brewer.pal(n,"Set1")
  # names(myColors) <- levels(data$river)
  colScale <- scale_colour_manual(name = "river", values = cols)
  
  # create final plot
  plot_all <-
    ggplot(data, mapping = aes(x=q_m3_s, y=width_m, colour = river))+ 
    geom_point(alpha = 0.7, stroke = 0, size = 2)+
    geom_point(data = data_control, aes(x = q_m3_s, y = width_m), shape = "triangle", size = 3, stroke = 0.5, alpha = 0.7) +
    scale_y_continuous(trans='log10',
                       breaks=trans_breaks('log10', function(x) 10^x),
                       labels=trans_format('log10', math_format(10^.x)))+
    scale_x_continuous(trans='log10',
                       breaks=trans_breaks('log10', function(x) 10^x),
                       labels=trans_format('log10', math_format(10^.x)))+
    colScale+
    theme_light()+
    labs(
      title = "Q-width relationships - whole dataset",
      subtitle = satellite_info,
      x = "Daily discharge [m³/s]",
      y = "Water width [m]")
  
}


## ----------------------------------------------
#| Function: create graph of q-waterwidth relationships for one reach-station combi of S2 and Planet data
#|
#| Output:  point-geometry graph, including data from Sentinel-2 and Planet and control measurements from Morel study and BDORTHO
#| Input:   name_combi:      name of reach-station-combination, 
#|          qw_table_s2:     table of q-w values from gee and discharge measurements (S2 imagery),
#|          qw_table_planet: table of q-w values from gee and discharge measurements (Planet imagery),
#|          qw_control_tbl:  table of all control measurements
## ----------------------------------------------
create_width_plot_combined <- function(name_combi, qw_table_s2, qw_table_planet, qw_control_tbl) {
  
  # set name of reach from reach_station combination
  reach <- sub("([^_]+_[^_]+).*", "\\1", name_combi)  
  
  # get observed data and classify the time period of the year
  data <- qw_table_s2 |> 
    mutate(type = if_else(valid == FALSE, "unvalid", "Sentinel-2")) |> 
    bind_rows(qw_table_planet |> mutate(type = if_else(valid == FALSE, "unvalid", "Planet")))
  
  # get control data
  data_control <- qw_control_tbl |>
    filter(id_reach == reach) 
  
  # create color palette for observations
  cols_observations = c("Sentinel-2" = "darkgreen",
                        "Planet" = "#0077b6",
                        "unvalid" = "#ced4da")
  
  # final plot creation
  plot_combi <-
    ggplot() +
    geom_point(data = data, aes(x = q_m3_s, y = width_m, colour = type), alpha = 0.7, stroke = 0, size = 3) +
    scale_colour_manual(name = "type", values = cols_observations) +
    geom_point(data = data_control, aes(x = q_m3_s, y = width_m, shape = control), color = "red", alpha = 1, stroke = 0, size = 3) +
    theme_light() +
    labs(
      title = name_combi,
      subtitle = "PlanetScope and Sentinel-2 imagery",
      x = "Daily discharge [m³/s]",
      y = "Water width [m]",) +
    ylim(0, NA)
  
  return(plot_combi)
}