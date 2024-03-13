## ----------------------------------------------
#| Function: create planet-s2 results comparison plots and save them
#|
#| Output:  saving plots from create_comparison_plot()-function for each reach-station-combi existing for both S2 and Planet-data
#| Input:   combined_s2_list:     list-object with qw-tables for each reach-station-combination from Sentinel-2 data, 
#|          combined_planet_list: list-object with qw-tables for each reach-station-combination from Planet data,
#|          save_path:            path of archive to store the plots
## ----------------------------------------------
create_planet_s2_comparison_plots <- function(combined_s2_list, combined_planet_list, save_path){
  
  for (combi in names(combined_planet_list)) {
    # s2_data = combined_s2_list[[combi]]
    # planet_data = combined_planet_list[[combi]]
    
    # create plot via custom function
    plot <- create_comparison_plot(name_combi = combi, 
                                   combined_s2_tbl = combined_s2_list[[combi]], 
                                   combined_planet_tbl = combined_planet_list[[combi]])
    # save plots in defined location  
    ggsave(paste(save_path, combi, "_comparison.png", sep=""), plot, width = 10, height = 10, scale = 0.6)
  }
  
  return(NULL)
}


## ----------------------------------------------
#| Function: create planet-s2 results comparison plot
#|
#| Output:  point-geometry graph, with each point representing one date with water width measurements of S2 and Planet
#| Input:   name_combi:           name of reach-station-combination, 
#|          combined_s2_tbl:      table with qw-tables for reach-station-combination from Sentinel-2 data, 
#|          combined_planet_tbl:  table with qw-tables for reach-station-combination from Planet data,
## ----------------------------------------------
create_comparison_plot <- function(name_combi, combined_s2_tbl, combined_planet_tbl){
  
  # join both tables for the same dates
  data <- inner_join(combined_planet_tbl |> select(DATE, q_m3_s_planet = q_m3_s, width_m_planet = width_m), 
                     combined_s2_tbl |> select(DATE, q_m3_s_s2 = q_m3_s, width_m_s2 = width_m), 
                     by = join_by(DATE)) |> 
    rowwise() |> 
    mutate(
      period = case_when(
        str_split(DATE, "-")[[1]][2] %in% c("01","02","03") ~ "Jan-Mar",
        str_split(DATE, "-")[[1]][2] %in% c("04","05","06") ~ "Apr-Jun",
        str_split(DATE, "-")[[1]][2] %in% c("07","08","09") ~ "Jul-Sep",
        str_split(DATE, "-")[[1]][2] %in% c("10","11","12") ~ "Oct-Dec"
      )
    )
  
  # create color palette
  cols = c("Jan-Mar" = "#03045e",
           "Apr-Jun" = "#0077b6",
           "Jul-Sep" = "#00b4d8",
           "Oct-Dec" = "#023e8a")
  colScale <- scale_colour_manual(name = "period", values = cols)
  
  # create final plot
  plot <-
    ggplot(data, mapping = aes(x=width_m_planet, y=width_m_s2, colour = period))+ 
    geom_point( size = 3) +
    theme_light()+
    colScale+
    labs(
      title = name_combi,
      subtitle = "Comparison of reach averaged water widths from PlanetScope and Sentinel-2",
      x = "PlanetScope water width [m]",
      y = "Sentinel-2 water width [m]") 
  
  return(plot)
}
