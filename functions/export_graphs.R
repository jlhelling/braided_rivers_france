
## ----------------------------------------------
## Function: saves graphs of q-waterarea relationships
##
## Output:  point-geometry graph, including smooth trend line
## Input:   list with data q-area, Landsat/Sentinel-2 data type, export file path
## ----------------------------------------------
save_area_plots <- function(combined_list, satellite_type, save_path){
  
  for (combi in names(combined_list)) {
    
    plot_combi <-
      ggplot(combined_list[[combi]], mapping = aes(x=q_m3_s, y=WATER_AREA))+ # filter(libelle_qualification == "Bonne")
      geom_point(alpha = 0.7, stroke = 0)+
      geom_smooth()+
      theme_light()+
      labs(
        title = combi,
        subtitle = satellite_type,
        x = "Daily discharge [m³/s]",
        y = "Water Area [pixels]")
    
    ggsave(paste(save_path, combi, "_area.png", sep=""), width = 10, height = 10, scale = 0.6)
    
  }
}


## ----------------------------------------------
## Function: saves graphs of q-waterwidth relationships
##
## Output:  point-geometry graph, including smooth trend line and two measurements from morel-study
## Input:   list with data q-area, Landsat/Sentinel-2 data type, export file path, morel-study data table
## ----------------------------------------------
save_width_plots <- function(combined_list, satellite_type, save_path){
  
  for (combi in names(combined_list)) {
    
    data <- combined_list[[combi]]
    
    station <- sub("([^_]+_[^_]+).*", "\\1", combi)
    
    plot_combi <-
      ggplot(data, mapping = aes(x=q_m3_s, y=width_m))+ # filter(libelle_qualification == "Bonne")
      geom_point(alpha = 0.7, stroke = 0)+
      geom_smooth()+
      theme_light()+
      labs(
        title = combi,
        subtitle = satellite_type,
        x = "Daily discharge [m³/s]",
        y = "Water width [m]") 
    
    ggsave(paste(save_path, combi, "_width.png", sep=""), width = 10, height = 10, scale = 0.6)
    
  }
}



## ----------------------------------------------
## Function: saves all q-waterwidth relationships together in one plot
##
## Output:  point-geometry graph
## Input:   list with data q-area, Landsat/Sentinel-2 data type, export file path, morel-study data table
## ----------------------------------------------
all_width_plot <- function(combined_list, satellite_type, save_path){
  require(RColorBrewer)
  require(scales)
  
  # create empty tibble to save data in
  data <- tibble(river = factor(), q_m3_s = numeric(), width_m = numeric())
  
  # extract data for each reach-station-combi
  for (combi in names(combined_list)) {
    river <- sub("^([^_]*)_.*$", "\\1", combi)
    combi_morel <- sub("([^_]+_[^_]+).*", "\\1", combi)
    
    # Add observed data to tibble
    observed_data <- tibble(
      river = factor(rep(river, length(combined_list[[combi]]$q_m3_s))),
      q_m3_s = combined_list[[combi]]$q_m3_s, 
      width_m = combined_list[[combi]]$width_m
    )
    data <- bind_rows(data, observed_data)
  }
    
  # number of rivers
  n <- as.numeric(length(levels(data$river)))
  
  # create color palette
  cols = c("Roubion" = "darkorange",
          "Eygue" = "darkgreen",
          "Drac" = "red",
          "Asse" = "purple")
  
  # myColors <- brewer.pal(n,"Set1")
  # names(myColors) <- levels(data$river)
  colScale <- scale_colour_manual(name = "river", values = cols)
  
  # create final plot
  plot_all <-
    ggplot(data, mapping = aes(x=q_m3_s, y=width_m, colour = river))+ 
    geom_point(alpha = 0.7, stroke = 0, size = 2)+
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
      subtitle = satellite_type,
      x = "Daily discharge [m³/s]",
      y = "Water width [m]") 
  
  ggsave(paste(save_path, "q_width_all.png", sep=""), width = 10, height = 10, scale = 0.6)
}