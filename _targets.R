## ----------------------------------------------
#| Script by Leo Helling, created the 13.03.2024
#|
#| Name:    Braided Rivers Analysis Main function
#|
#| Purpose: organise the analysis in one main script, run the analysis stepwise in a pipeline approach 
#|          to skip costly runtime for tasks that are already up to date
#|
#| HOW-TO:  1) select the whole code in this script, press CTRL+SPACE to run it
#|          2) run "tar_make()" in the console to run the whole analysis via targets-pipeline
#|          3) run "tar_load_everything()" in the console to load objects into environment
#|          (more infos on the use of the targets-package: https://books.ropensci.org/targets/ )
## ----------------------------------------------


## load packages---------------------------------
# define packages needed
libs <- c(
  "targets", #run pipeline
  "tidyverse", #all necessary data wrangling functions
  "sf", #geospatial vector files handling
  "units", #units handling
  "readxl", #read excel-files
  "hubeau", # download discharge data from hubeau
  "RSQLite", # save data in sql-file
  "logging", # log errors when downloading and saving data
  "geojsonsf", # read geojson files as sf
  "httr", # retrieve the data via GET function
  "gridExtra" # arrange multiple plots
)

# install missing packages
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load packages
invisible(lapply(libs[1:5], library, character.only = T))

## ----------------------------------------------


## further options ------------------------------

# enable distributed computing
options(clustermq.scheduler = "multiprocess")

# Run the R scripts in the R/ folder with custom functions:
tar_source("functions")
## ----------------------------------------------


## Run analysis ---------------------------------
#|  1) read gee-results
#|  
#|  2) download q data from hubeau into sql-database
#|    
#|  3) Q-W-plot creation
#|     individual plots
#|     common plot
#|  4) b-value-table creation



path_morel_data <- "data/morel_france/Data_geomorphology.csv"
# FINAL LIST OF TARGETS TO BE COMPILED

list(
  # load table with reach-station connections
  tar_target(
    stations_overview_tbl,
    read_xlsx("data/stations_overview.xlsx")
  ),
  # load all discharge measurements in sql-table
  tar_target(
    q_data_path,
    load_q_in_db(stations_overview_tbl)
  ),
  # get table with all control q-w-observations (both morel et al. and from BDORTHO)
  tar_target(
    qw_controls_tbl,
    get_control_measurements(station_tbl = stations_overview_tbl, q_db_path = q_data_path, 
                             bdortho_path = "data/bdortho_waterextent/extends_bdortho.shp", 
                             morel_path = path_morel_data)
  ),
  

# SENTINEL-2 DATA ---------------------------------------------------------

  # load results from GEE-analysis of Sentinel-2 data
  tar_target(
    geedata_s2_1724_tbl,
    read_csv("data/gee_analysis/sentinel-2/results_s2_2017-2020.csv") |>
      bind_rows(read_csv("data/gee_analysis/sentinel-2/results_s2_2020-2024.csv")) |>
      select(3:18) |> relocate(DGO_FID, DATE) |> arrange(DGO_FID, DATE)
  ), 
  # combine discharge and gee-observation data in common table
  tar_target(
    combined_s2_list,
    combine_q_gee_data(station_tbl = stations_overview_tbl, gee_data_tbl = geedata_s2_1724_tbl,
                       q_db_path = q_data_path, scale = 10, satellite_type = "Sentinel-2", clear_score = 95)
  ),
  # get b-value table with values from model and from Morel et al.
  tar_target(
    b_values_s2_tbl,
    get_b_values(combined_list = combined_s2_list, morel_path = path_morel_data)
  ),
  # plot Q-W relationships of Sentinel-2 data
  tar_target(
    save_width_s2_plots,
    save_width_plots(qw_combined_list = combined_s2_list, 
                     save_path = "export/sentinel-2/", 
                     satellite_info = "Sentinel-2 data for 2018-2024", 
                     qw_control_tbl = qw_controls_tbl,bvalue_tbl = b_values_s2_tbl)
  ),
  
  
  # PLANET DATA -------------------------------------------------------------
  
  # load results from GEE-analysis of Planet data
  tar_target(
    geedata_planet_1823_tbl,
    read_csv("data/gee_analysis/planet/results_planet_eygue4_2018-2023.csv") |>
      bind_rows(read_csv("data/gee_analysis/planet/results_planet_eygue7_2018-2023.csv"))
  ), 
  # combine discharge and gee-observation data in common table
  tar_target(
    combined_planet_list,
    combine_q_gee_data(station_tbl = stations_overview_tbl, gee_data_tbl = geedata_planet_1823_tbl,
                       q_db_path = q_data_path, scale = 3, satellite_type = "Planet", clear_score = 0)
  ),
  # get b-value table with values from model and from Morel et al.
  tar_target(
    b_values_planet_tbl,
    get_b_values(combined_list = combined_planet_list, morel_path = path_morel_data)
  ),
  # plot Q-W relationships of Planet data
  tar_target(
    save_width_planet_plots,
    save_width_plots(qw_combined_list = combined_planet_list, 
                     save_path = "export/planet/", 
                     satellite_info = "PlanetScope data for 2018-2023", 
                     qw_control_tbl = qw_controls_tbl, bvalue_tbl = b_values_planet_tbl)
  ),


# COMPARISON S2-Planet ----------------------------------------------------
  tar_target(
    create_s2_planet_comparisonplots,
    create_planet_s2_comparison_plots(combined_s2_list = combined_s2_list, 
                                      combined_planet_list = combined_planet_list, 
                                      save_path = "export/comparison_s2-planet/")
  )

)
