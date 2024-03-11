## ----------------------------------------------
## Script by Leo Helling, created the 17.01.2024
##
## Name: Braided Rivers Analysis Main function
##
## Purpose: run the analysis stepwise
##
## ----------------------------------------------

## load packages---------------------------------
# install and load all necessary packages to run the code

# libraries we need
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

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs[1:5], library, character.only = T))

## ----------------------------------------------


## further options ------------------------------

# enable distributed computing
options(clustermq.scheduler = "multiprocess")

# Run the R scripts in the R/ folder with custom functions:
tar_source("functions")
## ----------------------------------------------


## Run analysis ---------------------------------
#|  1 - read gee-results
#|  
#|  2 - download, collection of q-data in sql-database
#|    FRA from hubeau
#|    CHL from common csv
#|    NZL from individul csvs
#|    ...
#|  3 - Q-W-plot creation
#|    individual plots
#|    common plot
#|  4 - b-value-table creation


# FINAL LIST OF TARGETS TO BE COMPILED

list(
  # load table with reach-station connections
  tar_target(
    stations_overview_tbl,
    read_xlsx("data/POI_stations_overview.xlsx")
  ),
  # load all discharge measurements in sql-table
  tar_target(
    q_data_path,
    load_q_all_countries(stations_overview_tbl)
  ),
  # load results from GEE-analysis
  tar_target(
    poi_gee_analysis_2021,
    read_csv("data/gee_analysis/poi_selected_s2_20240303_2021.csv")
  ),
  # combine discharge and gee-observation data in common table
  tar_target(
    combined_s2_tbl,
    combine_q_gee_data(station_tbl = stations_overview_tbl, 
                       gee_data_tbl = poi_gee_analysis_2021, 
                       q_db_path = q_data_path, 
                       scale = 10)
  ),
  # plot Q-W relationships
  tar_target(
    save_width_s2_plots,
    save_width_plots(combined_list = combined_s2_tbl, 
                     satellite_type = "Sentinel-2", 
                     save_path = "export/widths_s2/")
  )
  
  
  
)
