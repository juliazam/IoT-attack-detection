################################################################################
#
# Script for downloading all data files for all IoT devices from UCI Machine 
# Learning Repository: "detection_of_IoT_botnet_attacks_N_BaIoT Data Set"
# (https://archive.ics.uci.edu/ml/machine-learning-databases/00442)
# and creating data set for IoT attack detection script.
# (https://github.com/juliazam/IoT-attack-detection/blob/665833b79895f4422216a325476390fb0ebadb91/attacks-detection.R)
#
# It creates data folder in current working directory, folders for each devices 
# and downloads *.csv files and *.rar archives. It overwrites data files if they
# already exist.
#
# Make sure that current working directory is the directory with 
# attack-detection.R script.
#
################################################################################

# Install and load required libraries
if ( !require( tidyverse )) 
  install.packages( "tidyverse", repos = "http://cran.us.r-project.org" )
library( tidyverse )
library( rvest )
library( tools )

# Function that downloads data file
downloadFile <- function( filename ) {
  file_url <- paste( device_data_url, filename, sep = "/" )
  ext <- file_ext( filename )
  
  if( ext == 'rar') {
    file_url <- paste( file_url, "?raw=true", sep = "" )
    
  }
  tmp_filename <- file.path( device_folder, filename )
  download.file( file_url, tmp_filename, mode = "wb" )
}

# Dataset url
dataset_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00442"

# List of devices
devices <- list(
  Danmini_Doorbell = "Danmini_Doorbell",
  Ecobee_Thermostat = "Ecobee_Thermostat",
  Ennio_Doorbell = "Ennio_Doorbell",
  Philips_B120N10_Baby_Monitor = "Philips_B120N10_Baby_Monitor",
  Provision_PT_737E_Security_Camera = "Provision_PT_737E_Security_Camera",
  Provision_PT_838_Security_Camera = "Provision_PT_838_Security_Camera",
  Samsung_SNH_1011_N_Webcam = "Samsung_SNH_1011_N_Webcam",
  SimpleHome_XCS7_1002_WHT_Security_Camera = "SimpleHome_XCS7_1002_WHT_Security_Camera",
  SimpleHome_XCS7_1003_WHT_Security_Camera = "SimpleHome_XCS7_1003_WHT_Security_Camera"
)

# Data file folder
data_folder <- "data"

# If folder is already exists, do not show warning
dir.create( data_folder, showWarnings = FALSE )

# All IoT attacks data gathered in one file
data_file <- file.path( data_folder, "data.csv" )

################################################################################
#
# Step 1: download data from UCI Machine Learning Repository (auto)
#
################################################################################

# Download data files for each device
for( device in devices ) {
  
  # Create folder for device data files
  device_folder <- paste( data_folder, device, sep = "/" )
  
  # If folder is already exists, do not show warning
  dir.create( device_folder, showWarnings = FALSE )
  
  # Link to a page with data for the device
  device_data_url <- paste( dataset_url, devices[ device ], sep = "/" )
  
  # Collect filenames from the data page
  filenames <- read_html( device_data_url ) %>% html_nodes( "a" ) %>% html_attr( "href" )
  
  # First link - is a link to the parent directory, remove it
  filenames <- filenames[-1]
  
  # Download all data files for the device
  for( filename in filenames) {
    downloadFile( filename )
  }
}

################################################################################
#
# Step 2: unpack archive files (manual)
#
################################################################################

# Because R has no package that can unpack rar-archives for both, Windows and Unix, 
# please, unpack them manually to each device folder. 
# As both attacks, gafgyf and mirai, have "scan.csv" file, I recommend to
# rename all *.csv files using prefixes: 
#   "ga_" for gafgyt attacks, 
#   "ma_" - for mirai attacks.
# 
# Because file names will be used as botnet names, I recommend to keep the file
# names short.
# For example, after unpacking and renaming, the folder for Danmini_Doorbell 
# device should contain next files:
# - benign.csv 
# - ga_combo.csv
# - ga_junk.csv
# - ga_scan.csv
# - ga_tcp.csv
# - ga_udp.csv
# - ma_ack.csv
# - ma_scan.csv
# - ma_syn.csv
# - ma_udp.csv
# - ma_udpplain.csv

################################################################################
#
# Step 3: create data set (auto)
#
################################################################################

# Select each device separately 
device_number <- 1

# Choose the device itself
device <- devices[[ device_number ]]

# Folder with data files for the device
folder <- paste( data_folder, device, sep = "/" )

# Get list of all files in folder
files <- list.files( folder )

# If the folder contains *.rar files, the sum will be more then 0
if( !sum( grepl( "\\.rar$", files ) ) ) {
  # No *.rar files
  n_files <- length( files )
  
  # If folder exists and has files
  if( n_files > 0 ) {
    print( "Gathering data from device. Please, note this will take a time.")
    
    # Create data frame
    df <- NULL
    
    # Read all *.csv files
    for( i in 1:n_files ) {
      
      # Current data file
      file <- files[ i ]
      
      # Use file name as botnet name in data frame
      name <- substr( file, 1, nchar( file ) - 4 )
      
      ext <- file_ext( file )
      
      if( ext == "csv" ) {
        
        # Create full path to the data file
        file <- paste( folder, file, sep = "/")
        
        # Read data file
        tmp <- read.csv( file, as.is = TRUE )
        
        # Create data frame with information about botnet
        y <- data.frame( botnet = name )
        
        # Join both data frames in temporary data frame
        tmp <- left_join( y, tmp, by = character() )
        
        if( is.null( df ) ) {
          df <- tmp
        } else {
          df <- add_row( df, tmp )
        }
      }
    }
    
    # Save data file
    write.csv( df, file = data_file, row.names = FALSE )
  }
} else {
  print( "Please, unpack *.rar archives!" )
}
