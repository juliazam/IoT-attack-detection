# Install and load required libraries
if ( !require( tidyverse )) install.packages( "tidyverse", repos = "http://cran.us.r-project.org" )
library( tidyverse )
library(rvest)
library(tools)

# Data folder
data_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00442"

# Devices
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
df <- NULL

addCVS <- function( file ) {
  tmp <- read.csv( file, as.is = TRUE )
  
  # Get file name without extension as it shows a traffic
  name <- substr( filename, 1, nchar( filename ) - 4 )
  y <- data.frame( device = device, traffic = name )
  tmp <- left_join( y, tmp, by = character() )
  
  if( exists( 'df' ) && is.data.frame( get( 'df' ) ) ) {
    # Add rows from tmp file
    df <<- add_row( df, tmp )
  } else {
    # Create df
    df <<- tmp
  }
  return( df )
}

# If dataset downloaded already and saved in the local folder, read it from the file
data_folder <- "data"
data_file <- "data.cvs"

# If folder is already exists, do not show warning
dir.create( data_folder, showWarnings = FALSE )

if( file.exists( file.path( data_folder, data_file ) ) ) {
  print( "File exists")
  
  #f <- list.files(data_folder, include.dirs = FALSE, full.names = TRUE, recursive = TRUE)
  
} else { # else download all files, create dataset and save it in the folder
  print( "File doesn't exist")
  
  #lapply( names( devices ), function( device ) {
   # url <- paste( data_url, devices[ device ], sep = "/")
   # filesnames <- list.files( url, include.dirs = FALSE, full.names = TRUE, recursive = TRUE)
   # filesnames
 # })
  
  device <- names(devices[1])
  url <- paste( data_url, devices[device], sep = "/")
  
  filenames <- read_html( url ) %>% html_nodes( "a" ) %>% html_attr( "href" )
  # Remove link to parent directory
  filenames <- filenames[-1]
  
  filename <- filenames[2]
  fileurl <- paste( url, filename, sep = "/" )
  ext <- file_ext( filename )
  
  if( ext == 'rar') {
    fileurl <- paste( fileurl, "?raw=true", sep = "" )
    tmp_filename <- file.path( data_folder, "archive.rar" )
    res <- download.file( fileurl, tmp_filename, mode = "wb" )
    files <- unzip( tmp_filename )
    
    
    
    file.remove(tmp_filename)
  }
  
  
  if (ext == "csv") {
    addCVS( fileurl )
  }
  

}



