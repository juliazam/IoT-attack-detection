# Install and load required libraries
if ( !require( tidyverse )) install.packages( "tidyverse", repos = "http://cran.us.r-project.org" )
library( tidyverse )
library(rvest)
library(ggplot2)
library(reshape2)

# Folder with all *.csv files with data about IoT attacks
data_folder <- "data"
data_file <- file.path( data_folder, "data.csv" )
df <- NULL

# Function to create data frame from *.csv files in folder
# d - device
createDataFrame <- function( d ) {
  folder <- paste( data_folder, d, sep = "/" )
  files <- list.files( folder )
  
  for( i in 1:length( files ) ) {
    print(i)
    file <- files[ i ]
    
    # Use file name as botnet name
    name <- substr( file, 1, nchar( file ) - 4 )
    
    # Create full path to the data file
    file <- paste( folder, file, sep = "/")
    
    # Read data file
    tmp <- read.csv( file, as.is = TRUE )
    
    # Create data frame with information about device and botnet
    y <- data.frame( device = d, botnet = name )
    
    # Join both data frames
    tmp <- left_join( y, tmp, by = character() )
    
    if( exists( 'df' ) && is.data.frame( get( 'df' ) ) ) {
      # Add rows from tmp file to data frame
      df <<- add_row( df, tmp )
    } else {
      # Create data frame
      df <<- tmp
    }
  }
  
  return( df )
}

# If data file is exists, read from it 
if( file.exists( data_file ) ) {
  df <- read.csv( data_file, as.is = TRUE )
} else {
  # Devices
  devices <- c( "Danmini_Doorbell", "Ecobee_Thermostat", "Ennio_Doorbell", "Philips_B120N10_Baby_Monitor", 
                "Provision_PT_737E_Security_Camera", "Provision_PT_838_Security_Camera", "Samsung_SNH_1011_N_Webcam", 
                "SimpleHome_XCS7_1002_WHT_Security_Camera", "SimpleHome_XCS7_1003_WHT_Security_Camera" )
  
  device_number = 1
  device <- devices[ device_number ]
  
  # Create data frame from *.csv files and save it in data file
  df <- createDataFrame( device )

  if (nrow( df ) > 0 ) {
    write.csv( df, file = data_file, row.names = FALSE )
  }
}

# Sample 
sample_size <- 100
n_bots <- df %>% distinct(botnet) %>% nrow()
tmp <- df %>% group_by(botnet) %>% sample_n( sample_size )
tmp <- tmp %>% select(-device)

# Explore L5-L0.01 weight
tmp2 <- tmp %>% select(botnet, MI_dir_L5_weight, MI_dir_L3_weight, 
                       MI_dir_L1_weight, MI_dir_L0.1_weight, MI_dir_L0.01_weight)

# Reshape dataset to plot multiply boxplots in one plot
tmp3 <- melt(tmp2, id = "botnet")

tmp3 %>% ggplot(aes(variable, value,  color = as.factor(botnet))) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=90, hjust=1))

# 1 
df %>% group_by(botnet) %>% select(MI_dir_L5_weight) %>% 
  ggplot(aes(MI_dir_L5_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 2 
df %>% group_by(botnet) %>% select(MI_dir_L3_weight) %>% 
  ggplot(aes(MI_dir_L3_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 3 
df %>% group_by(botnet) %>% select(MI_dir_L1_weight) %>% 
  ggplot(aes(MI_dir_L1_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 4 
df %>% group_by(botnet) %>% select(MI_dir_L0.1_weight) %>% 
  ggplot(aes(MI_dir_L0.1_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 5 
df %>% group_by(botnet) %>% select(MI_dir_L0.01_weight) %>% 
  ggplot(aes(MI_dir_L0.01_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()


# Explore L5-L0.01 means
tmp2 <- tmp %>% select(botnet, MI_dir_L5_mean, MI_dir_L3_mean, 
                       MI_dir_L1_mean, MI_dir_L0.1_mean, MI_dir_L0.01_mean)

# Reshape dataset to plot multiply boxplots in one plot
tmp3 <- melt(tmp2, id = "botnet")

tmp3 %>% ggplot(aes(variable, value,  color = as.factor(botnet))) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=90, hjust=1))

# 1 
df %>% group_by(botnet) %>% select(MI_dir_L5_mean) %>% 
  ggplot(aes(MI_dir_L5_mean, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 2 
df %>% group_by(botnet) %>% select(MI_dir_L3_mean) %>% 
  ggplot(aes(MI_dir_L3_mean, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 3 
df %>% group_by(botnet) %>% select(MI_dir_L1_mean) %>% 
  ggplot(aes(MI_dir_L1_mean, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 4 
df %>% group_by(botnet) %>% select(MI_dir_L0.1_mean) %>% 
  ggplot(aes(MI_dir_L0.1_mean, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 5 
df %>% group_by(botnet) %>% select(MI_dir_L0.01_mean) %>% 
  ggplot(aes(MI_dir_L0.01_mean, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()


# Explore L5-L0.01 variance
tmp2 <- tmp %>% select(botnet, MI_dir_L5_variance, MI_dir_L3_variance, 
                       MI_dir_L1_variance, MI_dir_L0.1_variance, MI_dir_L0.01_variance)

# Reshape dataset to plot multiply boxplots in one plot
tmp3 <- melt(tmp2, id = "botnet")

tmp3 %>% ggplot(aes(variable, value,  color = as.factor(botnet))) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=90, hjust=1))



# Explore H_L5-H_L0.01 weight
tmp2 <- tmp %>% select(botnet, H_L5_weight, H_L3_weight, H_L1_weight,
                       H_L0.1_weight, H_L0.01_weight)

# Reshape dataset to plot multiply boxplots in one plot
tmp3 <- melt(tmp2, id = "botnet")

tmp3 %>% ggplot(aes(variable, value,  color = as.factor(botnet))) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=90, hjust=1))

# 1 
df %>% group_by(botnet) %>% select(H_L5_weight) %>% 
  ggplot(aes(H_L5_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 2 
df %>% group_by(botnet) %>% select(H_L3_weight) %>% 
  ggplot(aes(H_L3_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 3 
df %>% group_by(botnet) %>% select(H_L1_weight) %>% 
  ggplot(aes(H_L1_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 4 
df %>% group_by(botnet) %>% select(H_L0.1_weight) %>% 
  ggplot(aes(H_L0.1_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 5 
df %>% group_by(botnet) %>% select(H_L0.01_weight) %>% 
  ggplot(aes(H_L0.01_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()


# Explore H_L5-H_L0.01 means
tmp2 <- tmp %>% select(botnet,H_L5_mean, H_L3_mean, H_L1_mean,
                       H_L0.1_mean, H_L0.01_mean)

# Reshape dataset to plot multiply boxplots in one plot
tmp3 <- melt(tmp2, id = "botnet")

tmp3 %>% ggplot(aes(variable, value,  color = as.factor(botnet))) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=90, hjust=1))

# 1 
df %>% group_by(botnet) %>% select(H_L5_mean) %>% 
  ggplot(aes(H_L5_mean, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 2 
df %>% group_by(botnet) %>% select(H_L3_mean) %>% 
  ggplot(aes(H_L3_mean, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 3 
df %>% group_by(botnet) %>% select(H_L1_mean) %>% 
  ggplot(aes(H_L1_mean, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 4 
df %>% group_by(botnet) %>% select(H_L0.1_mean) %>% 
  ggplot(aes(H_L0.1_mean, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 5 
df %>% group_by(botnet) %>% select(H_L0.01_mean) %>% 
  ggplot(aes(H_L0.01_mean, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()





# Explore HH_L5-HH_L0.01 weight
tmp2 <- tmp %>% select(botnet, HH_L5_weight, HH_L3_weight, HH_L1_weight,
                       HH_L0.1_weight, HH_L0.01_weight)

# Reshape dataset to plot multiply boxplots in one plot
tmp3 <- melt(tmp2, id = "botnet")

tmp3 %>% ggplot(aes(variable, value,  color = as.factor(botnet))) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=90, hjust=1))

# 1 
df %>% group_by(botnet) %>% select(HH_L5_weight) %>% 
  ggplot(aes(HH_L5_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 2 
df %>% group_by(botnet) %>% select(HH_L3_weight) %>% 
  ggplot(aes(HH_L3_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 3 
df %>% group_by(botnet) %>% select(HH_L1_weight) %>% 
  ggplot(aes(HH_L1_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 4 
df %>% group_by(botnet) %>% select(HH_L0.1_weight) %>% 
  ggplot(aes(HH_L0.1_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 5 
df %>% group_by(botnet) %>% select(HH_L0.01_weight) %>% 
  ggplot(aes(HH_L0.01_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()


# Explore HH_L5-HH_L0.01 means
tmp2 <- tmp %>% select(botnet,HH_L5_mean, HH_L3_mean, HH_L1_mean,
                       HH_L0.1_mean, HH_L0.01_mean)

# Reshape dataset to plot multiply boxplots in one plot
tmp3 <- melt(tmp2, id = "botnet")

tmp3 %>% ggplot(aes(variable, value,  color = as.factor(botnet))) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=90, hjust=1))

# 1 
df %>% group_by(botnet) %>% select(HH_L5_mean) %>% 
  ggplot(aes(HH_L5_mean, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 2 
df %>% group_by(botnet) %>% select(HH_L3_mean) %>% 
  ggplot(aes(HH_L3_mean, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 3 
df %>% group_by(botnet) %>% select(HH_L1_mean) %>% 
  ggplot(aes(HH_L1_mean, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 4 
df %>% group_by(botnet) %>% select(HH_L0.1_mean) %>% 
  ggplot(aes(HH_L0.1_mean, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 5 
df %>% group_by(botnet) %>% select(HH_L0.01_mean) %>% 
  ggplot(aes(HH_L0.01_mean, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()


# Explore HH_L5-HH_L0.01 magnitude
tmp2 <- tmp %>% select(botnet, HH_L5_magnitude, HH_L3_magnitude, HH_L1_magnitude, 
                       HH_L0.1_magnitude, HH_L0.01_magnitude)

# Reshape dataset to plot multiply boxplots in one plot
tmp3 <- melt(tmp2, id = "botnet")

tmp3 %>% ggplot(aes(variable, value,  color = as.factor(botnet))) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=90, hjust=1))

# 1 
df %>% group_by(botnet) %>% select(HH_L5_magnitude) %>% 
  ggplot(aes(HH_L5_magnitude, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 2 
df %>% group_by(botnet) %>% select(HH_L3_magnitude) %>% 
  ggplot(aes(HH_L3_magnitude, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 3 
df %>% group_by(botnet) %>% select(HH_L1_magnitude) %>% 
  ggplot(aes(HH_L1_magnitude, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 4 
df %>% group_by(botnet) %>% select(HH_L0.1_magnitude) %>% 
  ggplot(aes(HH_L0.1_magnitude, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 5 
df %>% group_by(botnet) %>% select(HH_L0.01_magnitude) %>% 
  ggplot(aes(HH_L0.01_magnitude, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()


# Explore HH_L5-HH_L0.01 std
tmp2 <- tmp %>% select(botnet, HH_L5_std, HH_L3_std, HH_L1_std, 
                       HH_L0.1_std, HH_L0.01_std)

# Reshape dataset to plot multiply boxplots in one plot
tmp3 <- melt(tmp2, id = "botnet")

tmp3 %>% ggplot(aes(variable, value,  color = as.factor(botnet))) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=90, hjust=1))

# 1 
df %>% group_by(botnet) %>% select(HH_L5_std) %>% 
  ggplot(aes(HH_L5_std, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 2 
df %>% group_by(botnet) %>% select(HH_L3_std) %>% 
  ggplot(aes(HH_L3_std, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 3 
df %>% group_by(botnet) %>% select(HH_L1_std) %>% 
  ggplot(aes(HH_L1_std, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 4 
df %>% group_by(botnet) %>% select(HH_L0.1_std) %>% 
  ggplot(aes(HH_L0.1_std, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 5 
df %>% group_by(botnet) %>% select(HH_L0.01_std) %>% 
  ggplot(aes(HH_L0.01_std, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# Explore HH_L5-HH_L0.01 radius
tmp2 <- tmp %>% select(botnet, HH_L5_radius, HH_L3_radius, HH_L1_radius, 
                       HH_L0.1_radius, HH_L0.01_radius)

# Reshape dataset to plot multiply boxplots in one plot
tmp3 <- melt(tmp2, id = "botnet")

tmp3 %>% ggplot(aes(variable, value,  color = as.factor(botnet))) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=90, hjust=1))

# 1 
df %>% group_by(botnet) %>% select(HH_L5_radius) %>% 
  ggplot(aes(HH_L5_radius, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 2 
df %>% group_by(botnet) %>% select(HH_L3_radius) %>% 
  ggplot(aes(HH_L3_radius, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 3 
df %>% group_by(botnet) %>% select(HH_L1_radius) %>% 
  ggplot(aes(HH_L1_radius, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 4 
df %>% group_by(botnet) %>% select(HH_L0.1_radius) %>% 
  ggplot(aes(HH_L0.1_radius, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 5 
df %>% group_by(botnet) %>% select(HH_L0.01_radius) %>% 
  ggplot(aes(HH_L0.01_radius, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()


# Explore HH_L5-HH_L0.01 covariance
tmp2 <- tmp %>% select(botnet, HH_L5_covariance, HH_L3_covariance, HH_L1_covariance, 
                       HH_L0.1_covariance, HH_L0.01_covariance)

# Reshape dataset to plot multiply boxplots in one plot
tmp3 <- melt(tmp2, id = "botnet")

tmp3 %>% ggplot(aes(variable, value,  color = as.factor(botnet))) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=90, hjust=1))

# 1 
df %>% group_by(botnet) %>% select(HH_L5_covariance) %>% 
  ggplot(aes(HH_L5_covariance, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 2 
df %>% group_by(botnet) %>% select(HH_L3_covariance) %>% 
  ggplot(aes(HH_L3_covariance, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 3 
df %>% group_by(botnet) %>% select(HH_L1_covariance) %>% 
  ggplot(aes(HH_L1_covariance, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 4 
df %>% group_by(botnet) %>% select(HH_L0.1_covariance) %>% 
  ggplot(aes(HH_L0.1_covariance, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 5 
df %>% group_by(botnet) %>% select(HH_L0.01_covariance) %>% 
  ggplot(aes(HH_L0.01_covariance, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()


# Explore HH_jit_L5 - L0.01 weight
tmp2 <- tmp %>% select(botnet, HH_jit_L5_weight, HH_jit_L3_weight, HH_jit_L1_weight, 
                       HH_jit_L0.1_weight, HH_jit_L0.01_weight)

# Reshape dataset to plot multiply boxplots in one plot
tmp3 <- melt(tmp2, id = "botnet")

tmp3 %>% ggplot(aes(variable, value,  color = as.factor(botnet))) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=90, hjust=1))

# 1 
df %>% group_by(botnet) %>% select(HH_jit_L5_weight) %>% 
  ggplot(aes(HH_jit_L5_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 2 
df %>% group_by(botnet) %>% select(HH_jit_L3_weight) %>% 
  ggplot(aes(HH_jit_L3_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 3 
df %>% group_by(botnet) %>% select(HH_jit_L1_weight) %>% 
  ggplot(aes(HH_jit_L1_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 4 
df %>% group_by(botnet) %>% select(HH_jit_L0.1_weight) %>% 
  ggplot(aes(HH_jit_L0.1_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 5 
df %>% group_by(botnet) %>% select(HH_jit_L0.01_weight) %>% 
  ggplot(aes(HH_jit_L0.01_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()


# Explore HH_jit_L5 - L0.01 mean
tmp2 <- tmp %>% select(botnet, HH_jit_L5_mean, HH_jit_L3_mean, HH_jit_L1_mean, 
                       HH_jit_L0.1_mean, HH_jit_L0.01_mean)

# Reshape dataset to plot multiply boxplots in one plot
tmp3 <- melt(tmp2, id = "botnet")

tmp3 %>% ggplot(aes(variable, value,  color = as.factor(botnet))) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=90, hjust=1))

# 1 
df %>% group_by(botnet) %>% select(HH_jit_L5_mean) %>% 
  ggplot(aes(HH_jit_L5_mean, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 2 
df %>% group_by(botnet) %>% select(HH_jit_L3_mean) %>% 
  ggplot(aes(HH_jit_L3_mean, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 3 
df %>% group_by(botnet) %>% select(HH_jit_L1_mean) %>% 
  ggplot(aes(HH_jit_L1_mean, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 4 
df %>% group_by(botnet) %>% select(HH_jit_L0.1_mean) %>% 
  ggplot(aes(HH_jit_L0.1_mean, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 5 
df %>% group_by(botnet) %>% select(HH_jit_L0.01_mean) %>% 
  ggplot(aes(HH_jit_L0.01_mean, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# Explore HpHp_L5 - L0.01 weight
tmp2 <- tmp %>% select(botnet, HpHp_L5_weight, HpHp_L3_weight, HpHp_L1_weight,
                       HpHp_L0.1_weight, HpHp_L0.01_weight)

# Reshape dataset to plot multiply boxplots in one plot
tmp3 <- melt(tmp2, id = "botnet")

tmp3 %>% ggplot(aes(variable, value,  color = as.factor(botnet))) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=90, hjust=1))

# 1 
df %>% group_by(botnet) %>% select(HpHp_L5_weight) %>% 
  ggplot(aes(HpHp_L5_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 2 
df %>% group_by(botnet) %>% select(HpHp_L3_weight) %>% 
  ggplot(aes(HpHp_L3_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 3 
df %>% group_by(botnet) %>% select(HpHp_L1_weight) %>% 
  ggplot(aes(HpHp_L1_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 4 
df %>% group_by(botnet) %>% select(HpHp_L0.1_weight) %>% 
  ggplot(aes(HpHp_L0.1_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 5 
df %>% group_by(botnet) %>% select(HpHp_L0.01_weight) %>% 
  ggplot(aes(HpHp_L0.01_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# Explore HpHp_L5 - L0.01 mean
tmp2 <- tmp %>% select(botnet, HpHp_L5_mean, HpHp_L3_mean, HpHp_L1_mean,
                       HpHp_L0.1_mean, HpHp_L0.01_mean)

# Reshape dataset to plot multiply boxplots in one plot
tmp3 <- melt(tmp2, id = "botnet")

tmp3 %>% ggplot(aes(variable, value,  color = as.factor(botnet))) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=90, hjust=1))

# 1 
df %>% group_by(botnet) %>% select(HpHp_L5_mean) %>% 
  ggplot(aes(HpHp_L5_mean, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 2 
df %>% group_by(botnet) %>% select(HpHp_L3_mean) %>% 
  ggplot(aes(HpHp_L3_mean, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 3 
df %>% group_by(botnet) %>% select(HpHp_L1_mean) %>% 
  ggplot(aes(HpHp_L1_mean, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 4 
df %>% group_by(botnet) %>% select(HpHp_L0.1_mean) %>% 
  ggplot(aes(HpHp_L0.1_mean, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# 5 
df %>% group_by(botnet) %>% select(HpHp_L0.01_mean) %>% 
  ggplot(aes(HpHp_L0.01_mean, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()


tmp %>% ggplot(aes(MI_dir_L5_weight, MI_dir_L5_mean, color = as.factor(botnet))) + geom_point()
tmp %>% ggplot(aes(MI_dir_L1_weight, MI_dir_L1_mean, color = as.factor(botnet))) + geom_point()
tmp %>% ggplot(aes(MI_dir_L0.01_weight, MI_dir_L0.01_mean, color = as.factor(botnet))) + geom_point()
tmp %>% ggplot(aes(H_L1_weight, H_L1_mean, color = as.factor(botnet))) + geom_point()
tmp %>% ggplot(aes(HH_L1_weight, HH_L1_radius, color = as.factor(botnet))) + geom_point()
tmp %>% ggplot(aes(HH_L1_weight, HH_L1_covariance, color = as.factor(botnet))) + geom_point()
tmp %>% ggplot(aes(HH_L0.01_weight, HH_L0.01_covariance, color = as.factor(botnet))) + geom_point()

colnames <- colnames( df )
pattern <- "MI_dir_L(0)*(.)?(0)*\\d_(me)"
tmp <- df %>% select( botnet, grep( pattern = pattern, ignore.case = TRUE, x = colnames ) )

tmp %>% gather(attacks, number, -botnet) %>% 
  ggplot( aes(botnet, number, fill = as.factor(botnet))) +
  geom_boxplot() +
  facet_wrap( ~attacks, scales = "free", ncol = 5) +
  theme(axis.text.x = element_blank(), legend.position="bottom")

