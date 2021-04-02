# Install and load required libraries
if ( !require( tidyverse )) install.packages( "tidyverse", repos = "http://cran.us.r-project.org" )
library( tidyverse )
library( rvest )
library( ggplot2 )

# Data archive url
data_url <- "https://www118.zippyshare.com/d/NDCWjCMp/613861/data.zip"

# Data file folder
data_folder <- "data"

# Data file name
data_file <- file.path( data_folder, "data.csv" )

# Data frame to explore
df <- NULL

# Error messages
error_download <- "Can't download data file. Please, follow instruction to download it from UCI Machine Learning Repository."

# If data file is exists, read from it 
if( file.exists( data_file ) ) {
  df <- read.csv( data_file, as.is = TRUE )
} else { # Download from url (file size is 200MB, download will take a time)
  zipfile <- paste( data_folder, "data.zip", sep = "/" )
  res <- download.file( data_url, zipfile, mode = "wb" )
  if ( !res ) {
    unzip( zipfile, exdir = data_folder )
    df <- read.csv( data_file, as.is = TRUE )
    file.remove( zipfile )
  } else {
    print( error_download )
  }
  
}

# If data frame was successfully loaded
if( !is.null( df ) ) {
  colnames <- colnames( df )
  
  # Dataset description:
  # 1. The dataset has 5 time-frames L5, L3, L1, L0.1 and L0.01
  # 2. The statistics extracted from each stream for each time-frame:
  #     - weight: the weight of the stream (can be viewed as the number of items observed in recent history)
  #     - mean
  #     - std (variance)
  #     - radius: the root squared sum of the two streams' variances
  #     - magnitude: the root squared sum of the two streams' means 
  #     - cov: an approximated covariance between two streams
  #     - pcc: an approximated correlation coefficient between two streams
  # 3. Stream aggregations:
  #     - MI: ("Source MAC-IP" in N-BaIoT paper) Stats summarizing the recent traffic from this packet's host (IP + MAC)
  #     - H: ("Source IP" in N-BaIoT paper) Stats summarizing the recent traffic from this packet's host (IP)
  #     - HH: ("Channel" in N-BaIoT paper) Stats summarizing the recent traffic going from this packet's host (IP) 
  #           to the packet's destination host.
  #     - HH_jit: ("Channel jitter" in N-BaIoT paper) Stats summarizing the jitter of the traffic going from this packet's host 
  #           (IP) to the packet's destination host.
  #     - HpHp: ("Socket" in N-BaIoT paper) Stats summarizing the recent traffic going from this packet's host+port (IP) 
  #           to the packet's destination host+port. Example 192.168.4.2:1242 -> 192.168.4.12:80
  # Thus, the column 'MI_dir_L5_weight' shows the weight of recent traffic from the packet's host for L5 time-frame
  
  # The original dataset has 115 parameters (columns). I added 'botnet' column, that represents attacks from different botnets, where:
  # 'ga' is short for 'gafgyt attack'
  # 'ma' is short for 'mirai attack'
  
  # Explore MI_dir_L5_weight - MI_dir_L0.01_weight columns
  pattern <- "MI_dir_L(0)*(.)?(0)*\\d_(we)"
  
  tmp <- df %>% select( botnet, grep( pattern = pattern, ignore.case = TRUE, x = colnames ) )
  
  tmp %>% gather( attacks, data, -botnet ) %>% 
  ggplot( aes( botnet, data, fill = botnet ) ) +
  geom_boxplot() +
  facet_wrap( ~attacks, scales = "free", ncol = 5 ) +
  theme( axis.text.x = element_blank(), legend.position = "bottom" )
  
  # The plot shows that using only the weight parameter, I can easily separate benign traffic, ga_tcp and ga_udp from other attacks.
  # So, I have to find another parameter, that can help separate benign traffic from ga_tcp and ga_udp.
  
  # View close up the weight data for L1 and L0.01 time-frames
  df %>% group_by( botnet ) %>% select (MI_dir_L1_weight ) %>% 
    ggplot( aes( MI_dir_L1_weight, botnet,  fill = botnet ) ) + 
    geom_boxplot() +
    theme_minimal()
  
  df %>% group_by( botnet ) %>% select (MI_dir_L0.01_weight ) %>% 
    ggplot( aes( MI_dir_L0.01_weight, botnet,  fill = botnet ) ) + 
    geom_boxplot() +
    theme_minimal()
  
  # It's visible that ga_tcp an ga_udp disguise themselves well as benign traffic. 
  # Their medians are close to zero, all almost have no outers.
  
  # Let's explore mean data for  MI_dir_L5 -  MI_dir_L0.01 time-frames
  pattern <- "MI_dir_L(0)*(.)?(0)*\\d_(me)"
  
  tmp <- df %>% select( botnet, grep( pattern = pattern, ignore.case = TRUE, x = colnames ) )
  
  tmp %>% gather( attacks, data, -botnet ) %>% 
    ggplot( aes( botnet, data, fill = botnet ) ) +
    geom_boxplot() +
    facet_wrap( ~attacks, scales = "free", ncol = 5 ) +
    theme( axis.text.x = element_blank(), legend.position = "bottom" )
  
  # This plot shows that 'mean' parameter can help me separate benign traffic from ga_tcp and ga_udp, as median for benign traffic 
  # is higher that for these attacks. But I steel need one more parameter as all of them have outers, and using only weight and 
  # mean columns will not help to separate benign traffic from the attacks.
  
  # Let's check means data for L1 and L0.01 time-frames.
  df %>% group_by( botnet ) %>% select (MI_dir_L1_mean ) %>% 
    ggplot( aes( MI_dir_L1_mean, botnet,  fill = botnet ) ) + 
    geom_boxplot() +
    theme_minimal()
  
  df %>% group_by( botnet ) %>% select (MI_dir_L0.01_mean ) %>% 
    ggplot( aes( MI_dir_L0.01_mean, botnet,  fill = botnet ) ) + 
    geom_boxplot() +
    theme_minimal()
  
  # Next, explore variance column for MI_dir_L5 - MI_dir_L0.01 time-frames
  pattern <- "MI_dir_L(0)*(.)?(0)*\\d_(var)"
  
  tmp <- df %>% select( botnet, grep( pattern = pattern, ignore.case = TRUE, x = colnames ) )
  
  tmp %>% gather( attacks, data, -botnet ) %>% 
    ggplot( aes( botnet, data, fill = botnet ) ) +
    geom_boxplot() +
    facet_wrap( ~attacks, scales = "free", ncol = 5 ) +
    theme( axis.text.x = element_blank(), legend.position = "bottom" )
  
  # This plot shows that variance column doesn't give new information how to separate benign traffic from attacks, so I can easily
  # remove this column if I need to reduce the data frame (or martix) dimension.
  
  # I have finished with MI streams. Next, I will explore statistics for H and HH streams.
  # Weight
  pattern <- "H_L(0)*(.)?(0)*\\d_(we)"
  
  tmp <- df %>% select( botnet, grep( pattern = pattern, ignore.case = TRUE, x = colnames ) )
  
  tmp %>% gather( attacks, data, -botnet ) %>% 
    ggplot( aes( botnet, data, fill = botnet ) ) +
    geom_boxplot() +
    facet_wrap( ~attacks, scales = "free", ncol = 5 ) +
    theme( axis.text.x = element_blank(), legend.position = "bottom" )
  
  # Mean
  pattern <- "H_L(0)*(.)?(0)*\\d_(me)"
  
  tmp <- df %>% select( botnet, grep( pattern = pattern, ignore.case = TRUE, x = colnames ) )
  
  tmp %>% gather( attacks, data, -botnet ) %>% 
    ggplot( aes( botnet, data, fill = botnet ) ) +
    geom_boxplot() +
    facet_wrap( ~attacks, scales = "free", ncol = 5 ) +
    theme( axis.text.x = element_blank(), legend.position = "bottom" )
  
  # Variance
  pattern <- "H_L(0)*(.)?(0)*\\d_(var)"
  
  tmp <- df %>% select( botnet, grep( pattern = pattern, ignore.case = TRUE, x = colnames ) )
  
  tmp %>% gather( attacks, data, -botnet ) %>% 
    ggplot( aes( botnet, data, fill = botnet ) ) +
    geom_boxplot() +
    facet_wrap( ~attacks, scales = "free", ncol = 5 ) +
    theme( axis.text.x = element_blank(), legend.position = "bottom" )
  
  # Std
  pattern <- "H_L(0)*(.)?(0)*\\d_(std)"
  
  tmp <- df %>% select( botnet, grep( pattern = pattern, ignore.case = TRUE, x = colnames ) )
  
  tmp %>% gather( attacks, data, -botnet ) %>% 
    ggplot( aes( botnet, data, fill = botnet ) ) +
    geom_boxplot() +
    facet_wrap( ~attacks, scales = "free", ncol = 5 ) +
    theme( axis.text.x = element_blank(), legend.position = "bottom" )
  
  # All plots show that I can use weight and mean columns to separate benign traffic from attacks, 
  # and remove variance or std columns if I need to reduce data frame (or matrix) dimension.
  
  # Explore HH_L5-HH_L0.01 magnitude
  pattern <- "H_L(0)*(.)?(0)*\\d_(ma)"
  
  tmp <- df %>% select( botnet, grep( pattern = pattern, ignore.case = TRUE, x = colnames ) )
  
  tmp %>% gather( attacks, data, -botnet ) %>% 
    ggplot( aes( botnet, data, fill = botnet ) ) +
    geom_boxplot() +
    facet_wrap( ~attacks, scales = "free", ncol = 5 ) +
    theme( axis.text.x = element_blank(), legend.position = "bottom" )
  
  # The plot shows that magnitude column can be used for separation benign traffic from the attacks
  
  # Explore HH_L5-HH_L0.01 radius
  pattern <- "H_L(0)*(.)?(0)*\\d_(rad)"
  
  tmp <- df %>% select( botnet, grep( pattern = pattern, ignore.case = TRUE, x = colnames ) )
  
  tmp %>% gather( attacks, data, -botnet ) %>% 
    ggplot( aes( botnet, data, fill = botnet ) ) +
    geom_boxplot() +
    facet_wrap( ~attacks, scales = "free", ncol = 5 ) +
    theme( axis.text.x = element_blank(), legend.position = "bottom" )
  
  # The plot shows that radius column doesn't give any information how to separate benign traffic from the attacks.
  
  # Explore HH_L5-HH_L0.01 covariance
  pattern <- "H_L(0)*(.)?(0)*\\d_(co)"
  
  tmp <- df %>% select( botnet, grep( pattern = pattern, ignore.case = TRUE, x = colnames ) )
  
  tmp %>% gather( attacks, data, -botnet ) %>% 
    ggplot( aes( botnet, data, fill = botnet ) ) +
    geom_boxplot() +
    facet_wrap( ~attacks, scales = "free", ncol = 5 ) +
    theme( axis.text.x = element_blank(), legend.position = "bottom" )
  
  # Covariance can be used for separation attacks from benign traffic
  
  # Explore HH_L5-HH_L0.01 pcc
  pattern <- "H_L(0)*(.)?(0)*\\d_(pcc)"
  
  tmp <- df %>% select( botnet, grep( pattern = pattern, ignore.case = TRUE, x = colnames ) )
  
  tmp %>% gather( attacks, data, -botnet ) %>% 
    ggplot( aes( botnet, data, fill = botnet ) ) +
    geom_boxplot() +
    facet_wrap( ~attacks, scales = "free", ncol = 5 ) +
    theme( axis.text.x = element_blank(), legend.position = "bottom" )
  
  # The plot shows that, probably, pcc column can be used fro separation.
  
  # Explore HH_jit_L5 - L0.01 parameters
  pattern <- "HH_jit"
  
  tmp <- df %>% select( botnet, grep( pattern = pattern, ignore.case = TRUE, x = colnames ) )
  
  tmp %>% gather( attacks, data, -botnet ) %>% 
    ggplot( aes( botnet, data, fill = botnet ) ) +
    geom_boxplot() +
    facet_wrap( ~attacks, scales = "free", ncol = 5 ) +
    theme( axis.text.x = element_blank(), legend.position = "bottom" )
  
  # These plots have no new information how to separate benign traffic from attacks.
  
  # Explore HpHp_L5 - L0.01 statistic parameters
  pattern <- "HpHp"
  
  tmp <- df %>% select( botnet, grep( pattern = pattern, ignore.case = TRUE, x = colnames ) )
  
  tmp %>% gather( attacks, data, -botnet ) %>% 
    ggplot( aes( botnet, data, fill = botnet ) ) +
    geom_boxplot() +
    facet_wrap( ~attacks, scales = "free", ncol = 7 ) +
    theme( axis.text.x = element_blank(), legend.position = "bottom" )
  
  # I've noticed that all the plots have more pronounced data at small time-frames.
  
  # For example, pair weight-mean for L0.01 time-frame shows how easily some attacks can be separated,
  # compared with L1 time-frame
  df %>% ggplot( aes( MI_dir_L1_weight, MI_dir_L1_mean, color = botnet ) ) + geom_point()
  df %>% ggplot( aes( MI_dir_L0.01_weight, MI_dir_L0.01_mean, color = botnet ) ) + geom_point()
  
  # Pair mean-covariance better shows that ga_tcp and ga_udp can be separated from benign traffic.
  df %>% ggplot( aes( HH_L1_mean, HH_L1_covariance, color = botnet ) ) + geom_point()
  df %>% ggplot( aes( HH_L0.01_mean, HH_L0.01_covariance, color = botnet ) ) + geom_point()
  
  # All explorations show that I can use weight, mean and covariance to make 
  # a decision how to separate benign traffic from attacks, and remove other 
  # statistic parameters if I need to reduce data frame (or matrix) dimension.
}