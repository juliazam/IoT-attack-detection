################################################################################
#
# IoT attacks detection script
#
# Data source: https://archive.ics.uci.edu/ml/datasets/detection_of_IoT_botnet_attacks_N_BaIoT
#
# The dataset contains traffic data from 9 commercial IoT devices authentically 
# infected by Mirai and BASHLITE.
#
# The team that gathered this dataset used 2/3 of benign traffic data for each 
# device (their train set) to train autoencoder. Then they use 1/3 of benign 
# traffic data plus all the malicious data (their test set) to check if 
# autoencoder can detect anomaly. They got anomaly detection with 100% TPR.
# 
# My aim in this project is to check how well the algorithms I learned on
# HarvardX PH125.9x Data Science course (such as KNN, rpart and RandomForest) can
# detect malicious data.
#
################################################################################

# Install and load required libraries
if ( !require( tidyverse )) install.packages( "tidyverse", repos = "http://cran.us.r-project.org" )
if ( !require( rpart )) install.packages( "rpart", repos = "http://cran.us.r-project.org" )
if ( !require( randomForest )) install.packages( "randomForest", repos = "http://cran.us.r-project.org" )
if ( !require( ggraph )) install.packages( "ggraph", repos = "http://cran.us.r-project.org" )
if ( !require( igraph )) install.packages( "igraph", repos = "https://rdrr.io/cran" )

library( tidyverse )
library( rvest )
library( ggplot2 )
library( caret )
library( tools )
library( rpart )
library( randomForest )
library( ggraph )
library( igraph )

# Folder with all IoT attacks data files from all devices
data_folder <- "data"

# Variable for data frame
df <- NULL

# Error code
error_code <- 0

# Error messages
error <- c (
  "Can't download https://github.com/juliazam/IoT-attack-detection/raw/master/data/data_10.zip. Please, download it manually and save in 'data' folder or follow instructions in https://github.com/juliazam/IoT-attack-detection/raw/master/download-data.R script."
)

# Set seed depending on R version
setSeed <- function() {
  #Get R version
  r_version <- as.numeric_version( getRversion())
  if ( r_version >= 4) {
    # if using R 4.0 or later:
    set.seed( 2021, sample.kind="Rounding" )
  } else {
    # if using R 3.6 or earlier: 
    set.seed( 2021 )
  } 
}

# Read IoT data
readData <- function() {
  
  # All IoT attacks data gathered in one file
  data_file <- file.path( data_folder, "data.csv" )
  
  # https://www118.zippyshare.com/v/NDCWjCMp/file.html
  #datafile_url <- "https://www118.zippyshare.com/d/NDCWjCMp/137805/data.zip"
  datafile_url <- "https://download1319.mediafire.com/jjules6qldfg/4z8sk6r3lov2spc/data.zip"
  
  
  # If data file doesn't exist in local folder,download it 
  if( !file.exists( data_file ) ) {
    
    # Try download from url
    zipfile <- paste( data_folder, "data.zip", sep = "/" )
    res <- download.file( datafile_url, zipfile, mode = "wb" )
    size <- file.size( zipfile )
    
    # If successfully downloaded and file size > 200MB (in bites)
    if ( !res && size > 200000000 ) {
      
      print( "Unpacking downloaded archive.")
      
      # Unzip the data file
      unzip( zipfile, exdir = data_folder )
      
    } else {
      # Can't download file
      error_code <<- 1
      return()
    }
    # Remove archive file
    file.remove( zipfile )
  }
  
  print( "Reading data from data file.")
  df <- read.csv( data_file, as.is = TRUE )
  
  return( df )
}

# Read data set
df <- readData()

# If data frame was created, work with it
if( !is.null( df ) ) {
  
  # Data frame structure
  str( df )
  
  # Data frame column headers description:
  # 1. Data frame has 5 time-frames: L5, L3, L1, L0.1 and L0.01.
  # 2. The statistics extracted from each stream for each time-frame:
  #  - weight: the weight of the stream (can be viewed as the number of items 
  #           observed in recent history)
  #  - mean
  #  - std (variance)
  #  - radius: the root squared sum of the two streams' variances
  #  - magnitude: the root squared sum of the two streams' means 
  #  - covariance: an approximated covariance between two streams
  #  - pcc: an approximated correlation coefficient between two streams
  # 3. It has following stream aggregations:
  #  - MI: ("Source MAC-IP" in N-BaIoT paper) Stats summarizing the recent traffic 
  #       from this packet's host (IP + MAC)
  #  - H: ("Source IP" in N-BaIoT paper) Stats summarizing the recent traffic 
  #       from this packet's host (IP)
  #  - HH: ("Channel" in N-BaIoT paper) Stats summarizing the recent traffic going 
  #       from this packet's host (IP) to the packet's destination host.
  # - HH_jit: ("Channel jitter" in N-BaIoT paper) Stats summarizing the jitter 
  #       of the traffic going from this packet's host (IP) to the packet's destination host.
  # - HpHp: ("Socket" in N-BaIoT paper) Stats summarizing the recent traffic going 
  #       from this packet's host+port (IP) to the packet's destination host+port.
  #       Example 192.168.4.2:1242 -> 192.168.4.12:80
  
  # Thus, the column *'MI_dir_L5_weight'* in the data set shows the weight of 
  # the recent traffic from the packet's host for L5 time-frame.
  
  # 'botnet' column represents attacks from different botnets, where:
  # 'benign' is short for 'benign traffic'
  # 'ga' is short for 'gafgyt attack'
  # 'ma' is short for 'mirai attack'
  
  # Data frame dimension
  dim( df )
  
  # The data frame has more than 1 million rows and 115 predictors (in columns).
  # The 'botnet' column contains the outcome.
  
  # Column names in data frame.
  colnames <- colnames( df )
  
  # Convert 'botnet' column as factor because for most functions that used for
  # classification, the outcome should be in this format.
  df$botnet <- as.factor( df$botnet )
  
  # How many botnets are in data set?
  nrow <- nrow( df )
  df %>% group_by( botnet ) %>% summarise( n = n(), prop = n / nrow ) %>% arrange( desc( n ) )
  
  # Plot botnets distribution
  df %>%  select( botnet ) %>%
    ggplot( aes( botnet, fill = as.factor( botnet ) ) ) +
    geom_histogram( binwidth = 0.05, stat = "count" ) +
    theme_minimal() +
    theme( axis.text.x = element_text( angle = 60, vjust = 0.5 ) ) +
    guides( fill = guide_legend( "Botnet"))
  
  # My research has shown that I can use only 'weight', 'mean' and 'covariance' 
  # columns for attacks classification.
  pattern <- "MI_dir_L1_(we|me)"
  
  tmp <- df %>% select( botnet, grep( pattern = pattern, ignore.case = TRUE, x = colnames ) )
  
  tmp %>% gather( attacks, data, -botnet ) %>% 
    ggplot( aes( botnet, data, fill = botnet ) ) +
    geom_boxplot() +
    facet_wrap( ~attacks, scales = "free", ncol = 2 ) +
    theme( axis.text.x = element_blank(), legend.position = "bottom" )
  
  # The plot for 'weight' column shows that almost all types of the attacks, 
  # excluding ga_tcp and ga_udp, can be separated from benign traffic, 
  # as their IQR and medians have much higher values that benign traffic has. 
  # ga_tcp and ga_udp attacks camouflaging as benign traffic very well on this plot.
  # But the plot for 'mean' column shows that IQR and median is higher for benign
  # traffic comparing with these parameters for ga_tcp and ga_udp attacks.
  # Thus, as I wrote above, 'weight' and 'mean' columns can be used for 
  # attacks classification. 
  # In some cases the information they contain is not enough, so 'covariance' 
  # column can be used also for classification.

  # The full data set description and exploration can be found at 
  # https://github.com/juliazam/IoT-attack-detection/blob/master/data-exploration.R
  # https://github.com/juliazam/IoT-attack-detection/blob/master/data-exploration.Rmd or
  # https://github.com/juliazam/IoT-attack-detection/blob/master/data-exploration.pdf
  
  ##############################################################################
  #
  # KNN
  #
  ##############################################################################
  
  # 1. Using PCA
  setSeed()
  
  # 1. Check only columns with 'weight' attribute
  pattern <- "(weight)"
  
  # Select only columns that match the pattern. In this case - only 'weight' column
  tmp <- df %>% select( botnet, grep( pattern = pattern, ignore.case = TRUE, x = colnames ) )
  
  # As I face memory limit, I use only 'prop' of data set
  prop <- 0.6
  test_index <- createDataPartition( y = tmp$botnet, times = 1, p = prop, list = FALSE)
  tmp <- tmp[ test_index, ]
  
  # Create train and test sets
  test_index <- createDataPartition( y = tmp$botnet, times = 1, p = 0.5, list = FALSE)
  train_set <- tmp[ -test_index, ]
  
  # Number of rows in the train set
  n_train <- nrow(train_set)
  
  test_set <- tmp[test_index, ]
  
  # Number of rows in the test set
  n_test <- nrow(test_set)
  
  # Make both sets equal dimension (with equal number of rows)
  if( n_test > n_train ) {
    test_set <- test_set[ 1:n_train, ]
  } else {
    train_set <- train_set[ 1:n_test, ]
  }

  # Convert sets as matrix
  train_set <- train_set %>% data.matrix()
  
  # Create levels for attacks classification
  levels <- as.factor( train_set[ ,1 ] )
  
  # Remove 'botnet' column
  train_set <- train_set[ ,-1 ]
  
  # Convert test set as matrix
  test_set <- test_set %>% data.matrix()
  
  # Botnets outcome for the test set
  bots <- as.factor( test_set[ ,1 ] )
  
  # Remove 'botnet' column
  test_set <- test_set[ ,-1 ]
  
  # Perform PCA
  pca <- prcomp( train_set )
  summary( pca )
  
  # Make cross-validation and calculate classification accuracy
  ks <- seq( 10, 20, 2 )

  # Prepare test set
  col_means <- colMeans( test_set )
  x_test <- sweep( test_set, 2, col_means ) %*% pca$rotation  
  
  # Please, take a note that this takes a time
  accuracy <- sapply( ks, function( k ) {
    x_train <- pca$x[ , 1:k ]

    fit <- knn3( x_train, levels, use.all = FALSE )
    
    x_test_k <- x_test[ , 1:k ]
    
    y_hat <- predict( fit, x_test_k, type = "class" )
    cm <- confusionMatrix( y_hat, bots )
    return( cm$overall["Accuracy"] )
  })

  # Plot k ~ accuracy
  plot( ks, accuracy, type = "o", col = "dodgerblue3" )
  
  # Print k that gives max accuracy
  print( paste( "K = ", ks[ which.max( accuracy ) ] ) )
  
  # Print max accuracy
  print( paste( "Max accuracy = ", max( accuracy ) ) )
  
  # This method doesn't give significant result.
  
  # Save result
  results <- tibble( Method = "KNN, using PCA", 
                     Predictors = paste( ncol( tmp ) - 1, pattern, sep = ", " ),
                     Data_proportion = prop,  
                     Parapeters = paste( "k", ks[ which.max( accuracy ) ] , sep = " = " ),
                     Accuracy = max( accuracy ) )
  
  # Remove data
  rm( pca, x_test, n_test, n_train, ks, accuracy, col_means, levels, bots )

  # 2. Using train() function from caret package with "knn" method
  # 2% of data, all 115 columns
  setSeed()
  
  prop <- 0.02
  test_index <- createDataPartition( y = df$botnet, times = 1, p = prop, list = FALSE )
  tmp <- df[ test_index, ]
  
  # Create train and test sets
  test_index <- createDataPartition( y = tmp$botnet, times = 1, p = 0.5, list = FALSE)
  train_set <- tmp[ -test_index, ]
  test_set <- tmp[ test_index, ]
  
  # Use 10-fold cross-validation
  control <- trainControl( method = "cv", number = 10, p = 0.9 )
  fit_knn <- train( botnet ~ ., data = train_set, method = "knn",
                    tuneGrid = data.frame( k = seq( 1, 5, 2 ) ), 
                    trControl = control )
  
  ggplot( fit_knn, highlight = TRUE )
  
  y_hat <- predict( fit_knn, test_set, type = "raw" ) 
  cm <- confusionMatrix( y_hat, test_set$botnet )
  cm$overall[ "Accuracy" ]
  
  # Check sensitivity and specificity
  cm$byClass[ ,1:2 ]
  
  # Using only 2% of data and all the predictors, the final result is better.
  
  # Save this result
  results <- add_row( .data = results,
                      Method = "KNN, using train() function from caret package", 
                      Predictors = paste( ncol( tmp ) - 1 ),
                      Data_proportion = prop,  
                      Parapeters = paste( "k", fit_knn$bestTune , sep = " = " ),
                      Accuracy = cm$overall["Accuracy"] )
  
  rm (fit_knn)
  
  # With k=1 we can predict very well, because each row that represents attack is used 
  # to predict itself. 
  # It may look like overtraining, but in this particular case because of cyber 
  # attacks prediction , it seems reasonable.
  
  # With more than 100 predictors, the neighborhood is almost
  # the entire dataset. Even with 24 predictors, we have to use approximately 
  # 90% of the data set to predict each row.
  
  # Columns number without 'botnet' column
  ncol <- ncol( df ) - 1
  # All predictors
  ps <- 1:ncol
  # Take p predictors
  p <- 24
  size <- 0.1^(1/ps)
  plot( ps, size, ylim = c( 0, 1 ), type = "o", cex = 0.5 )
  lines( x = c( p, p ), y = c( 0, 1 ), col = "red")
  lines( x = c( 0, ncol ), y = c( 0.1^(1/p), 0.1^(1/p) ), col = "blue")
  text( x = 40, y = 0.85 , labels = paste( 'size = ', round( 0.1^(1/p), 2 ) ), col = "blue" )
  
  rm( ncol, p, ps, size )
  
  # 3. Let's increase data set size for classification and reduce it dimension
  # 20% of data (because of memory limit), 24 predictors
  setSeed()
  
  prop <- 0.2
  test_index <- createDataPartition( y = df$botnet, times = 1, p = prop, list = FALSE )
  tmp <- df[ test_index, ]
  
  pattern <- "L(0.0)?1_(weight|mean|covariance)"
  tmp <- tmp %>% select( botnet, grep( pattern = pattern, ignore.case = TRUE, x = colnames ) )
  
  # Separate to train and test sets
  test_index <- createDataPartition( y = tmp$botnet, times = 1, p = 0.5, list = FALSE)
  train_set <- tmp[ -test_index, ]
  test_set <- tmp[ test_index, ]
  
  # Use 10-fold cross-validation
  control <- trainControl( method = "cv", number = 10, p = 0.9 )
  fit_knn2 <- train( botnet ~ ., data = train_set, method = "knn",
                    tuneGrid = data.frame( k = seq(1, 5, 2 ) ),
                    trControl = control )
  
  ggplot( fit_knn2, highlight = TRUE )
  
  y_hat <- predict( fit_knn2, test_set, type = "raw" ) 
  cm <- confusionMatrix( y_hat, test_set$botnet )
  cm$overall["Accuracy"]
  
  # Check sensitivity and specificity
  cm$byClass[ ,1:2 ]
  
  # My research has shown that I can use 'weight' and 'mean' columns for
  # attacks classification.
  # This result shows that with only these predictors, weight and mean, the
  # accuracy of classification is pretty close to 100%!
  
  # Save this result
  results <- add_row( .data = results,
                      Method = "KNN, using train() function from caret package", 
                      Predictors = paste( ncol( tmp ) - 1, " (weight|mean)", sep = "," ),
                      Data_proportion = prop,  
                      Parapeters = paste( "k", fit_knn2$bestTune , sep = " = " ),
                      Accuracy = cm$overall["Accuracy"] )
  
  rm( control, pattern, fit_knn2, y_hat )
  
  ##############################################################################
  #
  # rpart
  #
  ##############################################################################
    
  setSeed()

  # Use all data set with all predictors
  tmp <- df
  
  # Create train and test sets
  test_index <- createDataPartition( y = tmp$botnet, times = 1, p = 0.5, list = FALSE)
  train_set <- tmp[ -test_index, ]
  test_set <- tmp[test_index, ]
  
  fit_rpart <- rpart( botnet ~ ., data = train_set )
  
  # Visualize decision tree
  plot( fit_rpart, margin = 0.1, main="rpart decision tree")
  text( fit_rpart, cex = 0.75)
  
  # Importance of predictors (columns)
  varImp( fit_rpart )
  
  y_hat <- predict( fit_rpart, test_set, type = "class" ) 
  cm <- confusionMatrix( y_hat, as.factor(test_set$botnet ) )
  cm$overall["Accuracy"]
  
  # Check sensitivity and specificity
  cm$byClass[,1:2]
  
  # Save this result
  results <- add_row( .data = results,
                      Method = "rpart", 
                      Predictors = paste( ncol( tmp ) - 1 ),
                      Data_proportion = 1,  
                      Parapeters = "",
                      Accuracy = cm$overall["Accuracy"] )
  
  # This algorithm works faster than previous, uses whole data set and all 155 
  # predictors. So with all these conditions, I think, the accuracy for 
  # the attack classification is really good.
  
  rm( y_hat, fit_rpart, cm )
  
  ##############################################################################
  #
  # randomForest
  #
  ##############################################################################
   
  # With random forest, computation time is a challenge. For each forest, 
  # we need to build hundreds of trees.
  
  # As I faced memory limit, I use only 10% of data
  # but all 115 columns for this algorithm.
  setSeed()
  prop <- 0.1
  test_index <- createDataPartition( y = df$botnet, times = 1, p = prop, list = FALSE )
  tmp <- df[ test_index, ]
  
  # Separate to train and test sets
  test_index <- createDataPartition( y = tmp$botnet, times = 1, p = 0.5, list = FALSE )
  train_set <- tmp[ -test_index, ]
  test_set <- tmp[ test_index, ]
  
  fit_rm <- randomForest( botnet ~ . , data = train_set )
  y_hat <- predict( fit_rm, test_set, type = "class")
  cm <- confusionMatrix( y_hat, test_set$botnet )
  cm$overall["Accuracy"]
  
  # Check sensitivity and specificity
  cm$byClass[,1:2]
  
  # The classification accuracy is close to 100%!
  
  # Save this result
  results <- add_row( .data = results,
                      Method = "randomForest", 
                      Predictors = paste( ncol( tmp ) - 1 ),
                      Data_proportion = prop,  
                      Parapeters = "",
                      Accuracy = cm$overall["Accuracy"] )
  
  # I would like to visualize randomForest decision tree.
  # I used a method described in "Plotting trees from Random Forest models 
  # with ggraph" article.
  # https://shiring.github.io/machine_learning/2017/03/16/rf_plot_ggraph
  treeNum <- min( fit_rm$forest$ndbigtree )
  tree <- getTree( fit_rm, k = treeNum, labelVar = TRUE) %>% 
    as.data.frame() %>% rownames_to_column( var = "tree" ) %>%
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
  
  # prepare data frame for graph
  graph_frame <- data.frame( from = rep( tree$tree, 2 ),
                            to = c( tree$`left daughter`, tree$`right daughter` ) )
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame( graph_frame ) %>% delete_vertices( "0" )
  
  # set node labels
  V(graph)$node_label <- gsub( "_", " ", as.character( tree$`split var` ) )
  V(graph)$leaf_label <- as.character( tree$prediction )
  V(graph)$split <- as.character( round( tree$`split point`, digits = 2 ) )
  
  # Create the decision tree
  decisionTree <- graph %>% ggraph( 'dendrogram' ) + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text( aes( label = node_label ), na.rm = TRUE, repel = TRUE ) +
    geom_node_label( aes( label = split ), vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label( aes( label = leaf_label, fill = leaf_label), na.rm = TRUE, repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text( size = 18 ) )
  
  # Plot the decision tree
  decisionTree
  
  # Importance of predictors (columns)
  varImp( fit_rm )
  
  # randomForest algorithm gives an opportunity to check the importance of
  # the predictors (columns). And it shows that it mostly used 'weight', 'mean' 
  # and 'covariance' columns as was expected after data exploration, plus
  # 'variance' column in some cases.
  
  rm( tree, treeNum, plot, fit_rm, graph, graph_frame, y_hat, cm, prop, df,
      tmp, test_index, train_set, test_set )
  
  ##############################################################################
  #
  # Result
  #
  ##############################################################################
  
  results
  
  # The result shows that all algorithms that I learnt on HarvardX PH125.9x 
  # Data Science course can be used for the attack classification.
  # Some of the algorithms, such as KNN with only 24 predictors (weight and mean) 
  # or rpart have pretty good classification accuracy.
  # And randomForest has significant result with it  classification accuracy 
  # close to 100%!

} else {
  
  # Print error
  if( error_code > 0 ) {
    print( error[ error_code ] )
  }
  
}