################################################################################
#
# IoT attacks detection script
#
# Data source: https://archive.ics.uci.edu/ml/datasets/detection_of_IoT_botnet_attacks_N_BaIoT
#
# The dataset contains traffic data from 9 commercial IoT (Internet of Things) 
# devices authentically infected by Mirai and BASHLITE (Gafgyt).
#
# The team that gathered this dataset used 2/3 of benign traffic data for each 
# device (their train set) to train deep autoencoder. Then they use 1/3 of benign 
# traffic data plus all the malicious data (their test set) to check if 
# their deep autoencoder can detect anomaly. They got anomaly detection with 100% TPR.
# 
# My aim in this project is to check how well KNN, rpart and randomForest methods
# can detect anomalies (classify benign traffic or attack) in the data set, 
# how accurate they can perform classification. 
#
# I have no task to compare methods between each other.
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
  "Can't download data archive from Mediafire.com. Please, visit https://www.mediafire.com/file/4z8sk6r3lov2spc/data.zip/file, copy the link to download data archive, set it as datafile_url and execute the script again."
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
  
  # If data file doesn't exist in local folder,download it 
  if( !file.exists( data_file ) ) {
    
    # https://www.mediafire.com/file/4z8sk6r3lov2spc/data.zip/file
    datafile_url <- "https://download1319.mediafire.com/76p2hux2x48g/4z8sk6r3lov2spc/data.zip"
    
    # Please, use alternative link, if you can't download from mediafire
    # https://www118.zippyshare.com/v/NDCWjCMp/file.html
    # datafile_url <- "https://www118.zippyshare.com/d/NDCWjCMp/48720/data.zip"    
    

    # If folder is already exists, do not show warning
    dir.create( data_folder, showWarnings = FALSE )
    
    zipfile <- paste( data_folder, "data.zip", sep = "/" )
    
    # Try download from url
    tryCatch(
      {
        res <- download.file( datafile_url, zipfile, mode = "wb" )
      },
      error = function( e) {
        # Can't download file, save error code and return
        error_code <<- 1
        return()
      },
      finally = {
        # If data archive has been downloaded, check its size
        size <- file.size( zipfile )
        
        # If successfully downloaded and file size > 200MB (in bites)
        if ( !res && size > 200000000 ) {
          
          print( "Unpacking downloaded archive.")
          
          # Unzip the data file
          unzip( zipfile, exdir = data_folder )
          
        } else {
          # Can't download file
          error_code <<- 1
        }
        # Remove archive file
        file.remove( zipfile )
      }
    )
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
  # 1. It has 5 time-frames: L5 (1 min), L3 (10 sec), L1 (1.5 sec), L0.1 (500 ms) and L0.01 (100 ms)
  # 2. The statistics extracted from each traffic stream for each time-frame:
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
  
  # Data frame dimensions
  dim( df )
  
  # The data frame has more than 1 million rows and 115 attributes.
  # The 'botnet' column contains the outcome.
  
  # Column names in data frame.
  colnames <- colnames( df )
  
  # Convert 'botnet' column as factor because the outcome for classification
  # should be in this format.
  df$botnet <- as.factor( df$botnet )
  
  # How many botnets are in data set?
  nrow <- nrow( df )
  df %>% group_by( botnet ) %>% summarise( n = n(), prop = n / nrow ) %>% arrange( desc( n ) )
  
  # Benign traffic is only 4.87% of all traffic.
  
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
  
  # The plot for 'weight' column shows that almost all types of attacks, 
  # excluding ga_tcp and ga_udp, can be separated from benign traffic, 
  # as their IQR and medians have higher values than benign traffic. 
  # ga_tcp and ga_udp attacks camouflage very well as benign traffic on this plot.
  # However, the plot for MI_dir_L1_mean column shows that IQR and median is 
  # higher for benign traffic compared with these parameters for ga_tcp and 
  # ga_udp attacks. Thus, this column data will help to separate ga_tcp and ga_udp
  # attacks from the benign traffic.
  
  # Therefore, as I wrote above, 'weight' and 'mean' columns can be used for 
  # attack classification. As all the attacks have outliers, the information these 
  # columns contain is not enough, so 'covariance' column can also be used for classification.

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
  
  # As I have memory limit, I use only 60% of the data set
  prop <- 0.6
  test_index <- createDataPartition( y = tmp$botnet, times = 1, p = prop, list = FALSE)
  tmp <- tmp[ test_index, ]
  
  # Create train and test sets
  test_index <- createDataPartition( y = tmp$botnet, times = 1, p = 0.5, list = FALSE)
  
  train_set <- tmp[ -test_index, ]
  test_set <- tmp[ test_index, ]
  
  # Number of rows in the train set
  n_train <- nrow(train_set)
  
  # Number of rows in the test set
  n_test <- nrow(test_set)
  
  # Make both sets equal dimensions (with equal number of rows)
  if( n_test > n_train ) {
    test_set <- test_set[ 1:n_train, ]
  } else {
    train_set <- train_set[ 1:n_test, ]
  }

  # Convert train set as matrix
  train_set <- train_set %>% data.matrix()
  
  # Create levels for the attacks classification
  levels <- as.factor( train_set[ ,1 ] )
  
  # Now remove 1st column, that represents 'botnet' column 
  train_set <- train_set[ ,-1 ]
  
  # Convert test set as matrix
  test_set <- test_set %>% data.matrix()
  
  # Create botnets outcome for the test set
  bots <- as.factor( test_set[ ,1 ] )
  
  # Now remove 1st column, that represents 'botnet' column 
  test_set <- test_set[ ,-1 ]
  
  # Perform PCA
  pca <- prcomp( train_set )
  summary( pca )
  
  # Make cross-validation and calculate classification accuracy
  ks <- seq( 10, 20, 2 )

  # Prepare test set
  col_means <- colMeans( test_set )
  x_test <- sweep( test_set, 2, col_means ) %*% pca$rotation  
  
  # Please, pay attention that this algorithm is slow.
  accuracy <- sapply( ks, function( k ) {
    x_train <- pca$x[ , 1:k ]

    fit <- knn3( x_train, levels, use.all = FALSE )
    
    x_test_k <- x_test[ , 1:k ]
    
    y_hat <- predict( fit, x_test_k, type = "class" )
    cm <- confusionMatrix( y_hat, bots )
    return( cm$overall[ "Accuracy" ] )
  })

  # Plot k ~ accuracy
  plot( ks, accuracy, type = "o", col = "dodgerblue3" )
  
  # Print k that gives max accuracy
  print( paste( "K = ", ks[ which.max( accuracy ) ] ) )
  
  # Print max accuracy
  print( paste( "Max accuracy = ", max( accuracy ) ) )
  
  # This method doesn't give significant result as its classification accuracy 
  # is only 79.54%. On the other hand, I used only one 'weight' column for classification.
  
  # I decided not to perform classification using 'weight', 'mean' and 'covariance'
  # columns with this algorithm, as even with only 'weight' column, it works really 
  # slow (about 3hrs with my memory limit).
  
  # Save result
  results <- tibble( Method = "KNN (with PCA)", 
                     Predictors = paste( ncol( tmp ) - 1, pattern, sep = ", " ),
                     Data_proportion = prop,  
                     Parameters = paste( "k", ks[ which.max( accuracy ) ] , 
                                         sep = " = " ),
                     Accuracy = max( accuracy ) )
  
  # Remove data
  rm( pca, x_test, n_test, n_train, ks, accuracy, col_means, levels, bots )

  # 2. Using train() function from caret package with "knn" method
  # 2% of data, all 115 attributes
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
  
  # 'Sensitivity' is the quantity, referred to as the true positive rate (TPR), 
  # while 'specificity' is also called the true negative rate (TNR). Both of them 
  # show how accurate the classification was made for each attack or benign traffic.
  cm$byClass[ ,1:2 ]
  
  # Save this result
  results <- add_row( .data = results,
                      Method = "KNN (with caret package)", 
                      Predictors = paste( ncol( tmp ) - 1 ),
                      Data_proportion = prop,  
                      Parameters = paste( "k", fit_knn$bestTune , sep = " = " ),
                      Accuracy = cm$overall[ "Accuracy" ] )
  
  rm (fit_knn)
  
  # We have learned in the course, that k=1 may lead to over-training, because each row 
  # in the data set is used to predict itself. In this particular case, it's reasonable, 
  # in my opinion, because the team that collected the data set used a  similar approach. 
  # They used only 'benign traffic' to train their 'deep autoencoder', this is pretty 
  # close to use of k=1 in KNN method. Furthermore, data exploration has shown that 
  # some attribute combinations (such as 'weight', 'mean' and 'covariance') can 
  # help to clearly separate 'benign traffic' from the attacks, and thus, even 
  # with k=1 it becomes possible to make great classification.
  
  # 3. Let's reduce the data set dimensions and increase its size
  # 20% of data (because of memory limit), 24 attributes
  setSeed()
  
  prop <- 0.2
  test_index <- createDataPartition( y = df$botnet, times = 1, p = prop, list = FALSE )
  tmp <- df[ test_index, ]
  
  pattern <- "L(0.0)?1_(weight|mean|covariance)"
  tmp <- tmp %>% select( botnet, grep( pattern = pattern, ignore.case = TRUE, x = colnames ) )
  
  # Create train and test sets
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
  cm$overall[ "Accuracy" ]
  
  # Check sensitivity and specificity
  cm$byClass[ ,1:2 ]
  
  # The result shows that using only 'weight', 'mean' and 'covariance' columns 
  # for L1 and L0.01 time frames gives the accuracy of the classification 
  # (attack detection) about 99.48%!
  
  # Save this result
  results <- add_row( .data = results,
                      Method = "KNN (with caret, reduced data)", 
                      Predictors = paste( ncol( tmp ) - 1, " (L1, L0.01)", sep = "," ),
                      Data_proportion = prop,  
                      Parameters = paste( "k", fit_knn2$bestTune , sep = " = " ),
                      Accuracy = cm$overall[ "Accuracy" ] )
  
  rm( control, pattern, fit_knn2, y_hat )
  
  ##############################################################################
  #
  # rpart
  #
  ##############################################################################
    
  # This method allows the use all 115 attributes and whole original data set 
  # even with my memory limit.
  setSeed()

  # Use whole data set with all attributes
  tmp <- df
  
  # Create train and test sets
  test_index <- createDataPartition( y = tmp$botnet, times = 1, p = 0.5, list = FALSE)
  train_set <- tmp[ -test_index, ]
  test_set <- tmp[ test_index, ]
  
  fit_rpart <- rpart( botnet ~ ., data = train_set )
  
  # Visualize decision tree
  plot( fit_rpart, margin = 0.1, main="rpart decision tree" )
  text( fit_rpart, cex = 0.75 )
  
  # According to the decision tree above, rpart mostly uses 'mean' column and 
  # sometimes makes its decisions using 'weight' and 'covariance' columns. 
  # The data exploration also has shown that using only 'weight', 'mean' and 
  # 'covariance' columns will be enough to detect attacks (make their classification).
  
  y_hat <- predict( fit_rpart, test_set, type = "class" ) 
  cm <- confusionMatrix( y_hat, as.factor( test_set$botnet ) )
  cm$overall[ "Accuracy" ]
  
  # Check sensitivity and specificity
  cm$byClass[ ,1:2 ]
  
  # Save this result
  results <- add_row( .data = results,
                      Method = "rpart", 
                      Predictors = paste( ncol( tmp ) - 1 ),
                      Data_proportion = 1,  
                      Parameters = "",
                      Accuracy = cm$overall[ "Accuracy" ] )
  
  # The accuracy of the attack detection (classification) using rpart method 
  # is not as high and is about 98.66%.
  
  rm( y_hat, fit_rpart, cm )
  
  ##############################################################################
  #
  # randomForest
  #
  ##############################################################################
   
  # The method builds a lot of trees for each forest, so the computation time 
  # will be considerable. I will use only 10% of the original data set, 
  # because of the memory limit.
  
  setSeed()
  prop <- 0.1
  test_index <- createDataPartition( y = df$botnet, times = 1, p = prop, list = FALSE )
  tmp <- df[ test_index, ]
  
  # Create train and test sets
  test_index <- createDataPartition( y = tmp$botnet, times = 1, p = 0.5, list = FALSE )
  train_set <- tmp[ -test_index, ]
  test_set <- tmp[ test_index, ]
  
  # Perform classification
  fit_rm <- randomForest( botnet ~ . , data = train_set )
  y_hat <- predict( fit_rm, test_set, type = "class" )
  cm <- confusionMatrix( y_hat, test_set$botnet )
  cm$overall[ "Accuracy" ]
  
  # Check sensitivity and specificity
  cm$byClass[ ,1:2 ]
  
  # The accuracy of the classification (attack detection) is 99.98%! 
  # It really is a significant result!
  
  # Save this result
  results <- add_row( .data = results,
                      Method = "randomForest", 
                      Predictors = paste( ncol( tmp ) - 1 ),
                      Data_proportion = prop,  
                      Parameters = "",
                      Accuracy = cm$overall[ "Accuracy" ] )
  
  # It will be interesting to visualize at least one decision tree from the 'forest'. 
  # randomForest package has no tool for visualization, however, the decision 
  # tree can be visualized using method described in "Plotting trees from Random 
  # Forest models with ggraph" article.
  # https://shiring.github.io/machine_learning/2017/03/16/rf_plot_ggraph
  
  treeNum <- min( fit_rm$forest$ndbigtree )
  tree <- getTree( fit_rm, k = treeNum, labelVar = TRUE ) %>% 
    as.data.frame() %>% rownames_to_column( var = "tree" ) %>%
    mutate(`split point` = ifelse( is.na( prediction ), `split point`, NA ) )
  
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
  
  rm( tree, treeNum, decisionTree, fit_rm, graph, graph_frame, y_hat, cm, prop, df,
      tmp, test_index, train_set, test_set )
  
  ##############################################################################
  #
  # Result
  #
  ##############################################################################
  
  # I've checked how accurate KNN, rpart* and randomForest methods are in 
  # detecting the cyber attacks. Here is the final result:
  
  results
  
  # As seen from the result table, randomForest method gives 100% accuracy in 
  # classification. It also gives 100% TPR in detecting either 'benign traffic' 
  # or some attacks. But for other attacks, it gives TPR ~ 99.7%. 
  # Unfortunately, this means it can detect some attacks as 'benign traffic'. 
  # For security reason it's not as good. I found that the result highly correlates 
  # with the result that the team that collected the data set got.
  
  # rpart method uses all 115 attributes and whole data set to make its decision tree, 
  # works fast, but its classification accuracy is 98.7%.
  
  # KNN method on 'reduced' data set gives the classification accuracy of about 99.5%.

} else {
  
  # Print error
  if( error_code > 0 ) {
    print( error[ error_code ] )
  }
}