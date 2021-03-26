# Install and load required libraries
if ( !require( tidyverse )) install.packages( "tidyverse", repos = "http://cran.us.r-project.org" )
library( tidyverse )
library(rvest)
library(ggplot2)

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

str( df )
dim

df %>% group_by(botnet) %>% summarise(n = n()) %>% 
  mutate( botnet = reorder(botnet, n)) %>% 
  ggplot(aes(n, botnet,  fill = as.factor(botnet))) + 
  geom_bar( stat = "identity" )

# Show MI_dir_L5_weight
df %>% group_by(botnet) %>% select(MI_dir_L5_weight) %>% 
  ggplot(aes(MI_dir_L5_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# Show MI_dir_L3_weight
df %>% group_by(botnet) %>% select(MI_dir_L3_weight) %>% 
  ggplot(aes(MI_dir_L3_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# Show MI_dir_L1_weight
df %>% group_by(botnet) %>% select(MI_dir_L1_weight) %>% 
  ggplot(aes(MI_dir_L1_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

df %>% group_by(botnet) %>% select(MI_dir_L0.1_weight) %>% 
  ggplot(aes(MI_dir_L0.1_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

df %>% group_by(botnet) %>% select(MI_dir_L0.01_weight) %>% 
  ggplot(aes(MI_dir_L0.01_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# Sample 
sample_size <- 5
n_bots <- df %>% distinct(botnet) %>% nrow()
tmp <- df %>% group_by(botnet) %>% sample_n( sample_size )


library(caret)
# The tmp set will be 10% of df
test_index <- createDataPartition( y = df$botnet, times = 1, p = 0.2, list = FALSE)
tmp <- df[test_index,]

test_index <- createDataPartition( y = tmp$botnet, times = 1, p = 0.5, list = FALSE)
train_set <- tmp[ -test_index, ]
n_train <- nrow(train_set)
test_set <- tmp[test_index, ]
n_test <- nrow(test_set)
if( n_test > n_train) {
  test_set <- test_set[1:n_train, ]
} else {
  train_set <- train_set[1:n_test, ]
}

# Show MI_dir_L5_weight
tmp %>% group_by(botnet) %>% select(MI_dir_L5_weight) %>% 
  ggplot(aes(MI_dir_L5_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

# Show MI_dir_L3_weight
tmp %>% group_by(botnet) %>% select(MI_dir_L3_weight) %>% 
  ggplot(aes(MI_dir_L3_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()

tmp %>% group_by(botnet) %>% select(MI_dir_L1_weight) %>% 
  ggplot(aes(MI_dir_L1_weight, botnet,  fill = as.factor(botnet))) + 
  geom_boxplot()


tmp2 <- tmp %>% select(MI_dir_L5_weight, MI_dir_L3_weight, MI_dir_L1_weight,
                       MI_dir_L0.1_weight, MI_dir_L0.01_weight, H_L5_weight,
                       H_L3_weight, H_L1_weight, H_L0.1_weight, H_L0.01_weight,
                       HH_L5_weight, HH_L3_weight, HH_L1_weight, HH_L0.1_weight,
                       HH_L0.01_weight, HH_jit_L5_weight, HH_jit_L1_weight,
                       HH_jit_L0.1_weight, HH_jit_L0.01_weight, HpHp_L5_weight,
                       HpHp_L3_weight, HpHp_L1_weight, HpHp_L0.1_weight,
                       HpHp_L0.01_weight)


train_set <- train_set %>% select(botnet, MI_dir_L5_weight, MI_dir_L3_weight, MI_dir_L1_weight,
                                MI_dir_L0.1_weight, MI_dir_L0.01_weight, H_L5_weight,
                                H_L3_weight, H_L1_weight, H_L0.1_weight, H_L0.01_weight,
                                HH_L5_weight, HH_L3_weight, HH_L1_weight, HH_L0.1_weight,
                                HH_L0.01_weight, HH_jit_L5_weight, HH_jit_L1_weight,
                                HH_jit_L0.1_weight, HH_jit_L0.01_weight, HpHp_L5_weight,
                                HpHp_L3_weight, HpHp_L1_weight, HpHp_L0.1_weight,
                                HpHp_L0.01_weight) %>% data.matrix()
# Create levels for prediction
levels <- train_set[,1]
# Because 'botnet' was used for grouping, the test set contains extra column. Remove 'botnet'column
train_set <- train_set[, -1]

test_set <- test_set %>% select(botnet, MI_dir_L5_weight, MI_dir_L3_weight, MI_dir_L1_weight,
                       MI_dir_L0.1_weight, MI_dir_L0.01_weight, H_L5_weight,
                       H_L3_weight, H_L1_weight, H_L0.1_weight, H_L0.01_weight,
                       HH_L5_weight, HH_L3_weight, HH_L1_weight, HH_L0.1_weight,
                       HH_L0.01_weight, HH_jit_L5_weight, HH_jit_L1_weight,
                       HH_jit_L0.1_weight, HH_jit_L0.01_weight, HpHp_L5_weight,
                       HpHp_L3_weight, HpHp_L1_weight, HpHp_L0.1_weight,
                       HpHp_L0.01_weight) %>% data.matrix()
# Because 'botnet' was used for grouping, the test set contains extra column. Remove 'botnet'column
bots <- test_set[,1]
test_set <- test_set[,-1]

#tmp2 <- tmp %>% select(MI_dir_L5_weight, MI_dir_L3_weight, MI_dir_L1_weight)

#tmp3 <- data.matrix(tmp2)

#tmp3 <- data.matrix(tmp)
#tmp3 <- tmp3[,-1]
#tmp2 <- tmp2[,-1]
#dist <-dist(tmp3)
#h <-hclust(dist)
#plot(h, cex = 0.65, main = "", xlab = "")

#sums <- rowSums(tmp3[,-1])
#tmp3 <- tmp3 %>% cbind( sum = sums)

#tmp2 <- tmp2 %>% cbind(sums)
#names(tmp2)[5] <- "Sum"

#tmp2 %>% group_by(botnet) %>% select(Sum) %>% 
#  ggplot(aes(Sum, botnet,  fill = as.factor(botnet))) + 
#  geom_boxplot()

# Check that only 5 first columns give 99,996% of variability
#pca <- prcomp(tmp3[,-1])
#pca$rotation
#summary(pca)


# Check with train and test sets
pca <- prcomp(train_set)
summary(pca)
head(pca$x)

# Check that only 6 first columns give 99,996 % of variability
k <- 6
x_train <- pca$x[,1:k]
y <- factor(levels )
fit <- knn3(x_train, y)

col_means <- colMeans(test_set)
x_test <- sweep(test_set, 2, col_means) %*% pca$rotation
x_test <- x_test[,1:k]

y_hat <- predict(fit, x_test, type = "class")
confusionMatrix(y_hat, factor(bots))$overall["Accuracy"]
confusionMatrix(y_hat, factor(bots))




# DO NOT USE CODE BELOW
test_index <- createDataPartition( y = df$botnet, times = 1, p = 0.005, list = FALSE)
tmp <- df[test_index,] %>% select( MI_dir_L5_weight, MI_dir_L3_weight, MI_dir_L1_weight,
                              MI_dir_L0.1_weight, MI_dir_L0.01_weight, H_L5_weight) %>% data.matrix()
#col_means <- colMeans(tmp)
#row_means <- rowMeans(tmp)
#tmp <- sweep(tmp, 2, col_means)
#tmp <- sweep(tmp, 1, row_means)
d <- dist(tmp)
h <- hclust(d)
plot(h, cex = 0.65, main = "", xlab = "")
groups <- cutree(h, k = n_bots)
tmp %>% filter( rownames %in% names(groups)[groups == 4])


k <- kmeans(tmp, centers = n_bots)
groups <- k$cluster
heatmap(tmp, col = RColorBrewer::brewer.pal(11, "Spectral"))
