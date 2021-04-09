
# Sample 
#sample_size <- 5
#n_bots <- df %>% distinct(botnet) %>% nrow()
#tmp <- df %>% group_by(botnet) %>% sample_n( sample_size )


# Before 29.03.2021
# The tmp set will be 10% of df
#test_index <- createDataPartition( y = df$botnet, times = 1, p = 0.2, list = FALSE)
#tmp <- df[test_index,]

tmp <- df
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


#tmp2 <- tmp %>% select(MI_dir_L5_weight, MI_dir_L3_weight, MI_dir_L1_weight,
#                       MI_dir_L0.1_weight, MI_dir_L0.01_weight, H_L5_weight,
#                       H_L3_weight, H_L1_weight, H_L0.1_weight, H_L0.01_weight,
#                       HH_L5_weight, HH_L3_weight, HH_L1_weight, HH_L0.1_weight,
#                       HH_L0.01_weight, HH_jit_L5_weight, HH_jit_L1_weight,
#                       HH_jit_L0.1_weight, HH_jit_L0.01_weight, HpHp_L5_weight,
#                       HpHp_L3_weight, HpHp_L1_weight, HpHp_L0.1_weight,
#                       HpHp_L0.01_weight)


train_set <- train_set %>% 
  #select(botnet, MI_dir_L5_weight, MI_dir_L3_weight, MI_dir_L1_weight, MI_dir_L0.1_weight, MI_dir_L0.01_weight, H_L5_weight,
  #                              H_L3_weight, H_L1_weight, H_L0.1_weight, H_L0.01_weight,
  #                              HH_L5_weight, HH_L3_weight, HH_L1_weight, HH_L0.1_weight,
  #                              HH_L0.01_weight, HH_jit_L5_weight, HH_jit_L1_weight,
  #                              HH_jit_L0.1_weight, HH_jit_L0.01_weight, HpHp_L5_weight,
  #                              HpHp_L3_weight, HpHp_L1_weight, HpHp_L0.1_weight,
   #                             HpHp_L0.01_weight) %>% 
  data.matrix()
# Create levels for prediction
levels <- train_set[,1]
# Because 'botnet' was used for grouping, the test set contains extra column. Remove 'botnet'column
train_set <- train_set[, -1]

test_set <- test_set %>% 
  #select(botnet, MI_dir_L5_weight, MI_dir_L3_weight, MI_dir_L1_weight,
  #                     MI_dir_L0.1_weight, MI_dir_L0.01_weight, H_L5_weight,
  #                     H_L3_weight, H_L1_weight, H_L0.1_weight, H_L0.01_weight,
  #                     HH_L5_weight, HH_L3_weight, HH_L1_weight, HH_L0.1_weight,
  #                     HH_L0.01_weight, HH_jit_L5_weight, HH_jit_L1_weight,
  #                     HH_jit_L0.1_weight, HH_jit_L0.01_weight, HpHp_L5_weight,
  #                     HpHp_L3_weight, HpHp_L1_weight, HpHp_L0.1_weight,
  #                     HpHp_L0.01_weight) %>% 
  data.matrix()
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
k <- 115
x_train <- pca$x[,1:k]
y <- factor(levels )
fit <- knn3(x_train, y, use.all = F)

col_means <- colMeans(test_set)
x_test <- sweep(test_set, 2, col_means) %*% pca$rotation
x_test <- x_test[,1:k]

y_hat <- predict(fit, x_test, type = "class")
confusionMatrix(y_hat, factor(bots))$overall["Accuracy"]
confusionMatrix(y_hat, factor(bots))



# 29/03/2021
# The tmp set will be 10% of df
setSeed()
test_index <- createDataPartition( y = df$botnet, times = 1, p = 0.6, list = FALSE)
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

colnames <- colnames( df )
# 1. pattern <-  "L(0.0)?1_(weight|mean)"
# 2. pattern <- "L1"
# 3. pattern <- "L1_(weight|mean|magnitude|radius)"
# 4. pattern <- "L(0.0)?1_(weight|mean|magnitude|radius)"
pattern <- "(weight)"
# 6. pattern <- "(weight|mean)"


train_set <- train_set %>% 
  select( botnet, grep( pattern, colnames ) ) %>%
  data.matrix()

# Create levels for prediction
levels <- train_set[,1]
# Because 'botnet' was used for grouping, the test set contains extra column. Remove 'botnet' column
train_set <- train_set[, -1]

test_set <- test_set %>% 
  select( botnet, grep( pattern, colnames ) ) %>%
  data.matrix()

# Because 'botnet' was used for grouping, the test set contains extra column. Remove 'botnet' column
bots <- test_set[,1]
test_set <- test_set[,-1]

# Check with train and test sets
pca <- prcomp(train_set)
summary(pca)

# Check that only 6 first columns give 99,996 % of variability
# Make cross-validation and add validation set
k <- 8
x_train <- pca$x[,1:k]
y <- factor(levels )
fit <- knn3(x_train, y, use.all = F)

col_means <- colMeans(test_set)
x_test <- sweep(test_set, 2, col_means) %*% pca$rotation
x_test <- x_test[,1:k]

y_hat <- predict(fit, x_test, type = "class")
confusionMatrix(y_hat, factor(bots))$overall["Accuracy"]

# 30.03.2021
# Try to use their approach: the train set should contain only benign traffic.
#tmp <- df %>% select(-device)
tmp <- df

colnames <- colnames( df )
#pattern <- "(weight)"
#pattern <- "L(0.0)?1_(weight|mean|covariance)"
pattern <- "L(0.0)?1_(weight|mean|covariance)"

tmp <- tmp %>% select(botnet, grep(pattern = pattern, ignore.case = TRUE, x = colnames))
n_benign <- tmp %>% filter( botnet == 'benign_traffic' ) %>% nrow() * 2/3
#n_benign <- round( n_benign, digits = 0 )
train_set <- tmp[ 1:n_benign, ]
test_set <- tmp[ (n_benign + 1) : nrow(tmp), ] #%>% sample_n( n_benign )

# Binarise botnet column: set value "1" for benign traffic, "0" - oversize
train_set$botnet[ train_set$botnet == 'benign_traffic' ] <- 1
test_set$botnet[ test_set$botnet != 'benign_traffic' ] <- 0
test_set$botnet[ test_set$botnet == 'benign_traffic' ] <- 1



train_set <- train_set %>% 
  #select( botnet, grep( pattern, colnames ) ) %>% 
  data.matrix()
test_set <- test_set %>% 
  #select( botnet, grep( pattern, colnames ) ) %>% 
  data.matrix()

train_set_levels <- as.factor( train_set[, 1] )
test_set_levels <- as.factor( test_set[, 1])
train_set <- train_set[, -1]
test_set <- test_set[, -1]

# Check with train and test sets
pca <- prcomp(train_set)
summary(pca)

# Check that only 6 first columns give 99,996 % of variability
# Make cross-validation and add validation set
k <-  ncol(train_set)
x_train <- pca$x[,1:k]
y <- factor( train_set_levels )
fit <- knn3(x_train, y, use.all = F)

col_means <- colMeans(test_set)
x_test <- sweep(test_set, 2, col_means) %*% pca$rotation
x_test <- x_test[,1:k]

y_hat <- predict(fit, x_test, type = "class") 
cf <- confusionMatrix(y_hat, test_set_levels )
cf$overall["Accuracy"]

# NO MATRICES, WORKS SLOWLY, faster with small dataset
# Try to use their approach: the train set should contain only benign_traffic traffic.
#tmp <- df %>% select(-device)


# As in source - train set is just from beging traffic
#n_benign <- tmp %>% filter( botnet == 'benign' ) %>% nrow() * 2/3
#train_set <- tmp[ 1:n_benign, ]
#test_set <- tmp[ (n_benign + 1) : nrow(tmp), ]

# split tmp to 2 sets
setSeed()
test_index <- createDataPartition( y = df$botnet, times = 1, p = 0.02, list = FALSE)
tmp <- df[test_index,]
tmp$botnet <- as.factor( tmp$botnet )

colnames <- colnames( df )
pattern <- "L(0.0)?1_(weight|mean|covariance)"

tmp <- tmp %>% select( botnet, grep( pattern = pattern, ignore.case = TRUE, x = colnames ) )

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

# Make cross-validation next and add validation set
control <- trainControl(method = "cv", number = 10, p = .9)
fit_knn <- train( botnet ~ ., data = train_set, method = "knn", tuneGrid = data.frame(k = seq(1, 5, 2)), trControl = control )
ggplot(fit_knn, highlight = TRUE)
y_hat <- predict( fit_knn, test_set, type = "raw") 
cf <- confusionMatrix(y_hat, as.factor(test_set$botnet ))
cf$overall["Accuracy"]
# With k=1 we can predict very well, because each row that represents attack is used 
# to predict itself. With more than 100 predictors, the neighborhood is almost
# the entire dataset. Even with 25 predictors, we have to use 90% of dataset to predict
# each row
p <- 1:115
# If p = 25
sz <- .1^(1/25)
plot(p, .1^(1/p), ylim = c(0,1), type = "o", cex = 0.5 )
lines(x = c(25, 25), y = c(0,1), col = "red")
lines(x = c(0, 115), y = c( sz, sz), col = "blue")




# 31.03.2021
# RPART
setSeed()
#test_index <- createDataPartition( y = df$botnet, times = 1, p = 0.2, list = FALSE)
#tmp <- df[test_index,]
tmp <- df
tmp <- tmp %>% select( -device )
tmp$botnet <- as.factor( tmp$botnet )

#colnames <- colnames( df )
#pattern <- "L(0.0)?1_(weight|mean|covariance)"
#tmp <- tmp %>% select( botnet, grep( pattern = pattern, ignore.case = TRUE, x = colnames ) )

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

fit_rpart <- rpart( botnet ~ ., data = train_set )
plot(fit_rpart, margin = 0.1)
text(fit_rpart, cex = 0.75)
y_hat <- predict( fit_rpart, test_set, type = "class") 
cf <- confusionMatrix( y_hat, as.factor(test_set$botnet ) )
cf$overall["Accuracy"]

# Random forest
# With random forest, computation time is a challenge. For each forest, we need to build hundreds of trees. 
setSeed()
tmp <- df 

test_index <- createDataPartition( y = tmp$botnet, times = 1, p = 0.1, list = FALSE)
tmp <- df[test_index,]
tmp$botnet <- as.factor( tmp$botnet )

colnames <- colnames( df )
pattern <- "L(0.0)?1_(weight|mean|covariance)"
tmp <- tmp %>% select( botnet, grep( pattern = pattern, ignore.case = TRUE, x = colnames ) )

test_index <- createDataPartition( y = tmp$botnet, times = 1, p = 0.5, list = FALSE)
train_set <- tmp[ -test_index, ]
test_set <- tmp[test_index, ]

fit <- randomForest( botnet ~ . , data = train_set )
plot(fit)
y_hat <- predict( fit, test_set, type = "class")
cf <- confusionMatrix( y_hat, as.factor(test_set$botnet ) )
cf$overall["Accuracy"]


if ( !require( ggraph )) install.packages( "ggraph", repos = "http://cran.us.r-project.org" )
if ( !require( igraph )) install.packages( "igraph", repos = "https://rdrr.io/cran" )
library(ggraph)
library(igraph)

treeNum <- min(fit$forest$ndbigtree)
tree <- getTree(fit, k = treeNum, labelVar = TRUE) %>% as.data.frame() %>% rownames_to_column( var = "tree" ) %>%
  mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))

# prepare data frame for graph
graph_frame <- data.frame(from = rep(tree$tree, 2),
                          to = c(tree$`left daughter`, tree$`right daughter`))

# convert to graph and delete the last node that we don't want to plot
graph <- graph_from_data_frame(graph_frame) %>%
  delete_vertices("0")

# set node labels
V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
V(graph)$leaf_label <- as.character(tree$prediction)
V(graph)$split <- as.character(round(tree$`split point`, digits = 2))

plot <- ggraph(graph, 'dendrogram') + 
  theme_bw() +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
  geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
  geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                  repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
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
        plot.title = element_text(size = 18))

plot

imp <- importance(fit)
imp
