# Robert Gotwals
# October 26, 2019
# mlpackageinstaller.R
# this file installs a series of useful packages for machine learning
#
# SOURCE: https://www.kdnuggets.com/2015/06/top-20-r-machine-learning-packages.html
#
#  e1071 Functions for latent class analysis, short time Fourier transform, fuzzy clustering, support vector machines, shortest path computation, bagged clustering, naive Bayes classifier etc
#  rpart Recursive Partitioning and Regression Trees.
#  igraph A collection of network analysis tools.
#  nnet Feed-forward Neural Networks and Multinomial Log-Linear Models.
#  randomForest Breiman and Cutler's random forests for classification and regression. (105375)
#  caret package (short for Classification And REgression Training) is a set of functions that attempt to streamline the process for creating predictive models. (87151)
#  kernlab Kernel-based Machine Learning Lab. 
#  glmnet Lasso and elastic-net regularized generalized linear models.
#  ROCR Visualizing the performance of scoring classifiers.
#  gbm Generalized Boosted Regression Models. 
#  party A Laboratory for Recursive Partitioning. 
#  arules Mining Association Rules and Frequent Itemsets. 
#  tree Classification and regression trees. 
#  klaR Classification and visualization. 
#  RWeka R/Weka interface. 
#  ipred Improved Predictors.
#  lars Least Angle Regression, Lasso and Forward Stagewise.
#  earth Multivariate Adaptive Regression Spline Models. 
#  CORElearn Classification, regression, feature evaluation and ordinal evaluation.
#  mboost Model-Based Boosting.
#

##  NOTE!  There are OTHER packages here that I've included
#
mlps <- c("caret", "randomForest", "rpart", "glmnet", "e1071", "party", "arules", "recommenderlab", "nnet", "h2o", "class", "gmodels", "ggvivs", "kernlab", "ROCR","earth", "lars", "CORElearn","ipred", "mboost","inum", "lattice")
#
# Install with any dependencies (other packages)
install.packages(mlps, dependencies = TRUE)
#
# Check to make sure they have loaded
library("caret")
library("randomForest")
library("rpart")
library("glmnet")
library("e1071")
library("party")
library("arules")
library("recommenderlab")
library("nnet")
library("h2o")
library("class")
library("gmodels")
library("ggvis")
library("kernlab")
library("ROCR")
library("earth")
library("lars")
library("CORElearn")
library("ipred")
library("mboost")
