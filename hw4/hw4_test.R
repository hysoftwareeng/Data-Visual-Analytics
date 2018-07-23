mnist_train = read.csv('mnist/mnist_train.csv', header=FALSE)
mnist_test = read.csv('mnist/mnist_test.csv', header=FALSE)

train_0_1 = mnist_train[,((mnist_train[785,] == 0) | (mnist_train[785,] == 1))]
train_3_5 = mnist_train[,((mnist_train[785,] == 3) | (mnist_train[785,] == 5))]

test_0_1 = mnist_test[,((mnist_test[785,] == 0) | (mnist_test[785,] == 1))]
test_3_5 = mnist_test[,((mnist_test[785,] == 3) | (mnist_test[785,] == 5))]


train_data_0_1 = as.matrix(t(train_0_1[-785, ]))
train_data_3_5 = as.matrix(t(train_3_5[-785, ]))
train_labels_0_1 = as.matrix(t(train_0_1[785, ]))
train_labels_3_5 = as.matrix(t(train_3_5[785, ]))

test_data_0_1 = as.matrix(t(test_0_1[-785, ]))
test_data_3_5 = as.matrix(t(test_3_5[-785, ]))
test_labels_0_1 = as.matrix(t(test_0_1[785, ]))
test_labels_3_5 = as.matrix(t(test_3_5[785, ]))

train_data_0_1 = cbind(1, train_data_0_1)
train_data_3_5 = cbind(1, train_data_3_5)
test_data_0_1 = cbind(1, test_data_0_1)
test_data_3_5 = cbind(1, test_data_3_5)

train_labels_0_1 = ifelse(train_labels_0_1 == 0, -1, 1)
train_labels_3_5 = ifelse(train_labels_3_5 == 3, -1, 1)
test_labels_0_1 = ifelse(test_labels_0_1 == 0, -1, 1)
test_labels_3_5 = ifelse(test_labels_3_5 == 3, -1, 1)



### end of processing ###

####################################
#####Define the sigmoid function
####################################
sigmoid <- function(z)
{
  gz <- 1/(1+exp(-z))
  return(gz)
}

####################################
#####Predict y
####################################
predict <- function(theta, data){
  prediction <- sigmoid(data %*% theta)
  prediction <- ifelse(prediction>0.5,1,-1)
  return (prediction)
}

####################################
#####Calculate the accuracy
####################################
accuracy = function(labels_pred, labels){
  error = labels - labels_pred
  acc = length(error[error==0])/length(labels)
  return(acc)
}

#############################################
#####Calculate stochastic Gradient Descent
#############################################
calculate_gradient_descent_sgd <- function (x, y, epochs, alpha, threshold) {
  
  #Initialize theta to random values.
  theta <- runif(dim(x)[2], min=0, max=1)
  theta <- as.matrix(theta)
  temp = theta
  number_of_samples = dim(x)[1]
  
  # For every epoch, do the following, I need to check for convergance every epoch. Also, I need to run through all the data in every epoch.
  for(epoch in 1:epochs) {
    print(epoch)
    #For every epoch, shuffle the dataset.
    #Bind X and Y so that we can shuffle X and Y equally
    temp_binded_df <- cbind(x, y)
    
    #shuffle X and Y equally by row
    shuffled_df <- temp_binded_df[sample(nrow(temp_binded_df)),]
    
    #unbind X and Y after the shuffling is done
    xtemp <- shuffled_df[,1:785]
    ytemp <- shuffled_df[,786]
    ytemp <- as.matrix(ytemp)
    #Now I am done with shuffling the dataset for this epoch.
    
    #Save Previous theta value. Previous value should be for every epoch.
    theta_previous = theta
    
    #Iterate over each sample in the dataset
    for (i in 1:number_of_samples)
    {
      xi <- t(as.matrix(xtemp[i,]))
      yi <- t(as.matrix(ytemp[i,]))
      
      n = alpha / ((exp(yi * (xi %*% theta)) + 1))
      theta = theta + (as.numeric(n) * t(xi) %*% yi)
    }
    
    delta = sum(abs(theta_previous - theta))
    print(delta)
    
    #Stopping condition if delta in theta is less than threshold
    if (delta < threshold) {return(theta)}
  }
  return(theta)
}


##################################################
#####Train SGD
##################################################
train <- function(data, labels, alpha){
  theta <- calculate_gradient_descent_sgd(data, labels, 8, alpha, 0.1)
  return (theta)
}


theta2 = train(train_data_3_5, train_labels_3_5, 0.6)
prediction = predict(theta2, train_data_3_5)
acc = accuracy(prediction, train_labels_3_5)
sprintf("Accuracy of prediction in train_data_3_5 is %f", acc)
