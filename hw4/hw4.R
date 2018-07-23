#processing
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

predict = function(theta, data) {
  #get the probability by pluging the matrix multiplication of data and theta into the sigmoid function
  probability = 1/(1+exp(-(as.matrix(data) %*% theta)))
  result = ifelse(probability > 0.5, 1, -1)
  return (result)
}

bind_and_shuffle = function(data, labels) {
  bound_data_labels = cbind(data, labels)
  bound_data_labels_shuffled = bound_data_labels[sample(nrow(bound_data_labels)), ]
  return (bound_data_labels_shuffled)
}

train = function(data, labels, alpha){
  threshold = 0.01
  epochs = 20
  theta = as.matrix(runif(dim(data)[2], 0, 1))
  loss_old = Inf
  product = data %*% theta
  loss_new = sum(1 + exp(-(labels * product)))
  
  for(epoch in 1:epochs) {
    temp_binded_df = cbind(data, labels)
    
    shuffled_data_labels = bind_and_shuffle(data, labels)
    data = shuffled_data_labels[, -786]
    labels = as.matrix(shuffled_data_labels[, 786])

    #Iterate over each sample in the dataset
    for (i in 1:dim(data)[1])
    {
      x_i = t(as.matrix(data[i,]))
      y_i = t(as.matrix(labels[i,]))
      product = x_i %*% theta
      delta = t(x_i) %*% y_i / as.numeric(1 + exp(y_i * product))
      theta = theta + alpha * delta
    }
    
    loss_old = loss_new
    product = data %*% theta
    loss_new = sum(1 + exp(-(labels * product)))
    if (abs(loss_new - loss_old) <= threshold) {
      return (theta)
    }
  }
  return(theta)
}


### modelling ######

accuracy = function(predicted_labels, true_labels) {
  acc = sum(predicted_labels == true_labels) / dim(true_labels)[1]
  return (acc)
}

accuracy = function(predicted_labels, true_labels) {
  acc = sum(predicted_labels == true_labels) / dim(true_labels)[1]
  return (acc)
}

model = function(train_data, train_labels, test_data, test_labels, alpha) {
  theta = train(train_data, train_labels, alpha)
  prediction_train = predict(theta, train_data)
  acc_train = accuracy(prediction_train, train_labels)
  
  prediction_test = predict(theta, test_data)
  acc_test = accuracy(prediction_test, test_labels)
  return (list(theta, acc_train, acc_test))
}


theta1 = train(train_data_0_1, train_labels_0_1, 0.6)
# prediction = predict(theta1, train_data_0_1)
# acc = accuracy(prediction, train_labels_0_1)
# sprintf("Accuracy of prediction in train_data_0_1 is %f", acc)
# 
# prediction = predict(theta1, test_data_0_1)
# acc = accuracy(prediction, test_labels_0_1)
# sprintf("Accuracy of prediction in test_data_0_1 is %f", acc)

testing = model(train_data_0_1, train_labels_0_1, test_data_0_1, test_labels_0_1, 0.6)

theta2 = train(train_data_3_5, train_labels_3_5, 0.6)
prediction = predict(theta2, train_data_3_5)
acc = accuracy(prediction, train_labels_3_5)
sprintf("Accuracy of prediction in train_data_3_5 is %f", acc)

prediction = predict(theta2, test_data_3_5)
acc = accuracy(prediction, test_labels_3_5)
sprintf("Accuracy of prediction in test_data_3_5 is %f", acc)
