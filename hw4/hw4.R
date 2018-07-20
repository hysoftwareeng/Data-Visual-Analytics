mnist_train = read.csv('mnist/mnist_train.csv', header=FALSE)
mnist_test = read.csv('mnist/mnist_test.csv', header=FALSE)

train_0_1 = mnist_train[,((mnist_train[785,] == 0) | (mnist_train[785,] == 1))]
train_3_5 = mnist_train[,((mnist_train[785,] == 3) | (mnist_train[785,] == 5))]

test_0_1 = mnist_test[,((mnist_test[785,] == 0) | (mnist_test[785,] == 1))]
test_3_5 = mnist_test[,((mnist_test[785,] == 3) | (mnist_test[785,] == 5))]


print(sprintf("Dimension of train_0_1 are %i by %i.", dim(train_0_1)[1], dim(train_0_1)[2]))
print(sprintf("Dimension of train_3_5 are %i by %i.", dim(train_3_5)[1], dim(train_3_5)[2]))
print(sprintf("Dimension of test_0_1 are %i by %i.", dim(test_0_1)[1], dim(test_0_1)[2]))
print(sprintf("Dimension of test_3_5 are %i by %i.", dim(test_3_5)[1], dim(test_3_5)[2]))

train_data_0_1 = t(train_0_1[-785, ])
train_data_3_5 = t(train_3_5[-785, ])
train_labels_0_1 = t(train_0_1[785, ])
train_labels_3_5 = t(train_3_5[785, ])

test_data_0_1 = as.matrix(t(test_0_1[-785, ]))
test_data_3_5 = t(test_3_5[-785, ])
test_labels_0_1 = t(test_0_1[785, ])
test_labels_3_5 = t(test_3_5[785, ])

train_data_0_1 <- cbind(1, train_data_0_1)



### end of processing ###


train = function(data, labels, alpha) {
  #initialize theta with random values based on number of features (columns)
  theta = as.matrix(runif(dim(data)[2], 0, 1))
  epochs = 2
  for (epoch in 1:epochs){
    #bind for shuffling
    bound_data_labels = cbind(data, labels)
  
    #shuffle and unbind back to individual variables
    bound_data_labels_shuffled = bound_data_labels[sample(nrow(bound_data_labels)), ]
  
  
  }
}

predict = function(theta, data) {
  #get the probability by pluging the matrix multiplication of data and theta into the sigmoid function
  probability = 1/(1+exp(-(data %*% theta)))
  if (probability >= 0.5) {
    return (1)
  } else {
    return (-1)
  }
}


theta = train(train_data_0_1, train_labels_0_1, 0.2)