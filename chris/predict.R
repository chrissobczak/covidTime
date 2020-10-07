library(tidyverse)
library(Metrics)
library(h2o)
source('model-function.R')
set.seed(323)

folds = 10
trees = 30
layers = c(5, 15, 5)
y = 'log_y'

full_train_org = full_train
train_org = train
test_org = test

full_train = as.h2o(full_train)
train = as.h2o(train)
validation = as.h2o(validation)
test = as.h2o(test)

df = full_train

x = 	c(	'city','ill','weakness',
		'weak','pharyngeal','age',
		'dry','cough','empty',
		'malaise','sex','V1')


models = Model(df, x, folds, trees, layers, 1231, y)

	# Predicting ############################
#	y_valid = as.data.frame(validation)$duration
#	y_hat_valid = as.data.frame(h2o.predict(models$E_model, validation))$predict
#
#	resids = y_valid - y_hat_valid
#	
#	plot(
#		x = y_valid, y = y_hat_valid,
#		xlab = "Y", ylab = "Predicted Y",
#		main = "Predicted v Actual"
#	) + 
#	# abline(h = 0)


Id = as.data.frame(h2o.predict(models$E_model, test))
Id$predict = exp(Id$predict)
names(Id) = 'duration'
write.table(Id, sep = ',', file = 'mod.txt', quote = F, row.names = T)



