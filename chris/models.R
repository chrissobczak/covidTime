library(tidyverse)
library(Metrics)
library(h2o)

set.seed(323)

# Initialize the session
h2o.init()

nfolds = 3

full_train = as.h2o(full_train)
train = as.h2o(train)
validation = as.h2o(validation)
test = as.h2o(test)


x = 	c(	names(train)[1:8],
		names(train)[11:ncol(train)]
	)

	# Training a nueral network
	NN_model = h2o.deeplearning(
		x = x,
	  	y = "duration",
	  	training_frame = full_train,
		nfolds = nfolds,
		keep_cross_validation_predictions = T,
	  	hidden = c(5,5)
	)

	RF_model = h2o.randomForest(
		x = x,
		y = "duration",
		nfolds = nfolds,
		training_frame = full_train,
		ntrees = 50
	)

	GBM_model = h2o.gbm(
		x = x,
		y = 'duration',
		training_frame = full_train,
		distribution = 'AUTO',
		ntrees = 20,
		max_depth = 3,
		nfolds = nfolds,
		keep_cross_validation_predictions = T,
		learn_rate = 0.2
	)

#	ensemble = h2o.stackedEnsemble(
#		x = x,
#		y = 'duration',
#		training_frame = full_train,
#		base_models = list(NN_model, RF_model, GBM_model)
#	)


	# Predicting ############################
	y = as.data.frame(validation)$duration

	# NeuralNetwork:
	NN_y_hat = h2o.predict(NN_model, validation)
	NN_y_hat_v = as.data.frame(NN_y_hat)$predict
	NN_perf_sum = h2o.performance(NN_model, newdata = validation)

	rmse_NN = rmse(y, NN_y_hat_v)
	perf_NN = h2o.performance(NN_model, newdata = validation)
	res_NN = y - NN_y_hat_v
	pdf('plots/predictions/NN_model.pdf')
		plot(	x = y, res_NN,
			xlab = 'Actual Duration',
			ylab = 'Model Error',
			main = 	'NN Model Performance'
		)
		abline(h = 0)
	dev.off()

	Id = as.data.frame(h2o.predict(NN_model, test))
	names(Id) = 'duration'
	write.table(Id, sep = ',', file = 'NN_model.txt', quote = F, row.names = T)

	# RandomForest:
	RF_y_hat = h2o.predict(RF_model, validation)
	RF_y_hat_v = as.data.frame(RF_y_hat)$predict
	RF_perf_sum = h2o.performance(RF_model, newdata = validation)
	y = as.data.frame(validation)$duration

	rmse_RF = rmse(y, RF_y_hat_v)
	perf_RF = h2o.performance(RF_model, newdata = validation)
	res_RF = y - RF_y_hat_v
	pdf('plots/predictions/RF_model.pdf')
		plot(	x = y, res_RF,
			xlab = 'Actual Duration',
			ylab = 'Model Error',
			main = 	'RF Model Performance'
		)
		abline(h = 0)
	dev.off()

	Id = as.data.frame(h2o.predict(RF_model, test))
	names(Id) = 'duration'
	write.table(Id, sep = ',', file = 'RF_model.txt', quote = F, row.names = T)



	# GBM:
	GBM_y_hat = h2o.predict(GBM_model, validation)
	GBM_y_hat_v = as.data.frame(GBM_y_hat)$predict
	GBM_perf_sum = h2o.performance(GBM_model, newdata = validation)
	y = as.data.frame(validation)$duration

	rmse_GBM = rmse(y, GBM_y_hat_v)
	perf_GBM = h2o.performance(GBM_model, newdata = validation)
	res_GBM = y - GBM_y_hat_v
	pdf('plots/predictions/GBM_model.pdf')
		plot(	x = y, res_GBM,
			xlab = 'Actual Duration',
			ylab = 'Model Error',
			main = 	'GBM Model Performance'
		)
		abline(h = 0)
	dev.off()

	Id = as.data.frame(h2o.predict(GBM_model, test))
	names(Id) = 'duration'
	write.table(Id, sep = ',', file = 'GBM_model.txt', quote = F, row.names = T)

#	# Ensemble:
#	E_y_hat = h2o.predict(E_model, validation)
#	E_y_hat_v = as.data.frame(E_y_hat)$predict
#	E_perf_sum = h2o.performance(E_model, newdata = validation)
#	y = as.data.frame(validation)$duration
#
#	rmse_E = rmse(y, E_y_hat_v)
#	perf_E = h2o.performance(E_model, newdata = validation)


## Making some predictions
#y = validation$duration
#i = which(names(validation) == 'duration')
#
#validation = validation[,-i]
#
#mod1 = lm(duration ~ age, train)
#y_hat1 = predict(mod1, validation)
#rmse1 = rmse(y, y_hat1)
#
#mod2 = lm(duration ~ age + age_missing + age_changed, train)
#y_hat2 = predict(mod2, validation)
#rmse2 = rmse(y, y_hat2)
#
#mod3 = lm(
#	duration ~ age,
#	train
#	)
#y_hat3 = predict(mod3, validation)
#rmse3 = rmse(y, y_hat3)
#
#
#
#Id = as.data.frame(predict(mod3, test))
#names(Id) = 'duration'
#write.table(Id, sep = ',', file = 'mod.txt', quote = F, row.names = F)




