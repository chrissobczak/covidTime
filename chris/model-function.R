library(h2o)
h2o.init()

Model = function(df, x, folds, trees, layers, seed, y){
	
	NN_model = h2o.deeplearning(
		x = x,
	  	y = y,
	  	training_frame = df,
		nfolds = folds,
		keep_cross_validation_predictions = T,
		seed = seed,
	  	hidden = layers
	)

	RF_model = h2o.randomForest(
		x = x,
		y = y,
		nfolds = folds,
		training_frame = df,
		keep_cross_validation_predictions = T,
		seed = seed,
		ntrees = trees
	)

	GBM_model = h2o.gbm(
		x = x,
		y = y,
		training_frame = df,
		distribution = 'AUTO',
		seed = seed,
		ntrees = trees,
		max_depth = 3,
		nfolds = folds,
		keep_cross_validation_predictions = T,
		learn_rate = 0.2
	)
	
	GLM_model = h2o.glm(
		x = x,
		y = y,
		training_frame = df,
		keep_cross_validation_predictions = T,
		seed = seed,
		nfolds = folds,
		family = "AUTO"
	)

	E_model = h2o.stackedEnsemble(
		x = x,
		y = y,
		training_frame = df,
		seed = seed,
		base_models = list(NN_model, RF_model, GBM_model, GLM_model)
	)
	
	models = list(NN_model, RF_model, GBM_model, GLM_model, E_model)
	names(models) = c('NN_model', 'RF_model', 'GBM_model', 'GLM_model', 'E_model')
	return(models)
}


