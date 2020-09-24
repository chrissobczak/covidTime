library(tidyverse)
library(stringr)
library(Metrics)

# Importing the data
train = read.table('train2.txt', sep=',', header=T, row.names=NULL)
test = read.table('test2.txt', sep=',', header=T, row.names=NULL)

ind = sample(seq(1,nrow(train),1), .3*nrow(train), replace=F)
validation = train[ind, ]
train = train[-ind, ]


set.seed(12345)


UniqueSymptoms = function(symptoms){

	unique_symptoms = symptoms[which(!(symptoms == ''))]

	# Append the entire vector with the entry, but string split
	unique_symptoms = str_split(unique_symptoms, ';\\s')

	x = unlist(unique_symptoms)

	unique_symptoms = unique(x)[order(unique(x))]
	unique_symptoms = gsub('(\\s|\\s[[:punct:]])\\d.*$', '', unique_symptoms)
	unique_symptoms = gsub('(accute|pain|dry|low|ache|stiffness|congestion|severe|sore|Sore|systemic|distress|soreness|discomfort|symptoms|runny|of|shortness)', '', unique_symptoms)
	unique_symptoms = gsub('\\s', '', unique_symptoms)
	unique_symptoms[which(unique_symptoms == '')] = 'discomfort'
	
	return(unique_symptoms)
}


# Cleaning Process
Clean = function(df){
	
	# Add Column to Indicate Changed and Missing pts
	df$age_changed = numeric(length = nrow(df))
	df$age_missing = numeric(length = nrow(df))

	for(i in 1:nrow(df)){
		x = df$age[i]
		if(grepl('-', x, fixed=T)){
			z = strsplit(x, '-')
			x = mean(as.numeric(c(z[[1]][1], z[[1]][2])))
			df$age[i] = x
			df$age_changed[i] = 1
		}
	}

	df$age = as.numeric(df$age)
	df$age[which(is.na(df$age))] = mean(df$age, na.rm = T)
	df$age_missing[which(is.na(df$age))] = 1
	
	# Make a new table with the symptom information
	symptom_status = c('assymptomatic', UniqueSymptoms(train$symptoms))
	

	return(df)
}

train = Clean(train)
validation = Clean(validation)

test = Clean(test)




# Making some predictions
y = validation$duration
i = which(names(validation) == 'duration')

validation = validation[,-i]

mod1 = lm(duration ~ age, train)
y_hat1 = predict(mod1, validation)
rmse1 = rmse(y, y_hat1)

mod2 = lm(duration ~ age + age_missing + age_changed, train)
y_hat2 = predict(mod2, validation)
rmse2 = rmse(y, y_hat2)






