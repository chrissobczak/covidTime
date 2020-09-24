library(tidyverse)
library(stringr)
library(Metrics)
set.seed(324)

# Importing the data
train = read.table('train2.txt', sep=',', header=T, row.names=NULL)
test = read.table('test2.txt', sep=',', header=T, row.names=NULL)

# Splitting Data into a validation set	
ind = sample(seq(1,nrow(train),1), .3*nrow(train), replace=F)
validation = train[ind, ]
train = train[-ind, ]


# Once a set of symptoms is decided, we should stick with a fixed list,
# and not use this function dynamically to set up the data sets ...

UniqueSymptoms = function(symptoms){

	unique_symptoms = symptoms[which(!(symptoms == ''))]

	# Append the entire vector with the entry, but string split
	unique_symptoms = str_split(unique_symptoms, ';\\s')

	x = unlist(unique_symptoms)

	unique_symptoms = unique(x)
	unique_symptoms = gsub('(\\s|\\s[[:punct:]])\\d.*$', '', unique_symptoms)
	unique_symptoms = gsub('(accute|pain|dry|low|ache|stiffness|congestion|severe|sore|Sore|systemic|distress|soreness|discomfort|symptoms|runny|of|shortness)', '', unique_symptoms)
	unique_symptoms = gsub('\\s', '', unique_symptoms)
	unique_symptoms[which(unique_symptoms == '')] = 'discomfort'
	x = unique(unique_symptoms)
	unique_symptoms = x[order(x)]
	
	return(unique_symptoms)
}

# Some notes about symptoms:
#	What are the differences between pneumonia and pneumonitis ...
#	Myalgia v. malaise ...
#	Some Spelling Inconsistency


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

	# Converting the catagorical data into factors
	df$sex 		= as.factor(df$sex)
	df$city 	= as.factor(df$city)
	df$province 	= as.factor(df$province)
	df$country	= as.factor(df$country)
	df$V1		= as.factor(df$V1)

	df$confirmed	= as.Date(df$confirmed, format = '%d.%m.%Y')
	
	# Spelling Corrections
	df$symptoms = gsub('diarrheoa', 'diarrhea', df$symptoms)
	df$symptoms = tolower(as.character(df$symptoms))

	return(df)
}

Symps = function(df){

	# Make a new table with the symptom information

	# List of all the new columns we want
	symptom_status = c('assymptomatic', UniqueSymptoms(df$symptoms))
	
	# Intialize dataset to contain the symptom information
	symptoms = 	matrix(	data = NA,
				ncol = length(symptom_status),
				nrow = nrow(df)
			)
	symptoms = 	as.data.frame(symptoms)
	for(i in 1:ncol(symptoms)){
		symptoms[,i] = as.character(symptoms[,i])
	}
	names(symptoms)=symptom_status
	
	# Mark Assymptomatic Observations
	x = which(df$symptoms == '')
	symptoms[x,1] = 'assymptomatic'
	symptoms[-x,1] = 'symptomatic'

# This empty v. not empty symptom column is pretty important ...
# Maybe need to change the names, reread the competition descriptions

#	# Marking Breathing Problems
#	x = which(grepl('breath',df$symptoms))
#	symptoms[x,2] = gsub('brea.*(;|$)', '', df$symptoms)
#
#	for(i in 2:ncol(symptoms)){
#		
#		x = which(grepl(gsub('[a-z]{2}$', '', names(symptoms)), df$symptoms))
#		symptoms[x,i] =	gsub(
#					gsub('[a-z]{2}$', '', names(symptoms)),
#					'', df$symptoms[j]
#				)
#	}



	return(symptoms)

}

train = cbind(Clean(train), Symps(train))
validation = cbind(Clean(validation), Symps(validation))

test = cbind(Clean(test), Symps(test))




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

mod3 = lm(
	duration ~ age + assymptomatic,
	train
	)
y_hat3 = predict(mod3, validation)
rmse3 = rmse(y, y_hat3)



Id = as.data.frame(predict(mod3, test))
names(Id) = 'duration'
write.table(Id, sep = ',', file = 'mod.txt', quote = F)




