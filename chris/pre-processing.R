library(tidyverse)
library(stringr)
library(Metrics)
set.seed(324)

# Importing the data
train = read.table('train2.txt', sep=',', header=T, row.names=NULL)
test = read.table('test2.txt', sep=',', header=T, row.names=NULL)

full_train = train

# Splitting Data into a validation set	
ind = sample(seq(1,nrow(train),1), .3*nrow(train), replace=F)
validation = train[ind, ]
train = train[-ind, ]


# Once a set of symptoms is decided, we should stick with a fixed list,
# and not use this function dynamically to set up the data sets ...

UniqueSymptoms = function(list_of_sets){

	all_symptoms = c()

	for(i in 1:length(list_of_sets)){
		unique_symptoms = 	filter(
						select(list_of_sets[[i]], symptoms), 
						symptoms != ''
					)
		unique_symptoms = as.character(unique_symptoms$symptoms)

		# Append the entire vector with the entry, but string split
		unique_symptoms = str_split(unique_symptoms, ';\\s')
		unique_symptoms = unlist(unique_symptoms)

		unique_symptoms = gsub('(\\s|\\s[[:punct:]])\\d.*$', '', unique_symptoms)
		
		unique_symptoms = str_split(unique_symptoms, '\\s')
		unique_symptoms = unlist(unique_symptoms)

		all_symptoms = append(all_symptoms, unique_symptoms)

		all_symptoms = all_symptoms[order(all_symptoms)]
	}	

	return(unique(all_symptoms))
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

	# Converting the catagorical data into factors
	df$sex 		= as.factor(df$sex)
	df$city 	= as.factor(df$city)
	df$province 	= as.factor(df$province)
	df$country	= as.factor(df$country)
	df$V1		= as.factor(df$V1)

	df$confirmed	= as.Date(df$confirmed, format = '%d.%m.%Y')
	
	# Spelling Corrections
	df$symptoms = gsub('diar[a-z]*\\>', 'diarrhea', df$symptoms)
	df$symptoms = gsub('breath[a-z]*\\>', 'breath', df$symptoms)
	df$symptoms = tolower(as.character(df$symptoms))

	return(df)
}

Symps = function(df, vector_of_unique_symptoms){

	# Make a new table with the symptom information

	# List of all the new columns we want
	symptom_status = c('empty', vector_of_unique_symptoms)
	
	# Intialize a table to contain the symptom information
	symptoms = 	matrix(	data = NA,
				ncol = length(symptom_status),
				nrow = nrow(df)
			)
	# Convert to a dataframe so we can change the data classes
	symptoms = 	as.data.frame(symptoms)
	# Change each column into a character vector
	for(i in 1:ncol(symptoms)){
		symptoms[,i] = as.character(symptoms[,i])
	}
	# Name the columns according to the symptoms_status ...
	names(symptoms)=symptom_status
	
	# Mark Observations without symptoms listed
	x = which(df$symptoms == '')
	symptoms[x,1] = 'empty'
	symptoms[-x,1] = 'symptoms'
	symptoms[,1] = as.factor(symptoms[,1])

	for(i in 2:ncol(symptoms)){
		
		x = 	which(	grepl(
					gsub(	'[a-z]{2}$', 
						'', 
						names(symptoms)[i]
					), 
					df$symptoms)
			)

		if(length(x > 0)){
			symptoms[-x,i] = 'absent' 
			symptoms[x,i] =		gsub(	
							';.*$',
							'',
						gsub(
							'^.*;',
							'',
						str_extract(	
							df$symptoms[x],
							paste(	'(^|;).*',
								gsub(
									paste(
									'[a-z]{',
									
									# Needs some work here?
									as.character(
									str_length( 
									names(symptoms)[i]) %/% 2
									),
									
									'}', sep = ''
									),
									'',
									names(symptoms)[i]
								),
								'.*($|;)', sep = ''
							)
						)
						)
						)
		} else{
			symptoms[1:nrow(df),i] = 'absent'
		}

		symptoms[,i] = as.factor(symptoms[,i])
	}

	return(symptoms)
}

full_train = Clean(full_train)

train = Clean(train)
validation = Clean(validation)
test = Clean(test)

train$log_y = log(train$duration)
full_train$log_y = log(full_train$duration)
validation$log_y = log(validation$duration)

################################################################################
### These write a .txt file with all the unique words in the symptoms column ###
### Only redo these lines if the datasets change			     ###
################################################################################
# u_symptoms = UniqueSymptoms(list(train, validation, test))
# write.table(u_symptoms, sep = ',', file = 'unique_symptoms.txt', quote = F, row.names = F)
u_symptoms = read.table('unique_symptoms.txt', sep=',', header=T, row.names=NULL)$x

# The Symps function still needs improvement ... verifying it's outputting what is expected
# Identify a specific entry in 'fever' and checking ...

full_train = cbind(full_train, Symps(full_train, u_symptoms))

train = cbind(train, Symps(train, u_symptoms))
validation = cbind(validation, Symps(validation, u_symptoms))
test = cbind(test, Symps(test, u_symptoms))

names(full_train)[which(names(full_train) == 'symptoms')[1]] = 'symptoms_org'
names(train)[which(names(train) == 'symptoms')[1]] = 'symptoms_org'
names(validation)[which(names(validation) == 'symptoms')[1]] = 'symptoms_org'
names(test)[which(names(test) == 'symptoms')[1]] = 'symptoms_org'
