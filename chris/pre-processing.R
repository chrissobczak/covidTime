library(tidyverse)

train = read.table('train2.txt', sep=',', header=T, row.names=NULL)
test = read.table('test2.txt', sep=',', header=T, row.names=NULL)

for(i in 1:nrow(train)){
	x = train$age[i]
	if(grepl('-', x, fixed=T)){
		z = strsplit(x, '-')
		x = mean(as.numeric(c(z[[1]][1], z[[1]][2])))
		train$age[i] = x
	}
}

train$age = as.numeric(train$age)

png('age_hist1.png')
	hist(	train$age, 
		main = 'Age Distribution of Our Dataset',
		xlab = 'Age',
		ylab = 'Frequency',
	)
dev.off()

