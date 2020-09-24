## Meant to be run after 'pre-processing.R'
#
##pdf('age_hist.pdf')
#p1 = ggplot(	mapping = aes(age)) +
#	geom_density(	data = train, 
#			aes(fill = 'red', alpha = 0.3)
#	) +
#	geom_density(	data = validation, 
#			aes(fill = 'blue', alpha = 0.3)
#	)
##dev.off()
#
#p2 = ggplot(train) +
#	geom_point(aes(x = age, y = duration))
#	


p = ggplot() +
	geom_density(	data = train, 
			aes(x = duration[ind,], fill = 'red', alpha = 0.3)
	) +
	geom_density(	data = train, 
			aes(x = duration[-ind,], fill = 'blue', alpha = 0.3)
	)
