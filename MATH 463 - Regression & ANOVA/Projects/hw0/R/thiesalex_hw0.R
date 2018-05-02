# create a url object
mus.url = '~/Documents/School/2.UO/16.03-Spring/MATH463/Homework/hw0/mus.txt'

# create the data frame
mus <- read.csv(
		mus.url, 
		header = TRUE, 
		sep = ','
		)

# plot it
plot(WEIGHT~AGE,
	data = mus,
	xlab = 'Age',
	ylab = 'Weight',
	pch = c(16,18)[as.numeric(LOCATION)],
	col = c('Blue', 'Green')[as.numeric(LOCATION)]
	)

# split up by location
mus.loc1 <- mus[1:35,]
mus.loc2 <- mus[36:65,]

# make the models
loc1.lm <- lm(WEIGHT~AGE, 
	data = mus.loc1
	)
loc2.lm <- lm(WEIGHT~AGE, 
	data = mus.loc2
	)

# check their summaries
summary(loc1.lm)
summary(loc2.lm)

# plot the lines
abline(-0.4828, 0.3649, col = 'Blue')
abline(-1.2420, 0.6549, col = 'Green')

# add a legend
legend('topleft', 
	inset=.05, 
	title='Least-Squares Lines',
	c('Location 1','Location 2'),
	fill = c('Blue', 'Green'),
	horiz=FALSE
  	)