## A
	# reset environment
		rm(list=ls())

	# set seed
		seed <- 38703
		set.seed(seed)

	# create random data
		z <- rnorm(100)
		e <- rnorm(100)
		d <- rnorm(100)
		g <- rnorm(100)

	# run data through functions
		y <- 10*z + e
		x <- 8*z + d
		yi <- (80/65)*x + g

	# create model from which to make abline for plot 1
		lsline1 <- lm(y~x)
		summary(lsline1)

	# do it again for plot 2
		lsline2 <- lm(yi~x)
		summary(lsline2)

	# make the line and plot everything
		plot(y~x, pch = 16)
		abline(0.21392, 1.20955)

		plot(yi~x, pch = 18)
		abline(-0.04711, 1.22022)

## B
	## reset environment
		rm(list=ls())

	## load the data
		load(url("http://pages.uoregon.edu/dlevin/DATA/gala.R"))

	## a
		# create vectors
		spec <- gala$Species
		area <- gala$Area
		elev <- gala$Elevation
		near <- gala$Nearest
		scruz <- gala$Scruz
		adj <- gala$Adjacent

		# create lm
		gala.lm <- lm(spec~area+elev+near+scruz+adj, data = gala)
		summary(gala.lm)

	## b
		# create yhat and e
			yhat <- fitted(gala.lm)
			e <- residuals(gala.lm)

		# plot them
			plot(e~yhat, pch = 16)

	## c
		# make the design matrix
			X <- data.matrix(gala[3:7])
			gala.design <- cbind(rep(1,30),X)

		# make the y vector
			y <- gala[,1]
			y <- data.matrix(y)

		# make matrix from the normal equations
			gala.designT <- t(gala.design)
			N.gala <- (gala.designT %*% gala.design)
			N.galainv <- solve(N.gala)

		# compute and print b
			b <- N.galainv %*% gala.designT %*% y
			print(b)

## C
	# reset environment
		rm(list=ls())

	# make vectors for part e
		y <- matrix(c(2,6,7,8))
		one <- c(1,1,1,1)
		xstar <- c(-3/2, -1/2, 1/2, 3/2)

	# make the projection operator
		X <- matrix(c(one, xstar), ncol = 2)
		Xt <- t(X)
		N <- Xt %*% X
		Ninv <- solve(N)
		PiX <- X %*% Ninv %*% Xt

	# project y onto X
		Projy <- PiX %*% y

## D
	# reset environment
		rm(list=ls())

	# create vectors
		x1 <- c(1,1,1,1)
		x2 <- c(1,1,0,0)
		x3 <- c(1,1,1,0)
		x4 <- (3*x3 - 2*x2)
		y <- c(0,2,14,1)

	# create matrices
		V0 <- matrix(x4)
		V <- matrix(c(x1, x2, x3), ncol = 3)

	# create factors of hat matrices, the name N is in reference to the normal equations
		V0t <- t(V0)
		N0 <- V0t %*% V0
		N0inv <- solve(N0)

		Vt <- t(V)
		N <- Vt %*% V
		Ninv <- solve(N)

	# compute projection operators
		PiV0 <- V0 %*% N0inv %*% V0t
		PiV <- V %*% Ninv %*% Vt
		PiV1 <- PiV - PiV0

	# project y onto V0 and V
		projY0 <- PiV0 %*% y
		projY1 <- PiV1 %*% y
		projY <- PiV %*% y

	# print results
		print(PiV0)
		print(PiV)
		print(PiV1)

		print(projY0)
		print(projY1)
		print(projY)