# Homework 2
# Alex Thies
# Math 463; Spring 2017
# Professor David Levin

# clear environment & set seed
    rm(list = ls())
    set.seed(60735)

####Problem 1####
	# load data
		yule <- read.csv(file = '../data/yule.txt', header = TRUE, sep = " ")
		X <- yule - 100

	# create vectors
		y <- data.matrix(X$paup)
		x1 <- data.matrix(X$out)
		x2 <- data.matrix(X$old)
		x3 <- data.matrix(X$pop)

	# make a linear model
		X.lm <- lm(y~x1+x2+x3)
		summary(X.lm)

	# make residuals and fitted values
		e <- data.matrix(residuals(X.lm))
		yhat <- data.matrix(fitted(X.lm))

	# plot residuals against fitted valuesl
		plot(e~yhat, pch = 19, main = "Scatter plot of residuals against fitted values", xlab = "Fitted values", ylab = "Residuals")

####Problem 2####
    # My goal for this problem was to make a function for the simulations so that one could 
    # easily experiment with different hypotheses, variances, etc., but that didn't go as 
    # well as I had hoped. Instead, I made this sorry excuse for a function, which offers the 
    # convenience of easily changing parameters, at the expense of having to run the same block  
    # of code over and over. I only inlcude three iterations of the block of code to explicitly 
    # detail what I did to arrive at my answers.

		####Sim1####
        # Set coefficients, standard deviation, and desired number of simulations
        a <- -40
        b <- 0
        c <- 0.2
        d <- -0.3
        sd <- 15
        R <- 1000

        # create empty vectors, going to fill them with the for-loop
        tstat <- 1:R
        bhat <- 1:R
        SE <- 1:R

        # use a for-loop to populate the vectors
        for(i in 1:R){
            # generate error vector
            ep <- rnorm(n = 32, mean = 0, sd = sd)

            # make model
            y <- a + b*x1 + c*x2 + d*x3 + ep
            M <- lm(y~x1+x2+x3)

            # store vector values
            bhat[i] <- summary(M)$coef[2,1]
            SE[i] <- summary(M)$coef[2,2]
            tstat[i] <- summary(M)$coef[2,3]
        }

        # compute sample mean, sd, and true sd of bhat
        mu_bhat <- mean(bhat)
        SD_bhat <- sd(bhat)

        # compute true SD
        Xm <- model.matrix(M)
        Xvar <- t(Xm)%*%Xm
        SDt_bhat <- ((sd**2)*solve(Xvar)[2,2])**0.5
        
        # compute sample mean and sd of stat
        mu_tsat <- mean(tstat)
        SD_tsat <- sd(tstat)

        # compute sample mean and sd of SE
        mu_SE <- mean(SE)
        SD_SE <- sd(SE)

        # make b histogram using sample parameters
        hist(bhat, prob = T, main = "Sim 1 - Histogram and Distribution of b", xlab = "b", ylab = "Density")
        w <- seq(min(bhat),max(bhat), length.out = R)
        z <- dnorm(w, mu_bhat, SD_bhat)
        lines(w,z)    

        # make t histogram
        hist(tstat, prob = TRUE, main = "Sim 1 - Histogram and Distribution of t statistics", xlab = "t statistics", ylab = "Density")
        p <- seq(min(tstat), max(tstat), length.out = R)
        q <- dt(p,df = 28)
        lines(p,q)

        # make scatter plot
        plot(SE~bhat, pch = 16, main = "Sim 1 - Plot of SE,bhat pairs")

        # print results
        cat("S1: Sample mean of bhat:", mu_bhat)
        cat("S1: Sample SD of bhat:", SD_bhat)
        cat("S1: True SD of bhat:", SDt_bhat)
        cat("S1: Sample mean of tstat:", mu_tsat)
        cat("S1: Sample SD of tstat:", SD_tsat)
        cat("S1: Sample mean of SE:", mu_SE)
        cat("S1: Sample SD of SE:", SD_SE)

    ####Sim2####

    # What happens if we change some parameters?
        # Clear old stuff
        rm(a,b,c,d,sd,R,tstat,bhat,SE,ep,M,mu_bhat,SD_bhat,SDt_bhat,mu_tsat,SD_tsat,mu_SE,SD_SE,w,z,p,q)
        
        # Set new stuff
        a <- 10
        b <- 0
        c <- 0.1
        d <- -0.5
        sd <- 25
        R <- 1000
        
        # create empty vectors, going to fill them with the for-loop
        tstat <- 1:R
        bhat <- 1:R
        SE <- 1:R

        # use a for-loop to populate the vectors
        for(i in 1:R){
            # generate error vector
            ep <- rnorm(n = 32, mean = 0, sd = sd)

            # make model
            y <- a + b*x1 + c*x2 + d*x3 + ep
            M <- lm(y~x1+x2+x3)

            # store vector values
            bhat[i] <- summary(M)$coef[2,1]
            SE[i] <- summary(M)$coef[2,2]
            tstat[i] <- summary(M)$coef[2,3]
        }

        # compute sample mean, sd, and true sd of bhat
        mu_bhat <- mean(bhat)
        SD_bhat <- sd(bhat)

        # compute true SD
        Xm <- model.matrix(M)
        Xvar <- t(Xm)%*%Xm
        SDt_bhat <- ((sd**2)*solve(Xvar)[2,2])**0.5

        # compute sample mean and sd of stat
        mu_tsat <- mean(tstat)
        SD_tsat <- sd(tstat)

        # compute sample mean and sd of SE
        mu_SE <- mean(SE)
        SD_SE <- sd(SE)
        
        # print results
        cat("S2: Sample mean of bhat:", mu_bhat)
        cat("S2: Sample SD of bhat:", SD_bhat)
        cat("S2: True SD of bhat:", SDt_bhat)
        cat("S2: Sample mean of tstat:", mu_tsat)
        cat("S2: Sample SD of tstat:", SD_tsat)
        cat("S2: Sample mean of SE:", mu_SE)
        cat("S2: Sample SD of SE:", SD_SE)

    ####Sim3####

    # Try an alternative hypothesis, that b neq 0.
        # Clear old stuff
        rm(a,b,c,d,sd,R,tstat,bhat,SE,ep,M,mu_bhat,SD_bhat,SDt_bhat,mu_tsat,SD_tsat)

        # Set new stuff
        a <- -40
        b <- 0.5
        c <- 0.2
        d <- -0.3
        sd <- 15
        R <- 1000

        # create empty vectors, going to fill them with the for-loop
        tstat <- 1:R
        bhat <- 1:R
        SE <- 1:R

        # use a for-loop to populate the vectors
        for(i in 1:R){
            # generate error vector
            ep <- rnorm(32,sd)

            # make model
            y <- a + b*x1 + c*x2 + d*x3 + ep
            M <- lm(y~x1+x2+x3)

            # store vector values
            bhat[i] <- summary(M)$coef[2,1]
            SE[i] <- summary(M)$coef[2,2]
            tstat[i] <- summary(M)$coef[2,3]
        }

        # compute sample mean, sd, and true sd of bhat
        mu_bhat <- mean(bhat)
        SD_bhat <- sd(bhat)

        # compute true SD
        Xm <- model.matrix(M)
        Xvar <- t(Xm)%*%Xm
        SDt_bhat <- ((sd**2)*solve(Xvar)[2,2])**0.5

        # compute sample mean and sd of stat
        mu_tsat <- mean(tstat)
        SD_tsat <- sd(tstat)

        # compute sample mean and sd of SE
        mu_SE <- mean(SE)
        SD_SE <- sd(SE)
        
        # print results
        cat("S3: Sample mean of bhat:", mu_bhat)
        cat("S3: Sample SD of bhat:", SD_bhat)
        cat("S3: True SD of bhat:", SDt_bhat)
        cat("S3: Sample mean of tstat:", mu_tsat)
        cat("S3: Sample SD of tstat:", SD_tsat)
        cat("S3: Sample mean of SE:", mu_SE)
        cat("S3: Sample SD of SE:", SD_SE)

    ####Sim4####
        
    # What if the errors aren't normally distributed? (Using the same a and sd as in the previous simulation)
        # Clear old stuff
        rm(a,b,c,d,sd,R,tstat,bhat,SE,ep,M,mu_bhat,SD_bhat,SDt_bhat,mu_tsat,SD_tsat)
        
        # Set new stuff
        a <- -40
        b <- 0
        c <- 0.2
        d <- -0.3
        sd <- 15
        R <- 1000

        # create empty vectors, going to fill them with the for-loop
        tstat <- 1:R
        bhat <- 1:R
        SE <- 1:R
        
        # use a for-loop to make b's, SE's, and tstat's
        for(i in 1:R){
            # generate error vector
            ep <- rchisq(32,5)
            ep <- sd*((ep - 5)/sqrt(10))
            
            # make model
            y <- a + b*x1 + c*x2 + d*x3 + ep
            M <- lm(y~x1+x2+x3)
            
            # store vector values
            bhat[i] <- summary(M)$coef[2,1]
            SE[i] <- summary(M)$coef[2,2]
            tstat[i] <- summary(M)$coef[2,3]
        }

        # compute sample mean, sd, and true sd of bhat
        mu_bhat <- mean(bhat)
        SD_bhat <- sd(bhat)

        # compute true SD
        Xm <- model.matrix(M)
        Xvar <- t(Xm)%*%Xm
        SDt_bhat <- ((sd**2)*solve(Xvar)[2,2])**0.5

        # compute sample mean and sd of stat
        mu_tsat <- mean(tstat)
        SD_tsat <- sd(tstat)

        # compute sample mean and sd of SE
        mu_SE <- mean(SE)
        SD_SE <- sd(SE)
        
        # print results
        cat("S4: Sample mean of bhat:", mu_bhat)
        cat("S4: Sample SD of bhat:", SD_bhat)
        cat("S4: True SD of bhat:", SDt_bhat)
        cat("S4: Sample mean of tstat:", mu_tsat)
        cat("S4: Sample SD of tstat:", SD_tsat)
        cat("S4: Sample mean of SE:", mu_SE)
        cat("S4: Sample SD of SE:", SD_SE)

####Problem 3####
        
    # make design matrices
        X0 <- matrix(c(1,1,1,0,0,0,0,0,0,0,0,0,  0,0,0,1,1,1,0,0,0,0,0,0, 0,0,0,0,0,0,1,1,1,0,0,0, 0,0,0,0,0,0,0,0,0,1,1,1), ncol=4)
        X1 <- matrix(c(1,0,0,0,1,0,0,0,1,0,1,1, 0,1,0,0,-1,0,0,-1,1,1,0,0, 0,0,1,0,0,1,1,0,0,0,0,0, 0,0,0,1,0,-1,1,1,0,1,-1,1), ncol = 4)
    # compute variance matrices
        vX0 <- solve(t(X0) %*% X0)
        vX1 <- solve(t(X1) %*% X1)
        
    # print variance matrices
        print(vX0)
        print(vX1)