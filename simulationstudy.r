
    #gradient descent tracer function
    trace <- function(xstar, testy, bstar) {
        sum( (xstar %*% bstar - testy)^2 ) / (2*length(testy))
    }

    


    #training empirical data
    
    xstar<-drugx2
    testx<-xstar
    testy<-drugy
    
    a <- 0.01
    iters <- 1000
    ittrace=double(iters)
    bstartrace=list(iters)
    bstar=matrix(c(0,0), nrow=4)
    for (i in 1:iters) {
        error=(xstar %*% bstar - testy)
        delta=t(xstar) %*% error / length(testy)
        bstar=bstar - a * delta
        ittrace[i]=trace(xstar, testy, bstar)
        bstartrace[[i]] <- bstar
    }
    

    #plotting trace coverage for mle
    for (i in 2:m){
        plot(testx[,i],testy, col=rgb(0.2,0.4,0.6,0.4), main=paste("beta",i," gradient descent"))
        grid = seq( 0, 1, by=.001)

        for (z in c(1,3,6,10,14,seq(20,iters,by=10))) {
            abline(bstartrace[[z]][1],bstartrace[[z]][i], col="green")
        }
        abline(bstar[1],bstar[i], col='blue')

        plot(ittrace, type='l', col='green', lwd=2, main=paste('beta',i,' trace'),xlim=c(1,50), xlab='iteration')
    }
    sigma = sd(resid(druglm))
    mu = mean(resid(druglm))
    rnorm( n, mean=mu,sd=sigma)

    emppredict<-predict(bstar,drugx2,mu,sigma)
    squares=0
    for (i in 1:n){
        squares = squares + ((drugy[i] - emppredict[,i])^2)
    }
    emprmse = sqrt(squares/n)



    #synthetic data estimation study

    #calulating ps for dummy variables for synthetic reproduction
    p=matrix(NA, 1,m-1)
    for (k in 2:m){
        p[1,k-1]=sum(drugx2[,k]) / n
    }
    sims = 1000
    nS = 400
    sigma = sd(resid(druglm))
    mu = mean(resid(druglm))
    newx=matrix(NA, n,m-1)
    newx = cbind( 1, newx)
    betastars=matrix(NA, nS,m)
    rmses=matrix(NA, nS,m)
    for (t in 1:nS){
        newx=matrix(NA, n,m-1)
        newx = cbind( 1, newx)
        for (z in 2:m){
                newx[,z] = rbinom(n,1,p[1,z-1])
        }   
        newy = newx %*% beta2 + rnorm( n, mean=mu,sd=sigma)
        xstar = newx
        testy = newy
        #MLE for bstar
        a <- 0.01
        iters <- 1000
        ittrace=double(iters)
        bstartrace=list(iters)
        bstar=matrix(c(0,0), nrow=4)
        for (i in 1:iters) {
            error=(xstar %*% bstar - testy)
            delta=t(xstar) %*% error / length(testy)
            bstar=bstar - a * delta
            ittrace[i]=trace(xstar, testy, bstar)
            bstartrace[[i]] <- bstar
        }
        betastars[t,] = bstar
        symprd<-predict(bstar,drugx2,mu,sigma)
        squares=0
        for (i in 1:n){
            squares = squares + ((drugy[i] - symprd[,i])^2)
        }
        symrmse = sqrt(squares/n)

        rmses[t,] = symrmse
        paster = paste("Simulation Study:",t)
        print(paster)
    }
    
    bestsyn = min(rmses)








