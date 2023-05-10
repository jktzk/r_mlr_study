
    nBoot = 1000
    nS = 400
    bootBeta=matrix(NA, nBoot,10)
    gradbootBeta=matrix(NA, nBoot,1)
    bootmean = rep( NA, nBoot)

    m = 400
    totalsims = 100

    nominal_coverage = seq( .01, .99, by=.01)
    coverage = rep( 0, length(nominal_coverage))
    for(j in 1:totalsims){
        for(k in 1:nBoot){
            sub_index = sample( 1:n, size=m, replace=T)
            testx = drugx2[sub_index]
            xstar = cbind(1, matrix(testx))
            testy=drugy[sub_index]

            a <- 0.01
            iters <- 1000
            ittrace=double(iters)
            bstartrace=list(iters)
            bstar=matrix(c(0,0), nrow=2)

            for (i in 1:iters) {
                error=(xstar %*% bstar - testy)
                delta=t(xstar) %*% error / length(testy)
                bstar=bstar - a * delta
                ittrace[i]=trace(xstar, testy, bstar)
                bstartrace[[i]] <- bstar
            }
            gradbootBeta[k]=bstar[2]
        }
        paster = paste("Bootstrap simulation Study:",k)
        print(paster)
        for(r in 1:length(nominal_coverage)){
        alpha = 1-nominal_coverage[r]
        lower = quantile( gradbootBeta, probs=alpha/2)
        upper = quantile( gradbootBeta, probs=1-alpha/2)
        if(lower < betahat[9] & betahat[9] < upper) coverage[r] = coverage[r] + 1
    }

    cat("sim#",j,"/ 100\n")
    }
    coverage = coverage / totalsims


