
        #assumptions
        #checking out the exploratory graphs for any visual evidence of a linear relationship

        offlabelys = drugx[,5] * drugy
        summ1   = 0
        summ2 = 0
        count1 = 0
        count2 = 0
        m=ncol(drugx)
        binaryfreqmeans=matrix(NA, 2,8)
        for (k in 5:m){
            for (i in 1:n){ 
                if (drugx[i,k] == 1){
                    summ1 =  summ1 + drugy[i,1] 
                    count1 = count1 + 1
                }        
                else{
                    summ2 = summ2 + drugy[i,1]
                    count2 = count2 + 1
                }
                
            }

            binaryfreqmeans[1,k-4] = summ1 / count1
            binaryfreqmeans[2,k-4] = summ2 / count2

            plot(drugx[,k], y, main=paste(colnames(drugx)[k],"x TotalUserScore"),
            xlab = "False  |  True", ylab = "Reported Total User Score",
            pch = 19, frame = FALSE)
            segments(0,binaryfreqmeans[1,k-4],1,binaryfreqmeans[2,k-4])

            summ1 = 0
            summ2 = 0
            count1 = 0
            count2 = 0
        }

    #original distribution looked log normal so taking log to check
    x1<-drug$Price
        summary(drug$Price)
        hist(log(drug$Price),main = "Frequency of Price",xlab = "Price in USD")
        drugpricehist<-subset(drug, Price < 1000)
        hist(log(drugpricehist$Price),main = "Frequency of Price (Cropped for Outliers)",xlab = "Price in USD")

        plot(x1, y, main = "Price x TotalUserScore",
            xlab = "Price in USD", ylab = "Reported Total User Score",
            pch = 19, frame = FALSE)

        plot(x1, y, main = "Price x TotalUserScore (Cropped for Outliers)",
            xlab = "Price in USD", xlim =c(1,1000), ylab = "Reported Total User Score",
            pch = 19, frame = FALSE)
    #calling it normal is likely a stretch but for the purposes of this project i will hold the assumption it is





    #dropping labels and calculating empirical b0...10
    drugx<-drugx[-c(1,3,4)]
    drugy=as.matrix(drugy)
    set.seed(0400)
    drugx = cbind( 1, drugx)
    drugx=as.matrix(drugx)
    beta = solve((t(drugx)%*%drugx)) %*% t(drugx) %*% drugy

 
    #overall ftest
    predict = function(hats,x,mu,sigma){
        n = nrow(x)
        m = nrow(hats)
        yhat = matrix(0,1,n)
        
        for (i in 1:n){
            for (k in 1:m){
            yhat[1,i] = yhat[1,i] + hats[k,1]*x[i,k]
            }
        }
        yhat = yhat +  rnorm( n, mu,sigma)
        return(yhat)
    }  
     
    druglm<-lm(drugy~drugx)
    sigma = sd(resid(druglm))
    mu = mean(resid(druglm))
    summary(druglm)
   
    yhat = predict(beta,drugx,mu,sigma)
    ssr=0
    for (i in 1:n){
        ssr = ssr + ((yhat[1,i] - mean(drugy))^2)
    }
    sse=0
    for (i in 1:n){
        sse = sse + ((drugy[i] - yhat[1,i])^2)
    }

    mse=sse/(n-m)
    msr=ssr/(m-1)

    f=msr/mse
    fpval<- 1-pf(f,m-1,n-m-2)
    #atleast 1 predictior is significant

    #confirm/ttests

    #manual ts


    #slightly different due to rounding. 
    #pvaules from ttests show cream tablet and otc to contain significance 

    #refitting model now
    drugx2<-drugx[,-c(2,3,4,6,7,10)]
    druglm<-lm(drugy~drugx2)
    sigma = sd(resid(druglm))
    mu = mean(resid(druglm))
    m=ncol(drugx2)
    plot(density(drugy))
    beta2<-beta[-c(2,3,4,6,7,10)]
