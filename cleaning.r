

    #Data cleaning
        #reading in raw

        #drug<-read.csv("C:/Users/____/Documents/DataSets/Drug_clean.csv")

        #observations containing missing entries are removed see^2
        n<-nrow(drug)
        m<-ncol(drug)
        for (k in 1:m){
            for (i in 1:n){
                if (is.na(drug[i,k])){
                    drug<-drug[-c(i),]
                }
                else if (drug[i,k] == "\n"){
                    drug<-drug[-c(i),]
                }
            }
        }
        

        #renaming variables with long names and adjusting capitalization, making any graphics easier to read
        n<-nrow(drug)
        for (k in 1:n){
            if (grepl('Bacterial Urinary Tract Infection',drug[k,1])){
                drug[k,1]<-'Bacterial UTI'
            }
        }

        for (k in 1:n){
            if (grepl('diverticulitis of gastrointestinal tract',drug[k,1])){
                drug[k,1]<-'Diverticulitis'
            }
        }

        for (k in 1:n){
            if (grepl('gastroesophageal reflux disease',drug[k,1])){
                drug[k,1]<-'GERD'
            }
        }

        for (k in 1:n){
            if (grepl('Pharyngitis due to Streptococcus Pyogenes',drug[k,1])){
                drug[k,1]<-'Strep throat'
            }
        }

        for (k in 1:n){
            if (grepl('prevention of cerebrovascular accident',drug[k,1])){
                drug[k,1]<-'Stroke Prevention'
            }
        }

        for (k in 1:n){
            if (grepl('Sleepiness Due To Obstructive Sleep Apnea',drug[k,1])){
                drug[k,1]<-'Sleep Apnea'
            }
        }
        drug[,1]<-tools::toTitleCase(drug[,1])


        #creating variables with values assigned as the mean satisfaction, ease of use or effectiveness responses for all drugs across a given condition
        #these were never actually used but could prove to be useful for any future observation
        SatAvg<-vector("numeric",n)
        drug<-cbind( drug, SatAvg)
        count = 0
        m = 0
        k=0
        summ = 0
        start = 1
        for (k in 1:n){
            m = k + 1
            name = drug[k,1]
            if (!is.na(drug[m,1]) & name == drug[m,1] | name == "Colorectal Cancer" | name == "Triamterene"){
                summ = drug[k,9] + summ
                k = k + 1
            }
            else{
                summ = drug[k,9] + summ
                z = 0 
                for (z in start:k){
                    drug[z,11] = summ / (k - start + 1)
                    z = z + 1
                }
                if (k == 672){
                    break
                }
                else{
                k = k + 1
                start = k
                summ = 0
                }
            }

        }

        EoUAvg<-vector("numeric",n)
        drug<-cbind( drug,EoUAvg)
        count = 0
        m = 0
        k=0
        summ = 0
        start = 1
        for (k in 1:n){
            m = k + 1
            name = drug[k,1]
            if (!is.na(drug[m,1]) & name == drug[m,1] | name == "Colorectal Cancer" | name == "Triamterene" ){
                summ = drug[k,3] + summ
                k = k + 1
            }
            else{
                summ = drug[k,3] + summ
                z = 0 
                for (z in start:k){
                    drug[z,12] = summ / (k - start + 1)
                    z = z + 1
                }
                count = count + 1
                if (k == 672){
                    break
                }
                else{
                k = k + 1
                start = k
                summ = 0
                }
            }

        }

        EffAvg<-vector("numeric",n)
        drug<-cbind( drug,EffAvg)
        count = 0
        m = 0
        k=0
        summ = 0
        start = 1
        for (k in 1:n){
            m = k + 1
            name = drug[k,1]
            if (!is.na(drug[m,1]) & name == drug[m,1] | name == "Colorectal Cancer" | name == "Triamterene"){
                summ = drug[k,4] + summ
                k = k + 1
            }
            else{
                summ = drug[k,4] + summ
                z = 0 
                for (z in start:k){
                    drug[z,13] = summ / (k - start + 1)
                    z = z + 1
                }
                count = count + 1
                if (k == 672){
                    break
                }
                else{
                k = k + 1
                start = k
                summ = 0
                }
            }
        }

        #total score of all three individual scores for a given drug
        TotalUserScore<-vector("numeric",n)
        drug<-cbind( drug,TotalUserScore)
        k=0
        start = 1
        for (k in 1:n){
            drug[k,14] = (drug[k,3] + drug[k,4] + drug[k,9])
        }


        #initial variable exploration:

        y<-drug$TotalUserScore
        summary(drug$TotalUserScore)
        hist(drug$TotalUserScore,main = "Frequency of Total User Score",xlab = 'Reported Satisfaction')

        #the factor levels had to be split into seperate chunks to make the graphs more visible
        drugcondition<-drug$Condition
        table(unlist(drug$Condition))
        drugbox1<-subset(drug, Condition=='Acute Bacterial Sinusitis' | Condition=='Adenocarcinoma of Pancreas' |
                            Condition=='Atopic Dermatitis' | Condition=='Back Pain' |
                            Condition=='Bacterial Conjunctivitis') 
        drugbox2<-subset(drug, Condition=='Bacterial UTI' | Condition=='Biliary Calculus' |
                            Condition=='Chickenpox' | Condition=='Colorectal Cancer' |
                            Condition=='Depression' | Condition=='Edema')
        drugbox3<-subset(drug, Condition=='Endometriosis' | Condition=='Fever' |
                            Condition=='Fibromyalgia' | Condition=='Flatulence' |
                            Condition=='Furunculosis' | Condition=='Genital Herpes Simplex')
        drugbox4<-subset(drug, Condition=='GERD' | Condition=='Gout' |
                            Condition=='Hemorrhoids' | Condition=='Herpes Zoster' |
                            Condition=='Hypercholesterolemia' | Condition=='Hypertension')
        drugbox5<-subset(drug, Condition=='Impetigo' | Condition=='Infantile Autism' |
                            Condition=='Influenza' | Condition=='Meniere\'s Disease' |
                            Condition=='Oral Candidiasis' | Condition=='Pyelonephritis')
        drugbox6<-subset(drug, Condition=='Scabies' | Condition=='Sleep Apnea' |
                            Condition=='Sore Throat' | Condition=='Strep Throat' |
                            Condition=='Stroke Prevention' | Condition=='Vertigo' |Condition=='Vulvovaginal Candidiasis')      

        par(cex.axis=.7)               
        boxplot(TotalUserScore ~ Condition, data = drugbox1, frame = TRUE,main = "Treated Condition (1/6) x TotalUserScore",xlab = "Medical Condition", ylab = "Reported Total User Score")
        boxplot(TotalUserScore ~ Condition, data = drugbox2, frame = TRUE,main = "Treated Condition (2/6) x TotalUserScore",xlab = "Medical Condition", ylab = "Reported Total User Score")
        boxplot(TotalUserScore ~ Condition, data = drugbox3, frame = TRUE,main = "Treated Condition (3/6) x TotalUserScore",xlab = "Medical Condition", ylab = "Reported Total User Score")
        boxplot(TotalUserScore ~ Condition, data = drugbox4, frame = TRUE,main = "Treated Condition (4/6) x TotalUserScore",xlab = "Medical Condition", ylab = "Reported Total User Score")
        boxplot(TotalUserScore ~ Condition, data = drugbox5, frame = TRUE,main = "Treated Condition (5/6) x TotalUserScore",xlab = "Medical Condition", ylab = "Reported Total User Score")
        boxplot(TotalUserScore ~ Condition, data = drugbox6, frame = TRUE,main = "Treated Condition (6/6) x TotalUserScore",xlab = "Medical Condition", ylab = "Reported Total User Score")
        par(cex.axis=1)     

            

        x1<-drug$Price
        summary(drug$Price)
        hist(drug$Price,main = "Frequency of Price",xlab = "Price in USD")
        drugpricehist<-subset(drug, Price < 300)
        hist(drugpricehist$Price,main = "Frequency of Price (Cropped for Outliers)",xlab = "Price in USD")

        plot(x1, y, main = "Price x TotalUserScore",
            xlab = "Price in USD", ylab = "Reported Total User Score",
            pch = 19, frame = FALSE)

        plot(x1, y, main = "Price x TotalUserScore (Cropped for Outliers)",
            xlab = "Price in USD", xlim =c(1,300), ylab = "Reported Total User Score",
            pch = 19, frame = FALSE)

        

        x2<-drug$Indication
        table(unlist(drug$Indication))
        boxplot(Satisfaction ~ Indication, data = drug, frame = TRUE,main = "FDA Approval for Condition x TotalUserScore",xlab = "Form", ylab = "Reported Total User Score")
                                
                                            

        x3<-drug$Form
        table(unlist(drug$Form))
        boxplot(Satisfaction ~ Form, data = drug, frame = TRUE,main = "Drug Form x TotalUserScore",xlab = "Method of Delivery", ylab = "Reported Total User Score")


        x4<-drug$Type
        table(unlist(drug$Type))
        boxplot(Satisfaction ~ Type, data = drug, frame = TRUE,main = "Means of Acquisition x TotalUserScore",xlab = "Form", ylab = "Reported Total User Score")

    
        #re-organizing column positions to be consistent with model variable assignments
        a = drug[,9]
        b = drug[,5]
        c = drug[,8]
        d = drug[,10]
        e = drug[,11]
        f = drug[,12]
        g = drug[,13]

        drug[,5]  = a
        drug[,8]  = b 
        drug[,10] = c 
        drug[,9]  = d 
        drug[,11] = f
        drug[,12] = g
        drug[,13] = e
        colnames(drug)[4]  = c("Effectiveness")
        colnames(drug)[5]  = c("Satisfaction")
        colnames(drug)[8]  = c("Form")
        colnames(drug)[10] = c("Reviews")
        colnames(drug)[9]  = c("Type")
        colnames(drug)[11] = c("EOUAvg")
        colnames(drug)[12] = c("EffAvg")
        colnames(drug)[13] = c("SatAvg")



        #creating sub sets based on factors used in model consideration and the response
        drugx<-drug[c(6,7,8,9)]
        drugy<-drug[c(14)]

        #dummy variables are created for the qualitative farctors and fixed to the newly created drugx dataset
        OffLabel<-vector("numeric",n)
        Capsule<-vector("numeric",n)
        Cream<-vector("numeric",n)
        Drink<-vector("numeric",n)
        Injection<-vector("numeric",n)
        Tablet<-vector("numeric",n)
        OTC<-vector("numeric",n)
        RX<-vector("numeric",n)
        drugx<-cbind( drugx,OffLabel,Capsule,Cream,Drink,Injection,Tablet
                        ,OTC,RX)

        #assigning binary values to dummy variables
        #when drug[]
        for (k in 1:n){
            for (i in 1:length(drug)){
                if(drug[k,i] ==  "Off Label"){
                    drugx[k,5] = 1
                }
                if(drug[k,i] ==  "Capsule"){
                    drugx[k,6] = 1
                }
                if(drug[k,i] ==  "Cream"){
                    drugx[k,7] = 1
                }
                if(drug[k,i] ==  "Liquid (Drink)"){
                    drugx[k,8] = 1
                }
                if(drug[k,i] ==  "Liquid (Inject)"){
                    drugx[k,9] = 1
                }
                if(drug[k,i] ==  "Tablet"){
                    drugx[k,10] = 1
                }
                if(drug[k,i] ==  "OTC"){
                    drugx[k,11] = 1
                }    
                if(drug[k,i] ==  "RX"){
                    drugx[k,12] = 1
                }    
            }   
        }
