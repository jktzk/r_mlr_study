#The main goal of this case study is serve as an excercise in coding and statistical knowledge. 
#No R packages or any functions beyond base R's scope were utilized.
#Several of R's existing functions may have been re-derived to allow the researcher to explore a deeper understanding of the given concepts and their theoritical implications
#The first line of code in #cleaning.r would need to be edited with the intended users correct path

    #The study attempts to fit a linear model for the given data set and to also derive conclusions on various estimated population parameters
    #Population parameters will estimated via the empirical and via samples obtain through bootstrapping

    #The set used is a real data set of user submitted reviews of several medications posted by individuals from xxxx to yyyy to webmd.com 
    #The data set was harvested from webmd.com and posted https://zenodo.org/record/3571494#.ZFv8jHbMJPZ
    #The initial clean of the file was obtained from https://www.kaggle.com/datasets/thedevastator/drug-performance-evaluation?select=Drug_clean.csv
    #Further cleaning of the file is documented and explained in the comments of the runfile

    #The variables in the data set are as follows:
    #Condition- (condition user received treatment for)
    #Drug- (name of drug used to treat specified condition)
    #EaseOfUse- (score 1-5 reflecting individual’s opinion on the experience of the administration process of the drug)
    #Effectiveness- (score 1-5 reflecting individual’s opinion on the effectiveness of the drug)
    #Satisfaction- (score 1-5 reflecting individual’s opinion on the drug overall)
    #Indication- (is there FDA approval for the treatment of the specified condition the individual took the drug for existing on the drug’s label)
    #Price- ($USD estimated price of drug)
    #Form- (how the drug is administered)
    #Type- (is the drug only accessible with a prescription, is it available over the counter without a prescription or both)
    #Reviews- (# submitted reviews for a given drug)
    #EOUAvg- (average individuals reported ease of use for all drugs reported for a given condition)
    #EffAvg- (average individuals reported effectiveness for all drugs reported for a given condition)
    #SatAvg- (average individuals reported satisfaction for all drugs reported for a given condition)
    #TotalUserScore- (sum of specified drug’s EOU, effectiveness and satisfaction user scores)

    #TotalUserScore was considered the only response variable
    
    #This iteration of the study will attempt to fit the following linear model:
    # Y = B0 + B1X1 .... B9X9 + e
    #The lack of interactions in this model is intentional and was done to keep computations  simple
    #The severe impact this decision has on the accuracy of the results of the study should go into consideration if any actionable conclusion were to be derived from the study.
