library(mice)
library(plyr)
library(readr)

load_titanic_data = function() {
    print("Loading Running")
    train_file = "train.csv"
    train_ds = read.csv(train_file, stringsAsFactors = FALSE)
    set.seed(50)
    
    train_ds$Fsize = train_ds$SibSp + train_ds$Parch + 1
    
    train_ds$FsizeD[train_ds$Fsize == 1] <- 'singleton'
    train_ds$FsizeD[train_ds$Fsize < 5 & train_ds$Fsize > 1] <- 'small'
    train_ds$FsizeD[train_ds$Fsize > 4] <- 'large'
    
    embarked.na = which(train_ds$Embarked == '')
    train_ds$Embarked[embarked.na] =
        names(which.max(summary(as.factor(
            train_ds$Embarked
        ))))
    train_ds$Embarked = factor(train_ds$Embarked)
    
    fares.na = which(is.na(train_ds$Fare))
    train_ds$Fare[fares.na] = mean(train_ds$Fare, na.rm = TRUE)
    
    train_ds$Salutation = gsub("(.*, )|(\\..*)", '', train_ds$Name)
    
    other = c(
        'Dona',
        'Lady',
        'the Countess',
        'Capt',
        'Col',
        'Don',
        'Dr',
        'Major',
        'Rev',
        'Sir',
        'Jonkheer'
    )
    
    train_ds$Salutation[train_ds$Salutation == 'Mlle']   = 'Miss'
    train_ds$Salutation[train_ds$Salutation == 'Ms']     = 'Miss'
    train_ds$Salutation[train_ds$Salutation == 'Mme']    =  'Mrs'
    train_ds$Salutation[train_ds$Salutation %in% other]  = 'Other'
    train_ds$Salutation = factor(train_ds$Salutation)
    
    mice_mod = mice(train_ds[,!names(train_ds) %in%
                                 c('PassengerId', 'Name', 'Ticket', 'Cabin', 'Survived')], method =
                        'rf')
    mice_output = complete(mice_mod)
    train_ds$Age = mice_output$Age
    
    train_ds$Child = "Adult"
    train_ds$Child[train_ds$Age < 18] = "Child"
    
    train_ds$Child    = factor(train_ds$Child)
    train_ds$Survived = factor(train_ds$Survived, labels = c("Died", "Lived"))
    train_ds$Sex      = factor(train_ds$Sex)
    train_ds$Pclass   = factor(train_ds$Pclass)
    train_ds$FsizeD   = factor(train_ds$FsizeD)
    train_ds
}