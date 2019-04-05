## 'stepP' is like 'step' except that it uses a p-value as criterium
## Arguments:
##   object:  An lm object
##   level:   The model is reduced when p-values are above (Default 0.05)
##   verbose: Add a print of each model as they are reduced
## 
## Output:
##   object:  The reduced model
##   history: The history of the model reduction
stepP <- function(object, level=0.05, verbose=FALSE){
    if (!("lm" %in% class(object))){
        error("First argument should be an lm object")
    }
    d1 <- drop1(object, test="F")[-1,]
    maxP <- max(d1[["Pr(>F)"]], na.rm = TRUE)
    lmTmp <- object
    maxVar <- row.names(d1)[d1[["Pr(>F)"]]==maxP]
    history <-NULL # For storing the history of models
    tmpFormula <- paste(as.character(formula(lmTmp))[c(2,1,3)], collapse=" ")
    while(maxP > level & nrow(d1)>=1){
        maxVar <- row.names(d1)[d1[["Pr(>F)"]]==maxP]
        history <- rbind(history,data.frame(formula= tmpFormula, maxP=maxP, maxVar = maxVar) )
        lmTmp <- update(lmTmp, paste(".~.-",maxVar))
        d1 <- drop1(lmTmp, test="F")[-1,]
        maxP <- max(d1[["Pr(>F)"]], na.rm=TRUE)
        tmpFormula <- paste(as.character(formula(lmTmp))[c(2,1,3)], collapse=" ")
        if (verbose)
            print(tmpFormula) # Print the formula after each reduction
    }
    # Also adding the final model to document the p-value
    maxVar <- row.names(d1)[d1[["Pr(>F)"]]==maxP]
    tmpFormula <- paste(as.character(formula(lmTmp))[c(2,1,3)], collapse=" ")
    history <- rbind(history,data.frame(formula= tmpFormula, maxP=maxP, maxVar = maxVar) )
    return(list(object=lmTmp, history=history))  
}