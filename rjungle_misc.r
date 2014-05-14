debugger = function(dbg,prompt){
  if(dbg){
    return(function(msg,...)cat(sprintf('%s-[%s]',prompt,format(Sys.time(),'%H:%M:%S')), sprintf(msg,...),'\n'))
  } else return(function(msg,...) {})
}
if (!exists("importance")){
  importance = function(x, ...)  UseMethod("importance")
}

if (!exists("importance.default")){
  importance.default = function(x, ...)
    stop("No method implemented for this class of object")
}

if (!exists("predict")){
  predict = function(x, ...)  UseMethod("importance")
}

if (!exists("predict.default")){
  predict.default = function(x, ...)
    stop("No method implemented for this class of object")
}


print.rjungle.verbose = function(obj){
	cat(obj,'\n')
}
print.rjungle.confusion = function(obj){
	cat(obj,'\n')
}
rjungleHelp = function(){
	cat('
 dataSet       --> --file=NAME       NAME of file with data                
 tempFileName  --> --outprefix=NAME  NAME of output files prefixes         
 delimiter     --> --delimiter=CHAR  delimiter in data file is CHAR        
 treeType      --> --treetype=ID     choose base classifier ID:[1-5]       
                                   ID = 1: CART, y nominal, x numeric      
                                     CART might give a biased variable     
                                     selection on data sets with varying   
                                     category sizes per variables.         
                                   ID = 2: CART, y nominal, x nominal      
                                     Twoing trees.                         
                                   ID = 3: CART, y numeric, x numeric      
                                     Regression trees.                     
                                   ID = 4: CART, y numeric, x nominal      
                                     Regression trees.                     
                                   ID = 5: CART2, y nominal, x numeric     
                                     CART2 is slower than CART (ID=1)      
                                   if category size is small or            
                                   it is not a numeric variable.           
                                   Original Breiman/Cutler/Friedman idea.  
 ntree         --> --ntree=SIZE      number (SIZE) of trees in jungle      
 mtry          --> --mtry=SIZE   size (SIZE) of randomly choosen var sets  
                                     DEFAULT: sqrt(ncol)                   
 nrow          --> --nrow=SIZE       number (SIZE) of samples. 0: all      
 ncol          --> --ncol=SIZE       number (SIZE) of input vars. 0: all   
 skipRow       --> --skiprow=SIZE    skip SIZE rows (samples)              
 skipCol       --> --skipcol=SIZE    skip SIZE columns before reading data 
 missingCode   --> --missingcode=NUM missings code. [-127..127] def:-99    
 impMeasure    --> --impmeasure=ID   choose importance method              
                     ID = 1: Intrinsic Importrance (i.e. Gini index)       
                     ID = 2: Permutation Importance Breiman, Cutler (Fortran)
                     ID = 3: Permutation Importance Liaw, Wiener (R)
                     ID = 4: Permutation Importance, raw values, no normalization
                     ID = 5: Permutation Importance Meng
 condImp       --> --condimp=NUM     Perform conditional importance if -i > 1
                                     NUM is the pearson\'s cor. coef. cutoff. 
                                     The smaller NUM, the bigger
                                     a conditional importance permutation
                                     group will be created.
                                     (=> More accurate, but slower)
                                     Requires: 0 <= NUM <= 1
                                     NUM < 0 => switched off
 backSel       --> --backsel=ID      choose backward elimination.
                               ID = 0: No backward elimination
                               ID = 1: Backward elimination.
                                       Discard 50% at each step
                               ID = 2: Backward elimination.
                                       Discard 33% at each step
                               ID = 3: Backward elimination. 
                                       Discard only negative values at each step
                               ID = 4: Diaz-Uriarte variable selection.
 nImpVar       --> --nimpvar=SIZE    only necessary if backsel>0. SIZE=[1-...]
                                     how many variable should remain.
                                     The lesser SIZE is, 
                                       the reliable the result might be.
                                     The smaller SIZE is, 
                                       the higher computing time will be.
 memMode       -->  --memmode=ID     Usage of the heap memory (RAM).
                               ID = 0: double precision floating point (BIG)
                               ID = 1: single precision floating point (Normal)
                               ID = 2: char (small)
 depVarName    --> --depvarname=NAME Output variable name in the data SET
 seed          --> --seeed=NUM       Seed of run
 pedFile       --> --pedfile         Input file has got ped format
                                       (plink --recodeA)
 maxTreeDepth  --> --maxtreedepth=NUM Max tree depth
 votes         --> --votes           Print votes for each sample
 oobset        --> --oobset          Output the oobset data
 probability   --> --probability     Writes probability predictions
 noreplace     --> --downsampling    Choose randomly samples without replacement.
 classWeights  --> --classweights    Reweighting classes of dataset
 write         --> --write=ID        Save Random Jungle ...
                                     ID = 0: not
                                     ID = 1: to a gzipped XML file
                                     ID = 2: to a XML raw file
                           ID = 3: probability estimation to a XML raw file
')}


