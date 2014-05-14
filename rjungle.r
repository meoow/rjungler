randomJungle = function(
  dataSet,  delimiter=' ', tempDir=NULL, tempFileName=NULL, depVarName='class',
  treeType=1,  ntree=500,  mtry=NA,  impMeasure= 1,
  condImp=-1,  backSel=0,  nImpVar=100, maxTreeDepth=15000,
   votes=FALSE,  classWeights=c(),
  nrow=0,  ncol=0, skipRow=0,  skipCol=0, missingCode=-99, write = 2,
  rjBackendBinPath='rjungle',  seed=NA,  memMode = 0, noreplace = FALSE,
  keepTempFiles=FALSE,  rjungleXMLSaveDir=NULL, rjungleXMLSaveName = NULL,
  pedFile=FALSE, CPUs=1, verbose=TRUE, debug=FALSE){
# dataSet       --> --file=NAME       NAME of file with data                #
# tempFileName  --> --outprefix=NAME  NAME of output files prefixes         #
# delimiter     --> --delimiter=CHAR  delimiter in data file is CHAR        #
# treeType      --> --treetype=ID     choose base classifier ID:[1-5]       #
#                                   ID = 1: CART, y nominal, x numeric      #
#                                     CART might give a biased variable     #
#                                     selection on data sets with varying   #
#                                     category sizes per variables.         #
#                                   ID = 2: CART, y nominal, x nominal      #
#                                     Twoing trees.                         #
#                                   ID = 3: CART, y numeric, x numeric      #
#                                     Regression trees.                     #
#                                   ID = 4: CART, y numeric, x nominal      #
#                                     Regression trees.                     #
#                                   ID = 5: CART2, y nominal, x numeric     #
#                                     CART2 is slower than CART (ID=1)      #
#                                   if category size is small or            #
#                                   it is not a numeric variable.           #
#                                   Original Breiman/Cutler/Friedman idea.  #
# ntree         --> --ntree=SIZE      number (SIZE) of trees in jungle      #
# mtry          --> --mtry=SIZE   size (SIZE) of randomly choosen var sets  #
#                                     DEFAULT: sqrt(ncol)                   #
# nrow          --> --nrow=SIZE       number (SIZE) of samples. 0: all      #
# ncol          --> --ncol=SIZE       number (SIZE) of input vars. 0: all   #
# skipRow       --> --skiprow=SIZE    skip SIZE rows (samples)              #
# skipCol       --> --skipcol=SIZE    skip SIZE columns before reading data #
# missingCode   --> --missingcode=NUM missings code. [-127..127] def:-99    #
# impMeasure    --> --impmeasure=ID   choose importance method              #
#                     ID = 1: Intrinsic Importrance (i.e. Gini index)       #
#                     ID = 2: Permutation Importance Breiman, Cutler (Fortran)
#                     ID = 3: Permutation Importance Liaw, Wiener (R)
#                     ID = 4: Permutation Importance, raw values, no normalization
#                     ID = 5: Permutation Importance Meng
# condImp       --> --condimp=NUM     Perform conditional importance if -i > 1
#                                     NUM is the pearson's cor. coef. cutoff. 
#                                     The smaller NUM, the bigger
#                                     a conditional importance permutation
#                                     group will be created.
#                                     (=> More accurate, but slower)
#                                     Requires: 0 <= NUM <= 1
#                                     NUM < 0 => switched off
# backSel       --> --backsel=ID      choose backward elimination.
#                               ID = 0: No backward elimination
#                               ID = 1: Backward elimination.
#                                       Discard 50% at each step
#                               ID = 2: Backward elimination.
#                                       Discard 33% at each step
#                               ID = 3: Backward elimination. 
#                                       Discard only negative values at each step
#                               ID = 4: Diaz-Uriarte variable selection.
# nImpVar       --> --nimpvar=SIZE    only necessary if backsel>0. SIZE=[1-...]
#                                     how many variable should remain.
#                                     The lesser SIZE is, 
#                                       the reliable the result might be.
#                                     The smaller SIZE is, 
#                                       the higher computing time will be.
# memMode       -->  --memmode=ID     Usage of the heap memory (RAM).
#                               ID = 0: double precision floating point (BIG)
#                               ID = 1: single precision floating point (Normal)
#                               ID = 2: char (small)
# depVarName    --> --depvarname=NAME Output variable name in the data SET
# seed          --> --seeed=NUM       Seed of run
# pedFile       --> --pedfile         Input file has got ped format
#                                       (plink --recodeA)
# maxTreeDepth  --> --maxtreedepth=NUM Max tree depth
# votes         --> --votes           Print votes for each sample
# oobset        --> --oobset          Output the oobset data (unimplemented yet)
# probability   --> --probability     Writes probability predictions
# noreplace     --> --downsampling    Choose randomly samples without replacement.
# classWeights  --> --classweights    Reweighting classes of dataset
# write         --> --write=ID        Save Random Jungle ...
#                                     ID = 0: not
#                                     ID = 1: to a gzipped XML file
#                                     ID = 2: to a XML raw file
#                           ID = 3: probability estimation to a XML raw file
	if(exists('debugger')){
  printd = debugger(debug,'DBG')##p
  printv = debugger(verbose,'VBS')##p
  } else {
	  printd = function(...)return(NULL)##p
	  printv = function(...)return(NULL)##p
  }
  
  printv('Starting...')##p
  printv('Checking type of input data')##p
  if(inherits(dataSet,'data.frame')){
    type4test = data.frame()
    isDf = TRUE
  } else if(inherits(dataSet,'character')) {
    type4test = dataSet
    isDf = FALSE
  } else {
    stop('Input data supports only dataframe or path to file.')
  }
  printd('Input data type is %s',class(type4test))##p
  printv('Determining temperary directory and files')##p
  if(is.null(tempDir)) keepTempFiles = FALSE
  pathOfDirAndFile = checkIO(type4test,tempDir=tempDir)
  tempDir = pathNormalizer(pathOfDirAndFile[['tempDir']])
  inputFile = pathNormalizer(pathOfDirAndFile[['inputFile']])
  printd('tempDir is: %s',tempDir)##p
  printd('inputFile is: %s',inputFile)##p
  if (is.null(tempFileName)){
    tempFileName = tempfile(tmpdir='.')
  } else {
    stopifnot(is.character(tempFileName))
  }
  #parse the arguments, phase 1
  printv('read data header')##p
  if(isDf){
    headers = names(dataSet)
  } else {
    headers = scan(file=inputFile,what='',nlines=1,quiet=T)
  }
  varNames = headers[headers!=depVarName]
  
  numOfVars = length(varNames)
  printd('number of vars is: %s',numOfVars)##p
  if(is.na(mtry)) mtry = as.integer(sqrt(numOfVars))
  if(is.na(seed)) seed =  as.integer(Sys.time())
  tempFileName = pathNormalizer(file.path(tempDir,tempFileName))
  printd('tempFileName is: %s',tempFileName)##p
  if(!is.null(classWeights)) classWeights = do.call(paste,c(classWeights,list(sep=';')))
  #parse the arguments, phase 2
  printv('Generating command line arguments passing to rjungle')##p
  args4rj = parseArgs(file = inputFile, outprefix = tempFileName, 
                    delimiter = delimiter, 
                    treetype = treeType, ntree = ntree, mtry = mtry, 
                    depvarname = depVarName, nrow = nrow, ncol = ncol, 
                    skiprow = skipRow, skipcol = skipCol, missingcode = missingCode,
                    impmeasure = impMeasure, condimp = condImp, backsel = backSel, 
                    nimpvar = nImpVar, memmode = memMode, seeed = seed, 
                    pedfile = pedFile, maxtreedepth = maxTreeDepth, votes = votes,
                     nthreads = CPUs, verbose=verbose,
                    downsampling = noreplace, classweights = classWeights,
                    write = write)
  fullPassingCommArgs = do.call(paste,args4rj)
  ##
  printd(sprintf('Command line arguments is: %s',fullPassingCommArgs))##p
  tryCatch({ #will do some cleaning in case if the process've been failed or interupted.
    if (isDf) {
      printv('Writing data frame object to disk')##p
      write.table(dataSet,file=inputFile,quote=F,sep=delimiter,row.name=F)
    }
    # run rj
    #if(file_test('-x','rjBackendBinPath'))
    printv('Running random jungle, this may take a while')##p
    returnCode = system(paste(rjBackendBinPath, fullPassingCommArgs,sep=' '),wait=TRUE)
    if(returnCode==0){
      printv('Analysis done, gathering info from result files')##p
      result = parseResultFiles(outPrefix=tempFileName)
     #if(keepTempFiles){
    #   fileOperator = function(f,t)file.copy(f,t,overwrite=T)
     #} else {fileOperator = file.rename}
     if(!is.null(rjungleXMLSaveDir) && file_test('-d',rjungleXMLSaveDir)){
       if(!is.null(rjungleXMLSaveName)){
         rjungleXMLSavePath = file.path(rjungleXMLSaveDir,rjungleXMLSaveName)
         printd('Junle file path is: %s',rjungleXMLSavePath)##p
         printv('Saving jungle file')##p
         if(!identical(result@jungle,rjungleXMLSavePath)){
         file.copy(result@jungle,rjungleXMLSavePath, overwrite=T)
         } else warning('Can not copy jungle file, source and destination are identical')
         result@jungle = pathNormalizer(rjungleXMLSavePath)
       } else {
         warning('no jungle file name provided, ignore saving it')
         result@jungle = ''}
     } else result@jungle = ''
      printd('Jungle file path now is: %s',result@jungle)##p
     return(result)
    } else {
		return(NULL)
		printv('[!CAUTION!] Something went terribly wrong...')##p
	}
    },finally={
      if(isDf && file_test('-f',inputFile)){
        file.remove(inputFile) # remove temp input file
		printd('Input file has been deleted')##p
      }
      if(!keepTempFiles){
		printd('Do not keep temp files, removing them')##p
        possibleResultSuffix = c(confusion = '.confusion',
                                 confusion2 = '.confusion2',
                                 votes = '.votes',
                                 importance = '.importance',
                                 importance2 = '.importance2',
                                 jungle = '.jungle.xml',
                                 verbose = '.verbose',
                                 log = '.log')
	  printd('The junk files are: %s',paste(paste(tempFileName,possibleResultSuffix[['log']],' and so on',sep=''),sep='\n'))##p
        unlink(paste(tempFileName,possibleResultSuffix,sep=''),
               recursive=TRUE)#remove temp dir
      }
    }
  )
  printv('All done')
}

