predict.rjungle = function(rjungleModel, newData, depVarName=NULL, delimiter=NULL,
                           probability=FALSE, impMeasure=1, 
						   votes=FALSE, mtry=NA, 
                           outputFileName=NULL, tempDir=NULL, keepTempFiles=FALSE,
						   seed=NA, missingCode=-99, memMode = 0, 
                           rjBackendBinPath='rjungle', verbose=TRUE,debug=FALSE){
	if(exists('debugger')){
  printd = debugger(debug,'DBG')##p
  printv = debugger(verbose,'VBS')##p
  } else {
	  printd = function(...)return(NULL)##p
	  printv = function(...)return(NULL)##p
  }
	printv('Determining validity of data')##p
  if(!inherits(rjungleModel,'rjungle')) stop('Snap! Not a rjungle data given')
  if(rjungleModel@jungle=='') stop('Given data contains no rjungle xml file path')
  if(!file_test('-f',rjungleModel@jungle)) spot('Rjungle xml file dose not exist any more')
  printv('Handling temp directories and files')##p
  fileAndPath = checkIO(newData,tempDir)
  inputFile = pathNormalizer(fileAndPath[['inputFile']])
  printd('inputFile is: %s',inputFile)##p
  tempDir = pathNormalizer(fileAndPath[['tempDir']])
  printd('tempDir is: %s',tempDir)##p
  #if(!file_test('-f',rjungleXMLFile)) stop('Snap! Can not file XML file')
  if(is.null(outputFileName)){
    outputFileName = pathNormalizer(file.path(tempDir,tempfile(tmpdir='.')))
  } else outputFileName = pathNormalizer(file.path(tempDir,outputFileName))
  printd('outputFileName is: %s',outputFileName)##p
  printv('Generating command line arguments')##p
  if(is.null(depVarName)) depVarName = rjungleModel@log['depvarname',1]
  if(is.null(delimiter)) delimiter = rjungleModel@log['delimiter',1]
  if(is.na(seed)) seed = as.integer(Sys.time())
  if(is.na(mtry)) mtry = rjungleModel@log['mtry',1]
  if(probability && rjungleModel@log['write',1]!='3'){
	 probability=FALSE
	warning('`probability\' only support jungle xml file saved by write=3\n
			set it to FALSE')
				}
  printd('depVarName is: %s',depVarName)##p
  args4pred = parseArgs(file=inputFile,depvarname=depVarName,predict=rjungleModel@jungle ,
                           probability=probability,impmeasure=impMeasure,
						   outprefix=outputFileName, seeed=seed,mtry=mtry,
						   missingcode=missingCode,
                           votes=votes, verbose=verbose)
  fullPassingCommArgs = do.call(paste,args4pred)
  printd('commLineArgs is: %s',fullPassingCommArgs)##p
  tryCatch({
    if (inherits(newData,'data.frame')) {
      printv('Writing data frame object to disk')##p
      write.table(newData,file=inputFile,quote=F,sep=delimiter,row.name=F)
	}
	printv('Starting prediction subroutine')##p
    returnCode = system(paste(rjBackendBinPath, fullPassingCommArgs, sep=' '),wait=TRUE)
    if(returnCode==0){
		printv('Process done, gathering info from result file')##p
      result = parsePredictionFile(outputFileName)
	  return(result)
    } else printv('Something horror just happend')
  },
           finally={
			   if(inherits(newData,'data.frame') && file_test('-f',inputFile)){
					file.remove(inputFile) # remove temp input file
					printd('Input file has been deleted')##p
			   }
             if(!keepTempFiles){
				 printv('Removing junk files')##p
  possibleResultSuffix = c(confusion = '.confusion',
                           confusion2 = '.confusion2',
                           votes = '.votes',
                           importance = '.importance',
                           importance2 = '.importance2',
                           verbose = '.verbose',
                           log = '.log',
                           prediction = '.prediction')
        unlink(paste(outputFileName,possibleResultSuffix,sep=''),
               recursive=TRUE)#remove temp dir
             }
           })
  printv('All done')
  
}

importance.rjungle = function(rjungleModel, type=1,iteration=NULL){
  if(!inherits(rjungleModel,'rjungle')) stop('Snap! Not a rjungle data given')
  if(!inherits(iteration,c('numeric','integer','NULL'))) stop('Snap! `iteration\' needs to be a vector of numbers')
  if(type==1){
    data = rjungleModel@importance
  } else if(type==2) {
	data = rjungleModel@importance2
  } else stop('Snap! `type\' out of choices')
  if(is.null(iteration)){
    return(data)
	} else {
		return(rjungleModel@importance2[rjungleModel@importance2[['iteration']] %in% iteration,])}
}

importance2 = function(rjungleModel,iteration=NULL){
	return(importance(rjungleModel,type=2,iteration=iteration))
}
