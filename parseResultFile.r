require(stringr)

parseResultFiles = function(outPrefix){
  	possibleResultSuffix = c(confusion = '.confusion',
                           confusion2 = '.confusion2',
                           votes = '.votes',
                           importance = '.importance',
                           importance2 = '.importance2',
                           jungle = '.jungle.xml',
                           verbose = '.verbose',
                           log = '.log')
  resultFileHeaderPattern = list(importance = list(what=list(0,0,'',0),skip=1,quiet=T),
                  importance2 = list(what=list(0,0,'',0,0,0,0,0,0,0,0),skip=1,quiet=T),
                                 votes = NULL)
  possibleResultFile = sapply(possibleResultSuffix, function(x)paste(outPrefix,x,sep=''))
  result = new('rjungle',
               importance = fileParser(possibleResultFile[['importance']],
                                       resultFileHeaderPattern[['importance']]),
               importance2 = fileParser(possibleResultFile[['importance2']],
                                        resultFileHeaderPattern[['importance2']]),
               votes = fileParser(possibleResultFile[['votes']],
                                  resultFileHeaderPattern[['votes']]),
               confusion = new('rjungle.confusion',fileCacher(possibleResultFile[['confusion']])),
               confusion2 = character(0),#unable to parse confusion2 file (for now)
               #jungle = fileCacher(possibleResultFile[['jungle']]), # here just read whole xml file of rjungle model into result, because parsing a big xml in order to build a `meaningful' R object is way too complicated (using R) and not really required.
               jungle = possibleResultFile[['jungle']],
               verbose = new('rjungle.verbose',fileCacher(possibleResultFile[['verbose']])),
               log = logParser(possibleResultFile[['log']]))
  return(result)
}

parsePredictionFile = function(outPrefix){
  possibleResultSuffix = c(confusion = '.confusion',
                           confusion2 = '.confusion2',
                           votes = '.votes',
                           importance = '.importance',
                           importance2 = '.importance2',
                           verbose = '.verbose',
                           log = '.log',
                           prediction = '.prediction')
  resultFileHeaderPattern = list(importance = list(what=list(0,0,'',0),skip=1,quiet=T),
                  importance2 = list(what=list(0,0,'',0,0,0,0,0,0,0,0),skip=1,quiet=T),
                                 votes = NULL
  )
  possibleResultFile = sapply(possibleResultSuffix, function(x)paste(outPrefix,x,sep=''))
  result = new('rjungle.prediction',
               importance = fileParser(possibleResultFile[['importance']],
                                       resultFileHeaderPattern[['importance']]),
               importance2 = fileParser(possibleResultFile[['importance2']],
                                        resultFileHeaderPattern[['importance2']]),
               votes = fileParser(possibleResultFile[['votes']],
                                  resultFileHeaderPattern[['votes']]),
               confusion = new('rjungle.confusion',fileCacher(possibleResultFile[['confusion']])),
               confusion2 = character(0),#unable to parse confusion2 file (for now)
               #jungle = fileCacher(possibleResultFile[['jungle']]), # here just read whole xml file of rjungle model into result, because parsing a big xml in order to build a `meaningful' R object is way too complicated (using R) and not really required.
               #jungle = possibleResultFile[['jungle']],
               verbose = new('rjungle.verbose',fileCacher(possibleResultFile[['verbose']])),
               log = logParser(possibleResultFile[['log']]),
               prediction = read.table(possibleResultFile[['prediction']],header=F),
				jungle = ''
				)
  return(result)
}

fileParser = function(fileName,pattern){
  if(file_test('-f',fileName)){
    if(is.null(pattern)) return(read.table(fileName,header=T))
    pattern[['file']] = fileName
    header = scan(fileName,what='',nlines=1,quiet=T)
    data = do.call(scan, pattern)
    names(data) = header
    return(as.data.frame(data))
  } else return(data.frame())
}

fileCacher = function(fileName){
  if(file_test('-f',fileName)){
    size = file.info(fileName)$size
    return(readChar(fileName,size))
  } else return(character(0))
}
logParser = function(fileName){
  require(stringr)
  if(file_test('-f',fileName)){
  lines = readLines(fileName)
  lines = lines[lines != '']
  pairs = lapply(lines,function(x)str_split(x,': ',n=2)[[1]])
  labels = sapply(pairs,function(x)x[1])
  values = data.frame(values=sapply(pairs,function(x)x[2]),stringsAsFactors=F,row.names=labels)
  return(values)
  } else return(data.frame())
}
# unfinished function
#confusionParser = function(fileName){
#  if(file_test('-f',fileName)){
#    data = readLines(fileName)
#    iterationLines = grep('Iteration:',data)
#    iterations = 1:length(iterationLines)
#    numOfVarsInEveryInteration =  sapply(
#      sapply(
#        dude+1,function(x) str_split(shit[x],': ')),function(x)as.integer(x[2]))
#  }
#}
