checkIO = function(type4test,tempDir=NULL){
  if (is.null(tempDir)) { #if temp directory is NULL.
    tempDir = tempdir() #then make one.
  } else { #when you give a shit about it.
    if(!file_test('-d',tempDir)) { #if temp dir have been specified, then test if it is existent.
      stop("Given temperary directory doesn't exist")
    }}
  if (inherits(type4test,'data.frame')) {
    tempInputFileName = tempfile(tmpdir='.',fileext='.txt')
    #conjunct temp dir and file name to as whole path of input file.
    inputFile = file.path(tempDir,tempInputFileName)
  } else { #or just the string of path to the data file.
    if (inherits(type4test,'character')) { #if type contains path to data file
      #print(type4test)
      if(!file_test('-f',type4test)) { #there ain't?
        stop("Invalid input data") #crap!
      }
      inputFile = type4test
    }}
  return(list(tempDir=tempDir,inputFile=inputFile))
}
pathNormalizer = function(path){
  #####
  require(stringr) #string manipulation is frustrating using R, 
              #package `stringr' do save the ass a little bit, but ain't that promising.
  #####
  path = path.expand(path)
  if (base:::.Platform$OS.type == 'unix') { # running linux or osx or other unix-like os.
    startswith = '^/'                       # root starts with '/'
  } else if (base:::.Platform$OS.type == 'windows') { # running vicious windows
    startswith = '^[c-z]:/'                 # root starts with bollocks x_x
  }
  isAbsPath = FALSE
  if(str_detect(path,startswith)){
    isAbsPath = TRUE
  }
  #print(isAbsPath)
  if(!isAbsPath){
    path = file.path(getwd(),path)
  }
  #path = sub('/\\.?$','',gsub('/\\.(?!=[\\./])','',gsub('//','/',path),perl=T))
  while(str_detect(path,'//')) path = sub('//','/',path)
  while(str_detect(path,'/\\./')) path = sub('/\\./','/',path)
  if(str_detect(path,'/\\.\\.$')) path = paste(path,'/',sep='')
  path = trace2parent(sub('/\\.?$','/',path))
  return(path)
}
trace2parent= function(fullpath){
  if (base:::.Platform$OS.type == 'unix') {
    startswith = '^/'
  } else if (base:::.Platform$OS.type == 'windows') {
    startswith = '^[c-z]:/'
  }
  parentCrossRoot = sprintf('^%s\\.\\./.*',startswith)
  while(str_detect(fullpath,'/\\.\\./')){
    if(str_detect(fullpath,parentCrossRoot)){
      stop('Invalid path.')
    }
    fullpath = sub('/[^/]+/\\.\\./?','/',fullpath)
  }
  return(fullpath)
}

