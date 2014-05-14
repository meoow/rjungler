if('.rj' %in% search()) detach('.rj')
if(exists('.rj')) rm(.rj)
if(exists('.tempVars')) rm(.tempVars)
.rj = new.env()
.tempVars = new.env()
####
assign('pathIntrospection', function(){
		 frameFiles = Filter(Negate(is.null),
							 lapply(sys.frames(),function(x)x$ofile))
		 return(dirname(frameFiles[[length(frameFiles)]]))},
		 envir = .tempVars
				)
.tempVars$loadpath = .tempVars$pathIntrospection()
####
.tempVars$rfiles = c('rjungle_misc.r','rjungle.r','checkIO.r','parseArgs.r',
                     'parseResultFile.r','importanceNpredict.r')
.tempVars$path2rfiles = sapply(.tempVars$rfiles,function(x){
  ifelse(.tempVars$loadpath!='',file.path(.tempVars$loadpath,x),x)})

#print(.tempVars$path2rfiles)
setClass(
  'rjungle.verbose',
  representation(
    'character'
  ))
setClass('rjungle.confusion',contains='rjungle.verbose')
setClass(
  "rjungle",
  representation(
    importance = "data.frame",
    importance2 = 'data.frame',
    confusion = 'rjungle.confusion',
    confusion2 = 'character',
    votes = 'data.frame',
    jungle = 'character', #xml file is too big, storing it in R object is unefficient and would occupy too much memory, 
    verbose = 'rjungle.verbose',
    log = 'data.frame'
  ))
setClass(
  'rjungle.prediction',
  representation(
    prediction = 'data.frame'),
  contains='rjungle')

setOldClass('rjungle.verbose',S4Class='rjungle.verbose')
setOldClass('rjungle.confusion',S4Class='rjungle.confusion')

setMethod('show','rjungle',function(object)
		  cat(do.call(paste,list(paste('',slotNames(object),sep='@'),sep='\n'))))
setMethod('show','rjungle.verbose',function(object)cat(object,'\n'))
setMethod('show','rjungle.confusion',function(object)cat(object,'\n'))



sapply(.tempVars$path2rfiles,function(x)sys.source(x,.rj))
attach(.rj,warn.conflicts=FALSE)
rm(.tempVars)
