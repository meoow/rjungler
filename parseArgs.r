parseArgs = function(...){
    cArguments = function(arg, subj){
        if(inherits(subj,c('character'))){
            return(sprintf('--%s="%s"',arg, subj))
        } else if(inherits(subj,c('numeric','integer'))){
            if(length(subj)>1) {
                subj = sprintf("%s;",
                               do.call(paste,
                                lapply(c(subj,sep=";"),
									function(x){return(x)})))
            }
            return(sprintf('--%s=%s',arg,subj))
        } else if(inherits(subj,'logical')) {
            #print(arg)
            #print(subj)
            if(subj)return(sprintf('--%s',arg))
        } else return('')
    }
    args = list(...)
    out = list()
    if('' %in% names(args)){
        stop('parseArgs accepts only keyword arguments')
    }
    for(i in names(args)){
        out[[i]] = cArguments(i,args[[i]])
    }
    return(out)
}
