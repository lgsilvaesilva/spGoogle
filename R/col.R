ColAlpha <- function(col, alpha = 1){
if( (alpha < 0) || (alpha>1)) stop("alpha nao pertence ao intervalo [0, 1]")
#if((!is.na( strtoi( paste("0",substr(col,2,3),sep=""))[1] ) ) | (nchar(col) <= 7)){
#  subcol <- substr(col, start = 1, stop = 7)
#}
#else subcol <- col
#rgbcol <- t(col2rgb(subcol))/255
rgbcol <- t(col2rgb(col)[-4,])/255
colhex <- rgb(rgbcol, alpha = alpha)
return(colhex)
}

genprob <- function(wei,cuts,cuts.type){
 if(cuts.type == "quantile"){
   pb <- seq(0,1,length.out=cuts)
   prob <- quantile(wei,prob=pb,na.rm=TRUE)
 }
 else{
   wei <- wei[which(!is.na(wei))]
   prob <- seq(min(wei),max(wei), length.out = cuts)
 }
 prob[1] <- -Inf
 prob[cuts] <- Inf
 prob
}

