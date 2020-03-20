plot2axis=function(df1=NULL,df2=NULL,label2=NULL,type2=c("p","p"),col2=c("black","red"),cex2=c(1,1),pch2=c(16,16),lty2=c(1,1),addlegend=T,legendpos="topright",y1lim=NULL,y2lim=NULL){
  if(is.null(y1lim)){y1lim=range(df1[,2],na.rm = T)};if(is.null(y2lim)){y2lim=range(df2[,2],na.rm = T)}
  if(is.null(label2)){label2=c("x",names(df1)[2],names(df2)[2])}
  if(min(df1[,1],na.rm = T)<min(df2[,1],na.rm = T)){
    df2[(nrow(df2)+1),]=c(NA,NA)
    if ((class(df2[,1]) == "POSIXlt")[1]) {
      df2[(nrow(df2)+1),1]=as.POSIXct(min(df1[,1],na.rm = T))
    }else if((class(df2[,1]) == "POSIXct")[1]){
      df2[(nrow(df2)+1),1]=min(df1[,1],na.rm = T)
    }else{
      df2[(nrow(df2)+1),1]=min(df1[,1],na.rm = T)
    }
    
  }else{
    
    df1[(nrow(df1)+1),]=c(NA,NA)
    if ((class(df1[,1]) == "POSIXlt")[1]) {
      df1[(nrow(df1)+1),1]=as.POSIXct(min(df2[,1],na.rm = T))
    }else if((class(df1[,1]) == "POSIXct")[1]){
      df1[(nrow(df1)+1),1]=min(df2[,1],na.rm = T)
    }else{
      df1[(nrow(df1)+1),1]=min(df2[,1],na.rm = T)
    }
    
    
  }
  if(max(df1[,1],na.rm = T)>max(df2[,1],na.rm = T)){
    
    df2[(nrow(df2)+1),]=c(NA,NA)
    if ((class(df2[,1]) == "POSIXlt")[1]) {
      df2[(nrow(df2)+1),1]=as.POSIXct(max(df1[,1],na.rm = T))
    }else if((class(df2[,1]) == "POSIXct")[1]){
      df2[(nrow(df2)+1),1]=max(df1[,1],na.rm = T)
    }else{
      df2[(nrow(df2)+1),1]=max(df1[,1],na.rm = T)
    }
    
    
  }
  else{
    
    df1[(nrow(df1)+1),]=c(NA,NA)
    if ((class(df1[,1]) == "POSIXlt")[1]) {
      df1[(nrow(df1)+1),1]=as.POSIXct(max(df2[,1],na.rm = T))
    }else if((class(df1[,1]) == "POSIXct")[1]){
      df1[(nrow(df1)+1),1]=max(df2[,1],na.rm = T)
    }else{
      df1[(nrow(df1)+1),1]=max(df2[,1],na.rm = T)
    }
    
    
    
  }
  par(mar = c(5,5,2,5))
  with(data=df1,expr =  plot(df1[,1], df1[,2], type=type2[1], col=col2[1],cex=cex2[1],pch=pch2[1],lty=lty2[1], ylim=y1lim,xlab=label2[1],ylab=label2[2]))
  par(new=TRUE);with(data = df2, expr=plot(df2[,1], df2[,2], type=type2[2], col=col2[2],cex=cex2[2],pch=pch2[2],lty=lty2[2], ylim=y2lim,axes=F, xlab=NA, ylab=NA))
  axis(side = 4);mtext(side = 4, line = 3, label2[3])
  if(addlegend == T){legend(legendpos,legend=c(label2[2],label2[3]),lty=lty2, pch=pch2, col=col2)}
}
