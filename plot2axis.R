plot2axis=function(df1=NULL,df2=NULL,xlab="",ylab2=NULL,type2=c("p","p"),col2=c("black","red"),cex2=c(1,1),pch2=c(16,16),lty2=c(1,1),addlegend=T,xlim=NULL,y1lim=NULL,y2lim=NULL,xaxt="s"){
  if(is.null(y1lim)){
    y1lim=range(df1[,2],na.rm = T)
  }
  if(is.null(y2lim)){
    y2lim=range(df2[,2],na.rm = T)
  }
  if(is.null(ylab2)){
    ylab2=c(names(df1)[2],names(df2)[2])
  }
  if(min(df1[,1],na.rm = T)<min(df2[,1],na.rm = T)){
    df2=rbind(df2,c(NA,NA))
    if ((class(df2[,1]) == "POSIXlt")[1]) {
      df2[nrow(df2),1]=as.POSIXct(min(df1[,1],na.rm = T))
    }else if((class(df2[,1]) == "POSIXct")[1]){
      df2[nrow(df2),1]=min(df1[,1],na.rm = T)
    }else{
      df2[nrow(df2),1]=min(df1[,1],na.rm = T)
    }
  }else{
    df1=rbind(df1,c(NA,NA))
    if ((class(df1[,1]) == "POSIXlt")[1]) {
      #df1[(nrow(df1)+1),1]=as.POSIXct(min(df2[,1],na.rm = T))
      df1[nrow(df1),1]=as.POSIXct(min(df2[,1],na.rm = T))
    }else if((class(df1[,1]) == "POSIXct")[1]){
      df1[nrow(df1),1]=min(df2[,1],na.rm = T)
    }else{
      df1[nrow(df1),1]=min(df2[,1],na.rm = T)
    }
  }
  if(max(df1[,1],na.rm = T)>max(df2[,1],na.rm = T)){
    df2=rbind(df2,c(NA,NA))
    if ((class(df2[,1]) == "POSIXlt")[1]) {
      df2[nrow(df2),1]=as.POSIXct(max(df1[,1],na.rm = T))
    }else if((class(df2[,1]) == "POSIXct")[1]){
      df2[nrow(df2),1]=max(df1[,1],na.rm = T)
    }else{
      df2[nrow(df2),1]=max(df1[,1],na.rm = T)
    }
  }
  else{
    df1=rbind(df1,c(NA,NA))
    if ((class(df1[,1]) == "POSIXlt")[1]) {
      df1[nrow(df1),1]=as.POSIXct(max(df2[,1],na.rm = T))
    }else if((class(df1[,1]) == "POSIXct")[1]){
      df1[nrow(df1),1]=max(df2[,1],na.rm = T)
    }else{
      df1[nrow(df1),1]=max(df2[,1],na.rm = T)
    }
  }
  
  par(mar = c(5,5,2,5))
  with(data=df1,expr =  plot(df1[,1], df1[,2], type=type2[1], col=col2[1],cex=cex2[1],pch=pch2[1],lty=lty2[1],xlim=xlim,ylim=y1lim,xlab=xlab,ylab=ylab2[1],xaxt=xaxt))
  par(new=TRUE)
  with(data = df2, expr=plot(df2[,1], df2[,2], type=type2[2], col=col2[2],
                             cex=cex2[2],pch=pch2[2],lty=lty2[2],xlim=xlim,ylim=y2lim,
                             axes=F, xlab=NA, ylab=NA,xaxt=xaxt,yaxt="n"))
  axis(side = 4)
  mtext(side = 4, line = 3, ylab2[2])
  
  for (tt in 1:length(type2)) {
    if (type2[tt] == "p") {lty2[tt]=0}
    if (type2[tt] == "l") {cex2[tt]=0}
  }
  
  if(addlegend == T){
    legend("bottomright", c(ylab2[1],ylab2[2]), pch=pch2,lty=lty2,col=col2,pt.cex = cex2,
           inset=c(0,1), xpd=TRUE, horiz=TRUE, bty="n"
    )
  }
}
