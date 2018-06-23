ellipse_conf<-function(df,alpha){
  xm<-apply(df,2,mean)
  Sd<-apply(df,2,sd)
  Teta<-6.28/100*(0:100)
  cf<-sqrt(qf(alpha, 2,nrow(df) - 2))
  return(data.frame(xe=xm[1]+cf*cos(Teta)*Sd[1],ye=xm[2]+cf*sin(Teta)*Sd[2]))
}