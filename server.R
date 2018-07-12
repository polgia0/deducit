server = function(input, output) { # begin server
  source("chooser.R")
  source("ellipse_conf.R")
  values<-reactiveValues(DS=NULL,rdf=NULL,cdf=NULL,tdf=NULL,pcadf=NULL,plsdf=NULL,selobj=NULL,vargrp=NULL,seltrd=NULL,rowsel=NULL)
  plsdf<-reactive({
    validate(need(nrow(values$DS)!=0, "You must load a DataSet"))
    validate(need(length(input$plschooser_subcol$right)!=0, "Please select at least one responce"))
    req(input$plsslider_ncp)
    ncp<-as.numeric(input$plsslider_ncp)
    center<-FALSE
    scale<-FALSE
    for (ck in input$ncpcheckGroup){
      if (ck=="1") center <- TRUE
      if (ck=="2") scale<-TRUE
    }
    measurevar<-input$plschooser_subcol$right
    groupvars<-input$plschooser_subcol$left
    frm<-as.formula(paste('cbind(',paste(measurevar, collapse=","),") ~ ", paste(groupvars, collapse=" + "), sep=""))
    df<-values$DS[values$rdf$left,values$cdf$left]
    df<-df[values$tdf$left,]
    XN<-scale(df, center=center, scale=scale)
    if((input$plsvalidation==1)&(input$plsegment<nrow(XN))){
      plsobj<-plsr(frm,data=as.data.frame(XN),scale=TRUE,validation="CV", segments = input$plsegment,jackknife = TRUE, trace = TRUE)
    }
    if((input$plsvalidation==2)|(input$plsegment>=nrow(XN))){
      plsobj<-plsr(frm,data=as.data.frame(XN),scale=TRUE,validation="LOO",jackknife = TRUE, trace = TRUE)
    }
    if(input$plsvalidation==3){
      plsobj<-plsr(frm,data=as.data.frame(XN),scale=TRUE,validation="none")
    }
    values$plsdf<-plsobj
  })
  output$xplswi<-renderUI({
    validate(need(nrow(values$DS)!=0, "You must load a DataSet"))
    selectInput("xselectyplswi", label = tags$b("Component on X"),choices = 1:as.numeric(input$plsslider_ncp), selected = 1)
  })
  output$yplswi<-renderUI({
    validate(need(nrow(values$DS)!=0, "You must load a DataSet"))
    selectInput("yselectyplswi", label = tags$b("Component on Y"),choices = 1:as.numeric(input$plsslider_ncp), selected = 2)
  })
  output$plswi<-renderPlot({
    plt<-ggplot(plswiDS()$df,aes(x=W1,y=W2))+theme_bw()
    plt<-plt+geom_point()
    plt<-plt+labs(title=paste("Weight Plot for Model with ",toString(plswiDS()$ncp)," Components"), x=paste("W*",toString(input$xselectyplswi)), y = paste("W*",toString(input$yselectyplswi)))
    plt<-plt+theme(plot.title=element_text(face="bold",size="14", color="brown"),legend.position="none")
    if (as.logical(input$vplswi))plt<-plt+geom_text(aes(label=plswiDS()$Name,colour="red"),hjust=0,nudge_x=0.01,size=5)
    print(plt)
  })
  plswiDS<-reactive({
    req(input$xselectyplswi,input$yselectyplswi)
    plsobj<-plsdf()
    WSs<-as.data.frame(plsobj$projection[,1:as.numeric(input$plsslider_ncp)])
    n1<-as.numeric(input$xselectyplswi)
    n2<-as.numeric(input$yselectyplswi)
    df<-data.frame(W1=WSs[,n1],W2=WSs[,n2])
    list(df=df,ncp=as.numeric(input$plsslider_ncp),Name=row.names(WSs))
  })
  output$plswidwnl<-downloadHandler(filename = "pls_wi.csv",
                                       content = function(file) {write.csv2(plswiDS()$df, file, row.names = TRUE, sep=input$sep, dec=input$dec)}
  )
  output$xplsqi<-renderUI({
    validate(need(nrow(values$DS)!=0, "You must load a DataSet"))
    selectInput("xselectyplsqi",label=tags$b("Component on X"),choices = 1:as.numeric(input$plsslider_ncp), selected = 1)
  })
  output$yplsqi<-renderUI({
    validate(need(nrow(values$DS)!=0, "You must load a DataSet"))
    selectInput("yselectyplsqi",label=tags$b("Component on Y"),choices = 1:as.numeric(input$plsslider_ncp), selected = 2)
  })
  output$plsqi<-renderPlot({
    plt<-ggplot(plsqiDS()$df,aes(x=Q1,y=Q2))+theme_bw()
    plt<-plt+geom_point()
    plt<-plt+labs(title=paste("Loading Plot for Model with ",toString(plsqiDS()$ncp)," Comp."), x=paste("Q",toString(input$xselectyplsqi)), y = paste("Q",toString(input$yselectyplsqi)))
    plt<-plt+theme(plot.title=element_text(face="bold",size="14", color="brown"),legend.position="none")
    if (as.logical(input$vplsqi))plt<-plt+geom_text(aes(label=plsqiDS()$Name,colour="red"),hjust=0,nudge_x=0.01,size=5)
    print(plt)
  })
  plsqiDS<-reactive({
    req(input$xselectyplsqi,input$yselectyplsqi)
    plsobj<-plsdf()
    Q<-plsobj$Yloadings[,1:as.numeric(input$plsslider_ncp)]
    n1<-as.numeric(input$xselectyplsqi)
    n2<-as.numeric(input$yselectyplsqi)
    df<-data.frame(Q1=Q[,n1],Q2=Q[,n2])
    list(df=df,ncp=as.numeric(input$plsslider_ncp),Name=row.names(Q))
  })
  output$plsqidwnl<-downloadHandler(filename = "pls_qi.csv",
                                    content = function(file) {write.csv2(plsqiDS()$df, file, row.names = TRUE, sep=input$sep, dec=input$dec)}
  )
  output$xplswq<-renderUI({
    validate(need(nrow(values$DS)!=0, "You must load a DataSet"))
    selectInput("xselectyplswq",label=tags$b("Component on X"),choices = 1:as.numeric(input$plsslider_ncp), selected = 1)
  })
  output$yplswq<-renderUI({
    validate(need(nrow(values$DS)!=0, "You must load a DataSet"))
    selectInput("yselectyplswq",label=tags$b("Component on Y"),choices = 1:as.numeric(input$plsslider_ncp), selected = 2)
  })
  output$plswq<-renderPlot({
    plt<-ggplot(plswqDS()$df,aes(x=WQ1,y=WQ2,color=col))+theme_bw()
    plt<-plt+geom_point()
    plt<-plt+labs(title=paste("Join Loading Plot for Model with ",toString(plswqDS()$ncp)," Components"), x=paste("Q",toString(input$xselectyplswq)), y = paste("Q",toString(input$yselectyplswq)))
    plt<-plt+theme(plot.title=element_text(face="bold",size="14",color="brown"))
    if (as.logical(input$vplswq))plt<-plt+geom_text(aes(label=plswqDS()$Name),hjust=0,nudge_x=0.01,size=5)
    print(plt)
  })
  plswqDS<-reactive({
    req(input$xselectyplswq,input$yselectyplswq)
    plsobj<-plsdf()
    QQ<-plsobj$Yloadings[,1:as.numeric(input$plsslider_ncp)]
    WSs<-as.data.frame(plsobj$projection[,1:as.numeric(input$plsslider_ncp)])
    WSsQ<-rbind(WSs,QQ)
    n1<-as.numeric(input$xselectyplswq)
    n2<-as.numeric(input$yselectyplswq)
    df<-data.frame(WQ1=WSsQ[,n1],WQ2=WSsQ[,n2], col=c(rep("Factor",nrow(WSs)),rep("Response",nrow(QQ))))
    df$col<-as.factor(df$col)
    list(df=df,ncp=as.numeric(input$plsslider_ncp),Name=c(row.names(plsobj$projection),row.names(plsobj$Yloadings)))
  })
  output$plswqdwnl<-downloadHandler(filename="pls_wq.csv",
                                    content=function(file){write.csv2(plswqDS()$df,file,row.names=TRUE,sep=input$sep,dec=input$dec)}
  )
  output$plsht2<-renderPlot({
    plt<-ggplot(plsht2DS()$df,aes(x=obj,y=ht2))+theme_bw()
    plt<-plt+geom_bar(stat="identity",fill="steelblue")+scale_x_discrete(limits=1:plsht2DS()$nr,labels=plsht2DS()$Name)
    plt<-plt+labs(title=paste("CV Hoteling T2 Plot for Model with ",toString(plsht2DS()$ncp)," Components"),x="Objects",y="T^2")
    plt<-plt+theme(plot.title=element_text(face="bold",size="14",color="brown"),axis.text.x=element_text(angle=90,hjust = 1))
    if (as.logical(input$vplsht2))plt<-plt+geom_text(aes(label=round(ht2,digits=5)),position=position_dodge(width=0.9),vjust=-0.25)
    print(plt)
  })
  plsht2DS<-reactive({
    plsobj<-plsdf()
    TT<-plsobj$scores[,1:as.numeric(input$plsslider_ncp)]
    T2<-t(TT)%*%TT
    InvT2<-ginv(T2)
    nr<-nrow(TT)
    HT2<-diag((TT%*%InvT2)%*%t(TT))/(nr-1)
    df<-data.frame(obj=1:nr,ht2=HT2)
    list(df=df,ncp=as.numeric(input$plsslider_ncp),nr=nr,Name=row.names(TT))
  })
  output$plsht2dwnl<-downloadHandler(filename="pls_ht2.csv",
                                    content=function(file){write.csv2(plsht2DS()$df,file,row.names=TRUE,sep=input$sep,dec=input$dec)}
  )
  output$xplssco<-renderUI({
    selectInput("xselectyplssco",label=tags$b("Component on X"),choices = 1:as.numeric(input$plsslider_ncp), selected = 1)
  })
  output$yplssco<-renderUI({
    validate(need(nrow(values$DS)!=0, "You must load a DataSet"))
    selectInput("yselectyplssco",label=tags$b("Component on Y"),choices = 1:as.numeric(input$plsslider_ncp), selected = 2)
  })
  output$plssco<-renderPlot({
    plt<-ggplot(plssco2DS()$df,aes(x=T1,y=T2))+theme_bw()
    plt<-plt+geom_point()+ coord_fixed()
    plt<-plt+labs(title=paste("CV Score Plot for Model with ",toString(plssco2DS()$ncp)," Components"), x=paste("t",toString(input$xselectyplssco)), y = paste("t",toString(input$yselectyplssco)))
    plt<-plt+theme(plot.title=element_text(face="bold",size="14", color="brown"))
    if (as.logical(input$vplssco))plt<-plt+geom_text(aes(label=plssco2DS()$Name),hjust = 0,nudge_x =0.01,size=5)
    if (as.logical(input$eplssco))plt<-plt+ stat_ellipse(type = "norm", linetype = 1,position = "identity",level=(100-as.numeric(input$aplssco))/100)
    print(plt)
  })
  plssco2DS<-reactive({
    req(input$xselectyplssco,input$yselectyplssco)
    plsobj<-plsdf()
    TT<-plsobj$scores[,1:as.numeric(input$plsslider_ncp)]
    n1<-as.numeric(input$xselectyplssco)
    n2<-as.numeric(input$yselectyplssco)
    df<-data.frame(T1=TT[,n1],T2=TT[,n2])
    list(df=df,ncp=as.numeric(input$plsslider_ncp),Name=row.names(TT))
  })
  output$plsscodwnl<-downloadHandler(filename = "pls_sco.csv",
                                     content = function(file) {write.csv2(plsscoDS()$df, file, row.names = TRUE, sep=input$sep, dec=input$dec)}
  )
  output$plsspex<-renderPlot({
    plt<-ggplot(plsspexDS()$df,aes(x=obj,y=spex))+theme_bw()
    plt<-plt+geom_bar(stat="identity", fill="steelblue")+scale_x_discrete(limits=1:plsspexDS()$ncx,labels=plsspexDS()$Name)
    plt<-plt+labs(title=paste("CV-SPEX Plot for Model with ",toString(plsspexDS()$ncp)," Components"), x="Variables", y = "SPE X")
    plt<-plt+theme(plot.title=element_text(face="bold",size="14", color="brown"),axis.text.x = element_text(angle = 90, hjust = 1))
    if (as.logical(input$vplsspex))plt<-plt+geom_text(aes(label=round(spex,digits=2)),position=position_dodge(width=0.9), vjust=-0.25)
    print(plt)
  })
  plsspexDS<-reactive({
    plsobj<-plsdf()
    TT<-plsobj$scores[,1:as.numeric(input$plsslider_ncp)]
    PP<-plsobj$loadings[,1:as.numeric(input$plsslider_ncp)]
    ncx<-nrow(PP)
    XN<-as.matrix(plsobj$model[,-1])
    SPEX<-apply((TT%*%t(PP)-XN)^2,2,sum)
    df<-data.frame(obj=1:ncx,spex=SPEX)
    list(df=df,ncp=as.numeric(input$plsslider_ncp),ncx=ncx,Name=row.names(PP))
  })
  output$plsspexdwnl<-downloadHandler(filename = "pls_spex.csv",
                                     content = function(file) {write.csv2(plsspexDS()$df, file, row.names = TRUE, sep=input$sep, dec=input$dec)}
  )
  output$plsspey<-renderPlot({
    plt<-ggplot(plsspeyDS()$df,aes(x=var,y=spey))+theme_bw()
    plt<-plt+geom_bar(stat="identity",fill="steelblue")+scale_x_discrete(limits=1:plsspeyDS()$ncy,labels=plsspeyDS()$Name)
    plt<-plt+labs(title=paste("CV-SPEY Plot for Model with ",toString(plsspeyDS()$ncp)," Components"), x="Response", y = "SPE Y")
    plt<-plt+theme(plot.title=element_text(face="bold",size="14", color="brown"),axis.text.x = element_text(angle = 90, hjust = 1))
    if (as.logical(input$vplsspey))plt<-plt+geom_text(aes(label=round(spey,digits=2)), position=position_dodge(width=0.9), vjust=-0.25)
    print(plt)
  })
  plsspeyDS<-reactive({
    plsobj<-plsdf()
    Ey<-plsobj$residuals
    ncp<-as.numeric(input$plsslider_ncp)
    Ey<-as.data.frame(Ey[,,ncp])
    SPEY<-apply(Ey^2,2,sum)
    ncy<-ncol(Ey)
    df<-data.frame(var=1:ncy,spey=SPEY)
    list(df=df,ncp=ncp,ncy=ncy,Name=names(Ey))
  })
  output$plsspeydwnl<-downloadHandler(filename = "pls_spey.csv",
                                      content = function(file) {write.csv2(plsspeyDS()$df, file, row.names = TRUE, sep=input$sep, dec=input$dec)}
  )
  output$xplstu<-renderUI({
    validate(need(nrow(values$DS)!=0, "You must load a DataSet"))
    selectInput("xselectyplstu",label=tags$b("Component on t"),choices = 1:as.numeric(input$plsslider_ncp), selected = 1)
  })
  output$yplstu<-renderUI({
    validate(need(nrow(values$DS)!=0, "You must load a DataSet"))
    selectInput("yselectyplstu",label=tags$b("Component on u"),choices = 1:as.numeric(input$plsslider_ncp), selected = 1)
  })
  output$plstu<-renderPlot({
    plt<-ggplot(plstuDS()$df,aes(x=T,y=U))+theme_bw()
    plt<-plt+geom_point()
    plt<-plt+labs(title=paste("T vs.U Plot for Model with ",toString(plstuDS()$ncp)," Components"), x=paste("t",toString(input$xselectyplstu)), y = paste("u",toString(input$yselectyplstu)))
    plt<-plt+theme(plot.title=element_text(face="bold",size="14", color="brown"))
    if (as.logical(input$vplstu))plt<-plt+geom_text(aes(label=plstuDS()$Name),hjust=0,nudge_x=0.01,size=5)
    print(plt)
  })
  plstuDS<-reactive({
    req(input$xselectyplstu,input$yselectyplstu)
    plsobj<-plsdf()
    TT<-plsobj$scores[,1:as.numeric(input$plsslider_ncp)]
    UU<-plsobj$Yscores[,1:as.numeric(input$plsslider_ncp)]
    n1<-as.numeric(input$xselectyplstu)
    n2<-as.numeric(input$yselectyplstu)
    df<-data.frame(T=TT[,n1],U=UU[,n2])
    list(df=df,ncp=as.numeric(input$plsslider_ncp),Name=row.names(TT))
  })
  output$plstudwnl<-downloadHandler(filename = "pls_tu.csv",
                                      content = function(file) {write.csv2(plstuDS()$df, file, row.names = TRUE, sep=input$sep, dec=input$dec)}
  )
  output$plsvip<-renderPlot({
    plt<-ggplot(plsvipDS()$df,aes(x=var,y=vip))+theme_bw()
    plt<-plt+geom_bar(stat="identity",fill="steelblue")+scale_x_discrete(limits=1:plsvipDS()$ncx,labels=plsvipDS()$Name)
    plt<-plt+labs(title=paste("Variable Importance Plot (VIP) for Model with ",toString(plsvipDS()$ncp)," Components"), x="Variables", y = "VIP")
    plt<-plt+theme(plot.title=element_text(face="bold",size="14", color="brown"),axis.text.x = element_text(angle = 90, hjust = 1))
    if (as.logical(input$vplssvip))plt<-plt+geom_text(aes(label=round(vip,digits=2)), position=position_dodge(width=0.9), vjust=-0.25)
    print(plt)
  })
  plsvipDS<-reactive({
    plsobj<-plsdf()
    PP<-plsobj$loadings[,1:as.numeric(input$plsslider_ncp)]
    TT<-plsobj$scores[,1:as.numeric(input$plsslider_ncp)]
    ncx<-nrow(PP)
    UU<-plsobj$Yscores[,1:as.numeric(input$plsslider_ncp)]
    WW<-plsobj$loading.weights[,1:as.numeric(input$plsslider_ncp)]
    T2<-t(TT)%*%TT
    U2<-t(UU)%*%UU
    W2<-WW^2
    TU<-T2%*%U2
    SS<-diag(TU)
    VIP<-sqrt(W2%*%SS*ncx/sum(SS))
    df<-data.frame(var=1:ncx,vip=VIP)
    list(df=df,ncp=as.numeric(input$plsslider_ncp),ncx=ncx,Name=row.names(PP))
  })
  output$plsvipdwnl<-downloadHandler(filename = "pls_vip.csv",
                                    content = function(file) {write.csv2(plsvipDS()$df, file, row.names = TRUE, sep=input$sep, dec=input$dec)}
  )
  output$yplsfit<-renderUI({
    validate(need(nrow(values$DS)!=0, "You must load a DataSet"))
    selectInput("yselectyplsfit",label=tags$b("Response"),choices=c("All",input$plschooser_subcol$right), selected = 1)
  })
  output$plsfit<-renderPlot({
    req(input$yselectyplsfit)
    if((input$yselectyplsfit!="All")&(input$plscvspread==TRUE)&(input$plsvalidation!=3)){
          df<-plsfitDS()$df
          dfcv<-plsfitDS()$YCV
          avg<-unlist(by(dfcv$value,dfcv$index,mean))
          sdev<-unlist(by(dfcv$value,dfcv$index,sd))
          plot(df$measured,avg,xlab="Measured",ylab="Fitted")
          title(main="CV-Fitted vs.Measured Values",font.main="14",col.main="brown",font.main=2,adj=0.0,line=0.3)
          arrows(df$measured,avg-3*sdev,df$measured,avg+3*sdev,length=0.05,angle=90,code=3)
          abline(0,1)
          grid()
    }else{
          if(input$yselectyplsfit=="All"){
            plt<-ggplot(plsfitDS()$df,aes(x=measured,y=fitted,colour=key))+theme_bw()
          }else{
            plt<-ggplot(plsfitDS()$df,aes(x=measured,y=fitted))+theme_bw()
          }
          plt<-plt+geom_point()
          plt<-plt+labs(title="CV-Fitted vs. Measured Values", x="Measured",y ="Fitted")
          plt<-plt+theme(plot.title=element_text(face="bold",size="14",color="brown"))
          plt<-plt+ geom_abline(intercept=0,slope=1)
          print(plt)
    }
  })
  plsfitDS<-reactive({
    req(input$yselectyplsfit)
    plsobj<-plsdf()
    if (input$plsvalidation!=3)pred<-plsobj$validation$pred[,,as.numeric(input$plsslider_ncp)]
    if (input$plsvalidation==3)pred<-plsobj$fitted.values[,,as.numeric(input$plsslider_ncp)]
    mes<-plsobj$model[,1]
    if(input$yselectyplsfit=="All"){
      df<-data.frame(gather(as.data.frame(mes),key,measured),gather(as.data.frame(pred),key,fitted)[2])
      plt<-ggplot(df,aes(x=measured,y=fitted,colour=key))+theme_bw()
    }else{
      df<-data.frame(measured=mes[,input$yselectyplsfit],fitted=pred[,input$yselectyplsfit])
      plt<-ggplot(df,aes(x=measured,y=fitted))+theme_bw()
    }
    if((input$yselectyplsfit!="All")&(input$plscvspread==TRUE)&(input$plsvalidation!=3)){
      XN<-as.data.frame(plsobj$model[,-1])
      YN<-as.data.frame(plsobj$model[,1])
      YCV<-data.frame(index=NA,value=NA)
      for (j in 1:10){
            seg<-cvsegments(nrow(XN),input$plsegment,type="random")
            for (i in 1:input$plsegment){
                segi<-unlist(seg[i])
                plsobji<-oscorespls.fit(as.matrix(XN[-segi,]), as.matrix(YN[-segi,]),ncomp=input$plsslider_ncp)
                YP<-as.matrix(XN[segi,])%*%as.matrix(plsobji$coefficients[,input$yselectyplsfit,input$plsslider_ncp])
                YCV<-rbind(YCV,data.frame(index=segi,value=YP))
            }
      }
    }else{
      YCV=NULL
    }
    list(df=df,YCV=YCV)
  })
  output$plsfitdwnl<-downloadHandler(filename = "pls_fit.csv",
                                       content = function(file) {write.csv2(plsfitDS()$df,file,row.names = TRUE, sep=input$sep, dec=input$dec)}
  )
  output$yplscoeff<-renderUI({
    validate(need(nrow(values$DS)!=0, "You must load a DataSet"))
    selectInput("yselectplscoeff",label=tags$b("Response"),choices=input$plschooser_subcol$right,selected = 1)
  })
  output$plscoeff<-renderPlot({
    plt<-ggplot(plscoeffDS()$df,aes(x=Name,y=coeff))+theme_bw()
    plt<-plt+geom_bar(stat="identity", fill="steelblue")
    for(i in 3:ncol(plscoeffDS()$df)){
      plt<-plt+geom_point(data=plscoeffDS()$df[c(1,i)],aes_string(x="Name",y=names(plscoeffDS()$df)[i]))
    }
    plt<-plt+labs(title=paste("Coefficient of",input$yselectplscoeff,"vs. Variable"), x="Variables", y = "Coefficients")
    plt<-plt+theme(plot.title=element_text(face="bold",size="14", color="brown"),axis.text.x = element_text(angle = 90, hjust = 1),legend.position="none")
    if (as.logical(input$vplscoeff))plt<-plt+geom_text(aes(label=round(coeff,digits=2)), position=position_stack(vjust = 0.5))
    print(plt)
  })
  plscoeffDS<-reactive({
    req(input$yselectplscoeff)
    plsobj<-plsdf()
    coeff<-plsobj$coefficients
    coeff<-coeff[,,as.numeric(input$plsslider_ncp)]
    coeffcv<-plsobj$validation$coefficients[,input$yselectplscoeff,as.numeric(input$plsslider_ncp),]
    df<-data.frame(Name=row.names(coeff),coeff=coeff[,input$yselectplscoeff])
    nseg<-dim(coeffcv)[2]
    for(i in 1:nseg){df<-cbind(df,coeffcv[,i])}
    names(df)<-c("Name","coeff",paste('coeff',1:nseg,sep=''))
    list(df=df)
  })
  output$plscoeffdwnl<-downloadHandler(filename = "pls_coeff.csv",
                                       content = function(file) {write.csv2(plscoeffDS()$df,file,row.names=TRUE, sep=input$sep, dec=input$dec)}
  )
  output$yplspress<-renderUI({
    validate(need(nrow(values$DS)!=0, "You must load a DataSet"))
    selectInput("yselectyplspress",label=tags$b("Response"),choices=input$plschooser_subcol$right, selected = 1)
  })
  output$plspress<-renderPlot({
    df<-plspressDS()$df
    cutoff <- data.frame( x =input$plsslider_ncp, y =c(-Inf, Inf), cutoff = factor(input$plsslider_ncp) )
    plt<-ggplot(df,aes(x=Comp,y=Press))+theme_bw()
    plt<-plt+geom_line(aes( x, y, linetype = cutoff),colour="black", cutoff)
    plt<-plt+geom_point(colour = 'red', size = 3)+geom_line(colour = 'red')
    plt<-plt+scale_x_continuous(breaks = round(seq(min(df$Comp), max(df$Comp), by =1),1)) 
    plt<-plt+labs(title=paste("PRESS of Responce: ",input$yselectyplspress," vs. Components",sep=''), x="Components", y =paste(input$yselectyplspress,"PRESS", sep=' '))
    plt<-plt+theme(plot.title=element_text(face="bold",size="14", color="brown"),axis.text.x = element_text(angle = 90, hjust = 1),legend.position="none")
    print(plt)
  })
  plspressDS<-reactive({
    req(input$yselectyplspress)
    plsobj<-plsdf()
    press<-plsobj$validation$PRESS[input$yselectyplspress,]
    ncomp<-plsobj$validation$ncomp
    df<-data.frame(Comp=1:ncomp,Press=press)
    list(df=df)
  })
  output$plspressdwnl<-downloadHandler(filename = "pls_press.csv",
                                       content = function(file) {write.csv2(plspressDS()$df,file,row.names=TRUE, sep=input$sep, dec=input$dec)}
  )
  pcadf<-reactive({
    validate(need(nrow(values$DS)!=0, "You mut load a DataSet"))
    req(input$pcaslider_ncp)
    ncp<-as.numeric(input$pcaslider_ncp)
    center<-FALSE
    scale<-FALSE
    for (ck in input$ncpcheckGroup){
        if (ck=="1") center <- TRUE
        if (ck=="2") scale<-TRUE
    }
    df<-values$DS[values$rdf$left,values$cdf$left]
    df<-df[values$tdf$left,]
    XN<-scale(df, center=center, scale=scale)
    SSX<-apply(XN^2,2,sum)
    pcaobj<-nipals(x=XN,ncomp=ncp, center=center, scale=scale)
    TT<-pcaobj$scores %*% diag(pcaobj$eig)
    PP<-t(pcaobj$loadings)
    res<-XN-(TT %*% PP)
    MQ<-diag(ncp)-PP %*% t(PP)
    S<-diag(1/apply(TT,2,var))
    PS<-S %*% PP
    MT<-t(PS) %*% PP
    XMT<-XN%*%MT
    if (is.null(values$vargrp)){
      gr<-factor(rep(1,nrow(df)))
    }else{
      gr<-data.frame(grp=values$DS[values$rdf$left,values$vargrp])
      gr<-gr[values$tdf$left,]
    }
    pcaobj$gr<-gr
    pcaobj$HT2<-diag(XMT%*%t(XN))
    pcaobj$R<-res
    pcaobj$Q<-apply(res^2,1,sum)
    pcaobj$SSX<-SSX
    pcaobj$XN<-XN
    pcaobj$R2X<-(SSX-apply(res^2,2,sum))/SSX
    values$pcadf<-pcaobj
  })
  output$pcascores<-renderPlot({
    req(pcascoreDS)
    df<-pcascoreDS()$df
    R<-pcascoreDS()$R
    Name<-pcascoreDS()$Name
    nc<-pcascoreDS()$ncomp
    nr<-nrow(df)
    alf<-(100-as.numeric(input$alpsco))/100
    gr<-factor(pcadf()$gr)
    plt<-plot(df,type="n",xlab="",ylab="")
    if (as.logical(input$isosco)){
      par(pty="s")
      plt<-plot(df,type="n",asp=1,xlab="",ylab="")}
    for (i in 1:nlevels(gr)){
      dfi<-df[gr==levels(gr)[i],]
      coli<-rainbow(nlevels(gr))[i]
      points(dfi,type="p",pch=21,col=coli,bg=coli,cex=1.5)
      if (as.logical(input$ellsco))lines(ellipse_conf(dfi,alf),type="l",col=coli)
    }
    title(main=paste("Score Plot (",toString(sum(R)),"%)"),font.main="14",col.main="brown",font.main=2,adj=0.0,line=0.3)
    title(xlab=paste("PC",nc[1],"(",toString(R[1]),"%)"),ylab=paste("PC",nc[2],"(",toString(R[2]),"%)"),adj=0.5,line=2)
    if (as.logical(input$labsco))text(df$n1,df$n2,labels=Name,col="black",cex=1,pos=4)
    grid(col ="lightgray",lty ="dotted")
    # plt<-ggplot(df,aes(x=n1,y=n2))+geom_point()
    # plt<-plt+labs(title=paste("Score Plot (",toString(sum(R)),"%)"), x=paste("PC",nc[1],"(",toString(R[1]),"%)"),y=paste("PC",nc[2],"(",toString(R[2]),"%)"))
    # plt<-plt+theme(plot.title=element_text(face="bold",size="14",color="brown"),legend.position="none")
    # if (as.logical(input$labsco))plt<-plt+geom_text(aes(label=Name,colour="red"),hjust=0,nudge_x=0)
    # if (as.logical(input$ellsco))plt<-plt+stat_ellipse(type="norm",linetype=1,position="identity",level=(100-as.numeric(input$alpsco))/100)
    # if (as.logical(input$isosco))plt<-plt+ coord_fixed()
    print(plt)
  })
  observeEvent(input$pcascores_click, {
    res <- nearPoints(pcascoreDS()$df,input$pcascores_click,"n1","n2",allRows=FALSE)
    values$selobj<-c(row.names(res),values$selobj)
    values$selobj<-values$selobj[!values$selobj %in% values$selobj[duplicated(values$selobj)]]
    if(length(values$selobj)==0)values$selobj<-NULL
  })
  observeEvent(input$pcascores_brush, {
    res <- brushedPoints(pcascoreDS()$df,input$pcascores_brush,"n1","n2",allRows=FALSE)
    values$selobj<-c(row.names(res),values$selobj)
    values$selobj<-values$selobj[!values$selobj %in% values$selobj[duplicated(values$selobj)]]
    if(length(values$selobj)==0)values$selobj<-NULL
  })
  output$pcascores_clickinfo <- renderPrint({
    req(input$pcascores_click)
    values$selobj
  })
  pcascoreDS<-reactive({
    req(input$compxsconcp,input$compysconcp)
    n1<-as.numeric(input$compxsconcp)
    n2<-as.numeric(input$compysconcp)
    pcav<-round(pcadf()$R2[c(n1,n2)]*100, digits=1)
    Name<-row.names(as.data.frame(pcadf()$scores))
    df<-data.frame(n1=pcadf()$scores[,n1],n2=pcadf()$scores[,n2])
    list(df=df,ncomp=c(n1,n2),R=pcav,Name=Name)
  })
  output$pcascoredwnl<-downloadHandler(filename = "pca_scores.csv",
                                       content = function(file) {write.csv2(pcascoreDS()$df, file, row.names = TRUE, sep=input$sep, dec=input$dec)}
  )
  observeEvent(input$pcascorerem, {
    req(values$selobj)
    values$rdf$right<-c(values$rdf$right,values$selobj)
    values$rdf$left<-values$rdf$left[!values$rdf$left %in% values$selobj]
    values$tdf$left<-values$tdf$left[!values$tdf$left %in% values$selobj]
    values$selobj<-NULL
  })
  observeEvent(input$pcascoregrp, {
    req(values$selobj)
    if(is.null(values$vargrp)){
      gr<-data.frame(grp=rep("std",nrow(values$DS)))
      row.names(gr)<-row.names(values$DS)
      gr[values$selobj,"grp"]<-"grmk"
      values$DS<-cbind(values$DS,gr,deparse.level = 1)
      values$vargrp<-"grp"
    }else{
      gr<-data.frame(grp=values$DS[,values$vargrp])
      i <- sapply(gr, is.factor)
      gr[i] <- lapply(gr[i], as.character)
      row.names(gr)<-row.names(values$DS)
      gr[values$selobj,"grp"]<-"grmk"
      values$DS[,values$vargrp]<-factor(gr$grp)
    }
    values$selobj<-NULL
  })
  output$pcaloadings<-renderPlot({
    req(pcaloadingsDS)
    df<-pcaloadingsDS()$df
    R<-pcaloadingsDS()$R
    Name<-pcaloadingsDS()$Name
    nc<-pcaloadingsDS()$ncomp
    plt<-ggplot(df,aes(x=n1,y=n2))+geom_point()+theme_bw()
    if (as.logical(input$arrlod))plt<-plt+geom_segment(aes(x=0,y=0,xend = n1, yend =n2),colour="red", arrow = arrow(length = unit(0.5,"cm")))
    plt<-plt+labs(title=paste("Loading Plot (",toString(sum(R)),"%)"),x=paste("PC",nc[1],"(",toString(R[1]),"%)"),y=paste("PC",nc[2],"(",toString(R[2]),"%)"))
    plt<-plt+theme(plot.title=element_text(face="bold",size="14", color="brown"),legend.position="none")
    if (as.logical(input$lablod))plt<-plt+geom_text(aes(label=Name,colour="red"),hjust=0,nudge_x=0.01,size=5)
    if (as.logical(input$isolod))plt<-plt+ coord_fixed()
    print(plt)
  })
  output$pcaloddwnl<-downloadHandler(filename = "pca_loadings.csv",
    content = function(file) {write.csv2(pcaloadingsDS()$df, file, row.names = TRUE, sep=input$sep, dec=input$dec)}
  )
  pcaloadingsDS<-reactive({
    req(input$compxloncp,input$compyloncp)
    n1<-as.numeric(input$compxloncp)
    n2<-as.numeric(input$compyloncp)
    pcav<-round(pcadf()$R2[c(n1,n2)]*100, digits=1)
    Name<-row.names(as.data.frame(pcadf()$loadings))
    df<-data.frame(n1=pcadf()$loadings[,n1],n2=pcadf()$loadings[,n2])
    list(df=df,ncomp=c(n1,n2),R=pcav,Name=Name)
  })
  output$pcabiplt<-renderPlot({
    pcasco<-pcabipltDS()$df$score
    dxs<-apply(pcasco,2,max)[1]-apply(pcasco,2,min)[1]
    dys<-apply(pcasco,2,max)[2]-apply(pcasco,2,min)[2]
    pcalo<-pcabipltDS()$df$loading
    dxl<-apply(pcalo,2,max)[1]-apply(pcalo,2,min)[1]
    dyl<-apply(pcalo,2,max)[2]-apply(pcalo,2,min)[2]
    pcalo[,1]<-pcalo[,1]/dxl*dxs
    pcalo[,2]<-pcalo[,2]/dyl*dys
    DX<-c(min(apply(pcasco,2,min)[1],apply(pcalo,2,min)[1]),max(apply(pcasco,2,max)[1],apply(pcalo,2,max)[1]))
    DY<-c(min(apply(pcasco,2,min)[2],apply(pcalo,2,min)[2]),max(apply(pcasco,2,max)[2],apply(pcalo,2,max)[2]))
    R<-pcabipltDS()$R
    Name<-pcabipltDS()$Name
    nc<-pcabipltDS()$ncomp
    gr<-factor(pcabipltDS()$gr)
    alf<-(100-as.numeric(input$alpbi))/100
    plt<-plot(pcasco,type="n",xlab="",ylab="",xlim=DX,ylim=DY)
    if (as.logical(input$isobi)){
      par(pty="s")
      plt<-plot(pcasco,type="n",asp=1,xlab="",ylab="",xlim=c(min(DX,DY),max(DX,DY)),ylim=c(min(DX,DY),max(DX,DY)))}
    for (i in 1:nlevels(gr)){
      dfi<-pcasco[gr==levels(gr)[i],]
      coli<-rainbow(nlevels(gr))[i]
      points(dfi,type="p",pch=21,col=coli,bg=coli,cex=1.5)
      if (as.logical(input$ellbi))lines(ellipse_conf(dfi,alf),type = "l",col=coli)
    }
    points(pcalo,type="p",pch=21,col="black")
    arrows(0,0,pcalo[1:nrow(pcalo),1],pcalo[1:nrow(pcalo),2], angle = 15)
    title(main=paste("BiPlot (",toString(sum(R)),"%)"),font.main="14",col.main="brown",font.main=2,adj=0.0,line=0.3)
    title(xlab=paste("PC",nc[1],"(",toString(R[1]),"%)"),ylab=paste("PC",nc[2],"(",toString(R[2]),"%)"),adj=0.5,line=2)
    if (as.logical(input$labbi)){
      text(pcasco[,1],pcasco[,2],labels=Name$score,col="black",cex=1,pos=4)
      text(pcalo[,1],pcalo[,2],labels=Name$loading,col="black",cex=1,pos=4)
    }
    grid(col ="lightgray",lty ="dotted")
    print(plt)
    })
  pcabipltDS<-reactive({
    req(input$compxbincp,input$compybincp)
    n1<-as.numeric(input$compxbincp)
    n2<-as.numeric(input$compybincp)
    pcav<-round(pcadf()$R2[c(n1,n2)]*100, digits=1)
    Name<-list(score=row.names(as.data.frame(pcadf()$scores)),loading=row.names(as.data.frame(pcadf()$loadings)))
    df<-list(score=as.data.frame(pcadf()$scores[,c(n1,n2)]),loading=as.data.frame(pcadf()$loadings[,c(n1,n2)]))
    list(df=df,ncomp=c(n1,n2),R=pcav,Name=Name,gr=pcadf()$gr)
  })
  output$pcabidwnl<-downloadHandler(filename = "pca_biplot.csv",
     content=function(file){write.csv2(rbind(pcabipltDS()$df$score,pcabipltDS()$df$loading),file,row.names=TRUE,sep=input$sep, dec=input$dec)}
  )
  pcavarDS<-reactive({
    req(input$pcaslider_ncp)
    ncp<-input$pcaslider_ncp
    R2<-pcadf()$R2
    list(sing=data.frame(pcs=1:ncp,var=R2*100),cum=data.frame(pcs=1:ncp,var=cumsum(R2*100)))
  })
  output$pcavardwnl<-downloadHandler(filename = "pca_var.csv",
    content=function(file){write.csv2(rbind(pcavarDS()$sing,pcavarDS()$cum),file,row.names=TRUE,sep=input$sep,dec=input$dec)}
  )
  output$pcavarsing<-renderPlot({
    plt<-ggplot(pcavarDS()$sing,aes(x=pcs,y=var))+theme_bw()
    plt<-plt+geom_bar(stat="identity", fill="steelblue")
    plt<-plt+labs(title="% Explained Variance vs. Number of components", x="# Component", y = "% Explained Variance")
    plt<-plt+theme(plot.title=element_text(face="bold",size="14", color="brown"),legend.position="none",axis.text.x = element_text(angle = 90, hjust = 1))
    if (as.logical(input$labvar))plt<-plt+geom_text(aes(label=round(var,digits=2)), position=position_dodge(width=0.9), vjust=-0.25)
    print(plt)
  })
  output$pcavarcum<-renderPlot({
    plt<-ggplot(pcavarDS()$cum,aes(x=pcs,y=var))+theme_bw()
    plt<-plt+geom_bar(stat="identity", fill="red")
    plt<-plt+labs(title="% Cumulate Explained Variance vs. Number of components", x="# Component", y = "% Cumulate Explained Variance")
    plt<-plt+theme(plot.title=element_text(face="bold",size="14", color="brown"),legend.position="none",axis.text.x = element_text(angle = 90, hjust = 1)) 
    if (as.logical(input$labvar))plt<-plt+geom_text(aes(label=round(var,digits=2)), position=position_dodge(width=0.9), vjust=-0.25)
    print(plt)
  })
  pcaht2DS<-reactive({
    req(input$pcaslider_ncp)
    np<-length(pcadf()$HT2)
    df<-data.frame(obj=1:np,ht2=pcadf()$HT2)
    list(df=df,np=np,ncp=as.numeric(input$pcaslider_ncp),Name=row.names(pcadf()$scores),gr=pcadf()$gr)
  })
  output$pcaht2<-renderPlot({
    ncp<-pcaht2DS()$ncp
    Name<-pcaht2DS()$Name
    np<-pcaht2DS()$np
    gr<-factor(pcaht2DS()$gr)
    ngr<-nlevels(gr)
    alf<-1-input$alpht/100
    cf<-round((np-1)*ncp/(np-ncp)*sqrt(qf(alf,ncp,np-ncp)), digit=2)
    cutoff <- data.frame( x = c(-Inf, Inf), y =cf, cutoff = factor(cf) )
    plt<-ggplot(pcaht2DS()$df,aes(x=obj,y=ht2))+theme_bw()
    plt<-plt+geom_bar(stat="identity", fill=rainbow(ngr)[gr])
    plt<-plt+scale_x_discrete(limits=1:np,labels=Name)
    plt<-plt+labs(title=paste("Hoteling T2 - total components :",toString(ncp), sep=' '), x="Objects", y = "T2")
    plt<-plt+theme(plot.title=element_text(face="bold",size="14", color="brown"),axis.text.x = element_text(angle = 90, hjust = 1),legend.position="none")
    if (input$linht)plt<-plt+geom_line(aes( x, y, linetype = cutoff,color='red' ), cutoff)
    if (input$labht)plt<-plt+geom_text(aes(label=round(ht2,digits=2)), position=position_dodge(width=0.9), vjust=-0.25)
    print(plt)
  })
  output$pcaht2dwnl<-downloadHandler(filename = "pca_ht2.csv",
    content = function(file) {write.csv2(pcaht2DS()$df, file, row.names = TRUE, sep=input$sep, dec=input$dec)}
  )
  pcaQDS<-reactive({
    req(input$pcaslider_ncp)
    np<-length(pcadf()$Q)
    df<-data.frame(obj=1:np,Q=pcadf()$Q)
    list(df=df,np=np,ncp=as.numeric(input$pcaslider_ncp),Name=row.names(pcadf()$scores),gr=pcadf()$gr)
  })
  output$pcaQ<-renderPlot({
    ncp<-pcaQDS()$ncp
    np<-pcaQDS()$np
    alf<-1-input$alpQ/100
    gr<-factor(pcaQDS()$gr)
    ngr<-nlevels(gr)
    cf<-round((np-1)*ncp/(np-ncp)*sqrt(qf(alf,ncp,np-ncp)), digit=2)
    cutoff <- data.frame( x = c(-Inf, Inf), y =cf, cutoff = factor(cf) )
    plt<-ggplot(pcaQDS()$df,aes(x=obj,y=Q))+theme_bw()
    plt<-plt+geom_bar(stat="identity",fill=rainbow(ngr)[gr])
    plt<-plt+scale_x_discrete(limits=1:np,labels=pcaQDS()$Name)
    plt<-plt+labs(title=paste("Q, SPE - total components :",toString(ncp), sep=' '), x="Objects", y = "Q-SPE")
    plt<-plt+theme(plot.title=element_text(face="bold",size="14", color="brown"),axis.text.x = element_text(angle = 90, hjust = 1),legend.position="none")
    if (input$linQ)plt<-plt+geom_line(aes( x, y, linetype = cutoff,color='red' ), cutoff)
    if (input$labQ)plt<-plt+geom_text(aes(label=round(Q,digits=2)), position=position_dodge(width=0.9), vjust=-0.25)
    print(plt)
  })
  output$pcaQdwnl<-downloadHandler(filename = "pca_Q.csv",
    content = function(file) {write.csv2(pcaQDS()$df, file, row.names = TRUE, sep=input$sep, dec=input$dec)}
  )
  pcaresDS<-reactive({
    req(input$pcaslider_ncp)
    np<-length(pcadf()$R2X)
    df<-data.frame(variable=1:np,r2x=pcadf()$R2X)
    list(df=df,np=np,ncp=as.numeric(input$pcaslider_ncp),Name=row.names(pcadf()$loadings))
  })
  output$pcacres<-renderPlot({
    plt<-ggplot(pcaresDS()$df,aes(x=variable,y=r2x))+theme_bw()
    plt<-plt+geom_bar(stat="identity", fill="steelblue")
    plt<-plt+scale_x_discrete(limits=1:pcaresDS()$np,labels=pcaresDS()$Name)
    plt<-plt+labs(title=paste("Column Residuals - total components :",toString(pcaresDS()$ncp), sep=' '), x="Variables", y = "Column R2 over all components")
    plt<-plt+theme(plot.title=element_text(face="bold",size="14", color="brown"),axis.text.x = element_text(angle = 90, hjust = 1),legend.position="none")
    print(plt)
  })
  output$pcaresdwnl<-downloadHandler(filename = "pca_res.csv",
    content = function(file) {write.csv2(pcaresDS()$df, file, row.names = TRUE, sep=input$sep, dec=input$dec)}
  )
  output$pcaspe<-renderUI({
    sliderInput("sliderspe", label = tags$b("Object"), min = 1, max = nrow(pcadf()$scores),step=1, value = 1)
  })
  pcaspecDS<-reactive({
    req(input$sliderspe)
    ncp<-as.numeric(input$pcaslider_ncp)
    n1<-as.numeric(input$sliderspe)
    nc<-ncol(pcadf()$R)
    gr<-factor(pcadf()$gr)
    ngr<-nlevels(gr)
    df<-data.frame(obj=1:nc,spc=pcadf()$R[n1,])
    list(df=df,nobj=n1,nc=nc,Name=row.names(pcadf()$loadings),min_spc=min(pcadf()$R),max_spc=max(pcadf()$R),coli=rainbow(ngr)[gr[n1]])
  })
  output$pcaspec<-renderPlot({
    plt<-ggplot(pcaspecDS()$df,aes(x=obj,y=spc))+theme_bw()
    if (as.logical(input$limspec))plt<-plt+ylim(pcaspecDS()$min_spc,pcaspecDS()$max_spc)
    plt<-plt+geom_bar(stat="identity", fill=pcaspecDS()$coli)
    plt<-plt+scale_x_discrete(limits=1:pcaspecDS()$nc,labels=pcaspecDS()$Name)
    plt<-plt+labs(title=paste("Residual Plot for object :",toString(pcaspecDS()$nobj), sep=' '), x="Variables", y = "Variable Contribution on SPE")
    plt<-plt+theme(plot.title=element_text(face="bold",size="14", color="brown"),axis.text.x = element_text(angle = 90, hjust = 1),legend.position="none")
    if (input$labspec)plt<-plt+geom_text(aes(label=round(spc,digits=2)), position=position_dodge(width=0.9), vjust=-0.25)
    print(plt)
  })
  output$pcaspecdwnl<-downloadHandler(filename = "pca_SPEc.csv",
    content = function(file) {write.csv2(pcaspecDS()$df, file, row.names = TRUE, sep=input$sep, dec=input$dec)}
  )
  output$pcascoasl<-renderUI({
    sliderInput("sliderscoa", label = tags$b("Object"), min = 1, max = nrow(pcadf()$XN),step=1, value = 1)
  })
  output$pcascoavg<-renderPlot({
    req(input$compxscoavgncp,input$sliderscoa)
    ncp<-as.numeric(input$pcaslider_ncp)
    ncp1<-as.numeric(input$compxscoavgncp)
    n1<-as.numeric(input$sliderscoa)
    nc<-ncol(pcadf()$XN)
    XN<-pcadf()$XN
    Name<-row.names(pcadf()$loadings)
    PP<-t(pcadf()$loadings)
    Vx<-XN[n1,] * PP[ncp1,]
    gr<-factor(pcadf()$gr)
    ngr<-nlevels(gr)
    df<-data.frame(obj=1:nc,spc=Vx)
    plt<-ggplot(df,aes(x=obj,y=spc))+theme_bw()
    plt<-plt+geom_bar(stat="identity", fill=rainbow(ngr)[gr[n1]])+scale_x_discrete(limits=1:nc,labels=Name)
    plt<-plt+labs(title=paste("Score Contribution Plot for object :",toString(n1),"to the Average", sep=' '), x="Variables", y = "Variable Contribution on SPE")
    plt<-plt+theme(plot.title=element_text(face="bold",size="14", color="brown"),axis.text.x = element_text(angle = 90, hjust = 1),legend.position="none")
    if (as.logical(input$labscoavg))plt<-plt+geom_text(aes(label=round(spc,digits=2)), position=position_dodge(width=0.9), vjust=-0.25)
    print(plt)
  })
  output$pcascoosl<-renderUI({
    sliderInput("sliderscoo", label = tags$b("Object Range"), min = 1, max = nrow(pcadf()$XN),step=1, value = c(1,nrow(pcadf()$XN)))
  })
  output$pcascoo<-renderPlot({
    req(input$compxscooncp,input$sliderscoo[1],input$sliderscoo[2])
    ncp<-as.numeric(input$pcaslider_ncp)
    ncp1<-as.numeric(input$compxscooncp)
    n1<-as.numeric(input$sliderscoo[1])
    n2<-as.numeric(input$sliderscoo[2])
    XN<-pcadf()$XN
    nc<-ncol(XN)
    Name<-row.names(pcadf()$loadings)
    PP<-t(pcadf()$loadings)
    Vx<-(XN[n1,]-XN[n2,]) * PP[ncp1,]
    df<-data.frame(obj=1:nc,spc=Vx)
    plt<-ggplot(df,aes(x=obj,y=spc))+theme_bw()
    plt<-plt+geom_bar(stat="identity", fill="steelblue")
    plt<-plt+scale_x_discrete(limits=1:nc,labels=Name)
    plt<-plt+labs(title=paste("Score Contribution Plot for object :",toString(n1),"to object",toString(n2), sep=' '), x="Variables", y = "Variable Contribution on SPE")
    plt<-plt+theme(plot.title=element_text(face="bold",size="14", color="brown"),axis.text.x = element_text(angle = 90, hjust = 1),legend.position="none")
    if (as.logical(input$labscoo))plt<-plt+geom_text(aes(label=round(spc,digits=2)), position=position_dodge(width=0.9), vjust=-0.25)
    print(plt)
  })
  output$pcaadd<-renderPlot({
    req(values$tdf$right)
    df<-pcaaddDS()$df
    R<-pcaaddDS()$R
    Name<-pcaaddDS()$Name
    nc<-pcaaddDS()$ncomp
    plt<-ggplot(df,aes(x=n1,y=n2,color=typ))+geom_point()+ coord_fixed()+theme_bw()
    plt<-plt+labs(title=paste("Score Plot (",toString(sum(R)),"%)"), x=paste("PC",nc[1],"(",toString(R[1]),"%)"),y=paste("PC",nc[2],"(",toString(R[2]),"%)"))
    plt<-plt+theme(plot.title=element_text(face="bold",size="14",color="brown"),legend.position="right") 
    if (as.logical(input$labpcaadd))plt<-plt+geom_text(aes(label=Name),hjust=0,nudge_x=0.01)
    if (as.logical(input$ellpcaadd))plt<-plt+stat_ellipse(type="norm",linetype=1,position="identity",level=(100-as.numeric(input$alppcaadd))/100)
    print(plt)
  })
  pcaaddDS<-reactive({
    req(input$compxpcaaddncp,input$compypcaaddncp)
    n1<-as.numeric(input$compxpcaaddncp)
    n2<-as.numeric(input$compypcaaddncp)
    pcav<-round(pcadf()$R2[c(n1,n2)]*100, digits=1)
    df<-data.frame(n1=pcadf()$scores[,n1],n2=pcadf()$scores[,n2],typ=rep("Tr",nrow(pcadf()$scores)))
    xm<-attr(pcadf()$XN,"scaled:center")
    sd<-attr(pcadf()$XN,"scaled:scale")
    Xnew<-values$DS[values$rdf$left,values$cdf$left]
    Xnew<-Xnew[values$tdf$right,]
    Xnew<-Xnew-(as.matrix(rep(1,nrow(Xnew)),nrow(Xnew),1)%*% unlist(xm))
    Xnew<-Xnew/(as.matrix(rep(1,nrow(Xnew)),nrow(Xnew),1)%*% unlist(sd))
    sc<-as.matrix(Xnew) %*% as.matrix(pcadf()$loadings)
    df1<-data.frame(n1=sc[,n1],n2=sc[,n2],typ=rep("Ts",nrow(sc)))
    df<-rbind(df,df1)
    df$typ<-factor(df$typ)
    Name<-row.names(df)
    list(df=df,ncomp=c(n1,n2),R=pcav,Name=Name)
  })
  output$pcaadddwnl<-downloadHandler(filename="pca_add.csv",
    content = function(file) {write.csv2(pcaaddDS()$df,file,row.names=TRUE,sep=input$sec,dec=input$dec)}
  )
  output$pcasliderncp<-renderUI({
    sliderInput("pcaslider_ncp",label=tags$b("Number of Components"),min=2,max=min(length(values$tdf$left),length(values$cdf$left)),step=1,value=2)
  })
  output$plssliderncp<-renderUI({
    sliderInput("plsslider_ncp",label=tags$b("Number of Components"),min=2,max=min(length(values$tdf$left),length(values$cdf$left)),step=1,value=2)
  })
  output$compxsco<-renderUI({
    req(input$pcaslider_ncp)
    selectInput("compxsconcp",label=tags$b("Component on X"),choices=1:input$pcaslider_ncp,selected=1)
  })
  output$compysco<-renderUI({
    req(input$pcaslider_ncp)
    selectInput("compysconcp",label=tags$b("Component on Y"),choices=1:input$pcaslider_ncp,selected=2)
  })
  output$compxlo<-renderUI({
    req(input$pcaslider_ncp)
    selectInput("compxloncp",label=tags$b("Component on X"),choices=1:input$pcaslider_ncp,selected=1)
  })
  output$compylo<-renderUI({
    req(input$pcaslider_ncp)
    selectInput("compyloncp",label=tags$b("Component on Y"),choices=1:input$pcaslider_ncp,selected=2)
  })
  output$compxbi<-renderUI({
    req(input$pcaslider_ncp)
    selectInput("compxbincp",label=tags$b("Component on X"),choices=1:input$pcaslider_ncp,selected=1)
  })
  output$compybi<-renderUI({
    req(input$pcaslider_ncp)
    selectInput("compybincp",label=tags$b("Component on Y"),choices=1:input$pcaslider_ncp,selected=2)
  })
  output$compxscoo<-renderUI({
    req(input$pcaslider_ncp)
    selectInput("compxscooncp",label=tags$b("Component"),choices=1:input$pcaslider_ncp,selected=1)
  })
  output$compxscoavg<-renderUI({
    req(input$pcaslider_ncp)
    selectInput("compxscoavgncp",label=tags$b("Component"),choices=1:input$pcaslider_ncp,selected=1)
  })
  output$compxpcaadd<-renderUI({
    req(input$pcaslider_ncp)
    selectInput("compxpcaaddncp",label=tags$b("Component on X"),choices=1:input$pcaslider_ncp,selected=1)
  })
  output$compypcaadd<-renderUI({
    req(input$pcaslider_ncp)
    selectInput("compypcaaddncp",label=tags$b("Component on Y"),choices=1:input$pcaslider_ncp,selected=2)
  })
  output$viewvar<-renderUI({
    checkboxGroupInput("show_varview",tags$b("Columns to show:"),values$cdf$left,selected=values$cdf$left)
  })
  output$subcol<-renderUI({
    showNotification("I'm loading columns ...",duration=0.0002*ncol(values$DS))
    chooserInput("chooser_subcol",names(values$DS[,values$cdf$left]),values$cdf$right,size=20,multiple=TRUE)
  })
  output$subrow<-renderUI({
    showNotification("I'm loading rows ...",duration=0.0002*nrow(values$DS))
    chooserInput("chooser_subrow",row.names(values$DS[values$rdf$left,]),values$rdf$right,size=20,multiple=TRUE)
  })
  output$subtest<-renderUI({
    showNotification("I'm loading tests ...",duration=0.0002*nrow(values$DS))
    chooserInput("chooser_subtest",row.names(values$DS[values$rdf$left,]),c(),size=20,multiple=TRUE)
  })
  output$subgrp<-renderUI({
    if (is.null(values$vargrp)){
        selectInput("varsubgrp",label=tags$b("Variables"),choices =c("None",names(values$DS)),selected = 0)
    }else{
        selectInput("varsubgrp",label=tags$b("Variables"),choices =c("None",names(values$DS)),selected =values$vargrp)
    }
  })
  output$plssubcol <- renderUI({
    chooserInput("plschooser_subcol",values$cdf$left,c(),size=20,multiple=TRUE)
  })
  output$mysubcol<-renderPrint({
    req(input$chooser_subcol)
    input$chooser_subcol$right
  })
  observeEvent(input$varsubgrp,{
     if (input$varsubgrp!="None"){
            values$vargrp<-input$varsubgrp
            values$cdf$left<-values$cdf$left[values$cdf$left!=values$vargrp]
            values$cdf$right<-c(values$cdf$right,values$vargrp)
     }
   })
  output$mysubgrp<-renderPrint({
    req(input$varsubgrp)
    if (input$varsubgrp=="None"){
       values$vargrp<-NULL
      "There isn't any group"
    }else{
      paste(input$varsubgrp," is now the groupping variable",sep='')
    }
  })
  output$plsmysubcol<-renderPrint({
    req(input$plschooser_subcol)
    input$plschooser_subcol$right
  })
  observeEvent(input$chooser_subcol,{
    values$cdf<-input$chooser_subcol
  })
  output$mysubrow<-renderPrint({
    req(input$chooser_subrow)
    input$chooser_subrow$right
  })
  observeEvent(input$chooser_subrow,{
    values$rdf<-input$chooser_subrow
    values$tdf<-values$rdf
  })
  output$mysubtest<-renderPrint({
    req(input$chooser_subtest)
    input$chooser_subtest$right
  })
  observeEvent(input$chooser_subtest,{
    values$tdf<-input$chooser_subtest
  })
  tabsum<-reactive({
    ne<-dim(values$DS)[1]*dim(values$DS)[2]
    nas<-sum(is.na(values$DS))
    topic<-c("Number of Elements",
             "Number of Positives",
             "Number of Negatives",
             "Number of Null",
             "Number of NAs",
             "Number of Rows",
             "Number of Columns",
             "Number of Element in Training",
             "Number of Element in Test")
    value<-c(toString(ne),
             toString(sum(values$DS>0,na.rm = TRUE)),
             toString(sum(values$DS<0,na.rm = TRUE)),
             toString(sum(values$DS==0,na.rm = TRUE)),
             toString(nas),
             toString(nrow(values$DS)),
             toString(ncol(values$DS)),
             toString(length(values$tdf$left)),
             toString(length(values$tdf$right)))
    df<-data.frame(Topic=topic,Value=value)
  })
  output$viewsum<-({
    DT::renderDataTable({DT::datatable(tabsum(),caption=tags$b("Basic Information on DataSet"),filter='none',rownames=FALSE)})
  })
  observeEvent(input$file1,{
      req(input$file1)
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      req(input$rowindex)
      if (input$rowindex) {
        df <- read.csv(inFile$datapath,header = input$header,sep = input$sep,
                       row.names=1,quote = input$quote, dec=input$dec)
      }else{
        df <- read.csv(inFile$datapath,header = input$header,sep = input$sep,
                       quote = input$quote, dec=input$dec)
      }
      values$DS<-df
      values$rdf$left<-row.names(df)
      values$tdf$left<-values$rdf$left
      values$rdf$right<-NULL
      values$tdf$right<-NULL
      values$cdf$left<-names(df)
      values$cdf$right<-NULL
      values$selobj<-NULL
      values$pcadf<-NULL
      values$plsdf<-NULL
      values$vargrp<-NULL
  })
  output$loadvalue<-renderText({paste(as.character(dim(values$DS)[1]*dim(values$DS)[2]), " Values Loaded")})
  output$viewDS <-({
    DT::renderDataTable({
      DT::datatable(values$DS[values$rdf$left,values$cdf$left][values$tdf$left,input$show_varview, drop = FALSE], caption="Training Dataset with active Rows and Columns",options = list(scrollX = TRUE), rownames=FALSE)
    })
  })
  output$viewDStest<-({
    DT::renderDataTable({
      DT::datatable(values$DS[values$rdf$left,values$cdf$left][values$tdf$right,input$show_varview, drop = FALSE], caption="Test Dataset with active Rows and Columns",options = list(scrollX = TRUE), rownames=FALSE)
    })
  })
  output$trend_ui1 <- renderUI({
    req(input$trend_filter1)
    df<-values$DS[values$rdf$left,values$cdf$left]
    if (input$trend_filter1=="None") {
      return()
    }else{
      fases<-levels(factor(df[,input$trend_filter1]))
      "checkboxGroup" = checkboxGroupInput("trend_dynamic1", "Subsets",choices = fases,selected=fases)
    }
  })
  output$trend_ui2 <- renderUI({
    req(input$trend_filter2)
    df<-values$DS[values$rdf$left,values$cdf$left]
    if (input$trend_filter2=="None") {
      return()
    }else{
      fases<-levels(factor(df[,input$trend_filter2]))
      "checkboxGroup" = checkboxGroupInput("trend_dynamic2", "Subsets",choices = fases,selected=fases)
    }
  })
  output$trendvar <- renderUI({
      selectInput("trend_variable", "Variables:",choices=c("None",values$cdf$left))
  })
  output$trendgroup <- renderUI({
      if(is.null(values$vargrp)){
        selectInput("trend_group", "Group by:",choices=c("None",values$cdf$left),selected=0)
      }else{
          selectInput("trend_group", "Group by:",choices=c("None",values$vargrp,values$cdf$left),selected=values$vargrp)
      }
  })
  output$trendcolor <- renderUI({
      selectInput("trend_color", "Colored by:",choices=c("None",values$cdf$left))
  })
  output$trendfilter1 <- renderUI({
    selectInput("trend_filter1", "Filtered by:",choices=c("None",values$cdf$left))
  })
  output$trendfilter2<- renderUI({
    selectInput("trend_filter2", "Filtered by:",choices=c("None",values$cdf$left))
  })
  output$trends<-renderPlot({
    req(input$trend_variable)
    if(input$trend_variable!="None"){
      df<-trendDS()$df
      if (input$trend_group=="None") {
        plot(df[,input$trend_variable],main=input$trend_variable,ylab=input$trend_variable,xlab="Index")
      }else{
        if(input$trend_color=="None"){
          groups<-trendDS()$gr
          plot(df$time,df$var,type='n',xlab="Time Index",ylab=input$trend_variable)
          for (gr in groups){
            lines(df$time[df$batch==gr],df$var[df$batch==gr])
            points(df$time[df$batch==gr],df$var[df$batch==gr])
          }
        }else{  
          groups<-trendDS()$gr
          ncl<-nlevels(df$color)
          plot(df$time,df$var,type='n',xlab="Time Index",ylab=input$trend_variable)
          for (gr in groups){
            lines(df$time[df$batch==gr],df$var[df$batch==gr],col=rainbow(ncl)[df$color[df$batch==gr]])
            points(df$time[df$batch==gr],df$var[df$batch==gr],col=rainbow(ncl)[df$color[df$batch==gr]])
          }
        }
        grid()
        for (pt in values$seltrd){
          text(df[pt,"time"],df[pt,"var"],labels=df[pt,"batch"],pos=4,offset=1)
        }
      }
    }  
  })
  trendDS<-reactive({
    req(input$trend_variable)
    if(input$trend_variable!="None"){
          df<-values$DS[values$rdf$left,values$cdf$left]
          if(!is.null(values$vargrp)){
              df<-cbind(values$DS[values$vargrp],df)
          }
          req(input$trend_variable,input$trend_group,input$trend_color)
          ip1<-input$trend_filter1 # variable definition for filter 1
          ip2<-input$trend_filter2 # variable definition for filter 2
          d1<-input$trend_dynamic1 # set of values for filter 1
          d2<-input$trend_dynamic2 # set of values for filter 2
          if (ip1=="None" & ip2=="None" ) {
            dfo<-df
          }else{
            if( is.null(d1) & is.null(d2) ){
                  dfo<-df
            }else if (!is.null(d1) & is.null(d2) ){
                  vft<-factor(df[,ip1])
                  df[,ip1]<-vft
                  filters<-d1
                  dfo<-df[vft==filters[1],]
                  for (fti in filters[-1]){
                    dfo<-bind_rows(dfo,df[vft==fti,])
                  }
            }else if (is.null(d1) & !is.null(d2) ){
                  vft<-factor(df[,ip2])
                  df[,ip2]<-vft
                  filters<-d2
                  dfo<-df[vft==filters[1],]
                  for (fti in filters[-1]){
                    dfo<-bind_rows(dfo,df[vft==fti,])
                  }
            }else if (!is.null(d1) & !is.null(d2) ){
                  df[,ip1]<-factor(df[,ip1])
                  df[,ip2]<-factor(df[,ip2])
                  filters<-expand.grid(d1,d2,stringsAsFactors = FALSE)
                  dfo<-df[df[,ip1]==filters[1,1]&df[,ip2]==filters[1,2],]
                  if (nrow(filters)>=2){
                      for (i in 2:nrow(filters)){
                         dfo<-bind_rows(dfo,df[df[,ip1]==filters[i,1]&df[,ip2]==filters[i,2],])
                      }
                  }
            }
          }
          if (input$trend_group=="None") {
                list(df=dfo)
          }else{
                if(input$trend_color=="None"){
                      dfo[input$trend_group]<-factor(dfo[input$trend_group][,1])
                      groups<-levels(dfo[input$trend_group][,1])
                      ngr<-length(groups)
                      dft<-data.frame(time=c(),var=c(),batch=c())
                      for (gr in groups){
                        dfg<-dfo[dfo[input$trend_group]==gr,]
                        dfg1<-data.frame(time=1:nrow(dfg),var=dfg[,input$trend_variable],batch=dfg[input$trend_group])
                        row.names(dfg1)<-row.names(dfg)
                        dfg1<-dfg1[!(rowSums(is.na(dfg1))),]
                        dft<-bind_rows(dft,dfg1)
                      }
                      names(dft)<-c("time","var","batch")
                      list(df=dft,gr=groups)
                }else{
                      dfo[input$trend_group]<-factor(dfo[input$trend_group][,1])
                      groups<-levels(dfo[input$trend_group][,1])
                      dfo[input$trend_color]<-factor(dfo[input$trend_color][,1])
                      colors<-levels(dfo[input$trend_color][,1])
                      dft<-data.frame(time=c(),var=c(),batch=c(),color=c())
                      for (gr in groups){
                        dfg<-dfo[dfo[input$trend_group]==gr,]
                        dfg1<-data.frame(time=1:nrow(dfg),var=dfg[,input$trend_variable],batch=dfg[input$trend_group],color=dfg[input$trend_color])
                        row.names(dfg1)<-row.names(dfg)
                        dfg1<-dfg1[!(rowSums(is.na(dfg1))),]
                        dft<-bind_rows(dft,dfg1)
                      }
                      names(dft)<-c("time","var","batch","color")
                      list(df=dft,gr=groups)
                }
        }
    }
  })
  observeEvent(input$trenddelb,{
    req(values$seltrd)
    values$seltrd<-NULL
  })
  output$trendexct<-downloadHandler(filename = "trend_extract.csv",
                                       content = function(file) {write.csv2(values$DS[values$rowsel,],file,row.names=TRUE,sep=input$sep, dec=input$dec)}
  )
  observeEvent(input$trend_brush,{
    res <- brushedPoints(trendDS()$df,input$trend_brush,"time","var",allRows=FALSE)
    values$rowsel<-row.names(res)
  })
  observeEvent(input$trend_click,{
    req(input$trend_click)
    res <- nearPoints(trendDS()$df,input$trend_click,"time","var",allRows=FALSE)
    values$seltrd<-c(values$seltrd,row.names(res)[1])
  })
  output$d2varx<-renderUI({
    selectInput("d2x", "Variable on X:",choices=values$cdf$left,selected=values$cdf$left[1])
  })
  output$d2vary<-renderUI({
    selectInput("d2y", "Variable on Y:",choices=values$cdf$left,selected=values$cdf$left[2])
  })
  output$d2plot<-renderPlot({
    req(input$d2x,input$d2y)
    df<-values$DS[values$rdf$left,values$cdf$left]
    if (!is.null(values$vargrp)){
      gr<-factor(values$DS[values$rdf$left,values$vargrp])
    } else{
      gr<-factor(rep(1,nrow(df)))
    }
    df<-as.data.frame(df[,c(input$d2x,input$d2y)])
    names(df)<-c("x","y")
    plot(df$x,df$y,type='p',pch=19,col="blue",cex=2,xlab=input$d2x,ylab=input$d2y)
    if(input$d2values)text(df$x,df$y,row.names(df),pos=4,offset=1,col="red")
    grid()
  })
  output$d3varx<-renderUI({
    selectInput("d3x", "Variable on X:",choices=values$cdf$left,selected=values$cdf$left[1])
  })
  output$d3vary<-renderUI({
    selectInput("d3y", "Variable on Y:",choices=values$cdf$left,selected=values$cdf$left[2])
  })
  output$d3varz<-renderUI({
    selectInput("d3z", "Variable on Z:",choices=values$cdf$left,selected=values$cdf$left[3])
  })
  output$d3plt<-renderRglwidget({
    req(input$d3x,input$d3y,input$d3z)
    df<-values$DS[values$rdf$left,values$cdf$left]
    if (!is.null(values$vargrp)){
      gr<-factor(values$DS[values$rdf$left,values$vargrp])
    } else{
      gr<-factor(rep(1,nrow(df)))
    }
    rgl.open(useNULL=TRUE)
    scatter3d(x=df[,input$d3x],y=df[,input$d3y],z=df[,input$d3z],point.col="blue",surface=input$d3surf,
    xlab=input$d3x,ylab=input$d3y,zlab=input$d3z,groups=gr,axis.ticks=TRUE,ticktype = "detailed")
    rglwidget()
  })    
  output$d3contx<-renderUI({
    selectInput("d3cx", "Variable on X:",choices=values$cdf$left,selected=values$cdf$left[1])
  })
  output$d3conty<-renderUI({
    selectInput("d3cy", "Variable on Y:",choices=values$cdf$left,selected=values$cdf$left[2])
  })
  output$d3contz<-renderUI({
    selectInput("d3cz", "Variable on Z:",choices=values$cdf$left,selected=values$cdf$left[3])
  })
  output$d3contour<-renderPlot({
    req(input$d3cx,input$d3cy,input$d3cz)
    df<-values$DS[values$rdf$left,values$cdf$left]
    if (!is.null(values$vargrp)){
      gr<-factor(values$DS[values$rdf$left,values$vargrp])
    } else{
      gr<-factor(rep(1,nrow(df)))
    }
    df<-df[,c(input$d3cx,input$d3cy,input$d3cz)]
    names(df)<-c("x","y","z")
    df.cnt<-getContourLines(df,nlevels=as.numeric(input$nlev))
    plt<-ggplot(data=df.cnt,aes(x,y,group=Group,colour=z)) + geom_path() + theme_bw()
    plt<-plt+labs(title=paste("Contour Plot of",input$d3cz,sep=" "), x=input$d3cx, y = input$d3cy)
    plt<-plt+theme(plot.title=element_text(face="bold",size="14", color="brown"),axis.text.x = element_text(angle = 90, hjust = 1))
    print(plt)
  })
  output$selvar<-renderUI({
    checkboxGroupInput("selvarbox",tags$b("Inluded Variables:"),values$cdf$left,selected=NULL)
  })
  output$boxplot<-renderPlot({
    req(input$selvarbox)
    df<-values$DS[values$rdf$left,values$cdf$left]
    df<-as.data.frame(df[,input$selvarbox])
    names(df)<-input$selvarbox
    plt<-boxplot(df, notch=TRUE, col=(c("gold","darkgreen")))
    grid()
    print(plt)
  })
  output$hlp_load   <-renderUI({includeHTML("hlp_load.html")})
  output$hlp_valcol <-renderUI({includeHTML("hlp_valcol.html")})
  output$hlp_valrow <-renderUI({includeHTML("hlp_valrow.html")})
  output$hlp_selgrp <-renderUI({includeHTML("hlp_selgrp.html")})
  output$hlp_valtest<-renderUI({includeHTML("hlp_valtest.html")})
  output$hlp_view<-renderUI({includeHTML("hlp_view.html")})
  output$hlp_summary<-renderUI({includeHTML("hlp_summary.html")})
} # end of server
