####FUNCTION---FUNCTION---FUNCTION---FUNCTION---FUNCTION---FUNCTION---FUNCTION---FUNCTION---FUNCTION---FUNCTIO##
#compare 1 mean 1 side##########################################################################################
################################################################################################################
onemean1<-function(alpha,power,u,u0,S){
  Za<-qnorm(1-alpha)
  Zb<-qnorm(power)
  B<-(Za+Zb)^2
  N<-ceiling(S^2*B/(u-u0)^2)
  y<-data.frame(N)
  return(y)
}

#compare 1 mean 2 sides#########################################################################################
################################################################################################################
onemean2<-function(alpha,power,u,u0,S){
  Za<-qnorm(1-alpha/2)
  Zb<-qnorm(power)
  B<-(Za+Zb)^2
  N<-ceiling(S^2*B/(u-u0)^2)
  y<-data.frame(N)
  return(y)
}

#compare 1 mean Non-inferiority#################################################################################
################################################################################################################
onemeannoninfer<-function(alpha,power,u,u0,delta,S){
  Za<-qnorm(1-alpha) 
  Zb<-qnorm(power)
  B<-(Za+Zb)^2
  C<-u-u0-delta
  Ni<-S^2*B/C^2
  N<-ceiling(S^2*B/C^2)
  y<-data.frame(N)
  return(y)
}

#compare 1 mean equivalence#####################################################################################
################################################################################################################
onemeanequ<-function(alpha,power,u,u0,delta,S){
  Za<-qnorm(1-alpha) 
  Zb<-qnorm(power/2+1/2)
  B<-(Za+Zb)^2
  C<-delta-abs(u-u0)
  N<-ceiling(S^2*B/C^2)
  y<-data.frame(N)
  return(y)
}

#compare 1 proportion 1 side####################################################################################
################################################################################################################
oneproportion1<-function(alpha,power,P,P0){
  Za<-qnorm(1-alpha)
  Zb<-qnorm(power)
  B<-(Za+Zb)^2
  A<-P*(1-P)
  C<-P-P0
  N<-ceiling(A*B/C^2)
  y<-data.frame(N)
  return(y)
}

#compare 1 proportion 2 sides###################################################################################
################################################################################################################
oneproportion2<-function(alpha,power,P,P0){
  Za<-qnorm(1-alpha/2) 
  Zb<-qnorm(power)
  B<-(Za+Zb)^2
  A<-P*(1-P)
  C<-P-P0
  N<-ceiling(A*B/C^2)
  y<-data.frame(N)
  return(y)
}

#compare 1 proportion Non-inferiority###########################################################################
################################################################################################################

oneporpnon<-function(alpha,power,P,P0,delta){
  Za<-qnorm(1-alpha)
  Zb<-qnorm(power)
  A<-P*(1-P)
  B<-(Za+Zb)^2
  C<-P-P0-delta
  N<-ceiling(A*B/C^2)
  y<-data.frame(N)
  return(y)
}

#compare 1 proportion equivalence###############################################################################
################################################################################################################
oneporpequ<-function(alpha,power,P,P0,delta){
  Za<-qnorm(1-alpha)
  Zb<-qnorm(power/2+1/2)
  B<-(Za+Zb)^2
  A<-P*(1-P)
  C<-abs(P-P0)-delta
  N<-ceiling(A*B/C^2)
  y<-data.frame(N)
  return(y)
  
}
#compare 2 means 1 side#########################################################################################
################################################################################################################
twomeans1<-function(alpha,power,uA,uB,sdA,sdB,k){
  ######k=nA/nB
  Za<-qnorm(1-alpha)
  Zb<-qnorm(power)
  Z<-(Za+Zb)^2
  nB<-ceiling((sdB^2+sdA^2/k)*Z/(uA-uB)^2)
  nB0<-(sdB^2+sdA^2/k)*Z/(uA-uB)^2
  nA<-ceiling(k*nB0)
  y<-data.frame(nA,nB)
  return(y)
}
#compare 2 means 2 sides########################################################################################
################################################################################################################
twomeans2<-function(alpha,power,uA,uB,sd,k){
  Za<-qnorm(1-alpha/2)
  Zb<-qnorm(power)
  Z<-(Za+Zb)^2
  nB<-ceiling((1+1/k)*sd^2*Z/(uA-uB)^2)
  nB0<-(1+1/k)*sd^2*Z/(uA-uB)^2
  nA<-ceiling(k*nB0)
  y<-data.frame(nA,nB)
  return(y)
}
#compare 2 means Non-inferiority################################################################################
################################################################################################################
twomeansnoninfer<-function(alpha,power,uA,uB,delta,sd,k){
  ###k=nA/nB
  Za<-qnorm(1-alpha)
  Zb<-qnorm(power)
  Z<-(Za+Zb)^2
  nB<-ceiling((1+1/k)*sd^2*Z/(uA-uB-delta)^2)
  nB0<-(1+1/k)*sd^2*Z/(uA-uB-delta)^2
  nA<-ceiling(k*nB0)
  y<-data.frame(nA,nB)
  return(y)
}
#compare 2 means equivalence####################################################################################
################################################################################################################
twomeansequ<-function(alpha,power,uA,uB,sd,delta,k){
  Za<-qnorm(1-alpha)
  Zb<-qnorm(power/2+1/2)
  Z<-(Za+Zb)^2
  nB<-ceiling((1+1/k)*sd^2*Z/(abs(uA-uB)-delta)^2)
  nB0<-(1+1/k)*sd^2*Z/(abs(uA-uB)-delta)^2
  nA<-ceiling(k*nB0)
  y<-data.frame(nA,nB)
  return(y)
}
#compare 2 proportions 1 side###################################################################################
################################################################################################################
twop1<-function(alpha,power,pA,pB,k){
  Za<-qnorm(1-alpha)
  Zb<-qnorm(power)
  Z<-(Za+Zb)^2
  nB<-ceiling((pA*(1-pA)/k+pB*(1-pB))*Z/(pA-pB)^2)
  nB0<-(pA*(1-pA)/k+pB*(1-pB))*Z/(pA-pB)^2
  nA<-ceiling(k*nB0)
  y<-data.frame(nA,nB)
  return(y)
}
#compare 2 proportions 2 sides##################################################################################
################################################################################################################
twop2<-function(alpha,power,pA,pB,k){
  Za<-qnorm(1-alpha/2)
  Zb<-qnorm(power)
  Z<-(Za+Zb)^2
  nB<-ceiling((pA*(1-pA)/k+pB*(1-pB))*Z/(pA-pB)^2)
  nB0<-(pA*(1-pA)/k+pB*(1-pB))*Z/(pA-pB)^2
  nA<-ceiling(k*nB0)
  y<-data.frame(nA,nB)
  return(y)
}
#compare 2 proportion Non-inferiority###########################################################################
################################################################################################################
twopnoninfer<-function(alpha,power,pA,pB,delta,k){
  Za<-qnorm(1-alpha)
  Zb<-qnorm(power)
  Z<-(Za+Zb)^2
  nB<-ceiling((pA*(1-pA)/k+pB*(1-pB))*Z/(pA-pB-delta)^2)
  nB0<-(pA*(1-pA)/k+pB*(1-pB))*Z/(pA-pB-delta)^2
  nA<-ceiling(k*nB0)
  y<-data.frame(nA,nB)
  return(y)
}
#compare 2 proportion equivalence###############################################################################
################################################################################################################
twopequ<-function(alpha,power,pA,pB,delta,k){
  Za<-qnorm(1-alpha)
  Zb<-qnorm(power/2+1/2)
  Z<-(Za+Zb)^2
  nB<-ceiling((pA*(1-pA)/k+pB*(1-pB))*Z/(abs(pA-pB)-delta)^2)
  nB0<-(pA*(1-pA)/k+pB*(1-pB))*Z/(abs(pA-pB)-delta)^2
  nA<-ceiling(k*nB0)
  y<-data.frame(nA,nB)
  return(y)
}
#compare k means 1 side#########################################################################################
################################################################################################################
kmean1side<-function(alpha,power,uA,uB,sdA,sdB,k,tau){
  nB0=(sdA^2+sdB^2/k)*((qnorm(1-alpha/tau)+qnorm(power))/(uA-uB))^2
  nB=ceiling((sdA^2+sdB^2/k)*((qnorm(1-alpha/tau)+qnorm(power))/(uA-uB))^2)
  nA<-ceiling(k*nB0)
  y<-data.frame(nA,nB)
  return(y)
}

#compare k means 2 sides########################################################################################
################################################################################################################
kmean2side<-function(alpha,power,uA,uB,sd,tau){
  n=ceiling(2*(sd*(qnorm(1-alpha/(2/tau))+qnorm(power))/(uA-uB))^2)
  y<-data.frame(n)
  return(y)
}
#compare k proportions##########################################################################################
################################################################################################################
 kp<-function(alpha,power,pA,pB,tau){
  n=ceiling((pA*(1-pA)+pB*(1-pB))*((qnorm(1-alpha/2/tau)+qnorm(power))/(pA-pB))^2)
  y<-data.frame(n)
  return(y)
}
################################################################################################################
####FUNCTION---FUNCTION---FUNCTION---FUNCTION---FUNCTION---FUNCTION---FUNCTION---FUNCTION---FUNCTION---FUNCTIO## 
################################################################################################################
shinyServer <- function(input, output){
#renderUI#######################################################################################################
################################################################################################################
  output$ui<-renderUI(
    if(input$num=="one sample"&input$type=="mean"){
      switch(
        input$subtype,
        "one side"=div( 
          helpText(tags$small("第一类错误")),
          numericInput(inputId = "alpha",label = "Alpha=",value =0.050,min=0,max=1,step = 0.01 ),
          helpText(tags$small("检验功效")),
          numericInput(inputId = "power",label ="Power=",value = 0.800,min=0,max = 1,step = 0.01),
          helpText(tags$small("样本均值")),
          numericInput(inputId = "u",label ="u=",value = 2,min=0,max = 1,step = 0.1),
          helpText(tags$small("假设均值")),
          numericInput(inputId = "u0",label ="u0=",value = 1.5,min=0,max = 1,step = 0.1),
          helpText(tags$small("标准差")),
          numericInput(inputId = "S",label = "S=",value = 1,min=NA,max = NA)
                      ),
        "two sides"=div( 
          helpText(tags$small("第一类错误")),
          numericInput(inputId = "alpha",label = "Alpha=",value =0.050,min=0,max=1,step = 0.01 ),
          helpText(tags$small("检验功效")),
          numericInput(inputId = "power",label ="Power=",value = 0.800,min=0,max = 1,step = 0.01),
          helpText(tags$small("样本均值")),
          numericInput(inputId = "u",label ="u=",value = 2,min=0,max = 1,step = 0.1),
          helpText(tags$small("假设均值")),
          numericInput(inputId = "u0",label ="u0=",value = 1.5,min=0,max = 1,step = 0.1),
          helpText(tags$small("标准差")),
          numericInput(inputId = "S",label = "S=",value = 1,min=NA,max = NA)
                      ),
        "non-infer"=div( 
          helpText(tags$small("第一类错误")),
          numericInput(inputId = "alpha",label = "Alpha=",value =0.050,min=0,max=1,step = 0.01 ),
          helpText(tags$small("检验功效")),
          numericInput(inputId = "power",label ="Power=",value = 0.800,min=0,max = 1,step = 0.01),
          helpText(tags$small("样本均值")),
          numericInput(inputId = "u",label ="u=",value = 2,min=0,max = 1,step = 0.1),
          helpText(tags$small("假设均值")),
          numericInput(inputId = "u0",label ="u0=",value = 1.500,min=0,max = 1,step = 0.1),
          helpText(tags$small("边界值")),
          numericInput(inputId = "delta",label ="delta=",value = -0.500,min=0,max = 1,step = 0.1),
          helpText(tags$small("标准差")),
          numericInput(inputId = "S",label = "S=",value = 1,min=NA,max = NA)
                      ),
        "equivalence"=div( 
          helpText(tags$small("第一类错误")),
          numericInput(inputId = "alpha",label = "Alpha=",value =0.050,min=0,max=1,step = 0.01 ),
          helpText(tags$small("检验功效")),
          numericInput(inputId = "power",label ="Power=",value = 0.800,min=0,max = 1,step = 0.01),
          helpText(tags$small("样本均值")),
          numericInput(inputId = "u",label ="u=",value = 2,min=0,max = 1,step = 0.1),
          helpText(tags$small("假设均值")),
          numericInput(inputId = "u0",label ="u0=",value = 2.00,min=0,max = 1,step = 0.1),
          helpText(tags$small("边界值")),
          numericInput(inputId = "delta",label ="delta=",value = 0.0500,min=0,max = 1,step = 0.1),
          helpText(tags$small("标准差")),
          numericInput(inputId = "S",label = "S=",value = 0.1,min=NA,max = NA)
                        )
             )
      
    }
    else if(input$num=="one sample"&input$type=="proportion"){
      switch(
        input$subtype,
        "one side"=div( 
          helpText(tags$small("第一类错误")),
          numericInput(inputId = "alpha",label = "Alpha=",value =0.050,min=0,max=1,step = 0.01 ),
          helpText(tags$small("检验功效")),
          numericInput(inputId = "power",label ="Power=",value = 0.800,min=0,max = 1,step = 0.01),
          helpText(tags$small("样本比例")),
          numericInput(inputId = "P",label ="P=",value = 0.500,min=0,max = 1,step = 0.1),
          helpText(tags$small("假设比例")),
          numericInput(inputId = "P0",label ="P0=",value = 0.300,min=0,max = 1,step = 0.1)
        ),
        "two sides"=div( 
          helpText(tags$small("第一类错误")),
          numericInput(inputId = "alpha",label = "Alpha=",value =0.050,min=0,max=1,step = 0.01 ),
          helpText(tags$small("检验功效")),
          numericInput(inputId = "power",label ="Power=",value = 0.800,min=0,max = 1,step = 0.01),
          helpText(tags$small("样本比例")),
          numericInput(inputId = "P",label ="P=",value = 0.500,min=0,max = 1,step = 0.1),
          helpText(tags$small("假设比例")),
          numericInput(inputId = "P0",label ="P0=",value = 0.300,min=0,max = 1,step = 0.1)
                      ),
        "non-infer"=div( 
          helpText(tags$small("第一类错误")),
          numericInput(inputId = "alpha",label = "Alpha=",value =0.050,min=0,max=1,step = 0.01 ),
          helpText(tags$small("检验功效")),
          numericInput(inputId = "power",label ="Power=",value = 0.800,min=0,max = 1,step = 0.01),
          helpText(tags$small("P")),
          numericInput(inputId = "P",label ="P=",value = 0.500,min=0,max = 1,step = 0.1),
          helpText(tags$small("假设均值")),
          numericInput(inputId = "P0",label ="P0=",value = 0.300,min=0,max = 1,step = 0.1),
          helpText(tags$small("边界值")),
          numericInput(inputId = "delta",label ="delta=",value = -0.100,min=0,max = 1,step = 0.1)
                      ),
        "equivalence"=div( 
          helpText(tags$small("第一类错误")),
          numericInput(inputId = "alpha",label = "Alpha=",value =0.050,min=0,max=1,step = 0.01 ),
          helpText(tags$small("检验功效")),
          numericInput(inputId = "power",label ="Power=",value = 0.800,min=0,max = 1,step = 0.01),
          helpText(tags$small("P")),
          numericInput(inputId = "P",label ="P=",value = 0.600,min=0,max = 1,step = 0.1),
          helpText(tags$small("假设均值")),
          numericInput(inputId = "P0",label ="P0=",value = 0.600,min=0,max = 1,step = 0.1),
          helpText(tags$small("边界值")),
          numericInput(inputId = "delta",label ="delta=",value = 0.200,min=0,max = 1,step = 0.1)
                        )
        
        
        
      )
    }
    else if(input$num=="two samples"&input$type=="mean"){
      switch(
        input$subtype,
        "one side"=div( 
          helpText(tags$small("第一类错误")),
          numericInput(inputId = "alpha",label = "Alpha=",value =0.050,min=0,max=1,step = 0.01 ),
          helpText(tags$small("检验功效")),
          numericInput(inputId = "power",label ="Power=",value = 0.800,min=0,max = 1,step = 0.01),
          helpText(tags$small("A组均值")),
          numericInput(inputId = "uA",label ="uA=",value = 0.500,min=0,max = 1,step = 0.1),
          helpText(tags$small("B组均值")),
          numericInput(inputId = "uB",label ="uB=",value = 0.600,min=0,max = 1,step = 0.1),
          helpText(tags$small("组A标准差")),
          numericInput(inputId = "sdA",label = "sdA=",value = 1,min=NA,max = NA),
          helpText(tags$small("组B标准差")),
          numericInput(inputId = "sdB",label = "sdB=",value = 1,min=NA,max = NA),
          helpText(tags$small("组A/组B")),
          numericInput(inputId = "k",label = "k=",value = 1,min=NA,max = NA)
        ),
        "two sides"=div( 
          helpText(tags$small("第一类错误")),
          numericInput(inputId = "alpha",label = "Alpha=",value =0.050,min=0,max=1,step = 0.01 ),
          helpText(tags$small("检验功效")),
          numericInput(inputId = "power",label ="Power=",value = 0.800,min=0,max = 1,step = 0.01),
          helpText(tags$small("A组均值")),
          numericInput(inputId = "uA",label ="uA=",value = 0.5,min=0,max = 1,step = 0.1),
          helpText(tags$small("B组均值")),
          numericInput(inputId = "uB",label ="uB=",value = 0.6,min=0,max = 1,step = 0.1),
          helpText(tags$small("总体标准差")),
          numericInput(inputId = "sd",label = "sd=",value = 1,min=NA,max = NA),
          helpText(tags$small("组A/组B")),
          numericInput(inputId = "k",label = "k=",value = 1,min=NA,max = NA)
        ),
        "non-infer"=div( 
          helpText(tags$small("第一类错误")),
          numericInput(inputId = "alpha",label = "Alpha=",value =0.050,min=0,max=1,step = 0.01 ),
          helpText(tags$small("检验功效")),
          numericInput(inputId = "power",label ="Power=",value = 0.800,min=0,max = 1,step = 0.01),
          helpText(tags$small("A组均值")),
          numericInput(inputId = "uA",label ="uA=",value = 0.500,min=0,max = 1,step = 0.1),
          helpText(tags$small("B组均值")),
          numericInput(inputId = "uB",label ="uB=",value = 0.500,min=0,max = 1,step = 0.1),
          helpText(tags$small("边界值")),
          numericInput(inputId = "delta",label ="delta=",value = 0.500,min=0,max = 1,step = 0.1),
          helpText(tags$small("总体标准差")),
          numericInput(inputId = "sd",label = "sd=",value = 1,min=NA,max = NA),
          helpText(tags$small("组A/组B")),
          numericInput(inputId = "k",label = "k=",value = 1,min=NA,max = NA)
        ),
        "equivalence"=div( 
          helpText(tags$small("第一类错误")),
          numericInput(inputId = "alpha",label = "Alpha=",value =0.050,min=0,max=1,step = 0.01 ),
          helpText(tags$small("检验功效")),
          numericInput(inputId = "power",label ="Power=",value = 0.800,min=0,max = 1,step = 0.01),
          helpText(tags$small("A组均值")),
          numericInput(inputId = "uA",label ="uA=",value = 0.500,min=0,max = 1,step = 0.1),
          helpText(tags$small("B组均值")),
          numericInput(inputId = "uB",label ="uB=",value = 0.500,min=0,max = 1,step = 0.1),
          helpText(tags$small("边界值")),
          numericInput(inputId = "delta",label ="delta=",value = 0.500,min=0,max = 1,step = 0.1),
          helpText(tags$small("总体标准差")),
          numericInput(inputId = "sd",label = "sd=",value = 1,min=NA,max = NA),
          helpText(tags$small("组A/组B")),
          numericInput(inputId = "k",label = "k=",value = 1,min=NA,max = NA)
        )
        
      )
      
    }
    else if(input$num=="two samples"&input$type=="proportion"){
      switch(
        input$subtype,
        "one side"=div( 
          helpText(tags$small("第一类错误")),
          numericInput(inputId = "alpha",label = "Alpha=",value =0.050,min=0,max=1,step = 0.01 ),
          helpText(tags$small("检验功效")),
          numericInput(inputId = "power",label ="Power=",value = 0.800,min=0,max = 1,step = 0.01),
          helpText(tags$small("A组比例")),
          numericInput(inputId = "pA",label ="pA=",value = 0.500,min=0,max = 1,step = 0.1),
          helpText(tags$small("B组比例")),
          numericInput(inputId = "pB",label ="pB=",value = 0.600,min=0,max = 1,step = 0.1),
          helpText(tags$small("组A/组B")),
          numericInput(inputId = "k",label = "k=",value = 1,min=NA,max = NA)
        ),
        "two sides"=div( 
          helpText(tags$small("第一类错误")),
          numericInput(inputId = "alpha",label = "Alpha=",value =0.050,min=0,max=1,step = 0.01 ),
          helpText(tags$small("检验功效")),
          numericInput(inputId = "power",label ="Power=",value = 0.800,min=0,max = 1,step = 0.01),
          helpText(tags$small("A组比例")),
          numericInput(inputId = "pA",label ="pA=",value = 0.500,min=0,max = 1,step = 0.1),
          helpText(tags$small("B组比例")),
          numericInput(inputId = "pB",label ="pB=",value = 0.600,min=0,max = 1,step = 0.1),
          helpText(tags$small("组A/组B")),
          numericInput(inputId = "k",label = "k=",value = 1,min=NA,max = NA)
        ),
        "non-infer"=div( 
          helpText(tags$small("第一类错误")),
          numericInput(inputId = "alpha",label = "Alpha=",value =0.050,min=0,max=1,step = 0.01 ),
          helpText(tags$small("检验功效")),
          numericInput(inputId = "power",label ="Power=",value = 0.800,min=0,max = 1,step = 0.01),
          helpText(tags$small("A组比例")),
          numericInput(inputId = "pA",label ="pA=",value = 0.500,min=0,max = 1,step = 0.1),
          helpText(tags$small("B组比例")),
          numericInput(inputId = "pB",label ="pB=",value = 0.500,min=0,max = 1,step = 0.1),
          helpText(tags$small("边界值")),
          numericInput(inputId = "delta",label ="delta=",value = 0.500,min=0,max = 1,step = 0.1),
          helpText(tags$small("组A/组B")),
          numericInput(inputId = "k",label = "k=",value = 1,min=NA,max = NA)
        ),
        "equivalence"=div( 
          helpText(tags$small("第一类错误")),
          numericInput(inputId = "alpha",label = "Alpha=",value =0.050,min=0,max=1,step = 0.01 ),
          helpText(tags$small("检验功效")),
          numericInput(inputId = "power",label ="Power=",value = 0.800,min=0,max = 1,step = 0.01),
          helpText(tags$small("A组比例")),
          numericInput(inputId = "pA",label ="pA=",value = 0.500,min=0,max = 1,step = 0.1),
          helpText(tags$small("B组比例")),
          numericInput(inputId = "pB",label ="pB=",value = 0.500,min=0,max = 1,step = 0.1),
          helpText(tags$small("边界值")),
          numericInput(inputId = "delta",label ="delta=",value = 0.500,min=0,max = 1,step = 0.1),
          helpText(tags$small("组A/组B")),
          numericInput(inputId = "k",label = "k=",value = 1,min=NA,max = NA)
        )
        
      )
    }
    # else if(input$num=="k samples"&input$type=="mean"){
    #   switch(
    #     input$subtype,
    #     "one side"=div(tags$strong("比较k个总体均值(ANOVA)只进行"),tags$strong("双边检验",style="color:red")),
    #     "non-infer"=div(tags$strong("比较k个总体均值(ANOVA)只进行"),tags$strong("双边检验",style="color:red")),
    #     "equivalence"=div(tags$strong("比较k个总体均值(ANOVA)只进行"),tags$strong("双边检验",style="color:red")),
    #     "two sides"=div(
    #       helpText(tags$small("第一类错误")),
    #       numericInput(inputId = "alpha",label = "Alpha=",value =0.050,min=0,max=1,step = 0.01 ),
    #       helpText(tags$small("检验功效")),
    #       numericInput(inputId = "power",label ="Power=",value = 0.800,min=0,max = 1,step = 0.01),
    #       helpText(tags$small("A组均值")),
    #       numericInput(inputId = "uA",label ="u=",value = 2,min=0,max = 1,step = 0.1),
    #       helpText(tags$small("B组均值")),
    #       numericInput(inputId = "uB",label ="u0=",value = 1.5,min=0,max = 1,step = 0.1),
    #       helpText(tags$small("标准差")),
    #       numericInput(inputId = "sd",label = "S=",value = 1,min=NA,max = NA),
    #       helpText(tags$small("配对数目")),
    #       numericInput(inputId = "tau",label ="tau",value = 1,min=NA,max = NA )
    #     )
    #     
    #   )
    #   
    # }
    # else if(input$num=="k samples"&input$type=="proportion"){
    #   switch(
    #     input$subtype,
    #     "one side"=div(tags$strong("比较k个总体比例(ANOVA)只进行"),tags$strong("双边检验",style="color:red")),
    #     "non-infer"=div(tags$strong("比较k个总体比例(ANOVA)只进行"),tags$strong("双边检验",style="color:red")),
    #     "equivalence"=div(tags$strong("比较k个总体比例(ANOVA)只进行"),tags$strong("双边检验",style="color:red")),
    #          "two sides"=div( 
    #            helpText(tags$small("第一类错误")),
    #            numericInput(inputId = "alpha",label = "Alpha=",value =0.050,min=0,max=1,step = 0.01 ),
    #            helpText(tags$small("检验功效")),
    #            numericInput(inputId = "power",label ="Power=",value = 0.800,min=0,max = 1,step = 0.01),
    #            helpText(tags$small("A组比例")),
    #            numericInput(inputId = "pA",label ="pA=",value = 0.500,min=0,max = 1,step = 0.1),
    #            helpText(tags$small("B组比例")),
    #            numericInput(inputId = "pB",label ="pB=",value = 0.600,min=0,max = 1,step = 0.1),
    #            helpText(tags$small("配对组数")),
    #            numericInput(inputId = "tau",label = "tau=",value = 1,min=NA,max = NA)
    #          )
    #          
    #          )
    #   
    #   
    # }
  )
#value##########################################################################################################
################################################################################################################
output$value<-renderTable(
  if(input$num=="one sample"&input$type=="mean"){
    switch(
      input$subtype,
      "one side"=onemean1(alpha=input$alpha,power=input$power,u=input$u,u0=input$u0,S=input$S),
      "two sides"=onemean2(alpha=input$alpha,power=input$power,u=input$u,u0=input$u0,S=input$S),
      "non-infer"=onemeannoninfer(alpha=input$alpha,power=input$power,u=input$u,u0=input$u0,delta=input$delta,S=input$S),
      "equivalence"=onemeanequ(alpha=input$alpha,power=input$power,u=input$u,u0=input$u0,delta=input$delta,S=input$S)
    )
  }
  else if(input$num=="one sample"&input$type=="proportion"){
    switch(
      input$subtype,
      "one side"=oneproportion1(alpha=input$alpha,power=input$power,P=input$P,P0=input$P0),
      "two sides"=oneproportion2(alpha=input$alpha,power=input$power,P=input$P,P0=input$P0),
      "non-infer"=oneporpnon(alpha=input$alpha,power=input$power,P=input$P,P0=input$P0,delta=input$delta),
      "equivalence"=oneporpequ(alpha=input$alpha,power=input$power,P=input$P,P0=input$P0,delta=input$delta)
    )
  }
  else if(input$num=="two samples"&input$type=="mean"){
    switch(
      input$subtype,
      "one side"=twomeans1(alpha=input$alpha,power=input$power,uA=input$uA,uB=input$uB,sdA=input$sdA,sdB=input$sdB,k=input$k),
      "two sides"=twomeans2(alpha=input$alpha,power=input$power,uA=input$uA,uB=input$uB,sd=input$sd,k=input$k),
      "non-infer"=twomeansnoninfer(alpha=input$alpha,power=input$power,uA=input$uA,uB=input$uB,delta=input$delta,sd=input$sd,k=input$k),
      "equivalence"=twomeansequ(alpha=input$alpha,power=input$power,uA=input$uA,uB=input$uB,delta=input$delta,sd=input$sd,k=input$k)
    )
  }
  else if(input$num=="two samples"&input$type=="proportion"){
    switch(
      input$subtype,
      "one side"=twop1(alpha=input$alpha,power=input$power,pA=input$pA,pB=input$pB,k=input$k),
      "two sides"=twop2(alpha=input$alpha,power=input$power,pA=input$pA,pB=input$pB,k=input$k),
      "non-infer"=twopnoninfer(alpha=input$alpha,power=input$power,pA=input$pA,pB=input$pB,delta=input$delta,k=input$k),
      "equivalence"=twopequ(alpha=input$alpha,power=input$power,pA=input$pA,pB=input$pB,delta=input$delta,k=input$k)
    )
  }
  # else if(input$num=="k samples"&input$type=="mean"){
  #   switch(
  #     input$subtype,
  #     "one side"=NULL,
  #     "non-infer"=NULL,
  #     "equivalence"=NULL,
  #     "two sides"=kmean2side(alpha = input$alpha,power = input$power,uA=input$uA,uB=input$uB,sd=input$sd,tau=input$tau)
  #   )
  # }
  # else if(input$num=="k samples"&input$type=="proportion"){
  #   switch(
  #     input$subtype,
  #     "one side"=NULL,
  #     "non-infer"=NULL,
  #     "equivalence"=NULL,
  #     "two sides"=kp(alpha = input$alpha,power=input$power,pA=input$pA,pB=input$pB,tau=input$tau)
  #     
  #   )
  # }
)
#chartUI########################################################################################################
################################################################################################################
output$chartui<-renderUI(
  if(input$num=="one sample"&input$type=="mean"){
    radioButtons(inputId = "which",label = "选择一个变量看其与样本量的关系图",c("u","S"))
  }
  else if(input$num=="one sample"&input$type=="proportion"){
    radioButtons(inputId = "which",label = "选择一个变量看其与样本量的关系图",c("P"))
  }
  else if(input$num=="two samples"&input$type=="mean"){
    radioButtons(inputId = "which",label = "选择一个变量看其与样本量的关系图",c("u"))
  }
  else if(input$num=="two samples"&input$type=="proportion"){
    radioButtons(inputId = "which",label = "选择一个变量看其与样本量的关系图",c("P"))
  }
)
#Hypothesis#####################################################################################################
################################################################################################################
output$Hyph<-renderUI(
  if(input$num=="one sample"&input$type=="mean"){
    switch(
      input$subtype,
      "one side"=div(tags$p(" "),tags$p("H0:u=u0"),tags$p("H1:u<u0"),tags$p("或"),tags$p("H0:u=u0"),tags$p("H1:u>u0")),
      "two sides"=div(tags$p(" "),tags$p("H0:u=u0"),tags$p("H1:u<>u0")),
      "non-infer"=div(tags$p(" "),tags$p("H0:u-u0<=delta"),tags$p("H1:u-u0>delta")),
      "equivalence"=div(tags$p(" "),tags$p("H0:|u-u0|>=delta"),tags$p("H1:|u-u0|<delta"))
    )
  }
  else if(input$num=="one sample"&input$type=="proportion"){
    switch(
      input$subtype,
      "one side"=div(tags$p(" "),tags$p("H0:p=p0"),tags$p("H1:p<p0"),tags$p("或"),tags$p("H0:p=p0"),tags$p("H1:p>p0")),
      "two sides"=div(tags$p(" "),tags$p("H0:p=p0"),tags$p("H1:p<>p0")),
      "non-infer"=div(tags$p(" "),tags$p("H0:p-p0<=delta"),tags$p("H0:p-p0>delta")),
      "equivalence"=div(tags$p(" "),tags$p("|p-p0|>=delta"),tags$p("|p-p0|<delta"))
    )
  }
  else if(input$num=="two samples"&input$type=="mean"){
    switch(
      input$subtype,
      "one side"=div(tags$p(" "),tags$p("H0:uA=uB"),tags$p("H1:uA<uB"),tags$p("或"),tags$p("H0:uA=uB"),tags$p("H1:uA>uB")),
      "two sides"=div(tags$p(" "),tags$p("H0:uA=uB"),tags$p("H1:uA<>uB")),
      "non-infer"=div(tags$p(" "),tags$p("H0:uA-uB<=delta"),tags$p("H1:uA-uB>delta")),
      "equivalence"=div(tags$p(" "),tags$p("H0:|uA-uB|>=delta"),tags$p("H1:|uA-uB|<delta"))
    )
  }
  else if(input$num=="two samples"&input$type=="proportion"){
    switch(
      input$subtype,
      "one side"=div(tags$p(" "),tags$p("H0:pA=pB"),tags$p("H1:pA<pB"),tags$p("或"),tags$p("H0:pA=pB"),tags$p("H1:pA>pB")),
      "two sides"=div(tags$p(" "),tags$p("H0:pA=pB"),tags$p("H1:pA<>pB")),
      "non-infer"=div(tags$p(" "),tags$p("H0:pA-pB<=delta"),tags$p("H1:pA-pB>delta")),
      "equivalence"=div(tags$p(" "),tags$p("H0:|pA-pB|>=delta"),tags$p("H1:|pA-pB|<delta"))
    )
  }
  # else if(input$num=="k samples"&input$type=="mean"){
  #   switch(
  #     input$subtype,
  #     "one side"=NULL,
  #     "non-infer"=NULL,
  #     "equivalence"=NULL,
  #     "two sides"=div(tags$p(" "),tags$p("H0:uA=uB"),tags$p("H1:uA<>uB"))
  #   )
  # }
  # else if(input$num=="k samples"&input$type=="proportion"){
  #   switch(
  #     input$subtype,
  #     "one side"=NULL,
  #     "non-infer"=NULL,
  #     "equivalence"=NULL,
  #     "two sides"=div(tags$p(" "),tags$p("H0:pA=pB"),tags$p("H1:pA<>pB"))
  #     
  #   )
  # }
)
#Explanation####################################################################################################
################################################################################################################
output$explain<-renderUI(
  if(input$num=="one sample"&input$type=="mean"){
    switch(
      input$subtype,
      "one side"=sprintf("以上结果为已知样本均值为%s假设的均值为%s总体方差为%s进行显著性水平为%s的单边检验为得到%s的检验功效所需的样本量",input$u,input$u0,input$S,input$alpha,input$power),
      "two sides"=sprintf("以上结果为已知样本均值为%s假设的均值为%s总体方差为%s进行显著性水平为%s的双边检验为得到%s的检验功效所需的样本量",input$u,input$u0,input$S,input$alpha,input$power),
      "non-infer"=sprintf("以上结果为已知样本均值为%s假设的均值为%s总体方差为%s进行显著性水平为%s的非劣效性检验为得到%s的检验功效所需的样本量",input$u,input$u0,input$S,input$alpha,input$power),
      "equivalence"=sprintf("以上结果为已知样本均值为%s假设的均值为%s总体方差为%s进行显著性水平为%s的等效性检验为得到%s的检验功效所需的样本量",input$u,input$u0,input$S,input$alpha,input$power)
    ) 
  }
  else if(input$num=="one sample"&input$type=="proportion"){
    switch(
      input$subtype,
      "one side"=sprintf("以上结果为已知样本比例为%s假设的比例为%s总体方差为%s进行显著性水平为%s的单边检验为得到%s的检验功效所需的样本量",input$u,input$u0,input$S,input$alpha,input$power),
      "two sides"=sprintf("以上结果为已知样本比例为%s假设的比例为%s总体方差为%s进行显著性水平为%s的双边检验为得到%s的检验功效所需的样本量",input$u,input$u0,input$S,input$alpha,input$power),
      "non-infer"=sprintf("以上结果为已知样本比例为%s假设的比例为%s总体方差为%s进行显著性水平为%s的非劣效性检验为得到%s的检验功效所需的样本量",input$u,input$u0,input$S,input$alpha,input$power),
      "equivalence"=sprintf("以上结果为已知样本比例为%s假设的比例为%s总体方差为%s进行显著性水平为%s的等效性检验为得到%s的检验功效所需的样本量",input$u,input$u0,input$S,input$alpha,input$power)
    )
  }
  else if(input$num=="two samples"&input$type=="mean"){
    switch(
      input$subtype,
      "one side"=sprintf("以上结果为已知组A均值为%s组B均值为%s总体方差为%s进行显著性水平为%s的单边检验为得到%s的检验功效所需的样本量",input$u,input$u0,input$S,input$alpha,input$power),
      "two sides"=sprintf("以上结果为已知组A均值为%s组B均值为%s总体方差为%s进行显著性水平为%s的双边检验为得到%s的检验功效所需的样本量",input$u,input$u0,input$S,input$alpha,input$power),
      "non-infer"=sprintf("以上结果为已知组A均值为%s组B均值为%s总体方差为%s进行显著性水平为%s的非劣效性检验为得到%s的检验功效所需的样本量",input$u,input$u0,input$S,input$alpha,input$power),
      "equivalence"=sprintf("以上结果为已知组A均值为%s组B均值为%s总体方差为%s进行显著性水平为%s的等效性检验为得到%s的检验功效所需的样本量",input$u,input$u0,input$S,input$alpha,input$power)
      
    )
  }
  else if(input$num=="two samples"&input$type=="proportion"){
    switch(
      input$subtype,
      "one side"=sprintf("以上结果为已知组A比例为%s组B比例为%s总体方差为%s进行显著性水平为%s的单边检验为得到%s的检验功效所需的样本量",input$u,input$u0,input$S,input$alpha,input$power),
      "two sides"=sprintf("以上结果为已知组A比例为%s组B比例为%s总体方差为%s进行显著性水平为%s的双边检验为得到%s的检验功效所需的样本量",input$u,input$u0,input$S,input$alpha,input$power),
      "non-infer"=sprintf("以上结果为已知组A比例为%s组B比例为%s总体方差为%s进行显著性水平为%s的非劣效性检验为得到%s的检验功效所需的样本量",input$u,input$u0,input$S,input$alpha,input$power),
      "equivalence"=sprintf("以上结果为已知组A比例为%s组B比例为%s总体方差为%s进行显著性水平为%s的等效性检验为得到%s的检验功效所需的样本量",input$u,input$u0,input$S,input$alpha,input$power)
      
    )
  }
)
#Plot Function##################################################################################################
################################################################################################################
plotonemean1<-function(alpha,u0,S) {
  Za<-qnorm(1-alpha)
  Zb7<-qnorm(0.7)
  Zb8<-qnorm(0.8)
  Zb9<-qnorm(0.9)
  lower=max(0,u0-2*S)
  upper=u0+2*S
  step=(upper-lower)/100
  x=seq(lower,upper,step)
  y1<-ceiling(S^2*(Za+Zb7)^2/(x-u0)^2)
  y2<-ceiling(S^2*(Za+Zb8)^2/(x-u0)^2)
  y3<-ceiling(S^2*(Za+Zb9)^2/(x-u0)^2)
  yupper=ceiling(S^2*(Za+Zb7)^2/(u0+u0/10-u0)^2)
  plot(x,y1,type='l',col="greenyellow",pch=15,lwd=3,xlab = "u",ylab = "Sample Size",ylim=c(0,yupper))
  lines(x,y2,type='l',col="green",pch=16,lwd=3)
  grid(nx=10,ny=5,lwd=2)
  legend("topright",legend=c("power 70%","power 80%","power 90%"),col=c("greenyellow","green","green4"),lwd=3,lty=c(1,2))
  gg<-lines(x,y3,type="l",col="green4",pch=17,lwd=3)
  return(gg)
}  
plotonemean2<-function(alpha,u0,S){
  Za<-qnorm(1-alpha/2)
  Zb7<-qnorm(0.7)
  Zb8<-qnorm(0.8)
  Zb9<-qnorm(0.9)
  lower=max(0,u0-2*S)
  upper=u0+2*S
  step=(upper-lower)/100
  x=seq(lower,upper,step)
  y1<-ceiling(S^2*(Za+Zb7)^2/(x-u0)^2)
  y2<-ceiling(S^2*(Za+Zb8)^2/(x-u0)^2)
  y3<-ceiling(S^2*(Za+Zb9)^2/(x-u0)^2)
  yupper=ceiling(S^2*(Za+Zb7)^2/(u0+u0/10-u0)^2)
  plot(x,y1,type="l",col="greenyellow",pch=15,lwd=3,xlab = "u",ylab = "Sample Size",ylim=c(0,yupper))
  lines(x,y2,type="l",col="green",pch=16,lwd=3)
  grid(nx=10,ny=5,lwd=2)
  legend("topright",legend=c("power 70%","power 80%","power 90%"),col=c("greenyellow","green","green4"),lwd=3,lty=c(1,2))
  gg<-lines(x,y3,type="l",col="green4",pch=17,lwd=3)
  return(gg)
  
}
plotonemeannoninf<-function(alpha,u0,delta,S){
  Za<-qnorm(1-alpha)
  Zb7<-qnorm(0.7)
  Zb8<-qnorm(0.8)
  Zb9<-qnorm(0.9)
  lower=max(0,u0-2*S)
  upper=u0+2*S
  step=(upper-lower)/100
  x=seq(lower,upper,step)
  y1<-ceiling(S^2*(Za+Zb7)^2/(x-u0-delta)^2)
  y2<-ceiling(S^2*(Za+Zb8)^2/(x-u0-delta)^2)
  y3<-ceiling(S^2*(Za+Zb9)^2/(x-u0-delta)^2)
  yupper=ceiling(S^2*(Za+Zb7)^2/(u0+u0/10-u0)^2)
  plot(x,y1,type="l",col="greenyellow",pch=15,lwd=3,xlab = "u",ylab = "Sample Size",ylim=c(0,yupper))
  lines(x,y2,type="l",col="green",pch=16,lwd=3)
  grid(nx=10,ny=5,lwd=2)
  legend("topright",legend=c("power 70%","power 80%","power 90%"),col=c("greenyellow","green","green4"),lwd=3,lty=c(1,2))
  gg<-lines(x,y3,type="l",col="green4",pch=17,lwd=3)
  return(gg) 
}
plotonemeanequ<-function(alpha,u0,delta,S){
  Za<-qnorm(1-alpha)
  Zb7<-qnorm(0.7/2+0.5)
  Zb8<-qnorm(0.8/2+0.5)
  Zb9<-qnorm(0.9/2+0.5)
  lower=max(0,u0-2*S)
  upper=u0+2*S
  step=(upper-lower)/100
  x=seq(lower,upper,step)
  y1<-ceiling(S^2*(Za+Zb7)^2/(abs(x-u0)-delta)^2)
  y2<-ceiling(S^2*(Za+Zb8)^2/(abs(x-u0)-delta)^2)
  y3<-ceiling(S^2*(Za+Zb9)^2/(abs(x-u0)-delta)^2)
  yupper=500
  power=c(rep("0.7",10),rep("0.8",10),rep("0.9",10))
  plot(x,y1,type="l",col="greenyellow",pch=15,lwd=3,xlab = "u",ylab = "Sample Size",ylim=c(0,yupper))
  lines(x,y2,type="l",col="green",pch=16,lwd=3)
  grid(nx=10,ny=5,lwd=2)
  legend("topright",legend=c("power 70%","power 80%","power 90%"),col=c("greenyellow","green","green4"),lwd=3,lty=c(1,2))
  gg<-lines(x,y3,type="l",col="green4",pch=17,lwd=3)
  return(gg)  
}
plotonemean1S<-function(alpha,u,u0,S){
  Za<-qnorm(1-alpha)
  Zb7<-qnorm(0.7)
  Zb8<-qnorm(0.8)
  Zb9<-qnorm(0.9)
  x<-seq(S-9*S/10,(S+9*S/10),S)
  y1<-ceiling(x^2*(Za+Zb7)^2/(u-u0)^2)
  y2<-ceiling(x^2*(Za+Zb8)^2/(u-u0)^2)
  y3<-ceiling(x^2*(Za+Zb9)^2/(u-u0)^2)
  plot(x,y1,type="l",col="greenyellow",pch=15,lwd=3,xlab = "S",ylab = "Sample Size")
  lines(x,y2,type="l",col="green",pch=16,lwd=3)
  grid(nx=10,ny=5,lwd=2)
  legend("topright",legend=c("power 70%","power 80%","power 90%"),col=c("greenyellow","green","green4"),lwd=3,lty=c(1,2))
  gg<-lines(x,y3,type="l",col="green4",pch=17,lwd=3)
  return(gg)
}

plotonemean2S<-function(alpha,u,u0,S){
  Za<-qnorm(1-alpha/2)
  Zb7<-qnorm(0.7)
  Zb8<-qnorm(0.8)
  Zb9<-qnorm(0.9)
  x<-seq(S-9*S/10,(S+9*S/10),S)
  y1<-ceiling(x^2*(Za+Zb7)^2/(u-u0)^2)
  y2<-ceiling(x^2*(Za+Zb8)^2/(u-u0)^2)
  y3<-ceiling(x^2*(Za+Zb9)^2/(u-u0)^2)
  plot(x,y1,type="l",col="greenyellow",pch=15,lwd=3,xlab = "S",ylab ="Sample Size")
  lines(x,y2,type="l",col="green",pch=16,lwd=3)
  grid(nx=10,ny=5,lwd=2)
  legend("topright",legend=c("power 70%","power 80%","power 90%"),col=c("greenyellow","green","green4"),lwd=3,lty=c(1,2))
  gg<-lines(x,y3,type="l",col="green4",pch=17,lwd=3)
  return(gg)
}
plotonemeannonS<-function(alpha,u,u0,delta,S){
  Za<-qnorm(1-alpha)
  Zb7<-qnorm(0.7)
  Zb8<-qnorm(0.8)
  Zb9<-qnorm(0.9)
  x<-seq(S-9*S/10,(S+9*S/10),S)
  y1<-ceiling(x^2*(Za+Zb7)^2/(u-u0-delta)^2)
  y2<-ceiling(x^2*(Za+Zb8)^2/(u-u0-delta)^2)
  y3<-ceiling(x^2*(Za+Zb9)^2/(u-u0-delta)^2)
  plot(x,y1,type="l",col="greenyellow",pch=15,lwd=3,xlab = "S",ylab = "Sample Size")
  lines(x,y2,type="l",col="green",pch=16,lwd=3)
  grid(nx=10,ny=5,lwd=2)
  legend("topright",legend=c("power 70%","power 80%","power 90%"),col=c("greenyellow","green","green4"),lwd=3,lty=c(1,2))
  gg<-lines(x,y3,type="l",col="green4",pch=17,lwd=3)
  return(gg)
}
plotonemeanequS<-function(alpha,u,u0,delta,S){
  Za<-qnorm(1-alpha)
  Zb7<-qnorm(0.7/2+0.5)
  Zb8<-qnorm(0.8/2+0.5)
  Zb9<-qnorm(0.9/2+0.5)
  x<-seq(S-9*S/10,(S+9*S/10),S)
  y1<-ceiling(x^2*(Za+Zb7)^2/(abs(u-u0)-delta)^2)
  y2<-ceiling(x^2*(Za+Zb8)^2/(abs(u-u0)-delta)^2)
  y3<-ceiling(x^2*(Za+Zb9)^2/(abs(u-u0)-delta)^2)
  plot(x,y1,type="l",col="greenyellow",pch=15,lwd=3,xlab = "S",ylab = "Sample Size")
  lines(x,y2,type="l",col="green",pch=16,lwd=3)
  grid(nx=10,ny=5,lwd=2)
  legend("topright",legend=c("power 70%","power 80%","power 90%"),col=c("greenyellow","green","green4"),lwd=3,lty=c(1,2))
  gg<-lines(x,y3,type="l",col="green4",pch=17,lwd=3)
  return(gg)
}
plotoneproportion1<-function(alpha,P0){
  Za<-qnorm(1-alpha)
  Zb7<-qnorm(0.7)
  Zb8<-qnorm(0.8)
  Zb9<-qnorm(0.9)
  SS=P0*(1-P0)
  lower=max(0,P0-2*SS)
  upper=P0+2*SS
  step=(upper-lower)/100
  x=seq(lower,upper,step)
  y1<-ceiling(x*(1-x)*(Za+Zb7)^2/(x-P0)^2)
  y2<-ceiling(x*(1-x)*(Za+Zb8)^2/(x-P0)^2)
  y3<-ceiling(x*(1-x)*(Za+Zb9)^2/(x-P0)^2)
  xx<-P0-10*step
  yupper=ceiling(xx*(1-xx)*(Za+Zb7)^2/(xx-P0)^2)
  plot(x,y1,type="l",col="greenyellow",pch=15,lwd=3,xlab = "p",ylab = "Sample Size",ylim = c(0,yupper))
  lines(x,y2,type="l",col="green",pch=16,lwd=3)
  grid(nx=10,ny=5,lwd=2)
  legend("topright",legend=c("power 70%","power 80%","power 90%"),col=c("greenyellow","green","green4"),lwd=3,lty=c(1,2))
  gg<-lines(x,y3,type="l",col="green4",pch=17,lwd=3)
  return(gg)
}
plotoneproportion2<-function(alpha,P0){
  Za<-qnorm(1-alpha/2)
  Zb7<-qnorm(0.7)
  Zb8<-qnorm(0.8)
  Zb9<-qnorm(0.9)
  SS=P0*(1-P0)
  lower=max(0,P0-2*SS)
  upper=P0+2*SS
  step=(upper-lower)/100
  x=seq(lower,upper,step)
  y1<-ceiling(x*(1-x)*(Za+Zb7)^2/(x-P0)^2)
  y2<-ceiling(x*(1-x)*(Za+Zb8)^2/(x-P0)^2)
  y3<-ceiling(x*(1-x)*(Za+Zb9)^2/(x-P0)^2)
  xx<-P0-10*step
  yupper=ceiling(xx*(1-xx)*(Za+Zb7)^2/(xx-P0)^2)
  plot(x,y1,type="l",col="greenyellow",pch=15,lwd=3,xlab = "p",ylab = "Sample Size",ylim = c(0,yupper))
  lines(x,y2,type="l",col="green",pch=16,lwd=3)
  grid(nx=10,ny=5,lwd=2)
  legend("topright",legend=c("power 70%","power 80%","power 90%"),col=c("greenyellow","green","green4"),lwd=3,lty=c(1,2))
  gg<-lines(x,y3,type="l",col="green4",pch=17,lwd=3)
  return(gg)
}
plotoneproportionnon<-function(alpha,P0,delta){
  Za<-qnorm(1-alpha)
  Zb7<-qnorm(0.7)
  Zb8<-qnorm(0.8)
  Zb9<-qnorm(0.9)
  SS=P0*(1-P0)
  lower=max(0,P0-2*SS)
  upper=P0+2*SS
  step=(upper-lower)/100
  x=seq(lower,upper,step)
  y1<-ceiling(x*(1-x)*(Za+Zb7)^2/(x-P0-delta)^2)
  y2<-ceiling(x*(1-x)*(Za+Zb8)^2/(x-P0-delta)^2)
  y3<-ceiling(x*(1-x)*(Za+Zb9)^2/(x-P0-delta)^2)
  xx<-P0-10*step
  yupper=ceiling(xx*(1-xx)*(Za+Zb7)^2/(xx-P0)^2)
  plot(x,y1,type="l",col="greenyellow",pch=15,lwd=3,xlab = "p",ylab ="Sample Size",ylim = c(0,yupper))
  lines(x,y2,type="l",col="green",pch=16,lwd=3)
  grid(nx=10,ny=5,lwd=2)
  legend("topright",legend=c("power 70%","power 80%","power 90%"),col=c("greenyellow","green","green4"),lwd=3,lty=c(1,2))
  gg<-lines(x,y3,type="l",col="green4",pch=17,lwd=3)
  return(gg)
}
plotoneproportionequ<-function(alpha,P0,delta){
  Za<-qnorm(1-alpha)
  Zb7<-qnorm(0.7/2+0.5)
  Zb8<-qnorm(0.8/2+0.5)
  Zb9<-qnorm(0.9/2+0.5)
  SS=P0*(1-P0)
  lower=max(0,P0-2*SS)
  upper=P0+2*SS
  step=(upper-lower)/100
  x=seq(lower,upper,step)
  y1<-ceiling(x*(1-x)*(Za+Zb7)^2/(abs(x-P0)-delta)^2)
  y2<-ceiling(x*(1-x)*(Za+Zb8)^2/(abs(x-P0)-delta)^2)
  y3<-ceiling(x*(1-x)*(Za+Zb9)^2/(abs(x-P0)-delta)^2)
  xx<-P0-10*step
  yupper=ceiling(xx*(1-xx)*(Za+Zb7)^2/(xx-P0)^2)
  plot(x,y1,type='l',col="greenyellow",pch=15,lwd=3,xlab = "p",ylab ="Sample Size",ylim = c(0,yupper))
  lines(x,y2,type='l',col="green",pch=16,lwd=3)
  grid(nx=10,ny=5,lwd=2)
  legend("topright",legend=c("power 70%","power 80%","power 90%"),col=c("greenyellow","green","green4"),lwd=3,lty=c(1,2))
  gg<-lines(x,y3,type='l',col="green4",pch=17,lwd=3)
  return(gg)
}

plottwomean1<-function(alpha,uA,uB,sdA,sdB,k) {
  Za<-qnorm(1-alpha)
  Zb7<-qnorm(0.7)
  Zb8<-qnorm(0.8)
  Zb9<-qnorm(0.9)
  lower=max(0,uB-2*sdB)
  upper=uB+2*sdB
  step=(upper-lower)/100
  x=seq(lower,upper,step)
  y1<-ceiling((sdB^2+sdA^2/k)*Zb7/(uA-x)^2)
  y2<-ceiling((sdB^2+sdA^2/k)*Zb8/(uA-x)^2)
  y3<-ceiling((sdB^2+sdA^2/k)*Zb9/(uA-x)^2)
  yupper=ceiling((sdB^2+sdA^2/k)*Zb7/(uA-uB+step)^2)
  plot(x,y1,type='l',col="greenyellow",pch=15,lwd=3,xlab = "uB",ylab = "Sample Size nB",ylim = c(0,yupper))
  lines(x,y2,type='l',col="green",pch=16,lwd=3)
  grid(nx=10,ny=5,lwd=2)
  legend("topright",legend=c("power 70%","power 80%","power 90%"),col=c("greenyellow","green","green4"),lwd=3,lty=c(1,2))
  gg<-lines(x,y3,type="l",col="green4",pch=17,lwd=3)
  return(gg)
}  
plottwomean2<-function(alpha,uA,uB,sd,k) {
  Za<-qnorm(1-alpha/2)
  Zb7<-qnorm(0.7)
  Zb8<-qnorm(0.8)
  Zb9<-qnorm(0.9)
  lower=max(0,uB-2*sd)
  upper=uB+2*sd
  step=(upper-lower)/100
  x=seq(lower,upper,step)
  y1<-ceiling((1+1/k)*sd^2*Zb7/(uA-x)^2)
  y2<-ceiling((1+1/k)*sd^2*Zb8/(uA-x)^2)
  y3<-ceiling((1+1/k)*sd^2*Zb9/(uA-x)^2)
  yupper=ceiling((1+1/k)*sd^2*Zb9/(uA-uB+step)^2)
  plot(x,y1,type='l',col="greenyellow",pch=15,lwd=3,xlab = "uB",ylab = "Sample Size nB",ylim = c(0,yupper))
  lines(x,y2,type='l',col="green",pch=16,lwd=3)
  grid(nx=10,ny=5,lwd=2)
  legend("topright",legend=c("power 70%","power 80%","power 90%"),col=c("greenyellow","green","green4"),lwd=3,lty=c(1,2))
  gg<-lines(x,y3,type="l",col="green4",pch=17,lwd=3)
  return(gg)
}  
plottwomeannoninfer<-function(alpha,uA,uB,sd,k,delta) {
  Za<-qnorm(1-alpha/2)
  Zb7<-qnorm(0.7)
  Zb8<-qnorm(0.8)
  Zb9<-qnorm(0.9)
  lower=max(0,uB-2*sd)
  upper=uB+2*sd
  step=(upper-lower)/100
  x=seq(lower,upper,step)
  y1<-ceiling((1+1/k)*sd^2*Zb7/(uA-x-delta)^2)
  y2<-ceiling((1+1/k)*sd^2*Zb8/(uA-x-delta)^2)
  y3<-ceiling((1+1/k)*sd^2*Zb9/(uA-x-delta)^2)
  yupper=500
  plot(x,y1,type='l',col="greenyellow",pch=15,lwd=3,xlab = "uB",ylab = "Sample Size nB",ylim = c(0,yupper))
  lines(x,y2,type='l',col="green",pch=16,lwd=3)
  grid(nx=10,ny=5,lwd=2)
  legend("topright",legend=c("power 70%","power 80%","power 90%"),col=c("greenyellow","green","green4"),lwd=3,lty=c(1,2))
  gg<-lines(x,y3,type="l",col="green4",pch=17,lwd=3)
  return(gg)
}  
plottwomeanequ<-function(alpha,uA,uB,sd,delta,k){
  Za<-qnorm(1-alpha)
  Zb7<-qnorm(0.7/2+0.5)
  Zb8<-qnorm(0.8/2+0.5)
  Zb9<-qnorm(0.9/2+0.5)
  lower=max(0,uB-2*sd)
  upper=uB+2*sd
  step=(upper-lower)/100
  x=seq(lower,upper,step)
  y1<-ceiling((1+1/k)*sd^2*Zb7/(abs(uA-x)-delta)^2)
  y2<-ceiling((1+1/k)*sd^2*Zb8/(abs(uA-x)-delta)^2)
  y3<-ceiling((1+1/k)*sd^2*Zb8/(abs(uA-x)-delta)^2)
  yupper=500
  plot(x,y1,type='l',col="greenyellow",pch=15,lwd=3,xlab = "uB",ylab = "Sample Size nB",ylim = c(0,yupper))
  lines(x,y2,type='l',col="green",pch=16,lwd=3)
  grid(nx=10,ny=5,lwd=2)
  legend("topright",legend=c("power 70%","power 80%","power 90%"),col=c("greenyellow","green","green4"),lwd=3,lty=c(1,2))
  gg<-lines(x,y3,type="l",col="green4",pch=17,lwd=3)
  return(gg)
}
plottwoproportion1<-function(alpha,pA,pB,k){
  Za<-qnorm(1-alpha)
  Zb7<-qnorm(0.7)
  Zb8<-qnorm(0.8)
  Zb9<-qnorm(0.9)
  SS=pA*(1-pA)/k+pB*(1-pB)
  lower=max(0,pA-2*SS)
  upper=pA+2*SS
  step=(upper-lower)/100
  x=seq(lower,upper,step)
  y1<-ceiling((pA*(1-pA)/k+x*(1-x))*Zb7/(pA-x)^2)
  y2<-ceiling((pA*(1-pA)/k+x*(1-x))*Zb8/(pA-x)^2)
  y3<-ceiling((pA*(1-pA)/k+x*(1-x))*Zb9/(pA-x)^2)
  xx<-pA-10*step
  yupper=ceiling(xx*(1-xx)*(Za+Zb7)^2/(xx-pA)^2)
  plot(x,y1,type="l",col="greenyellow",pch=15,lwd=3,xlab = "p",ylab = "Sample Size",ylim = c(0,yupper))
  lines(x,y2,type="l",col="green",pch=16,lwd=3)
  grid(nx=10,ny=5,lwd=2)
  legend("topright",legend=c("power 70%","power 80%","power 90%"),col=c("greenyellow","green","green4"),lwd=3,lty=c(1,2))
  gg<-lines(x,y3,type="l",col="green4",pch=17,lwd=3)
  return(gg)
}
plottwoproportion2<-function(alpha,pA,pB,k){
  Za<-qnorm(1-alpha/2)
  Zb7<-qnorm(0.7)
  Zb8<-qnorm(0.8)
  Zb9<-qnorm(0.9)
  SS=pA*(1-pA)/k+pB*(1-pB)
  lower=max(0,pA-2*SS)
  upper=pA+2*SS
  step=(upper-lower)/100
  x=seq(lower,upper,step)
  y1<-ceiling((pA*(1-pA)/k+x*(1-x))*Zb7/(pA-x)^2)
  y2<-ceiling((pA*(1-pA)/k+x*(1-x))*Zb8/(pA-x)^2)
  y3<-ceiling((pA*(1-pA)/k+x*(1-x))*Zb9/(pA-x)^2)
  xx<-pA-10*step
  yupper=ceiling(xx*(1-xx)*(Za+Zb7)^2/(xx-pA)^2)
  plot(x,y1,type="l",col="greenyellow",pch=15,lwd=3,xlab = "p",ylab = "Sample Size",ylim = c(0,yupper))
  lines(x,y2,type="l",col="green",pch=16,lwd=3)
  grid(nx=10,ny=5,lwd=2)
  legend("topright",legend=c("power 70%","power 80%","power 90%"),col=c("greenyellow","green","green4"),lwd=3,lty=c(1,2))
  gg<-lines(x,y3,type="l",col="green4",pch=17,lwd=3)
  return(gg)
}
plottwoproportionnoninfer<-function(alpha,pA,pB,k,delta){
  Za<-qnorm(1-alpha)
  Zb7<-qnorm(0.7)
  Zb8<-qnorm(0.8)
  Zb9<-qnorm(0.9)
  SS=pA*(1-pA)/k+pB*(1-pB)
  lower=max(0,pA-2*SS)
  upper=pA+2*SS
  step=(upper-lower)/100
  x=seq(lower,upper,step)
  y1<-ceiling((pA*(1-pA)/k+x*(1-x))*Zb7/(pA-x-delta)^2)
  y2<-ceiling((pA*(1-pA)/k+x*(1-x))*Zb8/(pA-x-delta)^2)
  y3<-ceiling((pA*(1-pA)/k+x*(1-x))*Zb9/(pA-x-delta)^2)
  xx<-pA-10*step
  yupper=ceiling(xx*(1-xx)*(Za+Zb7)^2/(xx-pA)^2)
  plot(x,y1,type="l",col="greenyellow",pch=15,lwd=3,xlab = "p",ylab = "Sample Size",ylim = c(0,yupper))
  lines(x,y2,type="l",col="green",pch=16,lwd=3)
  grid(nx=10,ny=5,lwd=2)
  legend("topright",legend=c("power 70%","power 80%","power 90%"),col=c("greenyellow","green","green4"),lwd=3,lty=c(1,2))
  gg<-lines(x,y3,type="l",col="green4",pch=17,lwd=3)
  return(gg)
}
plottwoproportionequ<-function(alpha,pA,pB,k,delta){
  Za<-qnorm(1-alpha)
  Zb7<-qnorm(0.7/2+0.5)
  Zb8<-qnorm(0.8/2+0.5)
  Zb9<-qnorm(0.9/2+0.5)
  SS=pA*(1-pA)/k+pB*(1-pB)
  lower=max(0,pA-2*SS)
  upper=pA+2*SS
  step=(upper-lower)/100
  x=seq(lower,upper,step)
  y1<-ceiling((pA*(1-pA)/k+x*(1-x))*Zb7/(abs(pA-x)-delta)^2)
  y2<-ceiling((pA*(1-pA)/k+x*(1-x))*Zb8/(abs(pA-x)-delta)^2)
  y3<-ceiling((pA*(1-pA)/k+x*(1-x))*Zb8/(abs(pA-x)-delta)^2)
  xx<-pA-10*step
  yupper=ceiling(xx*(1-xx)*(Za+Zb7)^2/(xx-pA)^2)
  plot(x,y1,type="l",col="greenyellow",pch=15,lwd=3,xlab = "p",ylab = "Sample Size",ylim = c(0,yupper))
  lines(x,y2,type="l",col="green",pch=16,lwd=3)
  grid(nx=10,ny=5,lwd=2)
  legend("topright",legend=c("power 70%","power 80%","power 90%"),col=c("greenyellow","green","green4"),lwd=3,lty=c(1,2))
  gg<-lines(x,y3,type="l",col="green4",pch=17,lwd=3)
  return(gg)
}
#Plot############################################################################################################
################################################################################################################
output$viewchart<-renderPlot(
  if(input$num=="one sample"&input$type=="mean"&input$which=="u"){
   
    switch(
      input$subtype,
      "one side"=print(plotonemean1(alpha=input$alpha,u0=input$u0,S=input$S)),
      "two sides"=print(plotonemean2(alpha = input$alpha,u0=input$u0,S=input$S)),
      "non-infer"=print(plotonemeannoninf(alpha = input$alpha,u0=input$u0,delta=input$delta,S=input$S)),
      "equivalence"=print(plotonemeanequ(alpha = input$alpha,u0=input$u0,delta=input$delta,S=input$S))
     
    )
  }
  else if(input$num=="one sample"&input$type=="mean"&input$which=="S"){
    switch(
      input$subtype,
      "one side"=print(plotonemean1S(alpha=input$alpha,u=input$u,u0=input$u0,S=input$S)),
      "two sides"=print(plotonemean2S(alpha=input$alpha,u=input$u,u0=input$u0,S=input$S)),
      "non-infer"=print(plotonemeannonS(alpha=input$alpha,u=input$u,u0=input$u0,delta=input$delta,S=input$S)),
      "equivalence"=print(plotonemeanequS(alpha=input$alpha,u=input$u,u0=input$u0,delta=input$delta,S=input$S))
    )
  }
  else if(input$num=="one sample"&input$type=="proportion"&input$which=="P"){
    switch(
      input$subtype,
      "one side"=print(plotoneproportion1(alpha = input$alpha,P0=input$P0)),
      "two sides"=print(plotoneproportion2(alpha = input$alpha,P0=input$P0)),
      "non-infer"=print(plotoneproportionnon(alpha=input$alpha,P0=input$P0,delta=input$delta)),
      "equivalence"=print(plotoneproportionequ(alpha=input$alpha,P0=input$P0,delta=input$delta))
    )
  }
  else if(input$num=="two samples"&input$type=="mean"&input$which=="u"){
    switch(
      input$subtype,
      "one side"=print(plottwomean1(alpha = input$alpha,uA=input$uA,uB=input$uB,sdA=input$sdA,sdB = input$sdB,k=input$k)),
      'two sides'=print(plottwomean2(alpha = input$alpha,uA=input$uA,uB=input$uB,sd=input$sd,k=input$k)),
      "non-infer"=print(plottwomeannoninfer(alpha = input$alpha,uA=input$uA,uB=input$uB,sd=input$sd,k=input$k,delta = input$delta)),
      "equivalence"=print(plottwomeanequ(alpha = input$alpha,uA=input$uA,uB=input$uB,sd=input$sd,k=input$k,delta = input$delta))
    )
    
    
  }
  else if(input$num=="two samples"&input$type=="proportion"){
    switch(
      input$subtype,
      "one side"=print(plottwoproportion1(alpha = input$alpha,pA=input$pA,pB=input$pB,k=input$k)),
      'two sides'=print(plottwoproportion2(alpha = input$alpha,pA=input$pA,pB=input$pB,k=input$k)),
      "non-infer"=print(plottwoproportionnoninfer(alpha = input$alpha,pA=input$pA,pB=input$pB,k=input$k,delta = input$delta)),
      "equivalence"=print(plottwoproportionequ(alpha = input$alpha,pA=input$pA,pB=input$pB,k=input$k,delta = input$delta))
      
    )
  }
)
}