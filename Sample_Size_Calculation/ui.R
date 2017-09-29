library(shiny)
shinyUI(
  fluidPage(
    titlePanel(tags$b("样本量计算",style="color:green")),
    fluidRow(
      column( 3,
              wellPanel(
                      div(radioButtons(inputId="num",label="总体数目",list("单个总体"="one sample","两个总体"="two samples"),selected = NULL)),
                radioButtons(inputId = "type",label="检验类型",list("均值"="mean","比例"="proportion")),
              
                radioButtons(inputId="subtype",label = "类型",list("单边检验"="one side","双边检验"="two sides","非劣效性检验"="non-infer","等效性检验"="equivalence"))
              ),
            
              wellPanel(
                     tags$strong("假设检验形式",style="color:green"),
                     uiOutput("Hyph")
                       )),
    column(3, 
           wellPanel(
                    uiOutput("ui")
                    )
           ),
    column(5,
           wellPanel(
             tags$strong("计算结果",style="color:green"),
             tableOutput("value")),
           wellPanel(
                     div(tags$strong("结果解释：",style="color:green"),tags$p("")),
                     uiOutput("explain")
                     ),
              wellPanel(
                plotOutput("viewchart"),
                        uiOutput("chartui")
                       )
           )
    
    
           )
   #### fluidrow
           )
  ####fluidPage
  
      )
######shinyUI