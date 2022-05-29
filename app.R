#quick app for building intuition about sampling variation
source("global.R")
shinyApp(
ui = fluidPage(
  sidebarLayout(
    sidebarPanel(
      img(src='_iedlogo.png', align = "left"),
      h3("Build Your Intuition about Sampling Variation"),
      helpText("This app helps build your intuition about one souce of variation in survey results, ",
               "like surveys of patient experience data. When you change the number of patients surveyed, the rate of 'good performance'",
               " or the fraction of patients surveyed, the table and histograms update to show how estimated results vary,", 
               " just from the properties of simple random sampling.",
               "",
               "The run chart at the bottom of the right hand side of the page shows 20 simulated values from",
               "the adjusted model, to illustrate what data might look like when there are no changes in patient experience."),
      
      tags$div(class="header", checked=NA,
               tags$p("Want more details about this app?"),
               tags$a(href="https://github.com/klittle314/survey_simulator/blob/master/Details%20of%20the%20Survey%20Simulator%20web%20application.pdf", "Click here for a pdf file.")),         
      h3(""),
      sliderInput("p", 'Reference per cent for good performance (per cent "success"):', min=50, max=99, value = 80),
      sliderInput("obs", "Number of surveyed patients in the sample:", min = 5, max = 300, value = 30),
      sliderInput("frac", "Sample as % of patients who could be sampled (sampling fraction):", min=1,max=95,value=40),
      h3(""),
      helpText('Table entries are the per cent "success" estimated from the two sampling models'),
      tableOutput("values"),
      
      # author info
          shiny::hr(),
          em(
            span("Created by "),
            a("Kevin Little,Ph.D.", href = "mailto:klittle@iecodesign.com"),
            span(",Informing Ecological Design, LLC, revised 30 May 2022"),
            br(), br()
          )
      
    ),
    mainPanel(plotOutput("distPlot"),
              plotOutput("distPlotHG"),
              plotOutput("runChartHG")
    ) 
  )
),
server = function(input, output, session) {
  #generate the random values for binom 
  pcalcs <- reactive({
    p_calc(p1=input$p/100,num=input$obs)
  })
 

  
  #generate the random values for hypergeometric
  pcalcsHG <- reactive({
    #define the sampling fraction
    nfrac1 <- input$frac/100
    p_calcHG(p1=input$p/100,num=input$obs,nfrac=nfrac1)
  })
  
 
  #capture the max and min of the graph  x axis 
  range_binom <- reactive({
    df1 <- data.frame(100*pcalcs()[[1]])
    names(df1)[1] <- 'phats'
    n1 <- input$obs
    range1 <- range(df1$phats)
    range_use <- range1[2] - range1[1]
    if(n1<225) {
      div1 <- max(5,trunc(sqrt(n1)))
      wbins <- range_use/div1
    } else wbins <- range_use/15
    # wbins <- range_use/10
    p_01 <- 100*pcalcs()[[2]][1]
    p_99 <- 100*pcalcs()[[2]][7]
    #browser()
    plot_1 <- ggplot(data=df1,aes(x=phats))+
      geom_histogram(aes(y=..density..),binwidth=wbins,colour="black",fill="grey")
    #range_out <- ggplot_build(plot_1)$panel$ranges[[1]]$x.range
    range_out <- ggplot_build(plot_1)$layout$panel_scales_x[[1]]$range$range
  })
  
  #capture the max and min of the graph y axis
  range_y <- reactive({
    df1 <- data.frame(100*pcalcsHG()[[1]])
    names(df1)[1] <- 'phatsHG'
    n1 <- input$obs
    range1 <- range(df1$phatsHG)
    range_use <- range1[2] - range1[1]
    if(n1<225) {
      div1 <- max(5,trunc(sqrt(n1)))
      wbins <- range_use/div1
    } else wbins <- range_use/15
    # wbins <- range_use/10
    p_01 <- 100*pcalcsHG()[[2]][1]
    p_99 <- 100*pcalcsHG()[[2]][7]
    plot_min <- range_binom()[1]
    plot_max <- range_binom()[2]
    plot_1 <- ggplot(data=df1,aes(x=phatsHG))+
      geom_histogram(aes(y=..density..),binwidth=wbins,colour="black",fill="grey") #+
      #geom_histogram(binwidth=wbins,colour="black",fill="grey")
    #range_out <- ggplot_build(plot_1)$panel$ranges[[1]]$y.range
    range_out <- ggplot_build(plot_1)$layout$panel_scales_y[[1]]$range$range
  })
  
  
  output$distPlot <- renderPlot({
    df1 <- data.frame(100*pcalcs()[[1]])
    names(df1)[1] <- 'phats'
    n1 <- input$obs
    range1 <- range(df1$phats)
    range_use <- range1[2] - range1[1]
    if(n1<225) {
      div1 <- max(5,trunc(sqrt(n1)))
      wbins <- range_use/div1
    } else wbins <- range_use/15
    # wbins <- range_use/10
    p_01 <- 100*pcalcs()[[2]][1]
    p_99 <- 100*pcalcs()[[2]][7]
    plot_xmin <- range_binom()[1]
    plot_xmax <- min(range_binom()[2],100)
    plot_ymin <- range_y()[1]
    plot_ymax <- range_y()[2]
    
    #browser()
    plot_1 <- ggplot(data=df1,aes(x=phats))+
      geom_histogram(aes(y=..density..),binwidth=wbins,colour="black",fill="grey")+
      #geom_histogram(binwidth=wbins,colour="black",fill="grey")+
      theme_bw()+
      ggtitle(paste0("10,000 simulations of estimated success from simple model ignoring sampling fraction (n=",input$obs,", p=",input$p,")\n 1st & 99th percentiles as dashed lines"))+
      xlab('per cent "success"')+
      ylab("density")+
      geom_vline(xintercept=p_01,linetype="dashed")+
      geom_vline(xintercept=p_99,linetype="dashed")+
      xlim(plot_xmin,plot_xmax)+
      ylim(plot_ymin,plot_ymax)+
      theme(axis.text.x=element_text(size=rel(1.5)))+
      theme(axis.text.y=element_text(size=rel(1.5)))+
      theme(axis.title.x=element_text(size=rel(1.5)))+
      theme(axis.title.y=element_text(size=rel(1.5)))+
      theme(plot.title=element_text(size=rel(1.5)))
    print(plot_1)
    #hist(pcalcs()[[1]], col = 'darkgray', border = 'white')
  })
  
  
  
  ptiles <- reactive({
    df2 <- data.frame(
      Percentile=c("1%",
                   "5%",
                   "25%",
                   "50%",
                   "75%",
                   "95%",
                   "99%"),
	    Value=pcalcs()[[2]])
  })
  # Show the values using an HTML table
  output$values <- renderTable({
          ptiles()
  }, include.rownames=FALSE)

 
  output$distPlotHG <- renderPlot({
    df1 <- data.frame(100*pcalcsHG()[[1]])
    names(df1)[1] <- 'phatsHG'
    n1 <- input$obs
    range1 <- range(df1$phatsHG)
    range_use <- range1[2] - range1[1]
    if(n1<225) {
      div1 <- max(5,trunc(sqrt(n1)))
      wbins <- range_use/div1
    } else wbins <- range_use/15
    # wbins <- range_use/10
    p_01 <- 100*pcalcsHG()[[2]][1]
    p_99 <- 100*pcalcsHG()[[2]][7]
    plot_xmin <- range_binom()[1]
    plot_xmax <- min(range_binom()[2],100)
    plot_ymin <- range_y()[1]
    plot_ymax <- range_y()[2]
   #browser()
     plot_1 <- ggplot(data=df1,aes(x=phatsHG))+
      geom_histogram(aes(y=..density..),binwidth=wbins,colour="black",fill="grey")+
      #geom_histogram(binwidth=wbins,colour="black",fill="grey")+
      theme_bw()+
      ggtitle(paste0("10,000 simulations of estimated success from adjusted model (n=",input$obs,", p=",input$p,", sampling fraction =",input$frac,"%)\n 1st & 99th percentiles as dashed lines"))+
      xlab('per cent "success"')+
      ylab("density")+
      geom_vline(xintercept=p_01,linetype="dashed")+
      geom_vline(xintercept=p_99,linetype="dashed")+
      xlim(plot_xmin,plot_xmax)+
      ylim(plot_ymin,plot_ymax)+
      theme(axis.text.x=element_text(size=rel(1.5)))+
      theme(axis.text.y=element_text(size=rel(1.5)))+
      theme(axis.title.x=element_text(size=rel(1.5)))+
      theme(axis.title.y=element_text(size=rel(1.5)))+
      theme(plot.title=element_text(size=rel(1.5)))
    print(plot_1)
    #hist(pcalcs()[[1]], col = 'darkgray', border = 'white')
  })
 
  #Create table of percentiles
  ptiles <- reactive({
    df2 <- data.frame(
      Percentile=c("1st",
                   "5th",
                   "25th",
                   "50th",
                   "75th",
                   "95th",
                   "99th"),
      Value=pcalcs()[[2]])
  })
 
  
  ptilesHG <- reactive({
    df2 <- data.frame(
      Percentile=c("1st",
                   "5th",
                   "25th",
                   "50th",
                   "75th",
                   "95th",
                   "99th"),
      ValueHG=pcalcsHG()[[2]])
  })
  
  #Now create a data frame for the two distributions
  data_table <- reactive({
    nfrac1 <- input$frac
    x1 <- data.frame(ptiles(),ptilesHG()[,2])
    x1[,2] <- round(x1[,2]*100,1)
    x1[,3] <- round(x1[,3]*100,1)
    names(x1)[2] <- "simple model"
    names(x1)[3] <- paste0("sampling fraction =",nfrac1,"%")
    return(x1)
  })
  
  # Show the values using an HTML table
  output$values <- renderTable({
    data_table()
  }, include.rownames=FALSE,digits=1)
  
#   output$valuesHG <- renderTable({
#     ptilesHG()
#   }, include.rownames=FALSE)
  
  
  #plot a sample run chart with median
  output$runChartHG <- renderPlot({
    df2 <- data.frame(100*pcalcsHG()[[1]][1:20])
    names(df2)[1] <- "data"
    df2$order <- c(1:20)
    med_ref <- median(df2[,1])
    med_ref1 <- round(med_ref,1)
    range1 <- max(df2$data) - min(df2$data)
    p2 <- ggplot(data=df2,aes(x=order,y=data))+
      theme_bw()+
      geom_point(size=4)+
      geom_line()+
      xlab("Time Period")+
      ylab("Percent")+
      ggtitle(paste0("20 simulations of estimated good performance from adjusted model (n=",input$obs,", p=",input$p,", sampling fraction =",input$frac,"%)\n median of 20 values = ",med_ref1,"% as dashed line"))+
      geom_hline(yintercept=med_ref,linetype="dashed")+
      ylim(max(0,min(df2$data)-0.10*range1),min(100,max(df2$data)+ 0.10*range1))+
      theme(axis.text.x=element_text(size=rel(1.5)))+
      theme(axis.text.y=element_text(size=rel(1.5)))+
      theme(axis.title.x=element_text(size=rel(1.5)))+
      theme(axis.title.y=element_text(size=rel(1.5)))+
      theme(plot.title=element_text(size=rel(1.5)))
    print(p2)
  }) 
  
  
 }


  
)




#shinyApp(ui = ui, server = server)
