library("shiny")

library("utils")
library("flowCore")
library("Biobase")

library("gtools")
library("stringr")

options(shiny.maxRequestSize = 5000*1024^2, shiny.fullstacktrace = TRUE)

cytofCore.updateFlowFrameKeywords <- function(flowFrame){
  
  row.names(flowFrame@parameters) <- paste0("$P",c(1:length(row.names(flowFrame@parameters))))
  params = parameters(flowFrame)
  pdata = pData(params)
  for (i in 1:ncol(flowFrame)){
    s = paste("$P",i,"S",sep="");
    n = paste("$P",i,"N",sep="");
    r = paste("$P",i,"R",sep="");
    b = paste("$P",i,"B",sep="");
    e = paste("$P",i,"E",sep="");
    fcmax1 <- paste("flowCore_$P",i,"Rmax",sep="");
    fcmin1 <- paste("flowCore_$P",i,"Rmin",sep="");
    fcmax <- paste("flowCore_P",i,"Rmax",sep="");
    fcmin <- paste("flowCore_P",i,"Rmin",sep="");
    keyval=list();
    label <- pData(flowFrame@parameters)[,"desc"][i]
    if(is.na(label)) {label <- colnames(flowFrame)[i] }
    keyval[[s]] = label
    keyval[[n]] = colnames(flowFrame)[i]         
    keyval[[r]] = ceiling(max(exprs(flowFrame)[,i])-min(exprs(flowFrame)[,i]))
    keyval[[b]] = 32;
    keyval[[e]] = "0,0";
    keyval[[fcmax1]] <- ceiling(max(exprs(flowFrame)[,i])-min(exprs(flowFrame)[,i]))
    keyval[[fcmin1]] <- ceiling(min(exprs(flowFrame)[,i]))
    keyval[[fcmax]] <- ceiling(max(exprs(flowFrame)[,i])-min(exprs(flowFrame)[,i]))
    keyval[[fcmin]] <- ceiling(min(exprs(flowFrame)[,i]))
    keyword(flowFrame) = keyval;
    
    pdata[i,"minRange"]=min(exprs(flowFrame)[,i])
    pdata[i,"maxRange"]=max(exprs(flowFrame)[,i])
    
  }
  pData(params)=pdata
  parameters(flowFrame)=params
  row.names(flowFrame@parameters) <- paste0("$P",c(1:length(row.names(flowFrame@parameters))))
  # keyval[["$DATATYPE"]] <- "F"
  return(flowFrame)
}

shinyServer(function(input, output, session) {

	# listObject <- reactiveValues(
	# 	Dirname = 1, #dir name if uploaded in server
 #    Filesname = 1
 #    )

  fcsSelected <- reactive({
    if(is.null(input$fcs_upload)) return(NULL)
    return(TRUE)
  })

  fcsData <- reactive({
    if(is.null(fcsSelected())) return (NULL)
    if(!is.null(input$fcs_upload)) {files <- input$fcs_upload ; return(files)}
  })

  observe({
    output$loadCSV <- renderUI({
      if(!is.null(fcsData())) return(NULL)
      fileInput("csvTofcs","CSV to FCS",multiple=TRUE)
    })
  })

  observeEvent(input$linkFCS,{
    progress <- Progress$new()
    progress$set(message="Generate FCS", value=0)
    dir <- basename(tempdir())
    unlink(dir, recursive=TRUE)
    dir.create(dir)

    lapply(c(1:dim(input$csvTofcs)[1]), function(i) {
      progress$set(detail=input$csvTofcs[i,"name"], value=i/dim(input$csvTofcs)[1])
     
      csv <- read.csv(input$csvTofcs[i,"datapath"], header=TRUE, check.names=FALSE)
      data <- as.matrix(csv)
      res <- apply(data,2,function(x){sum(x)})
      data <- data[,which(!is.na(res))]
      metadata <- data.frame(name=dimnames(data)[[2]],
                    desc=paste(dimnames(data)[[2]])
                  )
      metadata$range <- apply(apply(data,2,range),2,diff)
      metadata$minRange <- apply(data,2,min)
      metadata$maxRange <- apply(data,2,max)

      data.ff <- new("flowFrame",exprs=data,parameters=AnnotatedDataFrame(metadata))

      lapply(c(1:dim(data)[2]),function(x){data.ff@description[[paste0("$P",x,"R")]] <<- metadata$maxRange[[x]]})
      # data.ff <- cytofCore.updateFlowFrameKeywords(data.ff)
      name <- paste0(dir,"/",input$csvTofcs[i,"name"],".fcs")
      write.FCS(data.ff, name,)
    })
    progress$close()

    output$linkFCSZip <- renderText({
      return("Convert Done !")
    #   unlink(paste0(dir,".zip"))
    #   zip(paste0(dir,".zip"),list.files(paste0(dir,"/"),full.names=TRUE,pattern=".fcs"))
    #   unlink(paste0(dir,"/*"),recursive = TRUE)
    })

  })

  # observeEvent(input$linkCSV,{ ## FAIRE UN NOM RANDOM POUR ZIP ET ECRIR AVEC LES NOM DE FICHIER UPLOADER OU LOADER.
  #   progress <- Progress$new()
  #   flow.frames <- listObject$Common
  #   dir <- basename(tempdir())
  #   progress$set(message="Generate CSV", value=0)
  #   lapply(c(1:length(flow.frames)), function(x){
  #     progress$set(detail=listObject$Filesname[[x]], value=x/length(flow.frames))
  #     dir.create(paste0("/media/data/html/temp/",dir))
  #     name <- paste0("/media/data/html/temp/",dir,"/",listObject$Filesname[[x]],".csv")
  #     mat <- exprs(flow.frames[[x]])
  #     write.csv(mat,name)
  #   })

  #   output$linkCSVZip <- renderText({
  #     unlink(paste0("/media/data/html/temp/",dir,".zip"))
  #     setwd("/media/data/html/temp/")
  #     zip(paste0(dir,".zip"),list.files(paste0(dir,"/"),full.names=TRUE,pattern=".csv"))
  #     setwd("/media/data/html/shiny/ToolsBox")
  #     unlink(paste0("/media/data/html/temp/",dir,"/*"),recursive = TRUE)
  #     print(paste0("<a href='","http://gameoftools.ciphe.local/temp/",dir,".zip","', target='_blank'>Download CSV</a>"))
  #   })

  #   progress$close()
  # })

})
