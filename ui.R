library("shinyjs")
library("flowCore")

shinyUI(fluidPage(
	shinyjs::useShinyjs(),
	titlePanel("CSVtoFCS"),
	sidebarLayout(
		sidebarPanel(
			# fileInput("fcs_upload","FCS to CSV",multiple=TRUE),
			uiOutput("loadCSV"),
			actionButton("linkFCS","CSV(s) to FCS(s)"),
			uiOutput("linkFCSZip")
		),
		mainPanel(
		)
	)
))
