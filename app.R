# =================================================
# : Coursera.org
# : Data Science Specialization
# : module: Developing Data Products - August. 2015
# :
# : Course Project - Peer Assessments
# : Shiny Application and Reproducible Pitch
# :
# : Author  - Sergio Vicente
# : twitter - @svicente99
# =================================================


server <- function(input, output, session) {

  observe({

	    # Change both the mean and the standard deviation, if user's clicked in "N(0,1)"
  		if(input$chkNorm01) {
		    updateTextInput(session, "mean", value = 0);
		    updateTextInput(session, "std",  value = 1)
  		}
  			
		if(input$mean=="") 
			updateTextInput(session, "mean", value = 0)
		if(input$std == "") 
			updateTextInput(session, "std", value = 1)
		
		mu <- as.numeric(input$mean)
		sd <- as.numeric(input$std)
		stp <- if(sd<10)  1 else 10

	    # Control the value, min, max, and step.
	    # The range is limited to six sigma. The initial range (area) is 20% around mean.
	    vMin <- floor(mu-6*sd);    vMax <- floor(mu+6*sd)
		if(mu!=0)  { inf <- as.numeric(mu*0.8); sup <- as.numeric(mu*1.2) }
		else       { inf <- -1; sup<- 1 }
	    updateSliderInput(session, "limits", value = c(inf,sup), 
	     min = vMin, max = vMax, step = stp)
      })

  output$distPlot <- renderPlot({
  
  	# get values from user interface (UI)
	mean=as.numeric(input$mean); sd=as.numeric(input$std)

	low_lim=as.numeric(input$limits[1])
	upp_lim=as.numeric(input$limits[2])

	# plot normal dist. graphics
	x <- seq(-4,4,length=100)*sd + mean
	hx <- dnorm(x,mean,sd)

	plot(x, hx, type="n", xlab="X values", ylab="", main="Normal Distribution", axes=FALSE)
	box(lwd=3, lty=2, col='#e0e0e0')
	
	# plot probability area under normal curve
	i <- x >= low_lim & x <= upp_lim
	lines(x, hx)
	polygon(c(low_lim,x[i],upp_lim), c(0,hx[i],0), col="red") 

	area <- pnorm(upp_lim, mean, sd) - pnorm(low_lim, mean, sd)
	
	# trace a line to assign mean of distribution
	arrows(mean, 0, mean, 1, col = "#c0c0c0", lty=2, lwd=2)
	
	# display probability area expression
	result <- paste("P(",low_lim,"< X <",upp_lim,") =", signif(area, digits=4))
	mtext(result, 3, cex=1.5, font=2)
	
	# show X axis, according to min and max limits
	vMin <- floor(mean-6*sd)
	vMax <- floor(mean+6*sd)
	step <- if(sd<10)  1 else 10
	axis(1, at=seq(vMin, vMax, step), pos=0)
  })
}

ui <- fluidPage(
	# Application title
  	titlePanel("Developing Data Products - Course Project - Sergio Vicente"),
  	
  	# User interface controls
    sidebarLayout(
	    sidebarPanel(
			p("For your normal distribution, please, fill these values as data input:"),	
			textInput(inputId="mean", label = "Mean or Average (mu)", value=100),
			textInput(inputId="std", label = "Standard Deviation (sigma)", value=15),
			sliderInput("limits", label = h4("Probability Area Limits"), 
				min = 0, max = 200, value = c(100-20, 100+20)),
			checkboxInput("chkNorm01", "Normal 0-1 ('Standard')", FALSE),
			tags$head(
				tags$style(type="text/css", "#mean {width: 80px}"),
				tags$style(type="text/css", "#std  {width: 80px}")
	    		)
    	),
    
		mainPanel(
		   	tabsetPanel(
			    tabPanel("Plot", plotOutput("distPlot")), 
			    tabPanel("Help", htmlOutput("help"),
			    	tags$div(id="help", 
			    	  HTML("<iframe id='ifrHelp' src='help.html' height='350' width='600'></iframe>")
			    	)
			    )
			)
		)
	)
)

shinyApp(ui = ui, server = server)


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

# References I've used to write this code:
# --------------------------------------- (in order of development)

# Normal Probability plot:
# http://www.statmethods.net/advgraphs/probability.html

# Slider range control:
# http://shiny.rstudio.com/gallery/slider-bar-and-slider-range.html

# Change the value of a slider input on the client:
# http://shiny.rstudio.com/reference/shiny/latest/updateSliderInput.html

# Shiny slider - retreiving values - list, or...
# http://stackoverflow.com/questions/24111838/shiny-slider-retreiving-values-list-or

# Add Arrows to a Plot
# https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/arrows.html

# Changing the values of inputs from the server
# http://shiny.rstudio.com/gallery/update-input-demo.html

# Application layout guide
# http://shiny.rstudio.com/articles/layout-guide.html

# Customize your UI with HTML
# http://shiny.rstudio.com/articles/html-tags.html
