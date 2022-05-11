# server.R

library(shiny)

setFirstGeneration<-function(selectedFirstGeneration, x) {
  nr<-nrow(x)
  nc<-ncol(x)
  xc<-as.integer(nr/2)
  yc<-as.integer(nc/2)
  
  for (i in 1:nr) {
    for (j in 1:nc) {
      x[i,j]<-FALSE
    }
  }
  
  if (selectedFirstGeneration == "Block of Five") {
    x[xc+2,yc+2]<-TRUE; x[xc+2,yc+1]<-TRUE; x[xc+2,yc]<-TRUE; x[xc+2,yc-1]<-TRUE; x[xc+2,yc-2]<-TRUE; 
    x[xc+1,yc+2]<-TRUE; x[xc+1,yc+1]<-TRUE; x[xc+1,yc]<-TRUE; x[xc+1,yc-1]<-TRUE; x[xc+1,yc-2]<-TRUE;
    x[xc+0,yc+2]<-TRUE; x[xc+0,yc+1]<-TRUE; x[xc+0,yc]<-TRUE; x[xc+0,yc-1]<-TRUE; x[xc+0,yc-2]<-TRUE;
    x[xc-1,yc+2]<-TRUE; x[xc-1,yc+1]<-TRUE; x[xc-1,yc]<-TRUE; x[xc-1,yc-1]<-TRUE; x[xc-1,yc-2]<-TRUE;
    x[xc-2,yc+2]<-TRUE; x[xc-2,yc+1]<-TRUE; x[xc-2,yc]<-TRUE; x[xc-2,yc-1]<-TRUE; x[xc-2,yc-2]<-TRUE;
  }
  else if (selectedFirstGeneration == "Glider") {
    x[xc-1, yc-1]<-TRUE
    x[xc,   yc-1]<-TRUE
    x[xc+1, yc-1]<-TRUE
    x[xc+1, yc]  <-TRUE
    x[xc,   yc+1]<-TRUE
  }
  else if (selectedFirstGeneration == "Glider Cannon") {
    x[xc-2,yc+16]<-TRUE; x[xc-1,yc+16]<-TRUE;
    x[xc-2,yc+17]<-TRUE; x[xc-1,yc+17]<-TRUE;
    x[xc-4,yc+6] <-TRUE; x[xc-3,yc+6] <-TRUE; x[xc+1,yc+6]<-TRUE; x[xc+2,yc+6]<-TRUE;
    x[xc-3,yc+4] <-TRUE; x[xc+1,yc+4] <-TRUE; 
    x[xc-2,yc+3] <-TRUE; x[xc-1,yc+3] <-TRUE; x[xc,  yc+3]<-TRUE; 
    x[xc-2,yc+2] <-TRUE; x[xc-1,yc+2] <-TRUE; x[xc,  yc+2]<-TRUE; 
    x[xc+1,yc-1] <-TRUE; 
    x[xc,  yc-2] <-TRUE; x[xc+1,yc-2] <-TRUE; x[xc+2,yc-2]<-TRUE; 
    x[xc-1,yc-3] <-TRUE; x[xc+3,yc-3] <-TRUE; 
    x[xc+1,yc-4] <-TRUE; 
    x[xc-2,yc-5] <-TRUE; x[xc+4,yc-5] <-TRUE; 
    x[xc-2,yc-6] <-TRUE; x[xc+4,yc-6] <-TRUE; 
    x[xc-1,yc-7] <-TRUE; x[xc+3,yc-7] <-TRUE; 
    x[xc,  yc-8] <-TRUE; x[xc+1,yc-8] <-TRUE; x[xc+2,yc-8]<-TRUE; 
    x[xc,  yc-17]<-TRUE; x[xc+1,yc-17]<-TRUE;
    x[xc,  yc-18]<-TRUE; x[xc+1,yc-18]<-TRUE;
  }
  x
}

nextGeneration<-function(x) {
  nr<-nrow(x)
  nc<-ncol(x)
  y<-matrix(data=FALSE,nrow=nr,ncol=nc)
  
  for (i in 1:nr) {
    for (j in 1:nc) {
      # count the number of occupied cells surrounding cell [i,j]
      occupied<-0
      
      if (i > 1)  {left  <-i-1} else {left  <-nr}
      if (i < nr) {right <-i+1} else {right <-1}
      if (j > 1)  {bottom<-j-1} else {bottom<-nc}
      if (j < nc) {top   <-j+1} else {top   <-1}
      
      if (x[left, top])    {occupied<-occupied+1}
      if (x[i,    top])    {occupied<-occupied+1}
      if (x[right,top])    {occupied<-occupied+1}
      if (x[left, j])      {occupied<-occupied+1}
      if (x[right,j])      {occupied<-occupied+1}
      if (x[left, bottom]) {occupied<-occupied+1}
      if (x[i,    bottom]) {occupied<-occupied+1}
      if (x[right,bottom]) {occupied<-occupied+1}
      
      # determine if cell [i,j] is occupied in the next generation
      if (x[i,j]) {
        # cell [i,j] is occupied -- check if it survives
        if (occupied==2 || occupied==3) {y[i,j]<-TRUE}
      }
      else {
        # cell[i.j] is unoccupied -- check if it gives birth
        if (occupied==3) {y[i,j]<-TRUE}
      }
    }
  }
  y
}

addOne<-function(n) {
  n+1
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # Reactive Value to Store Dataframe
  reactives <- reactiveValues()
  
  reactives$x<-matrix(data=FALSE,nrow=40,ncol=80)
  reactives$generation<-as.integer(1)
  
  observeEvent(input$nextGeneration, {
    reactives$x<-nextGeneration(reactives$x)
    reactives$generation<-addOne(reactives$generation)
  })
  
  observeEvent(input$setInitialConfiguration, {
    reactives$x<-setFirstGeneration(input$firstGeneration, reactives$x)
    reactives$generation<-as.integer(1)
  })
  
  output$distPlot<-renderPlot({
    plot(reactives$x, xlab="", ylab="", main="", asp=1.)
  })
  
  output$nGenerationLabel<-renderText({"Generation: "})
  output$nGeneration<-renderText({reactives$generation})
})
