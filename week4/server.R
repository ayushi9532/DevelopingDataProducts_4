library(shiny)
library(datasets)

mpg_Data <- mtcars
mpg_Data$am <- factor(mpg_Data$am, labels = c("Automatic", "Manual"))

shinyServer(function(input, output) {
    
    fTxt <- reactive({
        paste("mpg ~", input$variable)
    })
    
    fTxtPoint <- reactive({
        paste("mpg ~", "as.integer(", input$variable, ")")
    })
    
    fit <- reactive({
        lm(as.formula(fTxtPoint()), data=mpg_Data)
    })
    
    output$caption <- renderText({
        fTxt()
    })
    
    output$mpgBoxPlot <- renderPlot({
        boxplot(as.formula(fTxt()), 
                data = mpg_Data,
                outline = input$outliers)
    })
    
    output$fit <- renderPrint({
        summary(fit())
    })
    
    output$mpgPlot <- renderPlot({
        with(mpg_Data, {
            plot(as.formula(fTxtPoint()))
            abline(fit(), col=2)
        })
    })
    
})