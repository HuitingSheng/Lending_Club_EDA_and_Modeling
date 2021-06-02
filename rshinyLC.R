#install.packages('rsconnect')
library(rsconnect)
if (!require(shiny)){install.packages(shiny); require(shiny)}
if(!require(ggplot2)){install.packages("ggplot2"); require(ggplot2)}
library(ggplot2)
library(pROC)
load("roc.RData")

ui= fluidPage(titlePanel("lending club"),
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = "model", label="Select a Model", choices = c("Elastic Net", "XGBoost","CART","Bagged CART","Random Forest","ALL")),
                  sliderInput(inputId = "threshold1", label = "threshold", min=0, max=1, value=0.2)
                ),
                mainPanel(plotOutput(outputId = "AucPlot"))
              )
              )

server = function(input, output){
    output$AucPlot = renderPlot(
      if (input$model =="Elastic Net"){
        plot(elasticnet.roc,print.thres = input$threshold1,  main = "Elastic Net ROC Curve")
        legend("topleft",legend=paste("AUC = ",round(elasticnet.roc$auc,3)))

      }
      else if(input$model =="XGBoost"){
        plot(XGBoost.roc, print.thres = input$threshold1,  main = "eXtreme Gradient Boosting ROC Curve")
        legend("topleft",legend=paste("AUC = ",round(XGBoost.roc$auc,3)))
      }
      else if(input$model =="CART"){
        plot(CART.roc, print.thres = input$threshold1, main = "CART ROC Curve")
        legend("topleft",legend=paste("AUC = ",round(CART.roc$auc,3)))
      }
      else if(input$model =="Bagged CART"){
        plot(treebag.roc, print.thres = input$threshold1, main = "Bagged CART ROC Curve")
        legend("topleft",legend=paste("AUC = ",round(treebag.roc$auc,3)))
      }
      else if(input$model =="Random Forest"){
        plot(ranger.roc, print.thres = input$threshold1,  main = "CART ROC Curve")
        legend("topleft",legend=paste("AUC = ",round(ranger.roc$auc,3)))
      }
      else if (input$model =="ALL"){
        plot(elasticnet.roc,  col="black", main="ROC curve for all models")
        lines(XGBoost.roc,  col="red")
        lines(treebag.roc, col="blue")
        lines(CART.roc, col="green")
        lines(ranger.roc, col="orange")
        #lines(knn.roc,legacy.axes = TRUE, main = "KNN ROC Curve", col="grey")
        legend("topleft", col=c("black", "red", "blue", "green","orange","grey"), lty=1,
               legend=c("Elastic Net", "XGBoost","Treebag","CART","Random Forest"))
        
        }

    )
}
  
shinyApp(ui = ui, server = server)
  
