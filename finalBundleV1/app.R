#importing the packages
library(shiny)
library(shinyBS)
library(ggplot2)

#importing the prediction functions
source('prediction.R', local = TRUE)

ui <- 
fluidPage(
  pageWithSidebar(
    
    #header text
    headerPanel('Software Project Prediction System'),
    
    #side section 
     sidebarPanel(
       #system information panel
       conditionalPanel(
         condition = "false",
         selectInput(
           "active_select_panel",
           label = "Active Panel:",
           choices = c("","View System info"),
           selected = ""
         )
       ),
       
      #system inputs - 16 questions about critical success factors
      selectizeInput("sufficient_development_staff", "Software staff was sufficient to complete the project on-time", choices =  list(
        "Yes",
        "No")),
      selectizeInput("sufficient_overall_staff", 
                     "Overall staff was sufficient to complete the project on-time", 
                     choices =  list("Yes", "No")
      ),
      selectizeInput("experienced_project_manager", "Project Manager is experienced", 
                     choices =  list("Yes", "No")
      ),
      selectInput("reuse_assets_introduced",
                  label = tags$span("Re-use process/assets introduced in the project",
                                    bsButton("reuse_assets_info", label = "", 
                                             icon = icon("info"),
                                             style = "info", size = "extra-small")
                  ),
                  choices =  list( "Yes","No")
      ),
      
      #more info pop over for the reuse assets introduced question
      bsPopover(
        id = "reuse_assets_info",
        title = "More information",
        content = paste0(
          "Re-use assets such as domain analysis, qualifications, systematic practices and models"
        ),
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      ),
      selectizeInput("reuse_asset_qualification", "Project's re-use assets underwent a defined 
                 quality procedure to be declared as reusable", 
                     choices =  list("Yes","No")
      ),
      selectizeInput("top_management_support",
                     label = tags$span("Project recieved Top Management support/commitment",
                                       bsButton("top_management_support_info", label = "", 
                                                icon = icon("info"),
                                                style = "info", size = "extra-small")
                     ),
                     choices =  list("Yes","No")
      ),
      
      #more info pop over for the top management support question
      bsPopover(
        id = "top_management_support_info",
        title = "More information",
        content = paste0( "Support/commitment includes regular communication with Project Manager/team,  providing resources and funds, introducing business plan, requirements, schedule,standards and rules to satisfy"
        ),
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      ),
      selectizeInput("domain_analysis",
                     label = tags$span("Domain Analysis was performed before project initiation",
                                       bsButton("domain_analysis_info", label = "", 
                                                icon = icon("info"),
                                                style = "info", size = "extra-small")
                     ),
                     choices =  list("Yes","No")
      ),
      
      #more info pop over for the domain analysis question
      bsPopover(
        id = "domain_analysis_info",
        title = "More information",
        content = paste0( "Domain Analysis includes capturing clear understanding of the domain, identification of re-use models, objectives/goals"
        ),
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      ),
      selectizeInput("risks_identification", "Potential risks identified at an early stage of the project", 
                     choices =  list("Yes", "No")
      ),
      selectizeInput("risk_management", "Risk management performed in the project", 
                     choices =  list("Yes","No")
      ),
      selectizeInput("communication", 
                     label = tags$span("Adequate communication carried out in the project",
                                       bsButton("communication_info", label = "", 
                                                icon = icon("info"),
                                                style = "info", size = "extra-small")
                     ),
                     choices =  list("Yes","No")
      ),
      
      #more info pop over for the communication question
      bsPopover(
        id = "communication_info",
        title = "More information",
        content = paste0("Regular communication/feedbacks between team members, Project Manager and top management"
        ),
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      ),
      selectizeInput("time_line_monitoring", "Time-line was monitored throughout the project", 
                     choices =  list("Yes","No")
      ),
      selectizeInput("budget_monitoring", "Budget was monitored throughout the project", 
                     choices =  list("Yes","No")
      ),
      selectizeInput("change_monitoring", "Project's changes over time were monitored throughout the project", 
                     choices =  list("Yes","No")
      ),
      selectizeInput("resource_monitoring", "Project's resources were monitored throughout the project", 
                     choices =  list("Yes","No")
      ),
      selectizeInput("sufficient_budget_allocation", "Sufficient budget was allocated for the project", 
                     choices =  list("Yes","No")
      ),
      selectizeInput("human_factors",
                     label = tags$span("Human factors were considered",
                                       bsButton("human_factors_info", label = "", 
                                                icon = icon("info"),
                                                style = "info", size = "extra-small")
                     ),
                     choices =  list("Yes","No")
      ),
      
      #more info pop over for the human factors question
      bsPopover(
        id = "human_factors_info",
        title = "More information",
        content = paste0("Addressed via measures such as training, awareness, motivation actions"
        ),
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      ),
      actionButton("submit", "Submit", class = "btn-primary"),
      
    ),
    
    #main body section
    mainPanel(
      #view system information section with collapse functionality
      bsCollapse(id = "prediction_analysis", open = "System info",
                 bsCollapsePanel("View System info", "", 
                                 'This system is designed to predict the final outcome (success/failure) of your software project based on 16 success factors that you will be selecting from the side panel.
                                 To view the predicted outcome and the importance plots of the success factors, please select your options from the right side panel and click submit. This system will guide you to identify the most important factors at an initial stage of the project. By following these success factors you will be able to achieve a higher level of success in your project and thereby reducing the chances of potential project failure at the end.',
                                 style = "info")
      ),
      verbatimTextOutput("prediction"), #prediction result output
      verbatimTextOutput("info1"),  #text about importance factors
      plotOutput("plot")  #Important factors barplot representation
    )
  ), title = "Software Project Prediction System")

server <- function(input, output, session) {
  
  #submit button to get prediction outputs
  actionButton("submit", "Submit")
  
  #update the collapse functionality of view system information panel on click
  observe({
    shinyBS::updateCollapse(session,
                            "prediction_analysis",
                            open = input$activePanelSelect)
  })
  
  #on submit functionality
  observeEvent(c(input$submit), ignoreInit = TRUE, {
    prediction<- function(){
      
      #make the prediction based on 16 inputs/questions
     result_list <- make_prediction( isolate(list(sufficient_development_staff = input$sufficient_development_staff, 
                                  sufficient_overall_staff = input$sufficient_overall_staff,
                                  experienced_project_manager = input$experienced_project_manager, 
                                  reuse_assets_introduced = input$reuse_assets_introduced,
                                  reuse_asset_qualification = input$reuse_asset_qualification,
                                  top_management_support = input$top_management_support,
                                  domain_analysis = input$domain_analysis,
                                  risks_identification = input$risks_identification,
                                  risk_management = input$risk_management,
                                  communication = input$communication,
                                  time_line_monitoring = input$time_line_monitoring,
                                  budget_monitoring = input$budget_monitoring,
                                  change_monitoring = input$change_monitoring,
                                  resource_monitoring = input$resource_monitoring,
                                  sufficient_budget_allocation = input$sufficient_budget_allocation,
                                  human_factors = input$human_factors
                                  )
                               )
                       )
     return(result_list)
    }
    
    #get all prediction outputs including important factors by random forest and probability of the prediction
    returned_result_list <- prediction()
    imp <- data.frame(returned_result_list['imp'])
    final_prediction <- returned_result_list['prec']
    probability_of_success_or_failure = returned_result_list['prob']

    #set outputs to the main body section
    output$prediction <-renderPrint(get_prediction(final_prediction$prec, probability_of_success_or_failure$prob))
    output$info1 <-renderText( paste("To increase the chances of success in your project",
                                     " You may consider the most important success factors plotted below", sep="\n"))
    imp_asc <- imp[order(-imp$imp.MeanDecreaseAccuracy),]
    
    #add colours to the bar plot
    color_function <- colorRampPalette( c( "#104E8B" , "#CCCCCC") )
    #Reduce color shade as importance decreases 
    color_ramp <- color_function( n = nrow( x = imp ) )
            
    #plot the importance as a bar graph
    output$plot <- renderPlot({
      x <- barplot(imp_asc$imp.MeanDecreaseAccuracy, 
                   beside=TRUE, 
                   space=0.1, 
                   ylim = c(-10, 40),
                   xlab = 'Critical Success Factors',
                   ylab = 'Importance',
                   main='Barplot -Success Factors by its importance',
                   col = color_ramp)
      labs <- paste(row.names(imp_asc))
      text(cex=0.65, x=x-.25, y=-10, labs, xpd=TRUE, srt=90)
    })
    })
  }


#run the app as a shiny app
shinyApp(ui, server)
