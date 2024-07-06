######### UI
ui <- function(id){
      shiny::htmlTemplate("www/index.html",
  
                 ############## Content of sideBar navigation
                 sidebarNavigation =
                   tagList(
                     introjsUI(),
                     useShinyjs(),
                     tags$head(
                       # # Inclure le JavaScript nécessaire pour la redirection
                       # tags$script(
                       #   '
                       #    Shiny.addCustomMessageHandler("redirect", function(message) {
                       #      window.location = message;
                       #    });
                       #    '
                       # )
                     ),
                     filterStatesRouter_ui("filterStates")
                   ), 
                 ################## Content of Header Navigation
                 headerNav = htmlTemplate("./www/htmlComponents/headerNav.html",
                                          formModal = headerFormModal_ui(id = "formModal"),
                                          feedbackModal = headerFeedbackModal_ui(id = "feedbackModal"),
                                          methodsModal = headerMethodsModal_ui(id = "methodsModal"),
                                          dataModal = headerDataModal_ui(id = "data"),
                                          walkthrough = headerWalkthrough_ui(id = "walkthrough")
                 ),
                 ################# Content of Body display
                 contentSection = tagList(textOutput("url_param"), mainContentRouter_ui("mainContentRouter")
                                          ) 
      )
  
}


