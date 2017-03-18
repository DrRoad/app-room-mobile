# mobile UI to show actuators
# last update: 2017-03-18

actuators <- function(){
        tabPanel('Aktoren',
                 bsAlert('taskInfo'),
                 selectInput('actuatorList',
                             'Aktoren:',
                             actuatorUiList,
                             multiple = TRUE, 
                             selectize = FALSE,
                             size = 12,
                             choices = c('')),
                 actionButton('runActuatorList', 
                              'Jetzt ausführen',
                              icon('cogs')))
}