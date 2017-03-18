# mobile UI to show sensor data
# last update: 2017-03-18

source('appSelect.R')

sensors <- function(){
        tabPanel('Sensoren',
                 appSelect(),
                 bsAlert('dataStatus'),
                 plotlyOutput('sensorChart')
        )
}