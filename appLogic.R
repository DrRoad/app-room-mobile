# application specific logic
# last update: 2017-03-01

# any record manipulations before storing a record
appData <- function(record){
        record
}

getRepoStruct <- function(repo){
        appStruct[[repo]]
}

repoData <- function(repo){
        data <- data.frame()
        app <- currApp()
        if(length(app) > 0){
                url <- itemsUrl(app[['url']],
                                repo)
                data <- readItems(app, url)
        }
        data
}

# anything that should run only once during startup
appStart <- function(){
        sensors <- readNagiosItems()
        allSensors <- vector()
        sel1 <- ''
        if (nrow(sensors) > 0){
                allSensors <- rownames(sensors)
                sel1 <- rownames(sensors)[1]
        }
        updateSelectInput(
                session,
                'sensorSelect',
                choices = allSensors,
                selected = sel1)
        
        allItems <- readActuatorItems()
        updateSelectInput(session, 'actuatorList',
                          choices = rownames(allItems))
}

readNagiosItems <- reactive({
        app <- currApp()
        sensorItems <- data.frame()
        if(length(app) > 0){
                url <- itemsUrl(app[['url']], 
                                paste0(app[['app_key']],
                                       '.nagios'))
                sensorItems <- readItems(app, url)
                if(nrow(sensorItems) > 0){
                        rownames(sensorItems) <- sensorItems$name
                        sensorItems <- sensorItems[, c('nagiosUrl',
                                                       'repo',
                                                       'user',
                                                       'password',
                                                       'active')]
                }
        }
        sensorItems
})

readActuatorItems <- function(){
        app <- currApp()
        actuatorItems <- data.frame()
        if(length(app) > 0){
                url <- itemsUrl(app[['url']], 
                                paste0(app[['app_key']],
                                       '.actuator'))
                actuatorItems <- readItems(app, url)
                if(nrow(actuatorItems) > 0){
                        rownames(actuatorItems) <- actuatorItems$name
                        actuatorItems <- actuatorItems[, c('scenario',
                                                           'params',
                                                           'command',
                                                           'active')]
                }
        }
        actuatorItems
}

output$sensorChart <- renderPlotly({
        pdf(NULL)
        outputPlot <- plotly_empty()
        sel <- input$sensorSelect
        data <- data.frame()
        sensors <- readNagiosItems()
        if(sel != 'leer'){
                datRepo <- as.character(
                        sensors[rownames(sensors) == sel, 'repo'])
                data <- repoData(datRepo)
        }
        closeAlert(session, 'myDataStatus')
        if(nrow(data) > 0){
                myRange <- c(mymin = as.Date(Sys.Date()),
                             mymax = as.Date(Sys.Date()))
                switch(input$dateSelect,
                       '1' = { myRange <- c(mymin = as.Date(Sys.Date()-7),
                                            mymax = as.Date(Sys.Date())) },
                       '2' = { myRange <- c(mymin = as.Date(Sys.Date() - months(1)),
                                            mymax = as.Date(Sys.Date())) },
                       '3' = { myRange <- c(mymin = as.Date(Sys.Date() - months(2)),
                                            mymax = as.Date(Sys.Date())) },
                       '4' = { myRange <- c(mymin = as.Date(Sys.Date() - months(6)),
                                            mymax = as.Date(Sys.Date())) },
                       '5' = { myRange <- c(mymin = as.Date(paste(year(Sys.Date()),'1','1',sep='-')),
                                            mymax = as.Date(paste(year(Sys.Date()),'12','31',sep='-'))) },
                       '6' = { myRange <- c(mymin = as.Date(Sys.Date() - months(12)),
                                            mymax = as.Date(Sys.Date())) },
                       '10'= { myRange <- c(mymin = as.Date('1970-01-01'),
                                            mymax = as.Date('2070-01-01')) },
                       {})
                
                mymin <- myRange['mymin']
                mymax <- myRange['mymax']
                daterange <- seq(mymin, mymax, 'days')
                data$dat <- as.Date(as.POSIXct(as.numeric(data$timestamp), origin='1970-01-01'))
                data <- data[data$dat %in% daterange, ]
                if(nrow(data) > 0){
                        outputPlot <- plot_ly() %>%
                                add_lines(x = as.POSIXct(data$timestamp, 
                                                         origin='1970-01-01'),
                                          y = data$value,
                                          name = sel) %>%
                                layout( title = '',
                                        showlegend = FALSE,
                                        margin = list(l = 80, r = 80))
                } else {
                        createAlert(session, 'dataStatus', alertId = 'myDataStatus',
                                    style = 'warning', append = FALSE,
                                    title = 'Keine Daten im gewählten Zeitfenster',
                                    content = 'Für das ausgewählte Zeitfenster sind keine Daten vorhanden.')
                }
        } else {
                createAlert(session, 'dataStatus', alertId = 'myDataStatus',
                            style = 'warning', append = FALSE,
                            title = 'Keine Daten für gewählten Bereich',
                            content = 'Für den gewählten Bereich sind noch keine Daten vorhanden.')
        }
        dev.off()
        outputPlot
})

observeEvent(input$runActuatorList, {
        errMsg <- ''
        succMsg <- ''
        selItem <- input$actuatorList
        if(is.null(selItem)){
                errMsg <- 'Keine Aktion ausgewählt.'
        }
        if(errMsg == ''){
                allItems <- readActuatorItems()
                selItemName <- selItem
                selItemCommand <- allItems[rownames(allItems) == selItem, 
                                           'command']
                pcs <- strsplit(as.character(selItemCommand), "\\s+")[[1]]
                cmd <- pcs[1]
                params <- pcs[2:length(pcs)]
                system2(cmd, params)
                succMsg <- paste0('Befehl [', 
                                  paste(pcs, collapse = ' '), 
                                  '] ausgeführt.')
        }
        closeAlert(session, 'myActuatorItemStatus')
        if(errMsg != ''){
                createAlert(session, 'taskInfo', 
                            'myActuatorItemStatus',
                            title = 'Achtung',
                            content = errMsg,
                            style = 'warning',
                            append = FALSE)
        }
        if(succMsg != ''){
                createAlert(session, 'taskInfo', 
                            'myActuatorItemStatus',
                            title = 'Aktion erfolgreich',
                            content = succMsg,
                            style = 'info',
                            append = FALSE)
        }
})