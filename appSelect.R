# UI for selecting a date-range
# last update: 2017-03-01

appSelect <- function(){
        tagList(
               selectInput('dateSelect',
                           label = 'Auswahl',
                           choices = c('letzte Woche'='1',
                                       'letztes Monat'='2',
                                       'letzten 2 Monate'='3',
                                       'letzten 6 Monate'='4',
                                       'aktuelles Jahr'='5',
                                       'letztes Jahr'='6',
                                       'alle Daten'='10',
                                       'individuell'='7'),
                           selected = 2),
                selectInput('sensorSelect',
                            label = NA,
                            choices = c('leer'),
                            selected = 'leer'),
                hr()
        )

}
