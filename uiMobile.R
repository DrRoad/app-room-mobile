# top-level framework for mobile version
# last update:2016-10-06

uiMobile <- function(){
    tagList(
        initStore("store", "oydStore"),
        tags$script(
                'Shiny.addCustomMessageHandler("setPiaUrl", function(x) {      
                        $("#returnPIAlink").attr("href", x);
                })'
        ),
        navbarPage(
                uiOutput('hdrImageLinkMobile'),
                id='page',
                collapsible=TRUE,
                inverse=FALSE,
                windowTitle=paste0(appTitle, ' | OwnYourData'),
                tabPanel('Sensoren',
                         p('Sensoren')
                ),
                tabPanel('Aktoren',
                         p('Aktoren')
                ),
                tabPanel('Einrichtung',
                         h3('Datentresor'),
                         textInput('pia_urlMobile', 'Adresse:'),
                         textInput('app_keyMobile', 'Identifier:'),
                         textInput('app_secretMobile', 'Secret:'),
                         actionButton('mobilePiaSave', 'Speichern'),
                         br(), br(),
                         uiOutput('currentToken'),
                         conditionalPanel(
                                 condition = "output.currentToken != ''",
                                 actionButton('disconnectPIA', 'Verbindung zu Datentresor trennen', 
                                              icon('chain-broken'))
                         ),
                         br(),
                         uiOutput('connectError')
                )
        )
        # uiOutput('mobileUiStatusItemsRender')
    )
}