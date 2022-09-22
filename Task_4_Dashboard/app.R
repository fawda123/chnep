  rm(list=ls(all=TRUE))
  
  # Load libraries
    if(!require(shiny)){ install.packages("shiny") } ;  library(shiny)
    if(!require(bslib)){ install.packages("bslib") } ;  library(bslib)
    if(!require(leaflet)){ install.packages("leaflet") } ;  library(leaflet)
    if(!require(leaflegend)){ install.packages("leaflegend") } ;  library(leaflegend)
    if(!require(tidyr)){ install.packages("tidyr") } ;  library(tidyr)
    if(!require(dplyr)){ install.packages("dplyr") } ;  library(dplyr)
    if(!require(lubridate)){ install.packages("lubridate") } ;  library(lubridate)
    if(!require(echarty)){ install.packages("echarty") } ;  library(echarty)
    if(!require(echarts4r)){ install.packages("echarts4r") } ;  library(echarts4r)
    if(!require(sf)){ install.packages("sf") } ;  library(sf)
  
  # Load data
    load("shiny.dat.RData")  # Load R environment with shiny inputs

    
  ############################################
  #  Shiny interface
  ############################################
  ui <- fluidPage(
    # shinythemes::themeSelector(),  # un-comment for a floating widget to browse themes during app development
    # Layout for user options
    navbarPage( title = "Water Quality Trends",
                theme = bs_theme( version = 5, bootswatch = "cerulean" ),
                # Station dropdown menu
                div(style="display:inline-block;vertical-align:top;",
                    selectInput( "station", "Location"
                               , choices = stations
                               , selected = "Tidal Peace River Stratum"
                               , width = "350px"
                               )
                          ),
                # Horizontal space
                div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
                # Analyte dropdown menu
                div(style="display:inline-block;vertical-align:top;",
                    selectInput( "analyte", "Analyte"
                               , choices = analytes
                               , selected = "TN"
                               )
                          ),
                # Horizontal space
                div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
                # Trend period radio buttons
                div(style="display:inline-block;vertical-align:top;",
                    radioButtons( "trendperiod", "Trend Period"
                                , choiceNames  = list('5 years','10 years')
                                , choiceValues = list('5y','10y')
                                , selected = list('5y')
                                , inline = TRUE
                                )
                          ),
                ),
    # Layout for map and plots
    fluidRow(
      # Map occupies 1st column
      column( width = 5, leafletOutput("basemap",height=750),
              ),
      # GAM and trend plots occupy rows in the 2nd column
      column( width = 7, ecs.output("gam"),
              fluidRow(
                column( width = 12, ecs.output("trend") )
              )
           )
        )
  )  # // end ui()
  
  
  ############################################
  #  Shiny server
  ############################################
  server <- function( input, output, session ){
    
    # Reactive functions for subsetting data
      # Return results.df for selected analyte (for map and mixmod/trend plot)
      analyte.results <- reactive({
                           results.sub <- results.df[ which( results.df$Analyte==input$analyte ), ]
                           return( results.sub )
                          })
      # Return results.df for selected station (for map)
      station.results <- reactive({
                            results.sub <- results.df[ which( results.df$Station==input$station ), ]
                            return( results.sub )
                          })
      # Return gam.results list element for selected analyte and station (for data/GAM and mixmod/trend plots)
      trend.sub <- reactive({
                      # Get trend results for selected station/stratum and analyte
                      # name.string <- paste0( input$station, ", ", input$analyte )
                      name.string <- results.df$gam.results.name[ which( results.df$Station==input$station &
                                                                         results.df$Analyte==input$analyte ) ]
                      results.idx <- which( names(gam.results)==name.string )
                      this.trend <- gam.results[[results.idx]]
                      # Compute y-axis limits for mixmod trend plot
                      if( this.trend$trend.10y.success ){
                        ybuf1 <- (this.trend$trend.10y.toplo1$bt_upr - this.trend$trend.10y.toplo1$bt_met) |> mean()
                        yrng1 <- range( this.trend$trend.10y.toplo1$bt_upr, this.trend$trend.10y.toplo1$bt_lwr,
                                        this.trend$trend.10y.toplo2$bt_upr, this.trend$trend.10y.toplo2$bt_lwr )
                      } else {
                        ybuf1 <- 0
                        yrng1 <- c(0,0)
                      }
                      if( this.trend$trend.5y.success ){
                        ybuf2 <- (this.trend$trend.5y.toplo1$bt_upr - this.trend$trend.5y.toplo1$bt_met) |> mean()
                        yrng2 <- range( this.trend$trend.5y.toplo1$bt_upr, this.trend$trend.5y.toplo1$bt_lwr,
                                        this.trend$trend.5y.toplo2$bt_upr, this.trend$trend.5y.toplo2$bt_lwr )
                      } else {
                        ybuf2 <- 0
                        yrng2 <- c(0,0)
                      }
                      ybuf <- max( ybuf1, ybuf2 )
                      yrng <- range( yrng1, yrng2 )
                      ymin <- (min( yrng ) - ybuf) |> floor()
                      ymax <- (max( yrng ) + ybuf) |> ceiling()
                      # Remove trend period ("10y" or "5y") from this.trend names
                      idx.1 <- which( !grepl( "trend", names(this.trend) ) )  # Data and GAM
                      idx.2 <- which( grepl( input$trendperiod, names(this.trend) ) )  # Mixmod for selected period
                      tpnames1 <- names(this.trend)[idx.2] 
                      tpnames2 <- c()
                      for( i in 1:length(tpnames1) ){
                        tpnames2[i] <- paste(strsplit(tpnames1[i],"\\.")[[1]][c(1,3)],collapse=".")
                      }
                      names(this.trend)[idx.2] <- tpnames2
                      out.list <- this.trend[c(idx.1,idx.2)]
                      # Convert mixmodel yr from decimal to date
                        # out.list$trend.toplo1$yr <- out.list$trend.toplo1$yr |> date_decimal() |> as.Date()
                        # out.list$trend.toplo2$yr <- out.list$trend.toplo2$yr |> date_decimal() |> as.Date()
                      out.list$ylims <- c(ymin,ymax)
                      return( out.list )
                     })
      # Return colors for strata polygons
      strata.colors <- reactive({
                            strata.colors <- analyte.results()[which(analyte.results()$pt.station==FALSE),
                                                               c("Station","icon.5y.col","icon.10y.col")]
                            strata.colors <- strata.colors[ order(strata.colors$Station), ]
                            return( strata.colors )
                            })
                    
    # Initialize leaflet basemap; markers and selections are set within subsequent observers
    # to avoid resetting map zoom with each user selection
    output$basemap <- renderLeaflet( {
      # Render leaflet map
      leaflet() |> 
        addProviderTiles( providers$CartoDB.Voyager ) |>
        # addProviderTiles(providers$Esri.WorldTopoMap) |>
        # addProviderTiles(providers$Stamen.Terrain) |>
        setView( lng = -82.0, lat = 27.2, zoom = 9 ) |>
        addMapPane( "polygons", zIndex = 410 ) |>                     
        addMapPane( "points", zIndex = 420 )
      } ) 
      
    
    # Observe analyte selection from dropdown (and update selection on map)
    observeEvent( c(input$analyte,input$trendperiod), {
      # Get polygon shapefile and add color info
      strat.analyte <- strat.shp
      strat.analyte$colors <- if( input$trendperiod=="5y" ){ 
        strata.colors()[,"icon.5y.col"]
      } else if( input$trendperiod=="10y" ){
        strata.colors()[,"icon.10y.col"]
      }
      # Add strata polygons
      leafletProxy( "basemap" ) |>
      addPolygons( data = strat.analyte
                   , layerId = ~STRATUM
                   , label = ~STRATUM
                   , stroke = TRUE
                   , color = rgb(0,0,0,1)
                   , weight = 1
                   , opacity = 0.1
                   , fillColor = ~colors
                   , fillOpacity = 0.25
                   , options = pathOptions(pane = "polygons")
                   ) |>
        # Add point stations
        addCircleMarkers( data = analyte.results()[which(analyte.results()$pt.station==TRUE),]
                          , lng = ~lon
                          , lat = ~lat
                          , label = ~Station
                          , layerId = ~Station
                          , radius = if(input$trendperiod=="5y"){~icon.5y.size} else if(input$trendperiod=="10y"){~icon.10y.size}
                          , stroke = TRUE
                          , color = rgb(1,1,1,1)
                          , opacity = 1
                          , weight = 0.5
                          , fill = TRUE
                          , fillColor = if(input$trendperiod=="5y"){~icon.5y.col} else if(input$trendperiod=="10y"){~icon.10y.col}
                          , fillOpacity = 1
                          , options = pathOptions(pane = "points")
                          )
      }, ignoreNULL = FALSE  # draw stations and strata on map initialization
    )
  
        
    # Observe station or stratum selection from dropdown (and update selection on map)
    observeEvent( c(input$station,input$trendperiod,input$analyte), {
        event <- input$station
        # Remove map marker for previously selected station or stratum
        leafletProxy( "basemap" ) |> clearGroup(group="selected.location")
        # Update selection on map
        if( grepl("Stratum",event) ){  # if user selects a stratum...
          # Add map marker for newly selected stratum
          leafletProxy( "basemap" ) |>
            addMapPane( "polygons", zIndex = 410 ) |>
            addPolygons( data = strat.shp |> filter(STRATUM==event)
                       , group = "selected.location"
                       , fill = FALSE
                       , weight = 3
                       , color = rgb(0,0,0,1)
                       , opacity = 0.15
                       , options = pathOptions(pane = "polygons")
                       )
        } else {  # else (user selects a station)...
          # Get lat/long coordinates for selected point
          selected.coords <- data.frame( lon = station.results()$lon,
                                         lat = station.results()$lat )
          # Add map marker for newly selected station
          leafletProxy( "basemap" ) |>
            addMapPane( "points", zIndex = 420 ) |>
            addCircleMarkers( lng = selected.coords$lon
                            , lat = selected.coords$lat
                            , group = "selected.location"
                            , radius = 5
                            , stroke = TRUE
                            , fill = FALSE
                            , weight = 1.5
                            , color = rgb(0,0,0,0.3)
                            , opacity = 0.3
                            , options = pathOptions(pane = "points")
                            )
        }
      }, ignoreNULL=FALSE  # mark default selection on map initialization
    )
    
    
    # Observe station selection on map (and update selection in dropdown menu)
    observeEvent( input$basemap_marker_click, {
        event <- input$basemap_marker_click
        selected.coords <- data.frame( lon = station.results()$lon,
                                       lat = station.results()$lat )
        # Remove map marker for previously selected station or stratum
        leafletProxy( "basemap" ) |> clearGroup(group="selected.location")
        # Add map marker for newly selected station
        leafletProxy( "basemap" ) |> 
          addMapPane( "points", zIndex = 420 ) |> 
          addCircleMarkers( lng = selected.coords$lon
                          , lat = selected.coords$lat
                          , group = "selected.location"
                          , radius = 5
                          , stroke = TRUE
                          , fill = FALSE
                          , weight = 1.5
                          , color = rgb(0,0,0,0.3)
                          , opacity = 0.3
                          , options = pathOptions(pane = "points")
                          )
        # Update location selection in dropdown menu #######
        updateSelectInput( session, "station",
                           selected = event$id )
      } )
    
    # Observe stratum selection on map (and update selection in dropdown menu)
    observeEvent( input$basemap_shape_click, {
        event <- input$basemap_shape_click
        # Remove map marker for previously selected station or stratum
        leafletProxy( "basemap" ) |> clearGroup(group="selected.location")
        # Add map marker for newly selected station
        leafletProxy( "basemap" ) |>
          addMapPane( "polygons", zIndex = 410 ) |>
          addPolygons( data = strat.shp |> filter(STRATUM==event$id)
                     , group = "selected.location"
                     , fill = FALSE
                     , weight = 1.5
                     , color = rgb(0,0,0,1)
                     , opacity = 0.15
                     , options = pathOptions(pane = "polygons")
                     )
        # Update location selection in dropdown menu #######
        updateSelectInput( session, "station",
                           selected = event$id )
      } )
    
    # Observe analyte selection and add legend to map
    observeEvent( input$analyte, {
      leafletProxy( "basemap" ) |> removeControl(layerId=9)
      leafletProxy( "basemap" ) |>
        addLegend( position = "topright"
                 , labels = if(input$analyte %in% red.up.analytes ){
                       red.up.colors$labels
                     } else if(input$analyte %in% red.dn.analytes ){
                       red.dn.colors$labels
                     }
                 , colors = if(input$analyte %in% red.up.analytes ){
                       red.up.colors$colors
                     } else if(input$analyte %in% red.dn.analytes ){
                       red.dn.colors$colors
                     }
                 , opacity = 0.75
                 , layerId = 9
                 )
      }, ignoreNULL = FALSE  # add legend on map initialization
    )
    
    
    # Plot sample data and GAM curve against time, if available
    output$gam <- ecs.render({
            if( trend.sub()$gam.success ){
                # Render plot with data and GAM curve
                  this.unit <- analyte.results()$Unit |> unique() |> na.omit()
                  gam.tmp <- trend.sub()$gam.curve |> mutate(date=as.character(date))
                  dat.tmp <- trend.sub()$dat.ij |> mutate(date=as.character(date))
                  h2 <- paste0( trend.sub()$dat.ij$date |> min() |>
                                  lubridate::month(label=TRUE, abbr=TRUE) |> as.character()," ",
                                trend.sub()$dat.ij$date |> min() |> year()," \u2013 ",
                                trend.sub()$dat.ij$date |> max() |>
                                  lubridate::month(label=TRUE, abbr=TRUE) |> as.character()," ",
                                trend.sub()$dat.ij$date |> max() |> year()," (N=",
                                trend.sub()$dat.ij |> nrow(),")"
                                )
                  ec.init( load = 'custom', preset = FALSE,
                           title = list( text = "Sample Data and GAM Model"
                                       , subtext = h2
                                       , left = "5%"
                                       ),
                           legend = list( orient = "horizontal"
                                        , right = '10%'
                                        , top = '0%'
                                        ),
                           xAxis = list( show = TRUE
                                       , type ='time'
                                       , data = gam.tmp$date
                                       # , boundaryGap = c('0%','0%')
                                       , min = min(xrange)
                                       , max = max(xrange)
                                       , splitLine = list(show=TRUE)
                                       , splitNumber = 10
                                       ),
                           yAxis = list( show = TRUE
                                       , name = this.unit
                                       , boundaryGap = c('5%','5%')
                                       , scale = TRUE
                                       , nameLocation = "middle"
                                       , nameGap = 50
                                       , nameTextStyle = list( fontSize = 14 )
                                       , axisLine = list(show=TRUE)
                                       , axisTick = list(show=TRUE)
                                       ),
                           series = list(
                             list( type = 'scatter'
                                 , name = 'Data'
                                 , data = ec.data(dat.tmp |> select(date,value) |> na.omit())
                                 , symbol = 'emptyCircle'
                                 , symbolSize = 6
                                 , color = rgb(0,0,0,0.5)
                                 ),
                             list( type = 'line'
                                 , name = "Model"
                                 , data = ec.data(gam.tmp |> select(date,fit) |> na.omit())
                                 , symbol = 'none'
                                 , color = rgb(0,0.3,0.7,0.8)
                                 , lineStyle = list(width=2)
                                 )
                           )
                  ) |>
                    ec.upd( { tooltip = list( formatter = ec.clmn('<b>%@</b> <br>%M@ Value: <b>%@</b>', 1,2) ) } )
      } else {  # if gam.success==FALSE...
        if( !is.null( trend.sub()$dat.ij ) ){  # if gam.success==FALSE and !is.null(dat.ij)
             # Render plot with data (no GAM curve)
             this.unit <- analyte.results()$Unit |> unique() |> na.omit()
             dat.tmp <- trend.sub()$dat.ij |> mutate(date=as.character(date))
             h2 <- paste0( trend.sub()$dat.ij$date |> min() |>
                             lubridate::month(label=TRUE, abbr=TRUE) |> as.character()," ",
                           trend.sub()$dat.ij$date |> min() |> year()," \u2013 ",
                           trend.sub()$dat.ij$date |> max() |>
                             lubridate::month(label=TRUE, abbr=TRUE) |> as.character()," ",
                           trend.sub()$dat.ij$date |> max() |> year()," (N=",
                           trend.sub()$dat.ij |> nrow(),")"
             )
             ec.init( load = 'custom', preset = FALSE,
                      title = list( text = "Insufficient data for GAM"
                                    , subtext = h2
                                    , left = "5%"
                      ),
                      legend = list( orient = "horizontal"
                                     , right = '10%'
                                     , top = '0%'
                      ),
                      xAxis = list( show = TRUE
                                    , type ='time'
                                    , data = dat.tmp$date
                                    # , boundaryGap = c('0%','0%')
                                    , min = min(xrange)
                                    , max = max(xrange)
                                    , splitLine = list(show=TRUE)
                                    , splitNumber = 10
                      ),
                      yAxis = list( show = TRUE
                                    , name = this.unit
                                    , boundaryGap = c('5%','5%')
                                    , scale = TRUE
                                    , nameLocation = "middle"
                                    , nameGap = 50
                                    , nameTextStyle = list( fontSize = 14 )
                                    , axisLine = list(show=TRUE)
                                    , axisTick = list(show=TRUE)
                      ),
                      series = list(
                        list( type = 'scatter'
                              , name = 'Data'
                              , data = ec.data(dat.tmp |> select(date,value) |> na.omit())
                              , symbol = 'emptyCircle'
                              , symbolSize = 6
                              , color = rgb(0,0,0,0.5)
                              )
                      )
             ) |>
               ec.upd( { tooltip = list( formatter = ec.clmn('<b>%@</b> <br>%M@ Value: <b>%@</b>', 1,2) ) } )
        } else {  # if gam.success==FALSE and !is.null(dat.ij)
           # Render null plot
            ec.init( load = 'custom', preset = FALSE,
                     title = list( text = "No data to display" , left = "5%") )
         }  # // end if(nrow(dat.ij))
       }  # // end if(gam.success)
    })  # // end ecs.render()
    
    # Plot metric and mixmodel trend against time
    output$trend <- ecs.render( { 
      if( trend.sub()$trend.success ){
          this.unit <- analyte.results()$Unit |> unique() |> na.omit()
          met1.tmp <- trend.sub()$trend.toplo1 |> relocate(yr,bt_met,bt_lwr,bt_upr)
          met2.tmp <- trend.sub()$trend.toplo2
          # Get mixmodel slope and p-value
          if( input$trendperiod=="5y" ){
            slope <- analyte.results()$mix5.slope[ which( analyte.results()$Station==input$station ) ]
            pval <- analyte.results()$mix5.P.slope[ which( analyte.results()$Station==input$station ) ]
          } else if( input$trendperiod=="10y" ) {
            slope <- analyte.results()$mix10.slope[ which( analyte.results()$Station==input$station ) ]
            pval <- analyte.results()$mix10.P.slope[ which( analyte.results()$Station==input$station ) ]
          }
          P.string <- make_P.string(pval)
          # Set color for trend line and band
          if( pval < 0.05 ){
            line.col <- rgb(1,0.45,0.50,0.6) # rgb(1,0.2,0.3,0.7)
            band.col <- rgb(1,0.45,0.50,0.2) # rgb(1,0.2,0.3,0.1)
          } else {
            line.col <- rgb(0,0,0.4,0.3)
            band.col <- rgb(0,0,0.3,0.1)
          }
          # Subtitle for plot
          h2 <- paste0( "Approx. slope: ", slope," (",P.string,")" )
          # Generate echart
          ec.init( load = 'custom', preset = FALSE, 
                   title = list( text = "Trend in Annual Mean"
                               , subtext = h2
                               , left = "5%" 
                               ),
                   legend = list( orient = "horizontal"
                                , right = '10%'
                                , top = '0%'
                                ),
                   xAxis = list( type ='value'
                               , data = met1.tmp$yr
                               # , boundaryGap = c('0%','0%')
                               , min = xrange |> min() |> decimal_date()
                               , max = xrange |> max() |> decimal_date()
                               , splitLine = list(show=TRUE)
                               , splitNumber = 10
                               , axisLabel = list( formatter = htmlwidgets::JS('function(value, index){
                                                                                   return value;
                                                                                }')
                                                  )  # this JS removes commas from yr (2011 instead of 2,011)
                               ), 
                   yAxis = list( show = TRUE
                                 , min = min(trend.sub()$ylims)
                                 , max = max(trend.sub()$ylims)
                                 # , boundaryGap = c('5%','5%')
                                 , scale = TRUE
                                 , name = this.unit
                                 , nameLocation = "middle" 
                                 , nameGap = 50
                                 , nameTextStyle = list( fontSize = 14 )
                                 , axisLine = list(show=TRUE)
                                 , axisTick = list(show=TRUE)
                                 ),
                   series = list(
                               list( type ='scatter'
                                     , name ='Mean'
                                     , data = ec.data(met1.tmp |> select(yr,bt_met) )
                                     , symbol = 'emptyCircle'
                                     , symbolSize = 7
                                     , color = rgb(0,0.6,0.8,1)
                                     ),
                               list( type ='line'
                                     , name ='Trend'
                                     , data = ec.data(met2.tmp |> select(yr,bt_met) )
                                     , symbol ='none'
                                     , color = line.col
                                     ),
                               ecr.band( df = met2.tmp |> select(yr,bt_lwr,bt_upr)
                                         , lower = "bt_lwr"
                                         , upper = "bt_upr"
                                         , type = "polygon"
                                         , color = band.col
                                         , itemStyle = list(borderWidth = 0)
                                         )[[1]]
                                )
                  ) |> 
            ecr.ebars( met1.tmp # |> rename(SE=bt_met)
                     , hwidth = 2
                     , color = rgb(0,0.6,0.8,1)
                     , name = "SE"
                     ) # |>
          # tooltip not functioning properly (activated by band as well as means)
          # ec.upd( { tooltip = list( formatter = ec.clmn('%M@ Annual Mean: <b>%@</b>', 2) ) } )
      } else {  # If trend.success==FALSE..
        if( !is.null(trend.sub()$dat.ij) ){  # If trend.success==FALSE and !is.null(dat.ij)
          ec.init( load = 'custom', preset = FALSE,
                   title = list( text = "Insufficient data for trend analysis"
                               , left = "5%"
                               )
                   )
        } else {  # If trend.success==FALSE and is.null(dat.ij)
          ec.init( load = 'custom', preset = FALSE,
                   title = list( text = "" )
                   )
        }
        
      }  # // end if(trend.success)
    } )
  
    # Update trend line on mixmod plot when user changes trend period
    observeEvent( input$trendperiod, {
      event <- input$trendperiod
        if( trend.sub()$trend.success ){
          # Remove exiting trend line
          echarts4r_proxy( "trend" ) |>
            e_remove_serie("Trend")
          # Add new trend line
          met2.tmp <- trend.sub()$trend.toplo2
          echarts4rProxy( "trend"
                          , data = met2.tmp
                          , x = yr
          ) |>
            e_line( serie = bt_met
                    , name = "Trend"
                    , symbol = "none"
                    , color = rgb(1,0.2,0.3,0.7)
                    , lineStyle = list(width=2)
            ) |>
            # e_tooltip( trigger = "item" ) |>
            # use e_band() to add uncertainty band around trend
            e_execute()
        } else {
          if( !is.null(trend.sub()$dat.ij) ){  # If trend.success==FALSE and !is.null(dat.ij)
            ec.init( load = 'custom', preset = FALSE,
                     title = list( text = "Insufficient data for trend analysis"
                                   , left = "5%"
                     )
            )
          } else {  # If trend.success==FALSE and is.null(dat.ij)
            ec.init( load = 'custom', preset = FALSE,
                     title = list( text = "" )
            )
          }
        }
    } )
    
  } # // end server()
  
  
  ############################################
  #  Shiny app
  ############################################
  shinyApp( ui = ui, server = server )
  
