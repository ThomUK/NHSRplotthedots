#' Create ggplot2 (internal function)
#'
#' Creates a ggplot2 object using the parameters passed in.
#'
#' @param df A data frame containing the information to be plotted.
#' @param facetField A character vector naming the column in the data frame containing the facet names
#' @param plotOptions A list containing the options needed for plot customisations
#' @return The ggplot2 object
#'
#' @noRd

createGgplot <- function(df, facetField, plotOptions){

  # Colour Palette for ggplot
  .darkgrey = "#7B7D7D"
  .orange = "#fab428"
  .skyblue = "#289de0"
  .purple = "#361475"
  .red = "#de1b1b"

  if(!(is.null(plotOptions$yAxisBreaks))){ 
      yaxis <- c(df$y,df$upl,df$lpl)
      start <- floor(min(yaxis,na.rm = TRUE)/plotOptions$yAxisBreaks) * plotOptions$yAxisBreaks
      end <- max(yaxis,na.rm = TRUE)
      yaxislabels <- seq(from = start, to = end, by = plotOptions$yAxisBreaks)
  }

  plot <- ggplot(df,aes(x=.data$x,y=.data$y)) +
    theme_minimal() +
    geom_line(aes(y=.data$upl),linetype = "dashed",size=plotOptions$pointSize/3,color=.darkgrey) +
    geom_line(aes(y=.data$lpl),linetype = "dashed",size=plotOptions$pointSize/3,color=.darkgrey) +
    geom_line(aes(y=.data$target),linetype = "dashed",size=plotOptions$pointSize/3,color=.purple) +
    geom_line(aes(y=.data$trajectory),linetype = "dashed",size=plotOptions$pointSize/3,color=.red) +
    geom_line(aes(y=mean)) +
    geom_line(color=.darkgrey,size=plotOptions$pointSize/3) +
    geom_point(color=.darkgrey,size=plotOptions$pointSize)

  # Apply facet wrap if a facet field is present
  if(facetField != "pseudo_facet_col_name"){ 
    plot <- plot +
      facet_wrap(vars(.data$f), scales = plotOptions$facetScales)
  }

  plot <- plot +
    geom_point(aes(x=.data$x,y=.data$specialCauseImprovement),color=.skyblue,size=plotOptions$pointSize) +
    geom_point(aes(x=.data$x,y=.data$specialCauseConcern),color=.orange,size=plotOptions$pointSize) +
    labs(
      title = plotOptions$plottitle,
      x = plotOptions$xlabel,
      y = plotOptions$ylabel
    ) +
    scale_x_date(breaks=plotOptions$xaxislabels, labels = format(plotOptions$xaxislabels, format = plotOptions$xAxisDateFormat)) +
    theme(
      plot.margin = unit(c(5,5,5,5), "mm"), #5mm of white space around plot edge
      axis.text.x = element_text(angle = 90, hjust = 1),
      panel.grid.major.x = element_blank(), #remove major x gridlines
      panel.grid.minor.x = element_blank() #remove minor x gridlines

    )

  #if the plot is not faceted (ie it's the default facet column name)
  if(facetField == "pseudo_facet_col_name"){
    if(plotOptions$convertToPercentages == FALSE){
      if(!(is.null(plotOptions$yAxisBreaks))){
        plot <- plot +
          scale_y_continuous(breaks = yaxislabels, labels = yaxislabels)
      }
    } else if(plotOptions$convertToPercentages != 0) {
      percentLimit <- max(df$upl,na.rm = TRUE)

      interval <- if(!(is.null(plotOptions$yAxisBreaks))){plotOptions$yAxisBreaks} else {plotOptions$convertToPercentages}

      plot <- plot +
        scale_y_continuous(labels = scales::percent,breaks = seq(from = 0, to = percentLimit, by = interval))
    }
  } else{
    #else if the plot is faceted
    if(plotOptions$convertToPercentages != 0) {
      percentLimit <- max(df$upl,na.rm = TRUE)

      plot <- plot +
        scale_y_continuous(labels = scales::percent)
    }
  }

  #as a final step, apply any theme over-ride passed in by the user
  plot <- plot +
    plotOptions$themeOverride

  #create the grid viewports
  key_vp <- grid::viewport(x = 0, y = 0,
                        width = 1, height = 0.1,
                        just = c("left", "bottom"))

  spc_vp <- grid::viewport(x = 0, y = 1,
                            width = 1, height = 0.9,
                            just = c("left", "top"))

  #lay out the viewports and add a border
  grid::grid.newpage()
  grid::grid.draw(grid::rectGrob(gp = grid::gpar(col = "grey", lwd = 2))) #outer grey border
  grid::pushViewport(key_vp)
  grid::popViewport()
  grid::pushViewport(spc_vp)
  grid::grid.draw(ggplotGrob(plot))
  grid::popViewport()

  #draw the grobs into the key viewport
  .drawLine(key_vp, 0.1, 0.06, "black" , 1, 2)
  .drawTxt(key_vp, "Mean", 0.1)

  .drawDot(key_vp, 0.26, .darkgrey)
  .drawLine(key_vp, 0.26, 0.06, .darkgrey)
  .drawTxt(key_vp, "Data", 0.26)

  .drawLine(key_vp, 0.42, 0.06, .darkgrey, "33")
  .drawTxt(key_vp, "Process limits \n(3 sigma)", 0.42)

  .drawDot(key_vp, 0.58, .orange)
  .drawTxt(key_vp, "Special cause \n(concern)", 0.58)

  .drawDot(key_vp, 0.74, .skyblue)
  .drawTxt(key_vp, "Special cause \n(improvement)", 0.74)

  .drawLine(key_vp, 0.9, 0.06, .red, "33")
  .drawTxt(key_vp, "Target", 0.9)

}

#Helper function for drawing consistent dots in the key
.drawDot <- function(destination_vp, x, colour){
  grid::grid.circle(
    x = x,
    y = 0.95,
    r = 0.1,
    gp = grid::gpar(fill = colour, col = colour),
    vp = destination_vp
  )
}

#Helper function for drawing consistent lines in the key
.drawLine <- function(destination_vp, xPos, length, colour, linetype = 1, lineweight = 4){
  l2 <- length / 2
  grid::grid.lines(
    x = c(xPos - l2, xPos + l2),
    y = c(0.95, 0.95),
    gp = grid::gpar(
      col = colour,
      lwd =lineweight,
      lty = linetype
    ),
    vp = destination_vp
  )
}

#Helper function for drawing consistent text in the key
.drawTxt <- function(destination_vp, text, xPos){
  grid::grid.text(
    label = text,
    x = xPos,
    y = 0.5,
    gp = grid::gpar(fontsize = 10),
    vp = destination_vp
  )
}
