chooserInput <- function(inputId,leftChoices, rightChoices,size = 20, multiple = FALSE) {
  
  leftChoices <- lapply(leftChoices, tags$option)
  rightChoices <- lapply(rightChoices, tags$option)
  
  if (multiple)
    multiple <- "multiple"
  else
    multiple <- NULL
  
  tagList(
    singleton(tags$head(
      tags$script(src="chooser-binding.js"),
      tags$style(type="text/css",
                 HTML(".chooser-container { display: inline-block; }")
      )
    )),
    div(id=inputId, class="chooser",style="",
        div(style="display:table",
            div(style="min-width:200px; display:table-cell;",
                div(class="chooser-container chooser-left-container",
                    style="width:100%;",
                    tags$select(class="left", size=size, multiple=multiple, leftChoices,style="width:100%;min-width:200px")
                )
            ),
            div(style="min-width:50px; display:table-cell;vertical-align: middle;",
                div(class="chooser-container chooser-center-container",
                    style="padding:10px;",
                    icon("arrow-circle-o-right", "right-arrow fa-3x"),
                    tags$br(),
                    icon("arrow-circle-o-left", "left-arrow fa-3x")
                )
            ),
            div(style="min-width:200px; display:table-cell;",
                div(class="chooser-container chooser-right-container", style="width:100%;",
                    tags$select(class="right", size=size, multiple=multiple, rightChoices,style="width:100%;min-width:200px")
                )
            )
        )
    )
  )
}

registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
  if (is.null(data))
    NULL
  else
    list(left=as.character(data$left), right=as.character(data$right))
}, force = TRUE)
