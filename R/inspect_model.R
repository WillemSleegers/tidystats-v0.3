#' Inspect (a) statistical model(s) added to your tidystats list
#'
#' \code{inspect_model} is a function to inspect one or more models that are part of a tidystats
#' list. The function will open a Shiny widget in the Viewer pane, which will show the results of
#' one or more models. This allows the user to visually inspect the model output, as well as copy
#' the results in APA format.
#'
#' @param results A tidystats list.
#' @param ... Models to show in the viewer.
#'
#' @import shiny
#' @import miniUI
#' @import dplyr
#' @import purrr
#' @import knitr
#' @import kableExtra
#' @import stringr
#' @import htmlwidgets
#'
#' @export

inspect_model <- function(results, ...) {

  # Get identifiers and convert them to a vector of strings
  identifiers <- dplyr::quos(...) %>%
    purrr::map(quo_name) %>%
    unlist() %>%
    as.vector()

  # If identifiers are provided, select them from the results list
  if (length(identifiers) != 0) {
    results <-  results[identifiers]
  }

  # Define the UI
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Results overview",
                           right = miniTitleBarButton("done", "Done", primary = TRUE),
                           left = NULL),

    miniUI::miniContentPanel(
      shiny::tableOutput('table'),
      shiny::tags$style(type='text/css',
        "#apa_output {
          position: fixed;
          bottom: 0;
          width: 100%;
          margin-left: -14px;
          background-color: rgb(239, 239, 239);
          border-top: 1px solid rgb(213, 213, 213);
          padding: 5px;
          vertical-align: center;
        }

        #apa_output p {
          display: inline-block;
        }

        #apa {
          display: inline-block;
          background-color: white;
          border-radius: 5px;
          border: 1px solid rgb(213, 213, 213);
          padding: 0;

          width: calc(100vw - 155px);
        }

        #apa p {
          margin: 0;
          padding: 6px;
          padding-left: 8px;
        }

        #copy_button {
          float: right;
        }

        "),
      # htmlOutput('apa'),
      div(id = "apa_output",
        p("APA output:"),
        shiny::htmlOutput("apa"),
        actionButton('copy_button', 'Copy', onclick = "copy_to_clipboard('apa')")
      )
    ),

    shiny::tags$script(htmlwidgets::JS('function myAlert(e) {
      // Figure out what was clicked
      var what = e.target.className;

      if (what == "") {
        what = "statistic";
      }

      // Figure out the identifier
      if (what != "identifier") {
        // Get the previous row
        previous_row = e.target.parentNode.previousSibling.previousSibling;

        // Loop through all the rows until the row has a cell with the identifier class
        while (previous_row.firstChild.className != "identifier") {
          previous_row = previous_row.previousSibling.previousSibling;
        }

        // Store the identifier
        identifier = previous_row.firstChild.innerText;

        // Figure out the term
        if (what != "term") {
          // Get the previous row
          previous_row = e.target.parentNode.previousSibling.previousSibling;

          // Loop through all the rows until the row only has 1 child
          while (previous_row.childNodes.length != 1) {
          previous_row = previous_row.previousSibling.previousSibling;
          }

          // Check whether it is an identifier or term
          if (previous_row.firstChild.className == "identifier") {
          term = "";
          } else {
          term = previous_row.firstChild.innerText;
          }
        } else {
          term = e.target.innerText;
        }
      } else {
        identifier = e.target.innerText;
        term = "";
      }

      // If a statistic was clicked, return the statistic
      var statistic = "";
      if (what == "statistic") {
        statistic = e.target.parentNode.childNodes[1].innerText;
      }

      info = [what, identifier, term, statistic];
      console.log(info);

      Shiny.onInputChange("jsValue", info);
    }')),

    shiny::tags$script(htmlwidgets::JS("function copy_to_clipboard(element) {

      var doc = document, text = doc.getElementById(element), range, selection;

      selection = window.getSelection();
      range = doc.createRange();
      range.selectNodeContents(text);
      selection.removeAllRanges();
      selection.addRange(range);

      console.log(this);

      document.execCommand('copy');
      window.getSelection().removeAllRanges();
    }"
  )))

  # Server logic
  server <- function(input, output, session) {

    output$table <- function() {

      # Combine all the statistics into one table
      df <- results %>%
        purrr::map_df(dplyr::select, statistic, value) %>%
        dplyr::mutate(value = prettyNum(value))

      # Create the base table
      table <- knitr::kable(df, col.names = c("", "")) %>%
        kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                                  full_width = T)

      # Group the table by identifiers and terms
      row_model <- 1
      for (model in names(results)) {
        res <- results[[model]]

        # print(paste("Rows:", model))
        # print(row_model)
        # print(row_model + nrow(res) - 1)

        table <- table %>%
          kableExtra::group_rows(model, row_model, row_model + nrow(res) - 1,
                                 label_row_css = "font-weight: bold; hack: identifier;")

        if ("term" %in% names(res)) {
          row_term <- row_model
          for (term in unique(res$term)) {

            table <- table %>%
              kableExtra::group_rows(term, row_term, row_term + sum(res$term == term) - 1,
                                     label_row_css = "font-weight: bold; hack: term;")

            row_term <- row_term + sum(res$term == term)
          }
        }

        row_model <- row_model + nrow(res)
      }

      # Make the rows clickable
      table <- stringr::str_replace_all(table, "<tr", "<tr onclick=myAlert(event)")

      # Add classes
      table <- stringr::str_replace_all(
        table, 'hack: identifier;\"', '" class=identifier')
      table <- stringr::str_replace_all(table, 'hack: term;\"', '" class=term')

      # Remove strong HTML tags
      table <- stringr::str_replace_all(table, "<strong>", "")
      table <- stringr::str_replace_all(table, "</strong>", "")

      return(table)
    }

    observeEvent(input$jsValue, {
      #req(input$jsValue)
      #print(input$jsValue)
    })

    output$apa <- renderText({

      if (!is.null(input$jsValue)) {
        what = input$jsValue[1]
        identifier = input$jsValue[2]
        term = input$jsValue[3]
        statistic = input$jsValue[4]

        if (what == "statistic") {
          if (term != "") {
            output <- report(identifier = identifier, term = term,
                             statistic = statistic, results = results)
          } else {
            output <- report(identifier = identifier, statistic = statistic,
                             results = results)
          }
        } else {
          if (term != "") {
            output <- report(identifier = identifier, term = term,
                             results = results)
          } else {
            output <- report(identifier = identifier, results = results)
          }
        }

        # TODO: change the hover icon to a hand
        # TODO: automatically copy results to clipboard
        # shinyjs::runjs("copy_to_clipboard('apa');")
        output <- knitr::knit2html(text = output, fragment.only = TRUE)

      } else {
        output <- knitr::knit2html(text = "Click on a row for magic",
                                   fragment.only = TRUE)
      }
      return(output)
    })

    # When the Done button is clicked, return a value
    observeEvent(input$done, {
      stopApp()
    })
  }

  runGadget(ui, server)
}

