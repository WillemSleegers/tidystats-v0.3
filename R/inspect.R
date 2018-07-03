#' Inspect (a) statistical model(s) added to your tidystats list
#'
#' \code{inspect} is a function to inspect one or more models that are part of a
#' tidystats list. The function will open a Shiny widget in the Viewer pane,
#' which will show the results of one or more models. This allows the user to
#' visually inspect the model output, as well as copy the results in APA format.
#'
#' @param results A tidystats list.
#' @param ... Models to show in the viewer.
#'
#' @import shiny
#' @import miniUI
#' @import dplyr
#' @import stringr
#' @import purrr
#' @import knitr
#' @import kableExtra
#'
#' @export

# TODO: Make identifiers with terms non-clickable.
# TODO: Catch some more errors (e.g., clicking on the coefficient group)

inspect <- function(results, ...) {

  # Get identifiers and convert them to a vector of strings
  identifiers <- dplyr::quos(...) %>%
    purrr::map(quo_name) %>%
    unlist() %>%
    as.vector()

  # Check whether the identifiers exist in the results list
  if (sum(!identifiers %in% names(results))) {
    stop("Identifier(s) not found.")
  }

  # If identifiers are provided, select them from the results list
  if (length(identifiers) != 0) {
    results <-  results[identifiers]
  }

  # Define the UI
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(
      "Results overview",
      right = miniTitleBarButton("done", "Done", primary = TRUE),
      left = NULL
    ),

    miniUI::miniContentPanel(
      shiny::tableOutput('table'),

      # Hide the APA textbox until the results have loaded
      conditionalPanel(
        condition = "output.table",
        div(
          id = "apa_output",
          p("APA:"),
          shiny::htmlOutput("apa"),
          actionButton('copy_button', 'Copy',
                       onclick = "copy_to_clipboard('apa')")
        )
      )
    ),

    tidystats::css_style(),
    tidystats::inspect_click_script(),
    tidystats::copy_to_clipboard_script()
    )

  # Server logic
  server <- function(input, output, session) {

    output$table <- function() {

      # Combine all the statistics into one table
      df <- results %>%
        purrr::map_df(dplyr::select, statistic, value) %>%
        dplyr::mutate(value = prettyNum(value))

      # Create the base table
      table <- knitr::kable(df, col.names = c("", "")) %>%
        kableExtra::kable_styling(bootstrap_options = c("striped", "hover",
                                                        "condensed"),
                                  full_width = T)

      # Group the table by identifiers, groups, and terms
      row_model <- 1
      for (model in names(results)) {
        res <- results[[model]]

        # print(paste("Rows:", model))
        # print(row_model)
        # print(row_model + nrow(res) - 1)

        table <- table %>%
          kableExtra::group_rows(model, row_model, row_model + nrow(res) - 1,
                                 label_row_css = "
                                   font-weight: bold;
                                   background-color: rgb(225, 225, 225);
                                   hack: identifier;")

        if ("group" %in% names(res)) {
          row_group <- row_model

          for (group in unique(res$group)) {
            table <- table %>%
              kableExtra::group_rows(group, row_group, row_group +
                                       sum(res$group == group) - 1,
                                     label_row_css =
                                       "font-weight: bold; hack: group;")

            # Get only the group results and loop through terms, if there are
            # more than 1
            res_group <- res[res$group == group, ]

            if (!is.na(res_group$term[1])) {
              row_term <- row_group


              for (term in unique(res_group$term)) {

                res_term <- res_group[res_group$term == term, ]

                table <- table %>%
                  kableExtra::group_rows(term, row_term, row_term +
                                           sum(res_term$term == term) - 1,
                                         label_row_css =
                                           "font-weight: bold; hack: term;")

                row_term <- row_term + sum(res_term$term == term)
              }
            }

            row_group <- row_group + sum(res$group == group)


          }
        } else {
          if ("term" %in% names(res)) {
            row_term <- row_model
            for (term in unique(res$term)) {

              table <- table %>%
                kableExtra::group_rows(term, row_term, row_term +
                                         sum(res$term == term) - 1,
                                       label_row_css =
                                         "font-weight: bold; hack: term;")

              row_term <- row_term + sum(res$term == term)
            }
          }
        }

        row_model <- row_model + nrow(res)
      }

      # Make the rows clickable
      table <- stringr::str_replace_all(table, "<tr",
                                        "<tr onclick=inspect_click(event)")

      # Add classes
      table <- stringr::str_replace_all(
        table, 'hack: identifier;\"', '" class=identifier')
      table <- stringr::str_replace_all(
        table, 'hack: group;\"', '" class=group')
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
        group = input$jsValue[3]
        term = input$jsValue[4]
        statistic = input$jsValue[5]

        # print(paste("what:", what))
        # print(paste("identifier:", identifier))
        # print(paste("group:", group))
        # print(paste("term:", term))
        # print(paste("statistic:", statistic))

        # Check if the user clicked on an identifier with terms or on Residuals
        res <- results[[identifier]]

        if (what == "identifier" & "term" %in% names(res)) {
          output <- knitr::knit2html(text = "Click on a term instead.",
                                     fragment.only = TRUE)
        } else if (what == "term" & (term == "Residuals" | str_detect(
          term, "_Residuals"))) {

          output <- knitr::knit2html(text = "Not supported.",
                                       fragment.only = TRUE)
        } else {
          # Set variables to NULL if they are empty strings
          if (group == "") { group <- NULL }
          if (term == "") { term <- NULL }
          if (statistic == "") { statistic <- NULL }

          # Get output
          output <- report(results = results, identifier = identifier,
                           group = group, term = term, statistic = statistic)

          # shinyjs::runjs("copy_to_clipboard('apa');")
          # Replace ~ with <sub> to create subscript
          output <- str_replace(output, "~", "<sub>")
          output <- str_replace(output, "~", "</sub>")

          output <- knitr::knit2html(text = output, fragment.only = TRUE)
        }
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

# library(shiny); library(miniUI); library(knitr); library(kableExtra)
