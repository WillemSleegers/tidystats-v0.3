#' Inspect (a) statistical model(s) output
#'
#' \code{inspect} is a function to inspect the output of a statistical model via
#' the Viewer pane.
#'
#' @param results The output of a statistical test.
#' @param ... Unused
#'
#' @examples
#' # Run a statistical test
#' model <- t.test(extra ~ group, data = sleep)
#'
#' # Inspect the output
#' # inspect(model)
#'
#' @import shiny
#'
#' @export

# TODO: Make identifiers with terms non-clickable.

inspect.default <- function(results, ...) {

  var_name <- deparse(substitute(results))

  # Tidy the stats
  res <- tidy_stats(results)

  # Define the UI
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(
      "Results overview",
      right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE),
      left = NULL
    ),

    miniUI::miniContentPanel(
      shiny::tableOutput('table'),

      # Hide the APA textbox until the results have loaded
      shiny::conditionalPanel(
        condition = "output.table",
        shiny::div(
          id = "apa_output",
          shiny::htmlOutput("apa"),
          shiny::actionButton('copy_button', 'Copy',
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
      df <- res %>%
        dplyr::select(statistic, value) %>%
        dplyr::mutate(value = prettyNum(value))

      # Create the base table
      table <- knitr::kable(df, col.names = c("", "")) %>%
        kableExtra::kable_styling(bootstrap_options = c("striped", "hover",
          "condensed"),
          full_width = T)

      # Group the table by identifiers, groups, and terms
      row_model <- 1

      table <- table %>%
        kableExtra::group_rows(var_name, row_model, row_model + nrow(res) - 1,
          label_row_css = "font-weight: bold;
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

    output$apa <- shiny::renderText({

      if (!is.null(input$jsValue)) {
        what = input$jsValue[1]
        identifier = input$jsValue[2]
        group = input$jsValue[3]
        term = input$jsValue[4]
        statistic = input$jsValue[5]

        print(paste("what:", what))
        print(paste("identifier:", identifier))
        print(paste("group:", group))
        print(paste("term:", term))
        print(paste("statistic:", statistic))

        # Check if the user clicked on an identifier with terms or on Residuals
        print(res$method[1])

        if (what == "identifier" & "term" %in% names(res)) {
          output <- knitr::knit2html(text = "Click on a term instead.",
            fragment.only = TRUE)
        } else if (group == "coefficients" & term == "") {
          output <- knitr::knit2html(text = "Click on a term instead.",
            fragment.only = TRUE)
        } else if (group == "model" & res$method[1] == "Generalized linear model") {
          output <- knitr::knit2html(text = "Not supported.",
            fragment.only = TRUE)
        } else if (what == "term" & (term == "Residuals" | stringr::str_detect(
          term, "_Residuals"))) {

          output <- knitr::knit2html(text = "Not supported.",
            fragment.only = TRUE)
        } else {
          # Set variables to NULL if they are empty strings
          if (group == "") { group <- NULL }
          if (term == "") { term <- NULL }
          if (statistic == "") { statistic <- NULL }

          # Get output
          results <- list()
          results[[identifier]] <- res

          output <- report(identifier = identifier, group = group, term = term,
            statistic = statistic, results = results)

          # Replace ~ with <sub> to create subscript
          output <- stringr::str_replace(output, "~", "<sub>")
          output <- stringr::str_replace(output, "~", "</sub>")

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
      shiny::stopApp()
    })
}

  shiny::runGadget(ui, server)
}
