# ============================================================
# R/mod_filters.R
# ============================================================

mod_filters_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # ── Selected neighbourhood ───────────────────────────────
    div(class = "filter-section",
      div(class = "filter-selected-label",
        span("Selected:", class = "filter-selected-key"),
        uiOutput(ns("selected_label"), inline = TRUE)
      )
    ),

    # ── Crime Type ───────────────────────────────────────────
    div(class = "filter-section",
      div(class = "filter-section-header",
        span("Crime Type")
      ),
      div(class = "crime-btn-group",
        purrr::map(
          list(
            list(val = "Auto Theft",      icon = "\U0001F697"),
            list(val = "Assault",         icon = "\U0001F91B"),
            list(val = "Break and Enter", icon = "\U0001F6AA"),
            list(val = "Robbery",         icon = "\U0001F4B0"),
            list(val = "Theft Over",      icon = "\U0001F4E6")
          ),
          function(item) {
            # --btn-color is set from R so JS never needs to look it up
            btn_col <- unname(CRIME_COLOURS[item$val])
            tags$button(
              id           = ns(paste0("ct_", gsub(" ", "_", item$val))),
              type         = "button",
              class        = "crime-type-btn",
              `data-value` = item$val,
              style        = paste0("--btn-color:", btn_col, ";"),
              tags$span(class = "crime-dot"),
              tags$span(class = "crime-btn-icon", item$icon),
              tags$span(class = "crime-btn-label", item$val)
            )
          }
        )
      ),
      tags$input(id = ns("crime_type"), type = "hidden", value = "Auto Theft"),
      tags$script(HTML(sprintf("
        (function() {
          var pfx = '%s';

          function hexToRgba(hex, alpha) {
            hex = hex.replace('#', '');
            var r = parseInt(hex.slice(0,2), 16);
            var g = parseInt(hex.slice(2,4), 16);
            var b = parseInt(hex.slice(4,6), 16);
            return 'rgba(' + r + ',' + g + ',' + b + ',' + alpha + ')';
          }

          function highlightBtn(val) {
            var btns = document.querySelectorAll('[id^=\"' + pfx + 'ct_\"]');
            btns.forEach(function(b) {
              var isActive = b.getAttribute('data-value') === val;
              b.classList.toggle('is-active', isActive);
              if (isActive) {
                var col = getComputedStyle(b).getPropertyValue('--btn-color').trim();
                b.style.background  = hexToRgba(col, 0.28);
                b.style.borderColor = col;
                b.style.color       = '#ffffff';
              } else {
                b.style.background  = '';
                b.style.borderColor = '';
                b.style.color       = '';
              }
            });
          }

          function activate(val) {
            highlightBtn(val);
            Shiny.setInputValue(pfx + 'crime_type', val, {priority: 'event'});
          }

          function init() {
            var btns = document.querySelectorAll('[id^=\"' + pfx + 'ct_\"]');
            if (!btns.length) { setTimeout(init, 150); return; }
            highlightBtn('Auto Theft');
            btns.forEach(function(b) {
              b.addEventListener('click', function() {
                activate(this.getAttribute('data-value'));
              });
            });
          }
          init();

          Shiny.addCustomMessageHandler('activateCrimeBtn', function(msg) {
            if (msg.ns === pfx) highlightBtn(msg.val);
          });
        })();
      ", ns(""))))
    ),

    # ── Year ─────────────────────────────────────────────────
    div(class = "filter-section",
      div(class = "filter-section-header",
        span("Year")
      ),
      sliderInput(
        inputId = ns("selected_year"),
        label   = NULL,
        min     = 2014,
        max     = 2025,
        value   = 2025,
        step    = 1,
        sep     = "",
        ticks   = FALSE
      )
    ),

    # ── Neighbourhood ────────────────────────────────────────
    div(class = "filter-section",
      div(class = "filter-section-header",
        span("Neighbourhood")
      ),
      selectInput(
        inputId   = ns("selected_hood"),
        label     = NULL,
        choices   = c("All" = ""),
        selected  = "",
        size      = 9,
        selectize = FALSE
      )
    ),

    # ── Reset ────────────────────────────────────────────────
    div(class = "filter-section",
      actionButton(ns("reset"), "\u21BA  Reset Filters",
                   class = "btn-reset w-100")
    )
  )
}

mod_filters_server <- function(id, app_state, setters, app_data) {
  moduleServer(id, function(input, output, session) {

    observe({
      req(app_data$bar_data)
      hoods <- app_data$bar_data %>%
        dplyr::distinct(HOOD_158, neighbourhood_name) %>%
        dplyr::arrange(neighbourhood_name)
      choices <- c("All" = "", setNames(hoods$HOOD_158, hoods$neighbourhood_name))
      updateSelectInput(session, "selected_hood", choices = choices,
                        selected = app_state$selected_hood %||% "")
    })

    output$selected_label <- renderUI({
      hood <- app_state$selected_hood
      if (is.null(hood) || hood == "") {
        span("All", class = "selected-val")
      } else {
        req(app_data$bar_data)
        name <- app_data$bar_data %>%
          dplyr::filter(HOOD_158 == hood) %>%
          dplyr::pull(neighbourhood_name) %>%
          dplyr::first()
        span(name, class = "selected-val selected-val--active")
      }
    })

    observeEvent(input$crime_type, {
      req(input$crime_type)
      setters$set_crime_type(input$crime_type)
    }, ignoreNULL = TRUE)

    observeEvent(input$selected_year, {
      setters$set_selected_year(input$selected_year)
    })

    observeEvent(input$selected_hood, {
      hood <- if (input$selected_hood == "") NULL else input$selected_hood
      setters$set_selected_hood(hood)
    }, ignoreInit = TRUE)

    observeEvent(app_state$selected_hood, {
      updateSelectInput(session, "selected_hood",
                        selected = app_state$selected_hood %||% "")
    }, ignoreInit = TRUE)

    observeEvent(app_state$crime_type, {
      session$sendCustomMessage("activateCrimeBtn",
                                list(ns = session$ns(""), val = app_state$crime_type))
    }, ignoreInit = TRUE)

    observeEvent(input$reset, {
      updateSliderInput(session, "selected_year", value = 2025)
      updateSelectInput(session, "selected_hood", selected = "")
      setters$set_crime_type("Auto Theft")
      setters$set_selected_year(2025)
      setters$set_selected_hood(NULL)
      session$sendCustomMessage("activateCrimeBtn",
                                list(ns = session$ns(""), val = "Auto Theft"))
    })
  })
}