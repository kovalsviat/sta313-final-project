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
            btn_col <- unname(CRIME_COLOURS[item$val])
            # Pill style matching neighbourhood modal correlation pills:
            # transparent bg + coloured border + coloured text when inactive
            # solid bg + white text when active (set by JS)
            tags$button(
              id           = ns(paste0("ct_", gsub(" ", "_", item$val))),
              type         = "button",
              class        = "crime-type-btn",
              `data-value` = item$val,
              style        = paste0(
                "--btn-color:", btn_col, ";",
                # inactive: outlined pill
                "background:transparent;",
                "border:1.5px solid ", btn_col, ";",
                "border-radius:50px;",
                "color:", btn_col, ";",
                "font-size:10px; font-weight:600;",
                "padding:3px 10px; cursor:pointer;",
                "transition:background .15s, color .15s;",
                "white-space:nowrap; opacity:0.85;"
              ),
              tags$span(class = "crime-btn-icon",
                        style = "font-size:12px; margin-right:4px;",
                        item$icon),
              tags$span(item$val)
            )
          }
        )
      ),
      tags$input(id = ns("crime_type"), type = "hidden", value = "Auto Theft"),
      tags$script(HTML(sprintf("
        (function() {
          var pfx = '%s';

          function highlightBtn(val) {
            var btns = document.querySelectorAll('[id^=\"' + pfx + 'ct_\"]');
            btns.forEach(function(b) {
              var isActive = b.getAttribute('data-value') === val;
              var col = getComputedStyle(b).getPropertyValue('--btn-color').trim();
              if (isActive) {
                b.style.background  = col;
                b.style.borderColor = col;
                b.style.color       = '#fff';
                b.style.opacity     = '1';
                b.style.boxShadow   = '0 0 0 2.5px ' + col + '55';
              } else {
                b.style.background  = 'transparent';
                b.style.borderColor = col;
                b.style.color       = col;
                b.style.opacity     = '0.75';
                b.style.boxShadow   = 'none';
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

    # ── Sync slider when year changes externally (e.g. trend chart click) ──
    observeEvent(app_state$selected_year, {
      updateSliderInput(session, "selected_year", value = app_state$selected_year)
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