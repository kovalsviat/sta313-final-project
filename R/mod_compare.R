# ============================================================
# R/mod_compare.R
# ============================================================

COMPARE_COLOURS <- c("#1b7837", "#d6604d", "#4393c3")

NOTABLE_HI_C <- 1.30
NOTABLE_LO_C <- 0.70

mod_compare_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("compare_modal"))
}

mod_compare_server <- function(id, app_state, setters, app_data) {
  moduleServer(id, function(input, output, session) {

    compare_mode <- reactiveVal("overlay")

    observeEvent(app_state$compare_modal_open, {
      if (isTRUE(app_state$compare_modal_open) &&
          length(app_state$compare_hoods) == 0 &&
          !is.null(app_state$detail_hood)) {
        setters$set_compare_hoods(app_state$detail_hood)
      }
    }, ignoreInit = TRUE)

    observeEvent(app_state$compare_modal_open, {
      if (!isTRUE(app_state$compare_modal_open)) {
        removeModal(); return()
      }

      compare_mode("overlay")   # reset to overlay on every open

      selected_hoods <- isolate(unique(c(
        app_state$detail_hood, app_state$compare_hoods)))
      selected_hoods <- selected_hoods[
        !is.na(selected_hoods) & nzchar(selected_hoods)
      ][seq_len(min(length(selected_hoods), 3))]

      init_year <- isolate(as.integer(app_state$active_year %||% 2025))

      hood_choices <- app_data$neighbourhood_profile %>%
        dplyr::distinct(HOOD_158, neighbourhood_name) %>%
        dplyr::arrange(neighbourhood_name)
      choices_vec <- stats::setNames(hood_choices$HOOD_158,
                                     hood_choices$neighbourhood_name)

      removeModal()
      showModal(modalDialog(
        easyClose = FALSE, size = "xl", footer = NULL,
        tagList(
          tags$script(HTML(
            "$(document)
               .off('shown.bs.modal.cp shown.bs.tab.cp')
               .on('shown.bs.modal.cp shown.bs.tab.cp', function(){
                 setTimeout(function(){ $(window).trigger('resize'); }, 150);
               });
             setTimeout(function(){ $(window).trigger('resize'); }, 220);

             Shiny.addCustomMessageHandler('highlightSocioBars', function(msg) {
               var rows = document.querySelectorAll('[data-hood-idx]');
               rows.forEach(function(r) {
                 var active = parseInt(r.getAttribute('data-hood-idx')) === msg.idx;
                 r.style.background = active
                   ? r.getAttribute('data-hood-col') + '18'
                   : 'transparent';
                 r.style.borderRadius = active ? '4px' : '0';
                 r.style.padding = active ? '2px 4px' : '0';
                 r.style.marginLeft = active ? '-4px' : '0';
               });
             });"
          )),

          # Header
          div(style="padding:16px 22px 12px; border-bottom:1px solid #e8eaed;
                      display:flex; justify-content:space-between; align-items:flex-start;",
            div(
              h4(style="margin:0 0 2px; font-size:18px; font-weight:600; color:#1a2a38;",
                 "Compare neighbourhoods"),
              div(style="font-size:12px; color:#888;", "Crime rate profiles")
            ),
            actionButton(session$ns("close_compare"), "\u00D7",
              style="background:transparent; border:1px solid #d0d7de; color:#666;
                     border-radius:6px; padding:2px 10px; font-size:17px;
                     line-height:1.4; cursor:pointer;")
          ),

          div(style="padding:14px 22px 0;",

            # Neighbourhood selector
            div(style="margin-bottom:12px;",
              selectizeInput(
                inputId  = session$ns("hood_select"),
                label    = NULL,
                choices  = choices_vec,
                selected = selected_hoods,
                multiple = TRUE,
                options  = list(maxItems = 3,
                                placeholder = "Add neighbourhoods (max 3)")
              )
            ),

            # View toggle + year slider
            div(style="display:flex; justify-content:space-between; align-items:center;
                        margin-bottom:16px; gap:20px; flex-wrap:wrap;",
              # Toggle — rendered as uiOutput so active state updates reactively
              uiOutput(session$ns("mode_toggle")),
              # Year slider
              div(style="display:flex; align-items:center; gap:8px;
                          flex:1; max-width:340px;",
                span(style="font-size:11px; color:#888; white-space:nowrap;", "Year:"),
                sliderInput(session$ns("compare_year"), label=NULL,
                  min=2014, max=2025, value=init_year,
                  step=1, sep="", ticks=FALSE, width="100%")
              )
            ),

            uiOutput(session$ns("compare_body"))
          )
        )
      ))
    }, ignoreInit = TRUE)

    # ── Button handlers ────────────────────────────────────────────────────
    observeEvent(input$btn_overlay, { compare_mode("overlay")    }, ignoreInit = TRUE)
    observeEvent(input$btn_side,    { compare_mode("sidebyside") }, ignoreInit = TRUE)

    # Re-render toggle buttons whenever mode changes so active style is correct
    output$mode_toggle <- renderUI({
      mode <- compare_mode()
      ov_sty <- if (mode == "overlay")
        "font-size:11px; padding:5px 16px; background:#134E7C; color:#fff;
         border:none; cursor:pointer; border-radius:0; margin:0;"
      else
        "font-size:11px; padding:5px 16px; background:transparent; color:#666;
         border:none; cursor:pointer; border-radius:0; margin:0;"
      sb_sty <- if (mode == "sidebyside")
        "font-size:11px; padding:5px 16px; background:#134E7C; color:#fff;
         border:none; cursor:pointer; border-radius:0; margin:0;
         border-left:1px solid #d0d7de;"
      else
        "font-size:11px; padding:5px 16px; background:transparent; color:#666;
         border:none; cursor:pointer; border-radius:0; margin:0;
         border-left:1px solid #d0d7de;"
      div(style="display:flex; border:1px solid #d0d7de; border-radius:7px;
                  overflow:hidden; flex-shrink:0;",
        actionButton(session$ns("btn_overlay"), "Overlay",     style=ov_sty),
        actionButton(session$ns("btn_side"),    "Side by side", style=sb_sty)
      )
    })

    observeEvent(input$hood_select, {
      setters$set_compare_hoods(input$hood_select)
    }, ignoreInit = TRUE)

    observeEvent(input$compare_year, {
      req(input$compare_year)
      setters$set_active_year(as.integer(input$compare_year))
    }, ignoreInit = TRUE)

    # ── Body: single renderUI, reads compare_mode() directly ──────────────
    output$compare_body <- renderUI({
      hoods <- unique(c(app_state$detail_hood, app_state$compare_hoods))
      hoods <- hoods[!is.na(hoods) & nzchar(hoods)][seq_len(min(length(hoods), 3))]
      req(length(hoods) > 0)
      mode  <- compare_mode()   # reactive dependency here is correct

      if (mode == "overlay") {
        # ── OVERLAY ──────────────────────────────────────────────
        div(style="padding-bottom:16px;",

          div(style="display:flex; gap:22px; align-items:flex-start;",

            # Glyph — larger to fill available space
            div(style="flex:1; min-width:0;",
              plotlyOutput(session$ns("glyph_overlay"),
                           height="440px", width="100%")
            ),

            # Socio panel — fixed narrower width
            div(style="width:210px; flex-shrink:0;",
              div(style="font-size:10px; font-weight:700; text-transform:uppercase;
                          letter-spacing:.08em; color:#999; margin-bottom:10px;",
                  "Socioeconomic profile"),
              uiOutput(session$ns("overlay_socio")),
              div(style="font-size:10px; color:#bbb; font-style:italic; margin-top:8px;
                          border-top:1px solid #f0f0f0; padding-top:6px;",
                  "* 2021 census data")
            )
          )
        )

      } else {
        # ── SIDE BY SIDE ─────────────────────────────────────────
        div(style="padding-bottom:16px;",
          div(style=paste0("display:flex; gap:14px; align-items:flex-start;"),
            tagList(purrr::map(seq_along(hoods), function(i) {
              h   <- hoods[i]
              col <- COMPARE_COLOURS[i]
              nm  <- tryCatch(
                app_data$neighbourhood_profile %>%
                  dplyr::filter(HOOD_158==h) %>%
                  dplyr::slice(1) %>%
                  dplyr::pull(neighbourhood_name),
                error=function(e) h)
              if (length(nm)==0) nm <- h

              div(style="flex:1; min-width:0; border:1px solid #e8eaed;
                          border-radius:8px; padding:12px;",
                div(style=paste0("font-size:12px; font-weight:600; color:", col,
                                 "; margin-bottom:10px; padding-bottom:6px;",
                                 " border-bottom:1px solid #f0f0f0;"), nm),
                plotlyOutput(session$ns(paste0("glyph_side_", i)),
                             height="220px", width="100%"),
                div(style="margin-top:12px;",
                  div(style="font-size:9.5px; font-weight:700; text-transform:uppercase;
                              letter-spacing:.08em; color:#bbb; margin-bottom:8px;",
                      "Socioeconomic profile"),
                  uiOutput(session$ns(paste0("socio_side_", i)))
                ),
                div(style="font-size:9.5px; color:#bbb; font-style:italic;
                            margin-top:6px; border-top:1px solid #f5f5f5; padding-top:4px;",
                    "* 2021 census data")
              )
            }))
          )
        )
      }
    })

    # ── Overlay glyph ─────────────────────────────────────────────────────
    output$glyph_overlay <- renderPlotly({
      req(app_state$active_year)
      hoods <- unique(c(app_state$detail_hood, app_state$compare_hoods))
      hoods <- hoods[!is.na(hoods) & nzchar(hoods)][seq_len(min(length(hoods), 3))]
      req(length(hoods) > 0)

      yr          <- as.integer(app_state$active_year)
      crime_order <- names(CRIME_COLOURS)

      avg_df <- app_data$glyph_avg %>%
        dplyr::mutate(year=as.integer(year)) %>%
        dplyr::filter(year==yr)
      req(nrow(avg_df) > 0)

      safe_avg <- function(ct) {
        v <- avg_df %>% dplyr::filter(CSI_CATEGORY==ct) %>%
          dplyr::pull(s_city_avg_rate)
        if (length(v)==0||is.na(v[[1]])) 0 else v[[1]]
      }
      avg_r <- purrr::map_dbl(crime_order, safe_avg)

      p <- plotly::plot_ly() %>%
        plotly::add_trace(
          type="scatterpolar", mode="lines",
          r=c(avg_r,avg_r[[1]]), theta=c(crime_order,crime_order[[1]]),
          fill="toself", name="City average",
          line=list(color="#e78ac3",width=2),
          fillcolor="rgba(231,138,195,0.15)",
          hovertemplate="%{theta}<br>City avg: %{r:.2f}<extra></extra>"
        )

      for (i in seq_along(hoods)) {
        h   <- hoods[i]
        col <- COMPARE_COLOURS[i]
        hood_df <- app_data$glyph_crime_by_year %>%
          dplyr::mutate(HOOD_158=as.character(HOOD_158), year=as.integer(year)) %>%
          dplyr::filter(HOOD_158==h, year==yr)
        if (nrow(hood_df) == 0) next

        safe_hood <- function(ct) {
          v <- hood_df %>% dplyr::filter(CSI_CATEGORY==ct) %>%
            dplyr::pull(s_crime_rate)
          if (length(v)==0||is.na(v[[1]])) 0 else v[[1]]
        }
        hr <- purrr::map_dbl(crime_order, safe_hood)
        nm <- tryCatch(hood_df$neighbourhood_name[[1]], error=function(e) h)

        p <- p %>% plotly::add_trace(
          type="scatterpolar", mode="lines+markers",
          r=c(hr,hr[[1]]), theta=c(crime_order,crime_order[[1]]),
          fill="toself", name=nm,
          line=list(color=col,width=2.5),
          fillcolor=paste0(col,"30"),
          hovertemplate=paste0("%{theta}<br>Score: %{r:.2f}<extra>",nm,"</extra>"),
          marker=list(color=col,size=6,line=list(color="#fff",width=1.2)),
          customdata=rep(i, length(crime_order)+1)
        )
      }

      p %>% plotly::layout(
        polar=list(
          bgcolor="rgba(0,0,0,0)",
          radialaxis=list(showline=FALSE,showgrid=FALSE,showticklabels=FALSE),
          angularaxis=list(direction="clockwise",tickfont=list(size=11),
                           showline=FALSE,showgrid=TRUE,
                           gridcolor="rgba(180,180,180,0.3)")
        ),
        showlegend=TRUE,
        legend=list(orientation="h",x=0.5,xanchor="center",
                    y=-0.1,font=list(size=11)),
        margin=list(l=55,r=55,t=20,b=10),
        plot_bgcolor="rgba(0,0,0,0)", paper_bgcolor="rgba(0,0,0,0)"
      ) %>%
      plotly::event_register("plotly_click")
    })

    # ── Glyph click → highlight matching socio bars ───────────────────────
    observeEvent(plotly::event_data("plotly_click"), {
      click <- plotly::event_data("plotly_click")
      req(!is.null(click))

      hood_idx <- tryCatch(as.integer(click$customdata[[1]]), error=function(e) NULL)
      req(!is.null(hood_idx), hood_idx >= 1)
      session$sendCustomMessage("highlightSocioBars",
                                list(ns=session$ns(""), idx=hood_idx))
    })

    # ── Overlay socio bars ────────────────────────────────────────────────
    output$overlay_socio <- renderUI({
      hoods <- unique(c(app_state$detail_hood, app_state$compare_hoods))
      hoods <- hoods[!is.na(hoods) & nzchar(hoods)][seq_len(min(length(hoods), 3))]
      req(length(hoods) > 0)

      city <- app_data$neighbourhood_profile %>%
        dplyr::summarise(income=mean(income,na.rm=TRUE),
                         unemployment=mean(unemployment,na.rm=TRUE),
                         low_income=mean(low_income,na.rm=TRUE),
                         housing_cost=mean(housing_cost,na.rm=TRUE))

      profiles <- purrr::map(hoods, function(h)
        tryCatch(app_data$neighbourhood_profile %>%
                   dplyr::filter(HOOD_158==h) %>% dplyr::slice(1),
                 error=function(e) NULL))

      factors <- list(
        list(key="income",       label="Median income",
             city_val=city$income[[1]],
             fmt=function(v) paste0("$",formatC(round(v),format="d",big.mark=",")),
             dir_bad="low"),
        list(key="unemployment", label="Unemployment rate",
             city_val=city$unemployment[[1]],
             fmt=function(v) paste0(round(v,1),"%"), dir_bad="high"),
        list(key="low_income",   label="Low income rate",
             city_val=city$low_income[[1]],
             fmt=function(v) paste0(round(v,1),"%"), dir_bad="high"),
        list(key="housing_cost", label="Housing cost burden",
             city_val=city$housing_cost[[1]],
             fmt=function(v) paste0(round(v,1),"%"), dir_bad="high")
      )

      tagList(purrr::map(factors, function(d) {
        cv <- d$city_val
        if (is.na(cv)||cv==0) return(NULL)

        div(style="margin-bottom:13px;",
          div(style="font-size:11.5px; font-weight:500; color:#333; margin-bottom:5px;",
              d$label),
          tagList(purrr::map(seq_along(hoods), function(i) {
            pr <- profiles[[i]]
            if (is.null(pr)||nrow(pr)==0) return(NULL)
            hv <- tryCatch(pr[[d$key]][[1]], error=function(e) NA_real_)
            if (is.na(hv)) return(NULL)
            ratio    <- hv/cv
            bar_w    <- round(min(ratio/2*100,100),1)
            notable  <- if(d$dir_bad=="high") ratio>NOTABLE_HI_C else ratio<NOTABLE_LO_C
            col      <- COMPARE_COLOURS[i]
            fill_col <- col
            pct      <- round((ratio-1)*100)
            pct_txt  <- paste0(if(pct>=0)"+" else "",pct,"%")
            pct_col  <- if(notable) "#c0392b" else "#aaa"

            nm_label <- tryCatch(profiles[[i]]$neighbourhood_name[[1]],
                                  error=function(e) hoods[i])

            div(style="margin-bottom:4px; transition:background .2s, padding .2s;",
              title=nm_label,
              `data-hood-idx`=as.character(i),
              `data-hood-col`=col,
              div(style="display:flex; justify-content:space-between; margin-bottom:2px;",
                span(style=paste0("font-size:11px; color:",col,"; font-weight:500;"),
                     d$fmt(hv)),
                span(style=paste0("font-size:10px; color:",pct_col,";"), pct_txt)
              ),
              div(style="position:relative; height:5px; background:#edf0f3;
                          border-radius:3px; overflow:visible;",
                div(style="position:absolute; left:50%; top:-3px; width:1.5px; height:11px;
                            background:rgba(80,80,80,0.3); z-index:2; border-radius:1px;"),
                div(style=paste0("position:absolute; left:0; top:0; height:100%;",
                                 " width:",bar_w,"%; background:",fill_col,
                                 "; border-radius:3px; z-index:1;"))
              )
            )
          })),
          div(style="font-size:10px; color:#666; margin-top:2px;",
              paste0("city avg: ", d$fmt(cv)))
        )
      }))
    })

    # ── Side-by-side individual glyphs + socio ────────────────────────────
    # Shared scale reactive: max across all hoods so glyphs are comparable
    shared_axis_max <- reactive({
      req(app_state$active_year)
      yr          <- as.integer(app_state$active_year)
      crime_order <- names(CRIME_COLOURS)
      hoods <- unique(c(app_state$detail_hood, app_state$compare_hoods))
      hoods <- hoods[!is.na(hoods) & nzchar(hoods)][seq_len(min(length(hoods), 3))]

      avg_df <- app_data$glyph_avg %>%
        dplyr::mutate(year=as.integer(year)) %>%
        dplyr::filter(year==yr)

      all_vals <- c(
        purrr::map_dbl(crime_order, function(ct) {
          v <- avg_df %>% dplyr::filter(CSI_CATEGORY==ct) %>%
            dplyr::pull(s_city_avg_rate)
          if (length(v)==0||is.na(v[[1]])) 0 else v[[1]]
        }),
        purrr::map(hoods, function(h) {
          hd <- app_data$glyph_crime_by_year %>%
            dplyr::mutate(HOOD_158=as.character(HOOD_158), year=as.integer(year)) %>%
            dplyr::filter(HOOD_158==h, year==yr)
          purrr::map_dbl(crime_order, function(ct) {
            v <- hd %>% dplyr::filter(CSI_CATEGORY==ct) %>%
              dplyr::pull(s_crime_rate)
            if (length(v)==0||is.na(v[[1]])) 0 else v[[1]]
          })
        }) %>% unlist()
      )
      mx <- max(all_vals, na.rm=TRUE)
      if (mx > 0 && is.finite(mx)) round(mx / 0.80, 3) else 1
    })

    observe({
      hoods <- unique(c(app_state$detail_hood, app_state$compare_hoods))
      hoods <- hoods[!is.na(hoods) & nzchar(hoods)][seq_len(min(length(hoods), 3))]
      req(length(hoods) > 0)

      for (i in seq_along(hoods)) {
        local({
          idx <- i; h <- hoods[idx]; col <- COMPARE_COLOURS[idx]

          output[[paste0("glyph_side_", idx)]] <- renderPlotly({
            req(app_state$active_year)
            yr          <- as.integer(app_state$active_year)
            crime_order <- names(CRIME_COLOURS)

            hood_df <- app_data$glyph_crime_by_year %>%
              dplyr::mutate(HOOD_158=as.character(HOOD_158), year=as.integer(year)) %>%
              dplyr::filter(HOOD_158==h, year==yr)
            avg_df  <- app_data$glyph_avg %>%
              dplyr::mutate(year=as.integer(year)) %>%
              dplyr::filter(year==yr)

            validate(need(nrow(hood_df)>0,"No data."),
                     need(nrow(avg_df)>0, "No city avg."))

            sv <- function(df, cn, ct) {
              v <- tryCatch(df %>% dplyr::filter(CSI_CATEGORY==ct) %>%
                              dplyr::pull(dplyr::all_of(cn)),
                            error=function(e) numeric(0))
              if (length(v)==0||is.na(v[[1]])) 0 else v[[1]]
            }

            hr   <- purrr::map_dbl(crime_order, ~sv(hood_df,"s_crime_rate",.x))
            avgr <- purrr::map_dbl(crime_order, ~sv(avg_df, "s_city_avg_rate",.x))
            amax <- shared_axis_max()
            nm   <- tryCatch(hood_df$neighbourhood_name[[1]], error=function(e) h)

            plotly::plot_ly() %>%
              plotly::add_trace(
                type="scatterpolar", mode="lines",
                r=c(avgr,avgr[[1]]), theta=c(crime_order,crime_order[[1]]),
                fill="toself", name="City avg",
                line=list(color="#e78ac3",width=1.5),
                fillcolor="rgba(231,138,195,0.15)",
                hovertemplate="%{theta}<br>City avg: %{r:.2f}<extra></extra>"
              ) %>%
              plotly::add_trace(
                type="scatterpolar", mode="lines+markers",
                r=c(hr,hr[[1]]), theta=c(crime_order,crime_order[[1]]),
                fill="toself", name=nm,
                line=list(color=col,width=2),
                fillcolor=paste0(col,"30"),
                hovertemplate="%{theta}<br>Score: %{r:.2f}<extra></extra>",
                marker=list(color=col,size=5,line=list(color="#fff",width=1))
              ) %>%
              plotly::layout(
                polar=list(
                  bgcolor="rgba(0,0,0,0)",
                  radialaxis=list(showline=FALSE,showgrid=FALSE,
                                  showticklabels=FALSE,range=c(0,amax)),
                  angularaxis=list(direction="clockwise",tickfont=list(size=9),
                                   showline=FALSE,showgrid=TRUE,
                                   gridcolor="rgba(180,180,180,0.3)")
                ),
                showlegend=FALSE,
                margin=list(l=35,r=35,t=15,b=5),
                plot_bgcolor="rgba(0,0,0,0)", paper_bgcolor="rgba(0,0,0,0)"
              )
          })

          output[[paste0("socio_side_", idx)]] <- renderUI({
            pr <- tryCatch(
              app_data$neighbourhood_profile %>%
                dplyr::filter(HOOD_158==h) %>% dplyr::slice(1),
              error=function(e) NULL)
            req(!is.null(pr), nrow(pr)>0)

            city <- app_data$neighbourhood_profile %>%
              dplyr::summarise(income=mean(income,na.rm=TRUE),
                               unemployment=mean(unemployment,na.rm=TRUE),
                               low_income=mean(low_income,na.rm=TRUE),
                               housing_cost=mean(housing_cost,na.rm=TRUE))

            factors <- list(
              list(key="income",       label="Median income",
                   cv=city$income[[1]],
                   fmt=function(v) paste0("$",formatC(round(v),format="d",big.mark=",")),
                   dir_bad="low"),
              list(key="unemployment", label="Unemployment",
                   cv=city$unemployment[[1]],
                   fmt=function(v) paste0(round(v,1),"%"), dir_bad="high"),
              list(key="low_income",   label="Low income rate",
                   cv=city$low_income[[1]],
                   fmt=function(v) paste0(round(v,1),"%"), dir_bad="high"),
              list(key="housing_cost", label="Housing cost burden",
                   cv=city$housing_cost[[1]],
                   fmt=function(v) paste0(round(v,1),"%"), dir_bad="high")
            )

            tagList(purrr::map(factors, function(d) {
              hv <- tryCatch(pr[[d$key]][[1]], error=function(e) NA_real_)
              if (is.na(hv)||is.na(d$cv)||d$cv==0) return(NULL)
              ratio    <- hv/d$cv
              bar_w    <- round(min(ratio/2*100,100),1)
              notable  <- if(d$dir_bad=="high") ratio>NOTABLE_HI_C else ratio<NOTABLE_LO_C
              fill_col <- col
              pct      <- round((ratio-1)*100)
              pct_txt  <- paste0(if(pct>=0)"+" else "",pct,"% vs avg")
              pct_col  <- if(notable) "#c0392b" else "#aaa"

              div(style="margin-bottom:8px;",
                div(style="display:flex; justify-content:space-between; align-items:baseline; margin-bottom:2px;",
                  span(style="font-size:11px; font-weight:500; color:#333;", d$label),
                  span(style=paste0("font-size:10px; color:",pct_col,"; font-weight:",
                                    if(notable)"600" else "400",";"), pct_txt)
                ),
                div(style=paste0("font-size:13px; font-weight:600; color:#1a2a38;",
                                 " margin-bottom:4px;"), d$fmt(hv)),
                div(style="position:relative; height:6px; background:#edf0f3;
                            border-radius:4px; overflow:visible;",
                  div(style="position:absolute; left:50%; top:-4px; width:1.5px; height:14px;
                              background:rgba(80,80,80,0.3); z-index:2; border-radius:1px;"),
                  div(style=paste0("position:absolute; left:0; top:0; height:100%;",
                                   " width:",bar_w,"%; background:",fill_col,
                                   "; border-radius:4px; z-index:1;"))
                ),
                div(style="font-size:10px; color:#666; margin-top:2px;",
                    paste0("city avg: ", d$fmt(d$cv)))
              )
            }))
          })
        })
      }
    })

    observeEvent(input$close_compare, {
      setters$set_compare_hoods(character(0))
      setters$set_compare_modal_open(FALSE)
    })
  })
}