# ============================================================
# R/mod_neighbourhood.R
# ============================================================

NOTABLE_HI <- 1.30
NOTABLE_LO <- 0.70

# Plain-English sentence shown when a correlation pill is clicked
make_expl_sentence <- function(crime_type, soc_var, spearman_r, is_strong) {
  labels <- c(income       = "median income",
              unemployment = "unemployment rate",
              low_income   = "low income rate",
              housing_cost = "housing cost burden")
  flab     <- unname(labels[soc_var])
  strength <- if (is_strong) "strong" else "modest"
  r_str    <- sprintf("%+.2f", spearman_r)

  if (spearman_r > 0)
    paste0("In Toronto, higher ", flab, " is associated with higher ",
           crime_type, " rates (\u03C1\u00A0=\u00A0", r_str, ").",
           " This is a ", strength, " positive relationship \u2014 both tend to be high together.")
  else
    paste0("In Toronto, lower ", flab, " is associated with higher ",
           crime_type, " rates (\u03C1\u00A0=\u00A0", r_str, ").",
           " This is a ", strength, " negative relationship \u2014 when one is high, the other tends to be low.")
}

mod_neighbourhood_ui <- function(id) { ns <- NS(id); uiOutput(ns("hood_panel")) }

mod_neighbourhood_server <- function(id, app_state, setters, app_data) {
  moduleServer(id, function(input, output, session) {

    rv <- reactiveValues(pill_crime = NULL)

    hood_meta <- reactive({
      req(app_state$detail_hood)
      app_data$neighbourhood_profile %>%
        dplyr::filter(HOOD_158 == app_state$detail_hood) %>%
        dplyr::slice(1)
    })

    output$hood_panel <- renderUI({ NULL })

    observeEvent(
      list(app_state$hood_panel_open, app_state$detail_hood,
           app_state$compare_modal_open), {

      if (isTRUE(app_state$compare_modal_open)) return()
      if (!isTRUE(app_state$hood_panel_open) || is.null(app_state$detail_hood)) {
        removeModal(); return()
      }
      req(hood_meta())
      ns   <- session$ns
      meta <- hood_meta()
      rv$pill_crime <- NULL

      removeModal()
      showModal(modalDialog(
        easyClose = FALSE, size = "xl", footer = NULL,
        tagList(
          tags$script(HTML(
            "$(document).off('shown.bs.modal.hp shown.bs.tab.hp')
             .on('shown.bs.modal.hp shown.bs.tab.hp', function(){
               setTimeout(function(){ $(window).trigger('resize'); }, 150);
             });
             setTimeout(function(){ $(window).trigger('resize'); }, 220);

             // Hint content keyed by id
             var HINT_TEXT = {
               hint_socio: 'Click <em>Might correlate</em> under any factor to see which crime types are statistically linked to it (2021 Cencus). Then click a crime pill to read the explanation and highlight that crime on the chart.',
               hint_glyph: 'Hover any point to see the score for that crime type. The year slider lets you see how the profile shifted over time.'
             };

             // Shared tooltip div appended to body so overflow:hidden never clips it
             var _hintEl = null;
             var _hintActive = null;
             function getHintEl() {
               if (!_hintEl) {
                 _hintEl = document.createElement('div');
                 _hintEl.style.cssText = 'position:fixed; width:240px; background:#1a2a38; color:#e9eff3;' +
                   'font-size:11px; line-height:1.55; border-radius:6px; padding:9px 11px;' +
                   'z-index:99999; box-shadow:0 2px 10px rgba(0,0,0,0.35); display:none;';
                 document.body.appendChild(_hintEl);
                 // Click outside closes it
                 document.addEventListener('click', function(e) {
                   if (_hintEl && !_hintEl.contains(e.target) &&
                       !e.target.getAttribute('onclick')?.includes('showHint')) {
                     _hintEl.style.display = 'none';
                     _hintActive = null;
                   }
                 });
               }
               return _hintEl;
             }
             window.showHint = function(btn, id) {
               var el = getHintEl();
               if (_hintActive === id) {
                 el.style.display = 'none'; _hintActive = null; return;
               }
               el.innerHTML = HINT_TEXT[id] || '';
               var r = btn.getBoundingClientRect();
               el.style.display = 'block';
               // Position to the right; flip left if it would overflow viewport
               var left = r.right + 8;
               if (left + 250 > window.innerWidth) left = r.left - 258;
               el.style.left = left + 'px';
               el.style.top  = Math.max(8, r.top - 10) + 'px';
               _hintActive = id;
             };

             // pill click: toggle select/deselect
             window.pillClick = function(btn, inputId, crime, explId) {
               var wasActive = btn.dataset.active === '1';
               // Reset all pills
               document.querySelectorAll('[data-corr-pill]').forEach(function(p){
                 p.style.boxShadow='none'; p.style.opacity='0.7'; p.dataset.active='0';
               });
               var el = document.getElementById(explId);
               if (wasActive) {
                 // Second click: deselect — hide explanation, clear input
                 if(el) el.style.display='none';
                 Shiny.setInputValue(inputId, '', {priority:'event'});
               } else {
                 // First click: select
                 btn.style.boxShadow='0 0 0 2.5px '+btn.dataset.color+'99';
                 btn.style.opacity='1'; btn.dataset.active='1';
                 if(el){ el.textContent=btn.dataset.expl; el.style.display='block'; }
                 Shiny.setInputValue(inputId, crime, {priority:'event'});
               }
             };"
          )),

          div(style="padding:18px 24px 14px; border-bottom:1px solid #e8eaed;
                      display:flex; justify-content:space-between; align-items:flex-start;",
            div(
              h4(style="margin:0 0 3px; font-size:20px; font-weight:600; color:#1a2a38;",
                 meta$neighbourhood_name[[1]]),
              div(style="font-size:12px; color:#888;",
                  paste0("Hood\u00A0", meta$HOOD_158[[1]]))
            ),
            actionButton(ns("close_panel"), "\u00D7",
              style="background:transparent; border:1px solid #d0d7de; color:#666;
                     border-radius:6px; padding:2px 10px; font-size:17px;
                     line-height:1.4; cursor:pointer;")
          ),

          div(style="display:flex; align-items:stretch; min-height:640px;",

            # LEFT
            div(style="flex:0 0 300px; width:300px; padding:14px 18px;
                        border-right:1px solid #e8eaed; display:flex;
                        flex-direction:column; gap:8px;
                        overflow-y:auto; max-height:640px; box-sizing:border-box;",

              div(style="width:100%; height:175px; flex-shrink:0;
                          border-radius:8px; overflow:hidden; border:1px solid #e0e4e8;",
                leafletOutput(ns("hood_map"), height="175px", width="100%")
              ),
              actionButton(ns("open_compare"), "Compare",
                style="width:100%; background:transparent; border:1px solid #c8d4de;
                        color:#427196; border-radius:6px; font-size:12px;
                        font-weight:500; padding:6px 10px; cursor:pointer; flex-shrink:0;"),
              tags$hr(style="margin:2px 0; border:none; border-top:1px solid #eee; flex-shrink:0;"),
              div(style="display:flex; align-items:center; gap:5px; flex-shrink:0;",
                div(style="font-size:10px; font-weight:700; text-transform:uppercase;
                            letter-spacing:.08em; color:#999;",
                    "Socioeconomic profile vs city average"),
                tags$span("?",
                  id = "hint_btn_socio",
                  onclick = "showHint(this, 'hint_socio')",
                  style = "display:inline-flex; align-items:center; justify-content:center;
                           width:14px; height:14px; border-radius:50%;
                           background:#e8eaed; color:#555; font-size:9px;
                           font-weight:700; cursor:pointer; flex-shrink:0; line-height:1;"
                )
              ),
              tags$input(id=ns("crime_pill_click"), type="hidden", value=""),
              uiOutput(ns("socio_bars")),
              div(style="font-size:10px; color:#bbb; font-style:italic;
                          margin-top:2px; flex-shrink:0; padding-top:4px;
                          border-top:1px solid #f0f0f0;",
                  "* Socioeconomic data from 2021 census")
            ),

            # RIGHT
            div(style="flex:1; min-width:0; display:flex; flex-direction:column;",
              tabsetPanel(
                id=ns("detail_view"), selected="Overview", type="tabs",
                tabPanel("Overview",
                  div(style="padding:14px 20px 8px;",
                    div(style="display:flex; align-items:center; gap:6px; margin-bottom:6px;",
                      div(style="font-size:10px; font-weight:700; text-transform:uppercase;
                                  letter-spacing:.08em; color:#999;",
                          "Crime rate profile"),
                      tags$span("?",
                        id = "hint_btn_glyph",
                        onclick = "showHint(this, 'hint_glyph')",
                        style = "display:inline-flex; align-items:center; justify-content:center;
                                 width:14px; height:14px; border-radius:50%;
                                 background:#e8eaed; color:#555; font-size:9px;
                                 font-weight:700; cursor:pointer; flex-shrink:0; line-height:1;"
                      )
                    ),
                    plotlyOutput(ns("crime_glyph"), height="430px", width="100%"),
                    div(style="padding:8px 40px 0; display:flex; align-items:center; gap:10px;",
                      span(style="font-size:10px; color:#999; white-space:nowrap;", "Year:"),
                      sliderInput(ns("glyph_year"), label=NULL,
                        min=2014, max=2025,
                        value=isolate(as.integer(app_state$active_year %||% 2025)),
                        step=1, sep="", ticks=FALSE, width="100%")
                    )
                  )
                ),
                tabPanel("Patterns",
                  div(style="padding:14px 20px 8px;",
                    div(style="font-size:10px; font-weight:700; text-transform:uppercase;
                                letter-spacing:.08em; color:#999; margin-bottom:8px;",
                        "Monthly crime patterns"),
                    selectInput(ns("heatmap_crime"), label=NULL,
                      choices=names(CRIME_COLOURS), selected=isolate(app_state$crime_type),
                      width="200px"),
                    plotlyOutput(ns("heatmap"), height="360px", width="100%")
                  )
                )
              )
            )
          )
        )
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$glyph_year, {
      req(input$glyph_year)
      setters$set_active_year(as.integer(input$glyph_year))
    }, ignoreInit = TRUE)

    observeEvent(input$crime_pill_click, {
      ct <- input$crime_pill_click
      req(ct, ct != "")
      rv$pill_crime <- ct
      updateSelectInput(session, "heatmap_crime", selected = ct)
      setters$set_active_year(2021L)
      updateSliderInput(session, "glyph_year", value = 2021)
    }, ignoreNULL = TRUE)

    output$hood_map <- renderLeaflet({
      req(app_state$detail_hood)
      hood_sf <- app_data$neighbourhoods_sf %>%
        dplyr::filter(HOOD_158 == app_state$detail_hood)
      req(nrow(hood_sf) > 0)
      bbox <- sf::st_bbox(hood_sf)
      leaflet::leaflet(hood_sf,
          options=leaflet::leafletOptions(zoomControl=FALSE, attributionControl=FALSE)) %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        leaflet::fitBounds(bbox[["xmin"]], bbox[["ymin"]],
                           bbox[["xmax"]], bbox[["ymax"]]) %>%
        leaflet::addPolygons(fillColor="#134E7C", fillOpacity=0.5,
                             color="#0D3859", weight=2) %>%
        htmlwidgets::onRender("function(el,x){ this.invalidateSize(); }")
    })

    output$socio_bars <- renderUI({
      req(app_state$detail_hood)
      meta <- hood_meta()
      req(nrow(meta) > 0)

      city <- app_data$neighbourhood_profile %>%
        dplyr::summarise(income=mean(income,na.rm=TRUE),
                         unemployment=mean(unemployment,na.rm=TRUE),
                         low_income=mean(low_income,na.rm=TRUE),
                         housing_cost=mean(housing_cost,na.rm=TRUE))

      all_sig <- app_data$correlation_2021 %>%
        dplyr::filter(significant == TRUE, abs(spearman_r) > 0.20) %>%
        dplyr::arrange(soc_var, dplyr::desc(abs(spearman_r)))

      factors <- list(
        list(key="income",       label="Median income",
             hood_val=meta$income[[1]],       city_val=city$income[[1]],
             fmt=function(v) paste0("$",formatC(round(v),format="d",big.mark=",")), dir_bad="low"),
        list(key="unemployment", label="Unemployment rate",
             hood_val=meta$unemployment[[1]], city_val=city$unemployment[[1]],
             fmt=function(v) paste0(round(v,1),"%"), dir_bad="high"),
        list(key="low_income",   label="Low income rate",
             hood_val=meta$low_income[[1]],   city_val=city$low_income[[1]],
             fmt=function(v) paste0(round(v,1),"%"), dir_bad="high"),
        list(key="housing_cost", label="Housing cost burden",
             hood_val=meta$housing_cost[[1]], city_val=city$housing_cost[[1]],
             fmt=function(v) paste0(round(v,1),"%"), dir_bad="high")
      )

      ns_pill_id <- session$ns("crime_pill_click")

      q_text <- paste(
        "\u03C1 (Spearman correlation):",
        "Positive (\u03C1 > 0): both variables tend to be high together.",
        "Negative (\u03C1 < 0): when one is high, the other is low.",
        "Strong: |\u03C1| > 0.4 \u2014 Weak: statistically significant but smaller effect.",
        sep = " "
      )

      tagList(purrr::map(factors, function(d) {
        if (is.na(d$hood_val) || is.na(d$city_val) || d$city_val == 0) return(NULL)

        ratio    <- d$hood_val / d$city_val
        notable  <- if(d$dir_bad=="high") ratio > NOTABLE_HI else ratio < NOTABLE_LO
        bar_w    <- round(min(ratio/2*100, 100), 1)
        fill_col <- if(notable) "#c0392b" else "#134E7C"
        pct      <- round((ratio-1)*100)
        pct_txt  <- paste0(if(pct>=0)"+" else "", pct, "% vs avg")
        pct_col  <- if(notable) "#c0392b" else "#999"

        pills_df     <- all_sig %>% dplyr::filter(soc_var == d$key)
        strong_pills <- pills_df %>% dplyr::filter(show_in_ui == TRUE)
        weak_pills   <- pills_df %>% dplyr::filter(show_in_ui == FALSE)

        expl_div_id <- paste0("expl_", d$key)
        q_div_id    <- paste0("qdiv_", d$key)

        # Each pill shows crime name + ρ value inline
        make_pill_btn <- function(pr) {
          crime     <- pr$crime_type
          col       <- unname(CRIME_COLOURS[crime] %||% "#427196")
          r_str     <- sprintf("%+.2f", pr$spearman_r)
          is_strong <- isTRUE(pr$show_in_ui)
          expl      <- make_expl_sentence(crime, d$key, pr$spearman_r, is_strong)
          pill_sty  <- if (is_strong)
            paste0("background:", col, "; border:1.5px solid ", col, "; color:#fff;")
          else
            paste0("background:transparent; border:1.5px solid ", col,
                   "; color:", col, "; opacity:.85;")
          tags$button(
            type="button",
            `data-corr-pill`="", `data-color`=col, `data-expl`=expl, `data-active`="0",
            onclick=paste0("window.pillClick(this,'",ns_pill_id,"','",crime,"','",expl_div_id,"')"),
            style=paste0(pill_sty,
                         " border-radius:50px; font-size:10px; font-weight:600;",
                         " padding:2px 8px; cursor:pointer; transition:all .15s;",
                         " white-space:nowrap;"),
            crime
          )
        }

        div(style="margin-bottom:8px;",
          div(style="display:flex; justify-content:space-between; align-items:baseline; margin-bottom:2px;",
            span(style="font-size:12px; font-weight:500; color:#333;", d$label),
            span(style=paste0("font-size:10px; color:",pct_col,"; font-weight:",
                              if(notable)"600" else "400",";"), pct_txt)
          ),
          div(style="font-size:13px; font-weight:600; color:#1a2a38; margin-bottom:4px;",
              d$fmt(d$hood_val)),
          div(style="position:relative; height:6px; background:#e8eaed; border-radius:4px; overflow:visible;",
            div(style="position:absolute; left:50%; top:-4px; width:1.5px; height:14px;
                        background:rgba(80,80,80,0.3); z-index:2; border-radius:1px;"),
            div(style=paste0("position:absolute; left:0; top:0; height:100%; width:",bar_w,"%;",
                             " background:",fill_col,"; border-radius:4px; z-index:1;"))
          ),
          div(style="font-size:10.5px; color:#888; margin-top:2px;",
              paste0("city avg: ", d$fmt(d$city_val))),

          if (nrow(pills_df) > 0) {
            n_total <- nrow(pills_df)
            div(style="margin-top:7px;",
              # Header row: [<details>]
              div(style="display:flex; align-items:flex-start; gap:5px;",
                tags$details(
                  style="flex:1; min-width:0;",
                  tags$summary(
                    style="font-size:10.5px; color:#555; font-weight:600; cursor:pointer;
                           list-style:none; display:flex; align-items:center; gap:4px;
                           user-select:none; -webkit-user-select:none;",
                    "Might correlate",
                    tags$span(paste0("(", n_total, ")"),
                              style="font-size:9.5px; color:#bbb; font-weight:400;")
                  ),
                  # Pills
                  div(style="padding-top:5px; display:flex; flex-direction:column; gap:4px;",
                    if (nrow(weak_pills) > 0)
                      div(style="display:flex; align-items:center; gap:4px; flex-wrap:wrap;",
                        span(style="font-size:10px; color:#888; min-width:38px;", "Weak:"),
                        purrr::map(seq_len(nrow(weak_pills)), ~make_pill_btn(weak_pills[.x,]))
                      ),
                    if (nrow(strong_pills) > 0)
                      div(style="display:flex; align-items:center; gap:4px; flex-wrap:wrap;",
                        span(style="font-size:10px; color:#555; font-weight:600; min-width:38px;",
                             "Strong:"),
                        purrr::map(seq_len(nrow(strong_pills)), ~make_pill_btn(strong_pills[.x,]))
                      ),
                    # Explanation sentence (shown when a pill is clicked)
                    div(id=expl_div_id,
                      style=paste0("display:none; font-size:11px; color:#444; line-height:1.55;",
                                   " background:#f0f4fa; border-left:2.5px solid #134E7C;",
                                   " border-radius:0 4px 4px 0; padding:6px 9px; margin-top:4px;"))
                  )
                )
              )
            )
          }
        )
      }))
    })

    output$crime_glyph <- renderPlotly({
      req(app_state$detail_hood, app_state$active_year)
      detail_id   <- as.character(app_state$detail_hood)
      active_year <- as.integer(app_state$active_year)
      highlighted <- rv$pill_crime

      hood_df <- app_data$glyph_crime_by_year %>%
        dplyr::mutate(HOOD_158=as.character(HOOD_158), year=as.integer(year)) %>%
        dplyr::filter(HOOD_158==detail_id, year==active_year)
      avg_df <- app_data$glyph_avg %>%
        dplyr::mutate(year=as.integer(year)) %>%
        dplyr::filter(year==active_year)

      validate(need(nrow(hood_df)>0, paste("No crime data for", active_year,".")),
               need(nrow(avg_df)>0,  paste("No city average for", active_year,".")))

      crime_order <- names(CRIME_COLOURS)

      safe_val <- function(df, col_name, ct) {
        v <- tryCatch(
          df %>% dplyr::filter(CSI_CATEGORY==ct) %>%
            dplyr::pull(dplyr::all_of(col_name)),
          error=function(e) numeric(0)
        )
        if (length(v)==0||all(is.na(v))) 0 else v[[1]]
      }

      hood_r <- purrr::map_dbl(crime_order, ~safe_val(hood_df,"s_crime_rate",.x))
      avg_r  <- purrr::map_dbl(crime_order, ~safe_val(avg_df, "s_city_avg_rate",.x))

      max_val  <- max(c(hood_r, avg_r), na.rm=TRUE)
      axis_max <- if (max_val>0 && is.finite(max_val)) round(max_val/0.80, 3) else 1

      # Guaranteed-length dot sizes (NULL comparison gives logical(0) otherwise)
      dot_sizes <- if (!is.null(highlighted) && nzchar(highlighted) &&
                       highlighted %in% crime_order)
        ifelse(crime_order==highlighted, 14L, 7L)
      else
        rep(7L, length(crime_order))

      dot_colors <- purrr::map_chr(crime_order, ~unname(CRIME_COLOURS[.x] %||% "#134E7C"))
      hood_name  <- tryCatch(hood_df$neighbourhood_name[[1]], error=function(e) detail_id)

      p <- plotly::plot_ly() %>%
        plotly::add_trace(
          type="scatterpolar", mode="lines",
          r=c(avg_r,avg_r[[1]]), theta=c(crime_order,crime_order[[1]]),
          fill="toself", name="City average",
          line=list(color="#e78ac3",width=2), fillcolor="rgba(231,138,195,0.18)",
          hovertemplate="%{theta}<br>City avg score: %{r:.2f}<extra></extra>"
        ) %>%
        plotly::add_trace(
          type="scatterpolar", mode="lines+markers",
          r=c(hood_r,hood_r[[1]]), theta=c(crime_order,crime_order[[1]]),
          fill="toself", name=hood_name,
          line=list(color="#134E7C",width=2.5), fillcolor="rgba(19,78,124,0.22)",
          hovertemplate="%{theta}<br>Score: %{r:.2f}<extra></extra>",
          marker=list(color=c(dot_colors,dot_colors[[1]]),
                      size =c(dot_sizes, dot_sizes[[1]]),
                      line=list(color="#fff",width=1.5))
        )

      if (!is.null(highlighted) && nzchar(highlighted) && highlighted %in% crime_order) {
        hi <- which(crime_order==highlighted)
        hc <- unname(CRIME_COLOURS[highlighted] %||% "#134E7C")
        p  <- p %>% plotly::add_trace(
          type="scatterpolar", mode="markers",
          r=hood_r[hi], theta=highlighted,
          marker=list(color=hc, size=16, line=list(color="#fff",width=2.5)),
          showlegend=FALSE, name=""
        )
      }

      p %>% plotly::layout(
        polar=list(
          bgcolor="rgba(0,0,0,0)",
          radialaxis=list(
            showline=FALSE, showgrid=FALSE, showticklabels=FALSE, range=c(0,axis_max)
          ),
          angularaxis=list(
            direction="clockwise",
            tickfont=list(size=12),
            showline=FALSE,              # no outer circle
            showgrid=TRUE,               # show axis spokes
            gridcolor="rgba(180,180,180,0.35)",
            griddash="solid"
          )
        ),
        showlegend=TRUE,
        legend=list(orientation="h", x=0.5, xanchor="center", y=-0.08,
                    font=list(size=11)),
        margin=list(l=70,r=70,t=20,b=10),
        plot_bgcolor="rgba(0,0,0,0)", paper_bgcolor="rgba(0,0,0,0)"
      )
    })

    output$heatmap <- renderPlotly({
      req(app_state$detail_hood)
      crime_sel <- input$heatmap_crime %||% app_state$crime_type
      req(crime_sel)
      heat_df <- app_data$heatmap_neigh %>%
        dplyr::filter(HOOD_158==app_state$detail_hood, CSI_CATEGORY==crime_sel) %>%
        dplyr::mutate(month=factor(month,levels=month.name,ordered=TRUE))
      req(nrow(heat_df)>0)
      col <- unname(CRIME_COLOURS[crime_sel] %||% "#134E7C")
      plotly::plot_ly(data=heat_df,x=~year,y=~month,z=~crime_count,
        type="heatmap", colorscale=list(c(0,"#f7f7f7"),c(1,col)),
        hovertemplate=paste0("<b>",crime_sel,"</b><br>Year:%{x}<br>",
                             "Month:%{y}<br>Count:%{z}<extra></extra>")
      ) %>% plotly::layout(
        xaxis=list(title="Year",dtick=1,tickformat="d"),
        yaxis=list(title="",categoryorder="array",categoryarray=rev(month.name)),
        margin=list(l=80,r=20,t=10,b=40),
        plot_bgcolor="rgba(0,0,0,0)", paper_bgcolor="rgba(0,0,0,0)"
      )
    })

    observeEvent(input$open_compare, { setters$set_compare_modal_open(TRUE) })
    observeEvent(input$close_panel, {
      setters$set_is_playing(FALSE); setters$set_detail_hood(NULL)
      setters$set_hood_panel_open(FALSE)
    })
  })
}