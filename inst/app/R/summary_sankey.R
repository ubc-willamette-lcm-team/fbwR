
summary_sankey <- function(fbwR_summary, sankeycols, 
  # Users can pre-select months  
  months = c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May",
    "Jun", "Jul", "Aug")) {
  # fbwR_summary: summary_by_month object created by summarizeFBW
  # months <- c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May",
  #   "Jun", "Jul", "Aug")
  
  ### FLOW SANKEY
  ### First, create a Sankey of average outflow
  fbwR_summary_flow <- fbwR_summary %>%
    filter(Month %in% months) %>%
    select(starts_with("q"))

  nodelist_flow <- list(
    label = factor(c(
      "Outflow",
      # "In reservoir\n(1-DPE)", "Pass",    # DPE
      "FPS", "Powerhouse", "RO", "Spillway"    # Outlets
      # "Survive", "Die"     # Survive
    )),
    color = c(sankeycols$approach,
      # sankeycols$remain, sankeycols$pass,
      sankeycols$fps, sankeycols$turb, sankeycols$ro, sankeycols$spill),
    pad = 15,
    thickness = 20,
    line = list(
      color = c(sankeycols$approach,
      # sankeycols$remain, sankeycols$pass,
      sankeycols$fps, sankeycols$ro, sankeycols$spill, sankeycols$turb),
      # sankeycols$surv, sankeycols$die),
      width = 0.5
    ),
    pad = 1,
    y = c(0.001, 0.001, 0.33, 0.66, 0.999)
  )

  linklist_flow <- list(
    source = c(
      rep(c(0, 0, 0, 0), length(months))
    ),
    target = c(
      rep(c(1, 2, 3, 4), each = length(months))),
    value = unlist(fbwR_summary_flow), 
      # Jan, then Feb
      # fbwR_summary[x, ]),
    color = c(
      # rep(alpha(sankeycols$remain, 0.5), 12),
      # alpha(sankeycols$pass, 0.5),
      rep(alpha(sankeycols$fps, 0.5), length(months)), 
      rep(alpha(sankeycols$turb, 0.5), length(months)),
      rep(alpha(sankeycols$ro, 0.5), length(months)), 
      rep(alpha(sankeycols$spill, 0.5), length(months)) 
    ),
    label = unlist(lapply(1:4, FUN = function(i) {
      paste0(
        # "Month: ", months, "<br>Outflow: ",
        sapply(1:length(months), FUN = function(x) {
        months[x]}), 
      sep = "")}))
    # hovertemplate = 'Month: %{customdata}'
  )

  fig_flow <- plot_ly(
    type = "sankey",
    orientation = "h",
    arrangement = "snap",
    domain = list(
        x =  c(0,0.5),
        y =  c(0,1)
      ),
    hoverinfo = "+",
    valueformat = ".0f",
    valuesuffix = " cfs.",
    node = nodelist_flow,
    link = linklist_flow
    ) %>% layout(
    title = "Flow distribution",
    font = list(
      size = 14
    ),
    margin = list(t = 50) #, l = 50)
    # automargin = TRUE
  )
 
  ### SURVIVAL PLOT
  if (TRUE) {
    fbwR_summary_surv <- fbwR_summary %>%
      filter(Month %in% months) %>%
      select(ends_with("_pop") | starts_with("avgDPS"))
    
    nodelist_surv <- list(
      label = c(
        "Approaching",
        "In reservoir\n(1-DPE)", "Pass",    # DPE
        "FPS", "Powerhouse", "RO", "Spillway",    # Outlets
        "Survive", "Die"     # Survive
      ),
      color = c(sankeycols$approach,
        sankeycols$remain, 
        sankeycols$pass,
        sankeycols$fps, sankeycols$turb, sankeycols$ro, sankeycols$spill,
        sankeycols$surv, sankeycols$die),
      pad = 15,
      thickness = 20,
      line = list(
        color = c(sankeycols$approach,
        sankeycols$remain, 
        sankeycols$pass,
        sankeycols$fps, sankeycols$ro, sankeycols$spill, sankeycols$turb,
        sankeycols$surv, sankeycols$die),
        width = 0.5
      ),
      pad = 1,
      x = c(0.001, 0.33, 0.33, rep(NA, 6)),
      y = c(
        0.001, 
        0.001, NA,
        0.001, NA, NA, NA, 
        0.001, NA)
    )

    linklist_surv <- list(
      source = c(
        # First link: approaching to reservoir
        rep(0, length(months)),
        # Approaching to passing
        rep(0, length(months)),
        # Passing to FPS, then PH, then RO, then spill
        rep(2, length(months)),
        rep(2, length(months)),
        rep(2, length(months)),
        rep(2, length(months)),
        # DPS: FPS, then PH, then RO, then spill
        rep(3, length(months)),
        rep(4, length(months)),
        rep(5, length(months)),
        rep(6, length(months)),
        # 1-DPS: FPS, then PH, then RO, then spill
        rep(3, length(months)),
        rep(4, length(months)),
        rep(5, length(months)),
        rep(6, length(months))
      ),
      target = c(
        # First link: approaching to reservoir
        rep(1, length(months)),
        # Approaching to passing
        rep(2, length(months)),
        # Passing to FPS, then PH, then RO, then spill
        rep(3, length(months)),
        rep(4, length(months)),
        rep(5, length(months)),
        rep(6, length(months)),
        # DPS
        rep(7, length(months)),
        rep(7, length(months)),
        rep(7, length(months)),
        rep(7, length(months)),
        # 1-DPS; 
        rep(8, length(months)),
        rep(8, length(months)),
        rep(8, length(months)),
        rep(8, length(months))
      ),
      value = c(
        unlist(fbwR_summary_surv %>%
          dplyr::rowwise() %>%
          mutate(meanPassing_pop = 
            meanFPS_pop + meanTurb_pop + meanRO_pop + meanSpill_pop) %>%
          relocate(meanPassing_pop, .after = meanForebay_pop)
        ),
        unlist(fbwR_summary_surv %>%
          dplyr::rowwise() %>%
          mutate(
            avgDPM_FPS = meanFPS_pop - ifelse(meanFPS_pop == 0, NA, avgDPS_FPS),
            avgDPM_PH = meanTurb_pop - ifelse(meanTurb_pop == 0, NA, avgDPS_PH),
            avgDPM_RO = meanRO_pop - ifelse(meanRO_pop == 0, NA, avgDPS_RO),
            avgDPM_Spill = meanSpill_pop - ifelse(meanSpill_pop == 0, NA, 
              avgDPS_Spill)
          ) %>% select(starts_with("avgDPM")))
      ),
        # Jan, then Feb
        # fbwR_summary[x, ]),
      color = c(
        rep(alpha(sankeycols$remain, 0.5), length(months)),
        rep(alpha(sankeycols$pass, 0.5), length(months)),
        rep(alpha(sankeycols$fps, 0.5), length(months)), 
        rep(alpha(sankeycols$turb, 0.5), length(months)),
        rep(alpha(sankeycols$ro, 0.5), length(months)), 
        rep(alpha(sankeycols$spill, 0.5), length(months)),
        rep(rep(alpha(sankeycols$surv, 0.5), 4), length(months)),
        rep(rep(alpha(sankeycols$die, 0.5), 4), length(months))
        # rep(alpha(sankeycols$die, 0.5), length(months))
      ),
      label = unlist(lapply(1:14, FUN = function(i) {
        paste0(
          # "Month: ", months, "<br>Outflow: ",
          sapply(1:length(months), FUN = function(x) {
          months[x]}), 
        sep = "")}))
      # hovertemplate = 'Month: %{customdata}'
    )
    
    fig_surv <- plot_ly(
      type = "sankey",
      orientation = "h",
      arrangement = "snap",
      domain = list(
          x =  c(0, 1),
          y =  c(0, 1)
        ),
      hoverinfo = "+",
      valueformat = ".4f",
      # valuesuffix = " cfs.",
      node = nodelist_surv,
      link = linklist_surv
      ) %>% layout(
      title = "Fish distribution & survival",
      font = list(
        size = 14
      ),
      margin = list(t = 50)
      # automargin = TRUE
    )
  }

  return(list(
    "plotly_flow" = fig_flow, 
    "plotly_surv" = fig_surv)
  )
}
