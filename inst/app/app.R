# App for reactive elements using navlist

library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(openxlsx)
library(rhandsontable)
library(shinycssloaders)
# shiny bootstrap
library(shinyBS)
# format data tables nicely
library(formattable)

### Install the FBW library
# devtools::install_git("https://github.com/mairindeith/fbwR")
# devtools::load_all("../fbwR")
# library(fbwR)
devtools::load_all("../../")

### Load custom and helper functions -------------------------------------------
source("R/utils.R")
source("R/summary_sankey.R")

### Set up the UI -------------------------------------------------------------- 

secondbox_style <- "secondary"
infobox_style <- "info"
parambox_style <- "primary"
warningbox_style <- "warning"
backnext_style <- "btn-secondary"

formattable_fill <- colorspace::lighten("#2c3e50", 0.40)
formattable_fill_highlight <- colorspace::lighten("#3498db", 0.1)

# Colors for the Sankey diagram
sankeycols <- list(
  "approach" = "#3d3d3d",
  "remain" = "#912236", 
  "pass" = "#1b9e77", 
  "fps" = "#bcbd22",
  "ro" = "#17becf",
  "spill" = "#287ebc",
  "turb" = "#ff7f0e",
  "surv" = "#335366",
  "die" = "#aaaaaa"
)

ui <- navbarPage(
  title = "fbwR", 
  id = "fbw_navbar",
  selected = "ressim",
  position = "fixed-top",
  collapsible = TRUE,
  fluid = TRUE,
  lang = "en",
  windowTitle = "fbwR Shiny",
  # Some HTML to allow buttons to float on the page
  header = tags$style(type="text/css", "body {padding-top: 100px;}"),
  theme = shinytheme("flatly"),
  tabPanel(value = "app_howto",
    title = "How to use this app", icon = icon("question"),
    fluidRow(
      tags$style(
        HTML(floating_buttons_html)
      ),
    # column(1, )
    column(8, offset = 2, 
      includeHTML("about.html")
      # tags$iframe("about.html")
      ),
    column(1, offset = 1,
      div(class = "floating-element-r", 
      actionButton(inputId = "about_next", label = "Next", 
        icon = icon("arrow-right"), class = backnext_style)))
    )),
    # br(),

  navbarMenu("Hydrological inputs", icon = icon("water"),
    tabPanel(value = "ressim", 
      title = "Upload ResSim and water-year-type data",
      fluidRow(
        tags$style(HTML(floating_buttons_html)),
        column(1, 
          div(class = "floating-element-l",
            actionButton(inputId = "ressim_back", 
            label = HTML("Go to the<br>'Help' page"), 
              icon = icon("arrow-left"), class = backnext_style),
          )
        ),
        column(10, # offset = 1, 
        bsCollapse(id = "ressim_bs", 
          multiple = TRUE, 
          open = "ressim_upload",

          bsCollapsePanel(value = "ressim_about", 
            title = "Click here for guidance on using this tab",
            style = infobox_style,
            includeHTML("guide_hydrological.html")
          ),
          bsCollapsePanel(value = "ressim_upload",
            title = "Upload ResSim and water-year-type data",
            style = parambox_style,
            fluidRow(
              column(6,
                h4("Upload ResSim data"),
                p("The ResSim hydrological model predicts pool elevation and flow through a dam's outlets for each day in a period of record. See the help panel above for more details."),
                tipify(
                  fileInput(inputId = "ressim_input", label = "Upload ResSim file (only .xlsx/.xlsm files accepted)",
                    accept = c(".xlsx", ".xlsm"), multiple = FALSE,
                    width = "100%", buttonLabel = "Select file..."),
                  title = "File must be an Excel (.xlsx or .xlsm) with multiple sheets, one sheet per hydrological variable (one for pool elevation, and sheets containing outflow through each dam outlet).",
                  options = list(container = "body")),
                p("After uploading, the panel below will expand and you will be asked to confirm which ResSim sheets represent what kinds of hydrological data. Then, after compiling, you can review the compiled ResSim dataframe.")
              ),
              column(6,
                bsCollapse(id = "wyt_collapse_panel",
                  bsCollapsePanel(
                    title = "(If the ResSim data do not include water year types...)\nUpload water year type data",
                    fluidRow(
                      column(12,
                        p("If the ResSim file does not include temperature distribution data, you will also have to upload a file of water year types for the period of record. If left blank, temperature splits will not be simulated."),
                        # tipify(
                        fileInput(inputId = "wyt_input", label = "Upload water year types (only .csv files accepted)",
                          accept = c(".csv"), multiple = FALSE,
                          buttonLabel = "Select file...", width = "100%"),
                          # title = "File must be a .csv file with only two columns: years and corresponding water year types",
                          # options = list(container = "body")),
                        column(6,
                          # helpText("If there are any extra rows in the .csv file (e.g., for metadata), you can skip them here")
                          # tipify(
                          numericInput(inputId = "wyt_skiprows", label = "Skip the first ___ rows of the .csv file", 
                            value = 0, min = 0, step = 1)
                            # title = "There may be extra rows in the .csv file (e.g., containing metadata about the file). You can skip them while inputting the file."
                          # bsTooltip(id = "wyt_skiprows", title = "There mayIf there are extra rows above the header row for the data, they can be ignored while reading in the file.",
                            # placement = "right", trigger = "hover", list(container = "body")),
                        ),
                        column(6,
                          checkboxInput(inputId = "wyt_header", label = "Check this box if the first row of the .csv is a row of column names",
                            value = TRUE)
                        )
                      )
                    ),
                  style = infobox_style)
                )
              )
            )
          ),

          bsCollapsePanel(value = "ressim_compile", 
            title = "Compile ResSim sheets",
            style = parambox_style,
            uiOutput(outputId = "ressim_sheetselect_ui") %>% 
              withSpinner()
          )

          # bsCollapsePanel(value = "ressim_preview_panel",
          #   title = "Preview compiled ResSim data",
          #   style = parambox_style,
          #   span(textOutput(outputId = "wyt_warning"), 
          #     style = "color:orange; font-size:20px; font-weight:700"),
          #   dataTableOutput(outputId = "ressim_preview") %>% 
          #     withSpinner()
          # )
        )
      ), 
      column(1, 
        div(class = "floating-element-r", 
        # tipify(
          actionButton(inputId = "ressim_next", 
          label = HTML("Next<br>(compiles<br>ResSim)"), 
          icon = icon("arrow-right"), class = backnext_style)
          # title = "Before clicking 'Next', be sure you have uploaded ResSim data!"
          # )
        )
      )
      )
    ),

    tabPanel(value = "ressim_prev", title = "Preview hydrological data",
      fluidRow(
        tags$style(HTML(floating_buttons_html)),
        column(1, 
          div(class = "floating-element-l", 
            actionButton(inputId = "ressim_prev_back", label = "Back", 
              icon = icon("arrow-left"), class = backnext_style))
        ),
        column(10, # offset = 1,
        bsCollapse(multiple = TRUE, open = c("ressimpicker_bs", "ressimplot_bs"),
          bsCollapsePanel(value = "ressim_preview_panel",
            title = "View compiled ResSim data in long/table format",
            style = parambox_style,
            span(textOutput(outputId = "wyt_warning"), 
              style = "color:orange; font-size:20px; font-weight:700"),
            DT::DTOutput(outputId = "ressim_preview") %>% 
              withSpinner()
          ),
          bsCollapsePanel(value = "ressimpicker_bs", 
            title = "Select ResSim plotting options",
            uiOutput(outputId = "ressim_plotselect_ui") %>% 
                withSpinner(),
            style = parambox_style
          ),
          bsCollapsePanel(value = "ressimplot_bs", 
            title = "Plotted ResSim data",
            helpText("If the loading spinner is stuck 'on' here, or if you see an error message, check that you compiled the ResSim datasheets in the previous tab"),
            uiOutput(outputId = "ressim_plots") %>% 
                  withSpinner(),
            style = infobox_style
          )
        )),
        column(1,
        div(class = "floating-element-r", 
          actionButton(inputId = "ressim_prev_next", label = "Next", 
            icon = icon("arrow-right"), class = backnext_style))
        )
      )
    )
  ),

  navbarMenu("Fish passage parameters", 
    icon = icon("house-flood-water-circle-arrow-right"),
    tabPanel(value = "fbwtemplate", 
      title = "Upload parameters from a template",
      fluidRow(
        tags$style(HTML(floating_buttons_html)),
        column(1, 
          div(class = "floating-element-l",
            actionButton(inputId = "template_back", label = "Back", 
              icon = icon("arrow-left"), class = backnext_style)),
          ),
        column(10, # offset = 1, 
          h2("There are two ways to enter fish passage parameters in to this app:"),
          h4("1: Enter parameters manually (click 'Next' to begin entering parameters by hand. You can return to this page later to save your parameter inputs in template format)"), 
          # using a template Excel file (.xlsx) and uploading the template on this page."),
          h4("2: Use a parameter template (use this page)"),
          hr(),
          # h4("Below, you can download a blank parameter template, upload a previously filled-in template, and save your current parameters in template format (so you can return to this tab and upload files in a future model run."),
          fluidRow(
            column(6,
            bsCollapse(id = "template_files_bs",
              open = "template_bs_panel",
              multiple = TRUE,
              bsCollapsePanel(
                value = "template_bs_panel",
                title = "Upload and download template files",
                downloadButton("download_template",
                  label = "Click here to download a blank fbwR parameter template file", 
                  icon = icon("download"), class = "btn-info btn-block", width = "100%"),
                hr(),
                fileInput(inputId = "upload_parambutton", width = "100%",
                  label = "If you have already filled in a parameter template or have one saved from a previous session, upload it here."),
                style = parambox_style
              ))
            ),
            column(6,
              bsCollapse(id = "template_files_bs",
                open = "save_template", multiple = TRUE,
                bsCollapsePanel(value = "save_template",
                title = "Save your parameters as a template for later use",
                downloadButton("download_parameters",
                  label = "Click here to save your current parameter set", 
                  icon = icon("save"), class = "btn-info btn-block", width = "100%"),
                style = parambox_style
              )
            )
          )
        )
      ),
      column(1,
        div(class = "floating-element-r",
        actionButton(inputId = "template_next", label = "Next", 
          icon = icon("arrow-right"), class = backnext_style))
        )
    )),
    # "or define parameters manually",
    
    "----",
    "Or upload manually:",

    tabPanel(title = "Step 1. Dam passage/outlet settings", 
      value = "params_outlets",
      fluidRow(
        tags$style(HTML(floating_buttons_html)),
        column(1, 
          div(class = "floating-element-l", 
            actionButton(inputId = "outlets_back", label = "Back", 
              icon = icon("arrow-left"), class = backnext_style))
        ), 
        column(10, # offset = 1,
          bsCollapse(id = "params_outlets_BS", multiple = TRUE, 
            open = c("fps_inputs", "outlet_inputs", "routeeff_inputs"),
            bsCollapsePanel(value = "fps_inputs", 
              title = "Describe the fish passage structure (if present)",
              p("Here, define parameters that define if there is a fish passage structure (FPS) present, what kind, what elevations it can operate at, and (if the FPS is a weir), the dates of operation."),
              column(6, 
                uiOutput("input.in_collector"),
                uiOutput("input.fps_weir"),
                hr(),
                uiOutput("input.in_nets")
              ),
              column(6, 
                # uiOutput("input.fps_elevations")
              ),
            style = parambox_style),
            #  
            bsCollapsePanel(title = "Outlet rules",
              value = "outlet_inputs",
              # fluidRow(
                # column(12,
              p("Here, input route-specific operating rules. For each outlet (regulating outlet, turbines, spillway, and fish passage structure (FPS)), input any operating rules like minimum/maximum pool elevation for operation, minimum/maximum/target flow rates, how many gates exist in the outlet, and how flow should be distributed between those gates ('Gate method', which has to be blank or one of 'Target Q', 'Equal Q', 'Unit to Max Q', 'Min Q to equal', or 'Peaking performance').\n\nIf the gate method disappears after you write it, check that you input a valid gate method from this list."),
              fluidRow(
                column(12,
                  rHandsontableOutput(outputId = "routerules_hot")
                    # withSpinner()
                )
              ),
              style = parambox_style
            ),
            bsCollapsePanel(title = "Outlet attractiveness (aka. route effectiveness)",
              value = "routeeff_inputs",
                p("Here, input outlet attractiveness (aka. route effectiveness, R.E.). For each flow ratio (i.e., Q-ratios, ranging from 0-1), input the route effectiveness multiplier for each outlet. The Q-ratio is the proportion of flow through an outlet (e.g., if 700 cfs passes through an outlet, of a total 7000 cfs, the Q-ratio is 0.1). Route effectiveness is the proportion of fish attracted to an outlet divided by the Q-ratio. Values greater than 1 reflect outlets with high attractiveness at that Q-ratio, values close to 0 reflect unattractive outlets."),
                  # br(),
                fluidRow(
                  column(11,
                    # align = "center",
                  p("Greyed out text cannot be modified. The tables are also color-coded based on the numbers in the cells of each column. Yellow cells are those with the highest R.E. in the column, dark blue indicates low R.E."),
                    # p(),
                    br(),
                    rHandsontableOutput(outputId = "routeeff_hot")
                  )
                ),
                style = parambox_style
            )
          )
        ),
        column(1,
          div(class = "floating-element-r", 
          actionButton(inputId = "outlets_next", label = "Next", 
            icon = icon("arrow-right"), class = backnext_style))
        )
      )
    ),

    tabPanel(title = "Step 2. Monthly run timing and dam passage efficiency",
      value = "params_dpe",
      fluidRow(
        column(1, 
          div(class = "floating-element-l", 
            actionButton(inputId = "dpe_back", label = "Back", 
              icon = icon("arrow-left"), class = backnext_style))
        ), 
        column(10, # offset = 1,
          bsCollapse(id = "params_outlets_BS", multiple = TRUE, 
            open = c("dpe_inputs", "runtiming_inputs"),
                        bsCollapsePanel(value = "runtiming_inputs", 
              title = "Fish run timing information",
              fluidRow(
                column(6,
                  bsCollapse(open = "Monthly run timing",
                    bsCollapsePanel("Monthly run timing", 
                      uiOutput("input.in_fps_alternative"),
                      strong("What proportion of the annual fish population attempts to pass the dam in each month?"),
                      rHandsontableOutput(outputId = "monthly_runtiming"),
                      style = parambox_style
                    )
                  )
                ),
                column(6,
                  h3("Route fish with flow?"),
                  uiOutput("input.fish_with_flow")
                )
                # column(11, offset = 1,
                # )
              ),
              style = parambox_style
            ),
            bsCollapsePanel(value = "dpe_inputs", 
              title = "Dam passage efficiency",
              fluidRow(
                column(6, offset = 1,
                  uiOutput("input.dpe_colselector"),
                  uiOutput("input.fps_elevations"),
                  htmlOutput("fps_elev_warning"),
                  rHandsontableOutput(outputId = "dpe_hot"),
                  br(),
                  p(),
                  fluidRow(column(5,
                    actionButton(inputId = "dpe_addrow", icon = icon("plus"),
                      label = "Add row", width = "100%",
                      class = "btn-secondary btn-block"),
                    br()
                  ),
                  column(1),
                  column(5,
                    actionButton(inputId = "dpe_rmrow", icon = icon("minus"),
                      label = "Remove row", width = "100%",
                      class = "btn-secondary btn-block"),
                    br()
                  )
                )
                ),
                column(5,
                  plotlyOutput(outputId = "dpe_plotly", width = "100%", inline = TRUE)
                ),
              ),
              style = parambox_style
            )
          ),
        ),
        column(1,
          div(class = "floating-element-r",
          actionButton(inputId = "dpe_next", label = "Next", 
            icon = icon("arrow-right"), class = backnext_style)
          )
        )
      )
    ),

    tabPanel(title = "Step 3. Passage survival",
      value = "params_survival",
      fluidRow(
        tags$style(HTML(floating_buttons_html)),
        column(1, 
          div(class = "floating-element-l", 
            actionButton(inputId = "surv_back", label = "Back", 
              icon = icon("arrow-left"), class = backnext_style))
        ),
        column(10, # offset = 1,
          h3("Define survival for each outlet by clicking each box below"),
          bsCollapse(id = "params_outlets_BS", multiple = TRUE, 
            # Start the page with all tabs open
            open = c("ro_surv_bs", "turb_surv_bs", "spill_surv_bs",
              "fps_surv_bs"), 
            # "Survival through the regulating outlet"),
            bsCollapsePanel(value = "ro_surv_bs", 
              title = "Regulating outlet",
              # Input the type first, then generate the UI based on the type
              fluidRow(column(6,
                uiOutput("input.rosurv_type"),
                uiOutput("input.ro_surv")
              ),
              column(6,
                textOutput(outputId = "rosurv_plot_text"),
                plotlyOutput(outputId = "rosurv_plot")
              )),
              style = parambox_style),

            bsCollapsePanel(value = "turb_surv_bs",
              title = "Powerhouse turbines",
              fluidRow(column(6,
                # Input the type first, then generate the UI based on the type 
                uiOutput("input.turbsurv_type"),
                uiOutput("input.turb_surv")
                ),
                column(6,
                  textOutput(outputId = "turbsurv_plot_text"),
                  plotlyOutput(outputId = "turbsurv_plot")
                )
              ), style = parambox_style),
            
            bsCollapsePanel(value = "spill_surv_bs", 
              title = "Spillway",
              fluidRow(column(6,
                uiOutput("input.spillsurv_type"),
                uiOutput("input.spill_surv")
              ), column(6,
                textOutput(outputId = "spillsurv_plot_text"),
                plotlyOutput(outputId = "spillsurv_plot")
              )
            ), style = parambox_style),
            
            bsCollapsePanel(value = "fps_surv_bs", 
              title = "Fish passage structure",
              fluidRow(column(6,
                uiOutput("input.fpssurv_type"),
                uiOutput("input.fps_surv"),
              ),
              column(6,
                textOutput(outputId = "fpssurv_plot_text"),
                plotlyOutput(outputId = "fpssurv_plot")
              )),
              style = parambox_style)
          )
        ),
      column(1, 
        div(class = "floating-element-r",
        actionButton(inputId = "surv_next", label = "Next", 
          icon = icon("arrow-right"), class = backnext_style))
        )
    )
  )
  ),
  # tabP("Run fbwR", 
    tabPanel(value = "run_fbw",
      title = "Run the fbwR model", 
      icon = icon("fish"),
      fluidRow(
        tags$style(HTML(floating_buttons_html)),
        column(1,
          div(class = "floating-element-l", 
            actionButton(inputId = "fbwrun_back", label = "Back",
              icon = icon("arrow-left"), class = backnext_style)
          )
        ), 
        column(10, # offset = 1,
          column(6, offset = 3,
            htmlOutput(outputId = "runfbw_warnings"),
            br(),
            h4("If there are no errors above, the input check is complete. Run the fbwR model by clicking below. After the model runs, results will appear below."),
            actionButton("run_fbw_button",
              label = "Run fbwR", 
              icon = icon("fish"), class = "btn-warning btn-lrg btn-block", width = "100%")
          ),
          column(12,
            br(),
            bsCollapse(id = "fbw_results_bs", 
              multiple = TRUE,
              bsCollapsePanel(value = "summary_res", 
                title = "Results summarized by month/outlet",
                downloadButton("download_monthly_results", 
                  class = "btn-info btn-block",
                  "Download monthly results as a .csv (the two tables below will be combined into a single table)"),
                h4("Average monthly flow (cfs)"),
                formattableOutput("fbw_res_summary_monthly_flow", width = "100%") %>%
                  withSpinner(),
                plotlyOutput("summary_sankey_flow", width = "100%"),
                h4("Average monthly fish distribution and survival"),
                formattableOutput("fbw_res_summary_monthly_surv", width = "100%") %>%
                  withSpinner(),
                plotlyOutput("summary_sankey_surv", width = "100%"),
                style = parambox_style
              ),
              bsCollapsePanel(value = "wyt_res", 
                title = "Average annual passage survival, summarized by water year type",
                downloadButton("download_wyt_results",
                  class = "btn-info btn-block",
                  "Download water year type results as a .csv"),
                br(), 
                formattableOutput("fbw_res_summary_wyt", width = "100%") %>%
                  withSpinner(),
                style = parambox_style),
              bsCollapsePanel(value = "full_res",
                title = "Unsummarized FBW results (daily for each day in the period of record)",
                downloadButton("download_full_results", 
                  class = "btn-info btn-block",
                  "Download raw results as a .csv"),
                br(), 
                DT::DTOutput("fbw_res_full", width = "100%"),
                style = infobox_style
              )
            )
          )
      )
    )
  )
)

### SERVER ---------------------------------------------------------------------
server <- shinyServer(function(input, output, session) {
  ### Global reactive parameter lists
  # Increase maximum upload file size
  options(shiny.maxRequestSize=30*1024^2)

  ressim_sheets_raw <- reactive({
    readxl::excel_sheets(input$ressim_input$datapath)
  # Bind to the input being read in
  }) %>% bindEvent(input$ressim_input)
  
  ressim_dataframe <- reactive({
    wide <- input$ressim_wide
    ressim <- data.frame()
    # Modified from loadResSim
    if (!is.null(input$ressim_input$datapath)) {
      ressim <- fbwR::loadResSim(infile = input$ressim_input$datapath,
        wide = input$ressim_wide,
        elevsheet = input$ressim_pool,
        outflowsheet = input$ressim_outflow,
        powerhousesheet = input$ressim_phflow,
        rosheet = input$ressim_roflow,
        spillsheet = input$ressim_spillflow
        )
      # New features: 
      # Load water year type and temp. dist
      if (!is.null(input$wyt_input$datapath)) {
        wyt <- read.csv(file = input$wyt_input$datapath, header = input$wyt_header,
          skip = input$wyt_skiprows)
        wyt <- wyt[, 1:2]
        colnames(wyt) <- c("Year", "WaterYearType")
        ressim <- ressim %>%
          mutate(Year = lubridate::year(Date)) %>%
          left_join(x = ., y = wyt, by = "Year") %>%
          select(!Year)
      } else {
        if (!is.na(input$ressim_tempsplit)) {
          tempsplit <- suppressMessages(
            readxl::read_excel(input$ressim_input$datapath, 
              sheet = input$ressim_tempsplit))
          # wow this is a lot of work to modify column names
          year_types <- sapply(colnames(tempsplit)[-1], function(X) {
            # Remove the "X"
            substring(X, first = 1,
              last = regexpr(X, pattern = "...", fixed = TRUE)[1] - 1)})
          # There will be some extra columns which begin with ".." now, remove these
          #   after searching with regular expressions via grep()
          year_types <- year_types[-which(year_types == "")]
          names(year_types) <- as.numeric(tempsplit[6, 2:(1 + length(year_types))])
          wyt <- data.frame(
            year = as.numeric(names(year_types)),
            type = unlist(year_types)
          )
          colnames(wyt) <- c("Year", "WaterYearType")
          ressim <- ressim %>%
            mutate(Year = lubridate::year(Date)) %>%
            left_join(x = ., y = wyt, by = "Year") %>%
            select(!Year)
        } else {
          ressim <- ressim %>%
            mutate(WaterYearType = NA)
        }
      }
    ressim
    }
  # bindEvent links this to ressim_create so it only "activates" when the upload
  # is completed
  }) %>% bindEvent({
    input$ressim_create
    input$ressim_next
  })

  unique_wyt_list <- reactive({
    na.omit(unique(ressim_dataframe()$WaterYearType))
  })
  
  # Make a list of year plotting options to inform the UI defined below:
  unique_plotting_options <- reactive({
    if (length(na.omit(unique(ressim_dataframe()$WaterYearType))) == 0) {
      # If all-NA, ignore water year type plotting
      choicelist <- c("All Years", 
        unique(lubridate::year(ressim_dataframe()$Date)))
    } else {
      choicelist <- c("All Years", 
        paste0("Avg. of ", na.omit(factor(
          unique(ressim_dataframe()$WaterYearType)), 
          levels = c("ABUNDANT", "ADEQUATE", "DEFICIT", "INSUFFICIENT"))),
        unique(lubridate::year(ressim_dataframe()$Date)))
    }
    choicelist
  })

  ### Create a dataframe to plot ResSim uploaded data from
  ressim_dataframe_plotting_subset <- reactive({
    ressim_local <- isolate(ressim_dataframe())
    if (input$ressim_plot_select == "All Years") {
      lubridate::year(ressim_local$Date) <- 2000
      ressim_local %>%
        group_by(Date) %>%
        dplyr::summarize(
          elev = mean(elev, na.rm = TRUE),
          outflow_flow = mean(outflow_flow, na.rm = TRUE),
          turb_flow = mean(turb_flow, na.rm = TRUE),
          RO_flow = mean(RO_flow, na.rm = TRUE),
          spill_flow = mean(spill_flow, na.rm = TRUE)
        )
    } else if(input$ressim_plot_select %in% paste0("Avg. of ", unique_wyt_list())) {
      wyt_selected <- gsub(input$ressim_plot_select, pattern = "Avg. of ", 
        replacement = "")
      lubridate::year(ressim_local$Date) <- 2000
      ressim_local %>%
        filter(WaterYearType == wyt_selected) %>%
        group_by(Date) %>%
        dplyr::summarize(
          elev = mean(elev, na.rm = TRUE),
          outflow_flow = mean(outflow_flow, na.rm = TRUE),
          turb_flow = mean(turb_flow, na.rm = TRUE),
          RO_flow = mean(RO_flow, na.rm = TRUE),
          spill_flow = mean(spill_flow, na.rm = TRUE)
        )
    } else {
      ressim_local %>%
        filter(
          lubridate::year(Date) == as.numeric(input$ressim_plot_select)
        )
    }
  }) %>% bindEvent({
    input$ressim_create
    input$ressim_plot_select
  })

  ### PARAMETERS! Yay! ---------------------------------------------------------
  # This can be updated as we go
  param_list <- reactiveValues(
    alt_desc = list(
      scenario_name = "", 
      scenario_description = "", 
      fp_alternative = "N",
      nets = "N", 
      collector = "NONE",
      fps_max_elev = as.numeric(NA),
      rereg = "N",
      rereg_mortality = as.numeric(NA),
      fish_with_flow = "N",
      use_temp_dist = "N",
      dpe_column_name = "baseline_dpe",
      weir_start_date = as.Date(NA),
      weir_end_dat = as.Date(NA)
    ),
    route_specs = data.frame(      
      max_flow = as.numeric(rep(NA, 4)),
      bottom_elev = as.numeric(rep(NA, 4)),
      passage_surv_rate = as.character(rep(NA, 4)),
      gate_method = factor(rep(NA, 4), 
        levels = c(NA, "Equal Q", "Min Q to equal", "Unit to Max Q",
            "Target Q", "Peaking Performance")
        ),
      n_gates = as.numeric(rep(NA, 4)),
      min_flow = as.numeric(rep(NA, 4)),
      target_flow = as.numeric(rep(NA, 4)),
      normally_used = as.character(rep(NA, 4)),
      row.names = c("RO", "Turb", "Spill", "FPS")
    ),
    route_eff = data.frame(
      q_ratio = seq(0, 1, by = 0.1),
      Spill = as.numeric(c(0, rep(NA, 9), 1)),
      FPS = as.numeric(c(0, rep(NA, 9), 1)),
      RO = as.numeric(c(0, rep(NA, 9), 1)),
      Turb = as.numeric(c(0, rep(NA, 9), 1))
    ),
    route_dpe = data.frame(
      elev = as.numeric(c(0,10000)),
      elev_description = as.character(rep(NA, 2)),
      baseline_dpe = as.numeric(rep(NA, 2)),
      fss_dpe = as.numeric(rep(NA, 2)),
      fsc_dpe = as.numeric(rep(NA, 2)),
      weir_dpe = as.numeric(rep(NA, 2))
    ),
    monthly_runtiming = data.frame(
      Date = as.Date(paste0("2000-", c(9:12, 1:8), "-01")),
      approaching_baseline = as.numeric(rep(NA, 12)),
      approaching_alternative = as.numeric(rep(NA, 12))
    ),
    ro_surv_table = data.frame(
      flow = c(0,100),
      ro_surv_low = c(0,0.8),
      ro_surv_high = c(0,0.75)
    ),
    ro_elevs = data.frame(
      param = c("ro_lower_elev", "ro_upper_elev"),
      value = as.numeric(c(NA, NA))
    ),
    turb_surv_table = data.frame(
      flow = 0,
      turb_surv = 0
    ),
    spill_surv_table = data.frame(
      flow = 0,
      spill_surv = 0
    ),
    fps_surv_table = data.frame(
      flow = 0,
      fps_surv = 0
    ),
    temp_dist = data.frame(
      Date = as.Date(NA), 
      ABUNDANT = as.numeric(NA),
      ADEQUATE = as.numeric(NA),
      DEFICIT = as.numeric(NA),
      INSUFFICIENT = as.numeric(NA)
    ),
    water_year_types = data.frame(
      year = as.numeric(NA), 
      type = factor(NA, levels = c(NA, "ABUNDANT","ADEQUATE",
      "INSUFFICIENT", "DEFICIENT"))
    )
  )

  # Reactive elements: template reading in/out
  # Save a blank template
  output$download_template <- downloadHandler(
    filename = "fbwR_blankParameterTemplate.xlsx",
    content = function(file) {
      file.copy("www/fbwR_blankParameterTemplate_revised.xlsx", file)
    }
  )

  observeEvent(input$upload_parambutton, {
    param_list_in <- tryCatch(
      fbwR::loadFromTemplate(input$upload_parambutton$datapath),
      error = function(e) return(
        paste0(e, "; fix template file and try again"))
    )
    param_list$alt_desc <- param_list_in$alt_desc
    param_list$route_specs <- param_list_in$route_specs
    param_list$route_eff <- param_list_in$route_eff
    param_list$route_dpe <- param_list_in$route_dpe
    param_list$monthly_runtiming <- param_list_in$monthly_runtiming
    param_list$ro_surv_table <- param_list_in$ro_surv_table
    param_list$ro_elevs <- param_list_in$ro_elevs
    param_list$turb_surv_table <- param_list_in$turb_surv_table
    param_list$spill_surv_table <- param_list_in$spill_surv_table
    param_list$fps_surv_table <- param_list_in$fps_surv_table
    param_list$temp_dist <- param_list_in$temp_dist
    param_list$water_year_types <- param_list_in$water_year_types
    #  [1] "alt_desc"          "route_specs"       "route_eff"        
    #  [4] "route_dpe"         "monthly_runtiming" "ro_surv_table"
    #  [7] "ro_elevs"          "turb_surv_table"   "spill_surv_table"
    # [10] "fps_surv_table"    "temp_dist"         "water_year_types"
  })

  wb_rct <- reactive({
    wb <- openxlsx::loadWorkbook("www/fbwR_blankParameterTemplate_revised.xlsx")
    
    # Alt desc
    alt_desc_df <- data.frame(param_list$alt_desc)
    alt_desc_df$parameter_name <- rownames(alt_desc_df)
    alt_desc_df$value <- alt_desc_df[,1]
    alt_desc_df <- alt_desc_df %>%
      mutate(param_sorting = case_when(
        parameter_name == "scenario_name" ~ 1, 
        parameter_name == "scenario_description" ~ 2, 
        parameter_name == "fp_alternative" ~ 3, 
        parameter_name == "nets" ~ 4,
        parameter_name == "collector" ~ 5,
        parameter_name == "fps_max_elev" ~ 6,
        parameter_name == "rereg" ~ 7,
        parameter_name == "rereg_mortality" ~ 8,
        parameter_name == "fish_with_flow" ~ 9,
        parameter_name == "use_temp_dist" ~ 10,
        parameter_name == "dpe_column_name" ~ 11,
        parameter_name == "weir_start_date" ~ 12,
        parameter_name == "weir_end_date" ~ 13,
        TRUE ~ 14
      )) %>%
      arrange(param_sorting) %>%
      select(c(parameter_name, value))
    
    # message("saved_workbook (alt data)")
    # openxlsx::saveWorkbook(wb, file = "fbw_savedtemplate.xlsx", overwrite = TRUE)

    # Write to the worksheet
    openxlsx::writeData(wb, sheet = "alt_description", startRow = 7, 
      colNames = TRUE, rowNames = FALSE, x = alt_desc_df)
    # Route specs
    route_out <- data.frame(
      parameter = colnames(param_list$route_specs),
      RO = (unlist(param_list$route_specs[which(
        rownames(param_list$route_specs) == "RO"), ])),
      Turb = (unlist(param_list$route_specs[which(
        rownames(param_list$route_specs) == "Turb"), ])),
      Spill = (unlist(param_list$route_specs[which(
        rownames(param_list$route_specs) == "Spill"), ])),
      FPS = (unlist(param_list$route_specs[which(
        rownames(param_list$route_specs) == "FPS"), ])),
      row.names = NULL
    )
    openxlsx::writeData(wb, sheet = "route_specifications", startRow = 8, 
      colNames = FALSE, rowNames = FALSE, x = route_out)
    
    # Route effectiveness
    openxlsx::writeData(wb, sheet = "route_effectiveness", startRow = 7, 
      colNames = TRUE, rowNames = FALSE, x = param_list$route_eff)
    # DPE
    openxlsx::writeData(wb, sheet = "route_dpe", startRow = 7, 
      colNames = TRUE, rowNames = FALSE, x = param_list$route_dpe)
    # Monthly run timing
    openxlsx::writeData(wb, sheet = "route_dpe", startRow = 7, 
      colNames = TRUE, rowNames = FALSE, x = param_list$monthly_runtiming)
    # Monthly run timing
    openxlsx::writeData(wb, sheet = "monthly_runtiming", startRow = 7, 
      colNames = TRUE, rowNames = FALSE, x = param_list$monthly_runtiming)
    # RO survival table
    openxlsx::writeData(wb, sheet = "ro_surv_table", startRow = 7, 
      colNames = TRUE, rowNames = FALSE, x = param_list$ro_surv_table)
    openxlsx::writeData(wb, sheet = "ro_elevs", startRow = 7, 
      colNames = TRUE, rowNames = FALSE, x = param_list$ro_elevs)
    # Turb surv table
    openxlsx::writeData(wb, sheet = "turb_surv_table", startRow = 7, 
      colNames = TRUE, rowNames = FALSE, x = param_list$turb_surv_table)
    # Spill surv table
    openxlsx::writeData(wb, sheet = "spill_surv_table", startRow = 7, 
      colNames = TRUE, rowNames = FALSE, x = param_list$spill_surv_table)
    # FPS surv table
    openxlsx::writeData(wb, sheet = "fps_surv_table", startRow = 7, 
      colNames = TRUE, rowNames = FALSE, x = param_list$fps_surv_table)
    # Temperature distribution data and WYT
    openxlsx::writeData(wb, sheet = "temp_dist", startRow = 7, 
      colNames = TRUE, rowNames = FALSE, x = param_list$temp_dist)
    openxlsx::writeData(wb, sheet = "water_year_types", startRow = 7, 
      colNames = TRUE, rowNames = FALSE, x = param_list$water_year_types)
    wb 
    # message("saved_workbook")
    # openxlsx::saveWorkbook(wb, file = "fbw_savedtemplate.xlsx", overwrite = TRUE)
  })

  output$download_parameters <- downloadHandler(
    filename = paste0("fbwR_Template_", format(Sys.Date(), "%y_%m_%d"), 
      ".xlsx"),
    content = function(file) {
      # wb_tmp <- openxlsx::createWorkbook()
      # addWorksheet(wb_tmp, "Cars")
      # x <- mtcars[1:5,]
      # writeData(wb_tmp, "Cars", x, startCol = 2, startRow = 3, rowNames = TRUE)
      saveWorkbook(wb_rct(), file = file, overwrite = TRUE)
    })

  ### Dynamic UI elements ------------------------------------------------------
  output$ressim_sheetselect_ui <- renderUI({
    fluidRow(
      column(12,
        # h4("Select which Excel sheets to compile from the uploaded file"),
        p("Suggested sheet names are auto-filled below. If you have ResSim results for more than one dam/project or if the naming convention of ResSim sheets has been changed, these auto-filled suggestions may be incorrect.")
      ),
      column(4,
        selectInput(inputId = "ressim_pool", label = "Pool elevation sheet",
          choices = ressim_sheets_raw(), multiple = FALSE,
          selected = ressim_sheets_raw()[grep(
            pattern = "-elev$", x = tolower(ressim_sheets_raw()))]),
        selectInput(inputId = "ressim_outflow", label = "Total outflow sheet",
          choices = ressim_sheets_raw(), multiple = FALSE,
                    selected = ressim_sheets_raw()[grep(
            pattern = "-out$", x = tolower(ressim_sheets_raw()))])),
      column(4,
        selectInput(inputId = "ressim_phflow", label = "Powerhouse outflow sheet",
          choices = ressim_sheets_raw(), multiple = FALSE,
          selected = ressim_sheets_raw()[grep(
            pattern = "-ph$", x = tolower(ressim_sheets_raw()))]),
        selectInput(inputId = "ressim_roflow", label = "Regulating outlet outflow sheet",
            choices = ressim_sheets_raw(), multiple = FALSE,
            selected = ressim_sheets_raw()[grep(
              pattern = "-ro$", x = tolower(ressim_sheets_raw()))])
      ),
      column(4,
        selectInput(inputId = "ressim_spillflow", label = "Spillway outflow sheet",
          choices = ressim_sheets_raw(), multiple = FALSE,
          selected = ressim_sheets_raw()[grep(
            pattern = "-spill$", x = tolower(ressim_sheets_raw()))]),
        selectInput(inputId = "ressim_tempsplit", label = "Temperature split sheet",
          choices = ressim_sheets_raw(), multiple = FALSE,
          selected = ressim_sheets_raw()[grep(
            pattern = "-split$", x = tolower(ressim_sheets_raw()))])
      ),
      column(3),
      column(6,
        p("Usually, ResSim inputs are in wide format (see the orange information panel above for more details). FBW will transform ResSim inputs from wide into long format unless this box is unchecked."),
        checkboxInput(inputId = "ressim_wide", label = "ResSim results in wide format", value = TRUE),
        actionButton(inputId = "ressim_create", 
          label = "Compile ResSim and water year type data",
          class = "btn-info btn-block btn-lrg", width = "100%")
      ),
      column(3)
    )
  }) %>% bindEvent(input$ressim_input, input$wyt_input)
  outputOptions(output, "ressim_sheetselect_ui", suspendWhenHidden = FALSE)
  
  # Water year type warning
  output$wyt_warning <- renderText({
    req(ressim_dataframe())
    if (is.null(input$wyt_input) & all(is.na(ressim_dataframe() %>% select(WaterYearType)))) {
      return("Warning! No file provided for water year type, filling all years with 'NA'")
    } else {
      return("")
    }
  }) %>% bindEvent({
    input$ressim_create
    # input$ressim_next
    # ressim_dataframe()
  })

  output$ressim_preview <- DT::renderDT({
    # req(ressim_dataframe())
    if (is.null(input$ressim_input$datapath)) {
      df <- data.frame(
        "Error" = "Must provide a valid file path for at least a ResSim file"
      )
    } else {
    df <- data.frame(ressim_dataframe() %>%
    rename(
      # Make look nice
      `Pool Elevation (f)` = elev,
      `Total outflow (cfs)` = outflow_flow,
      `Powerhouse outflow (cfs)` = turb_flow,
      `RO outflow (cfs)` = RO_flow,
      `Spillway outflow (cfs)` = spill_flow
    ) %>%
    mutate(
      Date = format(Date, "%d-%B-%Y"),
      `Pool Elevation (f)` = prettyNum(`Pool Elevation (f)`), # big.mark = ","),
      `Total outflow (cfs)` = prettyNum(`Total outflow (cfs)`), # big.mark = ","),
      `Powerhouse outflow (cfs)` = prettyNum(`Powerhouse outflow (cfs)`), #big.mark = ","),
      `RO outflow (cfs)` = prettyNum(`RO outflow (cfs)`), #big.mark = ","),
      `Spillway outflow (cfs)` = prettyNum(`Spillway outflow (cfs)`) #, big.mark = ","),
    )
    )
    }
    colnames(df) <- c("Date", "Pool Elevation (f)", "Total outflow (cfs)",
      "Powerhouse outflow (cfs)", "RO outflow (cfs)", "Spillway outflow (cfs)",
      "Water year type")
    datatable(df, rownames = FALSE,
      escape = FALSE,
      options = list(
        lengthMenu = list(c(7, 31, 365), c('7', '31', '365')),
        pageLength = 7))
  }) %>% bindEvent(
    input$ressim_create,
    input$ressim_next
  )

  output$ressim_plotselect_ui <- renderUI({
    # req()
    fluidRow(
      column(1),
      column(10,
        p("Plot ResSim projections for a single year in the period of record, or calculate a multi-year average. There are two options for calculating an average:"),
        p("1) you can average across all years in the period of record, or"),
        p("2) you average by water year type (if water year type data have been uploaded)"),
        selectInput(inputId = "ressim_plot_select",
          label = "What year(s) to show in plots?",
          # If all WYTs are NA, ignore this as a filter
          choices = unique_plotting_options()
        )
      )
    )
  }) %>% bindEvent({
    input$ressim_create
    input$ressim_next
  })
  outputOptions(output, "ressim_plotselect_ui", suspendWhenHidden = FALSE)

  ### Parameter inputs
  output$input.in_collector <- renderUI({
    selectInput("in_collector", 
      label = "Type of FPS (select 'None' to simulate no FPS)", 
      choices = c(
        "Floating surface collector (FSC)" = "FSC",
        "Floating screen structure (FSS)" = "FSS",
        "Fish weir" = "FISH WEIR",
        "Modified fish weir (FSO)" = "FSO",
        "None" = "NONE"),
      selected = param_list$alt_desc[["collector"]],
      width = "100%")
  }); outputOptions(output, "input.in_collector", suspendWhenHidden = FALSE)
  observeEvent(input$in_collector, {
    param_list$alt_desc[["collector"]] <- input$in_collector
  })

  output$input.in_nets <- renderUI({
    selectInput(inputId = "in_nets", 
      label = "Do exclusion nets block entry to the regulating outlet, powerhouse, and spillway?",
      selected = param_list$alt_desc[["nets"]],
      choices = list("Yes" = "Y", "No" = "N"),
      width = "100%")
  }); outputOptions(output, "input.in_nets", suspendWhenHidden = FALSE)
  observeEvent(input$in_nets, {
    param_list$alt_desc[["nets"]] <- input$in_nets
  })

  output$input.fps_weir <- renderUI({
    if (input$in_collector == "FISH WEIR") {
      fluidRow(
        column(12,
          helpText("The fish weir operates in each year between a set date range. Set the beginning and end date range here. Even though only the current year is shown here, FBW will active the weir between these dates for ALL years in the period of record."),
          dateRangeInput(inputId = "weir_startend",
            label = "Fish weir start and end dates",
            format = "MM dd",
            start = param_list$alt_desc[["weir_start_date"]], 
            end = param_list$alt_desc[["weir_end_date"]],
            width = "100%")
        )
      )
    } else {
      NULL
    }
  }) %>% bindEvent(input$in_collector)
  outputOptions(output, "input.fps_weir", suspendWhenHidden = FALSE)

  output$input.fps_elevations <- renderUI({
    if (input$in_collector != "NONE") {
      fluidRow( 
        column(6,
          numericInput(inputId = "fps_minelev", 
            label = "Minimum pool elevation (feet) where fish passage structure is accessible",
            value = param_list$route_specs$bottom_elev[
              which(rownames(param_list$route_specs) == "FPS")
            ], step = 1)
        ),
        column(6,
          numericInput(inputId = "fps_maxelev", 
            label = "Maximum pool elevation (feet) where fish passage structure is accessible",
            value = param_list$alt_desc[["fps_max_elev"]], 
            # max(ressim_dataframe()$elev), 
            step = 1),
        ),
        column(12,
          helpText("Fish can only access the FPS when the reservoir pool is between the minimum and maximum elevation.")
        )
          # bsTooltip(id = "fps_maxelev", title = "If the pool elevation is above this number, the fish passage structure is considered unreachable", 
          #   placement = "right", trigger = "focus", options = list(container = "body")
          # )
      )
    } else {
      NULL
    }
  }) %>% bindEvent(input$in_collector)
  outputOptions(output, "input.fps_elevations", suspendWhenHidden = FALSE)
  # Update the parameter list when these change
  observeEvent(input$fps_minelev, {
    param_list$route_specs$bottom_elev[
              which(rownames(param_list$route_specs) == "FPS")] <- 
      input$fps_minelev
  })
  observeEvent(input$fps_maxelev, {
    param_list$alt_desc[["fps_max_elev"]] <- input$fps_maxelev
  })

  # DPE ------------------------------------------------------------------------
  output$input.dpe_colselector <- renderUI({
    # fluidRow(
    #   column(12,
    #   )
    # )
    selectInput(inputId = "dpe_colselector", label = "What fish passage structure should be used to calculate DPE?",
      choices = list(
        "Use baseline DPE" = "baseline_dpe", 
        "Floating surface screen (FSS)" = "fss_dpe",
        "Fish surface collector (FSC)" = "fsc_dpe",
        "Weir" = "weir_dpe"),
      selected = param_list$alt_desc[["dpe_column_name"]],
      width = "100%")
  }); outputOptions(output, "input.dpe_colselector", suspendWhenHidden = FALSE)
  observeEvent(input$dpe_colselector, {
    param_list$alt_desc[["dpe_column_name"]] <- input$dpe_colselector
  })

  # output$dpe_hot <- renderRHandsontable({
  #   rhandsontable(dpe_values[["dpe_df"]], digits = 2,
  #     colHeaders = c("Pool elevation", "Elevation description", "Baseline DPE", 
  #      "FSS DPE", "FSC DPE", "Weir DPE"),
  #     rowHeaders = NULL, 
  #     colHeaderWidth = 100)
  # })
  observeEvent(input$dpe_hot, {
    dpe_r <- hot_to_r(input$dpe_hot)
    param_list$route_dpe <- dpe_r
  })

  observeEvent(input$dpe_addrow, {
  # Save the "updated" table as DF2
  ### This is input$dpe_hot, on line 925 it's output$dpe_hot
    DF2 = param_list$route_dpe
    param_list$route_dpe <- rbind(DF2,
      # Add all NAs
      rep(NA, ncol(DF2)))
  })

  observeEvent(input$dpe_rmrow, {
    DF2 = param_list$route_dpe
    # Remove the final row
    param_list$route_dpe <- DF2[-nrow(DF2), ]
  })

  # Warning text for when the elevations of the FPS are incorrect
  fps_elev_text <- reactive({
    req(input$fps_minelev)
    req(input$fps_maxelev)
    low.elev <- input$fps_minelev
    high.elev <- input$fps_maxelev
    ifelse(high.elev < low.elev, 
      paste0("<font color=\"#e05959\"><b>",
        "Error: Max. pool elevation is LESS than min. pool elevation!\n",
        "</b></font>"),
      paste0("")
    )
  })
  output$fps_elev_warning <- renderText({
    warning <- fps_elev_text()
    HTML(warning)
  })

  output$input.in_fps_alternative <- renderUI({
    selectInput(inputId = "in_fps_alternative", 
      label = "Is fish run timing affected by a fish passage structure?",
      choices = list("Yes" = "Y", "No" = "N"),
      width = "100%",
      selected = param_list$alt_desc[["fp_alternative"]]
    )
  }); outputOptions(output, "input.in_fps_alternative", 
    suspendWhenHidden = FALSE)
  observeEvent(input$in_fps_alternative, {
    param_list$alt_desc[["fp_alternative"]] <- input$in_fps_alternative
  })

  output$input.fish_with_flow <- renderUI({
    selectInput(inputId = "fish_with_flow", 
      label = "Should fish timing be influenced by flow through the dam (such that in a given day, more flow results in a higher proportion of fish passing)?",
      choices = list("Yes" = "Y", "No" = "N"),
      width = "100%",
      selected = param_list$alt_desc[["fish_with_flow"]]
    )
  }); outputOptions(output, "input.fish_with_flow", 
    suspendWhenHidden = FALSE)
  observeEvent(input$fish_with_flow, {
    param_list$alt_desc[["fish_with_flow"]] <- input$fish_with_flow
  })

  # SURVIVAL -------------------------------------------------------------------
  # Selectors
  # RO 
  output$input.rosurv_type <- renderUI({
    route_specs_tmp <- param_list$route_specs[
      which(rownames(param_list$route_specs) == "RO"),]
    if (is.na(route_specs_tmp$passage_surv_rate)) {
      selected_out <- "rate"
    } else if (tolower(route_specs_tmp$passage_surv_rate) == "table") {
      selected_out <- "table"
    } else if (!is.na(as.numeric(route_specs_tmp$passage_surv_rate))) {
      selected_out <- "rate"
    }
    # fluidRow(
    # column(6,
    selectInput("rosurv_type", label = "How to calculate survival?",
      choices = list(
        "As a fixed rate" = "rate",
        "As a function of flow" = "table"),
      selected = selected_out,
      width = "100%"
    )
    # ))
  }); outputOptions(output, "input.rosurv_type", suspendWhenHidden = FALSE)
  # Turb
  output$input.turbsurv_type <- renderUI({
    route_specs_tmp <- param_list$route_specs[
      which(rownames(param_list$route_specs) == "Turb"),]
    if (is.na(route_specs_tmp$passage_surv_rate)) {
      selected_out <- "rate"
    } else if (tolower(route_specs_tmp$passage_surv_rate) == "table") {
      selected_out <- "table"
    } else if (!is.na(as.numeric(route_specs_tmp$passage_surv_rate))) {
      selected_out <- "rate"
    }
    selectInput("turbsurv_type", label = "How to calculate survival?",
      choices = list(
        "As a fixed rate" = "rate",
        "As a function of flow" = "table"),
      selected = selected_out,
      width = "100%"
    )
  }); outputOptions(output, "input.turbsurv_type", suspendWhenHidden = FALSE)
  
  # Spill
  output$input.spillsurv_type <- renderUI({
    route_specs_tmp <- param_list$route_specs[
      which(rownames(param_list$route_specs) == "Spill"),]
    if (is.na(route_specs_tmp$passage_surv_rate)) {
      selected_out <- "rate"
    } else if (tolower(route_specs_tmp$passage_surv_rate) == "table") {
      selected_out <- "table"
    } else if (!is.na(as.numeric(route_specs_tmp$passage_surv_rate))) {
      selected_out <- "rate"
    }
    selectInput("spillsurv_type", label = "How to calculate survival?",
      choices = list(
        "As a fixed rate" = "rate",
        "As a function of flow" = "table"),
      selected = selected_out,
      width = "100%"
    )
  }); outputOptions(output, "input.spillsurv_type", suspendWhenHidden = FALSE)
  
  # FPS
  output$input.fpssurv_type <- renderUI({
    route_specs_tmp <- param_list$route_specs[
      which(rownames(param_list$route_specs) == "FPS"),]
    if (is.na(route_specs_tmp$passage_surv_rate)) {
      selected_out <- "rate"
    } else if (tolower(route_specs_tmp$passage_surv_rate) == "table") {
      selected_out <- "table"
    } else if (!is.na(as.numeric(route_specs_tmp$passage_surv_rate))) {
      selected_out <- "rate"
    }
    selectInput("fpssurv_type", label = "How to calculate survival?",
      choices = list(
        "As a fixed rate" = "rate",
        "As a function of flow" = "table"),
      selected = selected_out,
      width = "100%"
    )
  }); outputOptions(output, "input.fpssurv_type", suspendWhenHidden = FALSE)
  
  ### param_list triggers
  # RO
  observeEvent({
    input$rosurv_type
    input$rosurv_rate
  }, {
    if (is.na(input$rosurv_type) || input$rosurv_type == "table") {
      param_list$route_specs$passage_surv_rate[
        which(rownames(param_list$route_specs) == "RO")] <- "table"
    } else {
      param_list$route_specs$passage_surv_rate[
        which(rownames(param_list$route_specs) == "RO")] <- as.character(
          input$rosurv_rate)
    }
  })
  # Turb
  observeEvent({
    input$turbsurv_type
    input$turbsurv_rate
  }, {
    if (is.na(input$turbsurv_type) || input$turbsurv_type == "table") {
      param_list$route_specs$passage_surv_rate[
        which(rownames(param_list$route_specs) == "Turb")] <- "table"
    } else {
      param_list$route_specs$passage_surv_rate[
        which(rownames(param_list$route_specs) == "Turb")] <- as.character(
          input$turbsurv_rate)
    }
  })
  # Spill
  observeEvent({
    input$spillsurv_type
    input$spillsurv_rate
  }, {
    if (is.na(input$spillsurv_type) || input$spillsurv_type == "table") {
      param_list$route_specs$passage_surv_rate[
        which(rownames(param_list$route_specs) == "Spill")] <- "table"
    } else {
      param_list$route_specs$passage_surv_rate[
        which(rownames(param_list$route_specs) == "Spill")] <- as.character(
          input$spillsurv_rate)
    }
  })
  # FPS
  observeEvent({
    input$fpssurv_type
    input$fpssurv_rate
  }, {
    if (is.na(input$fpssurv_type) || input$fpssurv_type == "table") {
      param_list$route_specs$passage_surv_rate[
        which(rownames(param_list$route_specs) == "FPS")] <- "table"
    } else {
      param_list$route_specs$passage_surv_rate[
        which(rownames(param_list$route_specs) == "FPS")] <- as.character(
          input$fpssurv_rate)
    }
  })

  # UI Inputs for each outlet's survival specifications
  # RO
  output$input.ro_surv <- renderUI({
    req(input$rosurv_type)
    # message(input$rosurv_type)
    if(is.na(input$rosurv_type) || input$rosurv_type == "table") {
      bsCollapse(open = "Build the survival table",
        bsCollapsePanel(title = "Build the survival table",
          fluidRow(
            column(12,
            # h4("Build the survival table below:"),
            textOutput(outputId = "rosurv_ngates"),
            helpText("Survival rate through the RO can differ between high and low pool elevations. When filling in the survival rate table, you can enter different values for the low and high pool. First, specify the high and low pool elevations."),
            column(6,
              numericInput(inputId = "ro_elev_low", 
                label = span("Low pool elevation", style = "color:#00868B"), # "<span style='color:#00868B'>Low pool elevation</span>",
                min = 0, max = 4000, 
                value = param_list$ro_elevs$value[
                  which(param_list$ro_elevs$param == "ro_lower_elev")], 
                width = "100%")
            ),
            column(6,
              numericInput(inputId = "ro_elev_high", 
                label = span("High pool elevation", style = "color:#cc7000"), 
                # label = "<span style='color:#cc7000'>High pool elevation</span>",
                min = 0, max = 4000, 
                value = param_list$ro_elevs$value[
                  which(param_list$ro_elevs$param == "ro_upper_elev")], 
                width = "100%")
            ),
            htmlOutput("ro_elev_warning"),
            br(),
            column(11, offset = 1,
            rHandsontableOutput(outputId = "rosurv_hot"),
              br()
            ),
            p()
            ),
            column(6,
            actionButton(inputId = "rosurv_addrow", icon = icon("plus"),
              label = "Add row", width = "100%",
              class = "btn-secondary btn-block")),
            column(6, 
            actionButton(inputId = "rosurv_rmrow", icon = icon("minus"),
              label = "Remove row", width = "100%",
              class = "btn-secondary btn-block"))
            ),
        style = infobox_style)
    )
    } else if (input$rosurv_type == "rate") {
      # fluidRow(
        # column(12,
        # p("Enter the survival rate below"),
        numericInput("rosurv_rate", label = "Survival rate through the regulating outlet",
          value = as.numeric(param_list$route_specs$passage_surv_rate[
            which(rownames(param_list$route_specs) == "RO")
          ]), 
          min = 0, max = 1, step = 0.01, width = "100%")
        # )
      # )
    } else if (input$rosurv_type == "sampler") {
      # fluidRow(
      #   column(12,
      #   # p("Type the sampler function below (any R-interpretable function will do). If a function of flow rate through a gate, use `flow` in the equation to represent this"),
      #   h4("Enter the mean and standard deviation (SD) of a beta probability density function (see the plot on the right)"),
      #   numericInput("spillsurv_beta_mean", "Mean:", 
      #     value = 0.5, width = "100%", min = 0, max = 1, step = 0.025),
      #   numericInput("spillsurv_beta_var", "Variance (SD^2):", 
      #     value = 0.1, width = "100%", min = 0, max = 0.5, step = 0.025)
      #   # plotOutput("spillsurv_beta_preview", width = "100%")
      #   # p(paste0("Attempting to evaluate '", input$spillsurv_sampler, "':")), 
      #   # p(eval(parse(text = input$spillsurv_sampler)))
      #   )
      # )
    } 
  }) %>% bindEvent({
    input$rosurv_type
    param_list$route_specs
  }); outputOptions(output, "input.ro_surv", suspendWhenHidden = FALSE)
  
  # Turb
  output$input.turb_surv <- renderUI({
    req(input$turbsurv_type)
    if(is.na(input$turbsurv_type) || input$turbsurv_type == "table") {
      bsCollapse(open = "Build the survival table",
        bsCollapsePanel(title = "Build the survival table",
          fluidRow(
            column(12,
            # h4("Build the survival table below:"),
            textOutput(outputId = "turbsurv_ngates"),
            br(),
            column(11, offset = 1,
            rHandsontableOutput(outputId = "turbsurv_hot"),
              br()
            ),
            p()
            ),
            column(6,
            actionButton(inputId = "turbsurv_addrow", icon = icon("plus"),
              label = "Add row", width = "100%",
              class = "btn-secondary btn-block")),
            column(6, 
            actionButton(inputId = "turbsurv_rmrow", icon = icon("minus"),
              label = "Remove row", width = "100%",
              class = "btn-secondary btn-block"))
            ),
        style = infobox_style)
    )
    } else if (input$turbsurv_type == "rate") {
        numericInput("turbsurv_rate", 
          label = "Survival rate through each gate",
          value = as.numeric(param_list$route_specs$passage_surv_rate[
            which(rownames(param_list$route_specs) == "Turb")
          ]), 
          min = 0, max = 1, step = 0.01, width = "100%")
    } else if (input$turbsurv_type == "sampler") {
      # fluidRow(
      #   column(12,
      #   # p("Type the sampler function below (any R-interpretable function will do). If a function of flow rate through a gate, use `flow` in the equation to represent this"),
      #   h4("Enter the mean and standard deviation (SD) of a beta probability density function (see the plot on the right)"),
      #   numericInput("spillsurv_beta_mean", "Mean:", 
      #     value = 0.5, width = "100%", min = 0, max = 1, step = 0.025),
      #   numericInput("spillsurv_beta_var", "Variance (SD^2):", 
      #     value = 0.1, width = "100%", min = 0, max = 0.5, step = 0.025)
      #   # plotOutput("spillsurv_beta_preview", width = "100%")
      #   # p(paste0("Attempting to evaluate '", input$spillsurv_sampler, "':")), 
      #   # p(eval(parse(text = input$spillsurv_sampler)))
      #   )
      # )
    } 
  }) %>% bindEvent({
    input$turbsurv_type
    param_list$route_specs
  }); outputOptions(output, "input.turb_surv", suspendWhenHidden = FALSE)
  
  # Spill
  output$input.spill_surv <- renderUI({
    req(input$spillsurv_type)
    if(is.na(input$spillsurv_type) || input$spillsurv_type == "table") {
      bsCollapse(open = "Build the survival table",
        bsCollapsePanel(title = "Build the survival table",
          fluidRow(
            column(12,
            # h4("Build the survival table below:"),
            textOutput(outputId = "spillsurv_ngates"),
            br(),
            column(11, offset = 1,
            rHandsontableOutput(outputId = "spillsurv_hot"),
              br()
            ),
            p()
            ),
            column(6,
            actionButton(inputId = "spillsurv_addrow", icon = icon("plus"),
              label = "Add row", width = "100%",
              class = "btn-secondary btn-block")),
            column(6, 
            actionButton(inputId = "spillsurv_rmrow", icon = icon("minus"),
              label = "Remove row", width = "100%",
              class = "btn-secondary btn-block"))
            ),
        style = infobox_style)
    )
    } else if (input$spillsurv_type == "rate") {
        numericInput("spillsurv_rate", 
          label = "Survival rate through each gate",
          value = as.numeric(param_list$route_specs$passage_surv_rate[
            which(rownames(param_list$route_specs) == "Spill")
          ]),
          min = 0, max = 1, step = 0.01, width = "100%")
    } else if (input$spillsurv_type == "sampler") {
      # fluidRow(
      #   column(12,
      #   # p("Type the sampler function below (any R-interpretable function will do). If a function of flow rate through a gate, use `flow` in the equation to represent this"),
      #   h4("Enter the mean and standard deviation (SD) of a beta probability density function (see the plot on the right)"),
      #   numericInput("spillsurv_beta_mean", "Mean:", 
      #     value = 0.5, width = "100%", min = 0, max = 1, step = 0.025),
      #   numericInput("spillsurv_beta_var", "Variance (SD^2):", 
      #     value = 0.1, width = "100%", min = 0, max = 0.5, step = 0.025)
      #   # plotOutput("spillsurv_beta_preview", width = "100%")
      #   # p(paste0("Attempting to evaluate '", input$spillsurv_sampler, "':")), 
      #   # p(eval(parse(text = input$spillsurv_sampler)))
      #   )
      # )
    } 
  }) %>% bindEvent({
    input$spillsurv_type
    param_list$route_specs
  }); outputOptions(output, "input.spill_surv", suspendWhenHidden = FALSE)
  
  # FPS
  output$input.fps_surv <- renderUI({
    req(input$fpssurv_type)
    if(is.na(input$fpssurv_type) || input$fpssurv_type == "table") {
      bsCollapse(open = "Build the survival table",
        bsCollapsePanel(title = "Build the survival table",
          fluidRow(
            column(12,
            # h4("Build the survival table below:"),
            textOutput(outputId = "fpssurv_ngates"),
            br(),
            column(11, offset = 1,
            rHandsontableOutput(outputId = "fpssurv_hot", 
              width = "100%"),
              br()
            ),
            p()
            ),
            column(6,
            actionButton(inputId = "fpssurv_addrow", icon = icon("plus"),
              label = "Add row", width = "100%",
              class = "btn-secondary btn-block")),
            column(6, 
            actionButton(inputId = "fpssurv_rmrow", icon = icon("minus"),
              label = "Remove row", width = "100%",
              class = "btn-secondary btn-block"))
            ),
        style = infobox_style)
    )
    } else if (input$fpssurv_type == "rate") {
        numericInput("fpssurv_rate", 
          label = "Survival rate through each gate",
          value = as.numeric(param_list$route_specs$passage_surv_rate[
            which(rownames(param_list$route_specs) == "FPS")
          ]), 
          min = 0, max = 1, step = 0.01, width = "100%")
    } else if (input$fpssurv_type == "sampler") {
      # fluidRow(
      #   column(12,
      #   # p("Type the sampler function below (any R-interpretable function will do). If a function of flow rate through a gate, use `flow` in the equation to represent this"),
      #   h4("Enter the mean and standard deviation (SD) of a beta probability density function (see the plot on the right)"),
      #   numericInput("spillsurv_beta_mean", "Mean:", 
      #     value = 0.5, width = "100%", min = 0, max = 1, step = 0.025),
      #   numericInput("spillsurv_beta_var", "Variance (SD^2):", 
      #     value = 0.1, width = "100%", min = 0, max = 0.5, step = 0.025)
      #   # plotOutput("spillsurv_beta_preview", width = "100%")
      #   # p(paste0("Attempting to evaluate '", input$spillsurv_sampler, "':")), 
      #   # p(eval(parse(text = input$spillsurv_sampler)))
      #   )
      # )
    } 
  }) %>% bindEvent({
    input$fpssurv_type
    param_list$route_specs
  }); outputOptions(output, "input.fps_surv", suspendWhenHidden = FALSE)
  
  # Update the lower/upper values for the RO elevations
  observeEvent(input$ro_elev_low, {
    param_list$ro_elevs[[2]][1] <- input$ro_elev_low
  })
  observeEvent(input$ro_elev_high, {
    param_list$ro_elevs[[2]][2] <- input$ro_elev_high
  })

  ## Indicator of the number of gates
  # RO 
  output$rosurv_ngates <- renderText({
    out <- paste0("Route specifications:\n",
      param_list$route_specs[[
        which(rownames(param_list$route_specs) == "RO"), 
        which(colnames(param_list$route_specs) == "n_gates")
      ]], " gates, distributed using gate method '", 
      param_list$route_specs[[
        which(rownames(param_list$route_specs) == "RO"), 
        which(colnames(param_list$route_specs) == "gate_method")
      ]], "'"
    )
    out
  }) %>% bindEvent(param_list$route_specs)
  # Turb
  output$turbsurv_ngates <- renderText({
    out <- paste0("Route specifications:\n",
      param_list$route_specs[[
        which(rownames(param_list$route_specs) == "Turb"), 
        which(colnames(param_list$route_specs) == "n_gates")
      ]], " gates, distributed using gate method '", 
      param_list$route_specs[[
        which(rownames(param_list$route_specs) == "Turb"), 
        which(colnames(param_list$route_specs) == "gate_method")
      ]], "'"
    )
    out
  }) %>% bindEvent(param_list$route_specs)
  # Spill
  output$spillsurv_ngates <- renderText({
    out <- paste0("Route specifications:\n",
      param_list$route_specs[[
        which(rownames(param_list$route_specs) == "Spill"), 
        which(colnames(param_list$route_specs) == "n_gates")
      ]], " gates, distributed using gate method '", 
      param_list$route_specs[[
        which(rownames(param_list$route_specs) == "Spill"), 
        which(colnames(param_list$route_specs) == "gate_method")
      ]], "'"
    )
    out
  }) %>% bindEvent(param_list$route_specs)
  # FPS
  output$fpssurv_ngates <- renderText({
    out <- paste0("Route specifications:\n",
      param_list$route_specs[[
        which(rownames(param_list$route_specs) == "FPS"), 
        which(colnames(param_list$route_specs) == "n_gates")
      ]], " gates, distributed using gate method '", 
      param_list$route_specs[[
        which(rownames(param_list$route_specs) == "FPS"), 
        which(colnames(param_list$route_specs) == "gate_method")
      ]], "'"
    )
    out
  }) %>% bindEvent(param_list$route_specs)

  # Hands-on survival-by-flow tables
  # RO 
  output$rosurv_hot <- renderRHandsontable({
    req(param_list$ro_surv_table)
    # the RO has multiple elevations
    toohigh_rows <- which(
      param_list$ro_surv_table$ro_surv_low > 1 | 
      param_list$ro_surv_table$ro_surv_high > 1)
    toohigh_cols <- which(
      c(any(param_list$ro_surv_table$ro_surv_low > 1),
        any(param_list$ro_surv_table$ro_surv_high > 1)) == TRUE)
    rhandsontable(data = param_list$ro_surv_table, digits = 5, 
      colHeaders = c("Flow per gate (cfs)", 
        "<span style='color:#00868B'>(Low pool)\nSurvival rate per gate</span>",
        "<span style='color:#cc7000'>(High pool)\nSurvival rate per gate</span>"),
      rowHeaders = FALSE, 
      ### For highlighting:
      col_highlight = toohigh_cols,
      row_highlight = toohigh_rows - 1, 
      overflow = 'visible') %>%
      hot_cols(renderer = "
          function(instance, td, row, col, prop, value, cellProperties) {
              Handsontable.renderers.NumericRenderer.apply(this, arguments);
              if (instance.params) {
                  hcols = instance.params.col_highlight
                  hcols = hcols instanceof Array ? hcols : [hcols]
                  hrows = instance.params.row_highlight
                  hrows = hrows instanceof Array ? hrows : [hrows]
              }
              if (instance.params && hrows.includes(row) && hcols.includes(col)) td.style.color = 'red';
          }") 
  })
  
  # Turb 
  output$turbsurv_hot <- renderRHandsontable({
    req(param_list$turb_surv_table)
    # the RO has multiple elevations
    toohigh_rows <- which(
      param_list$turb_surv_table$turb_surv > 1)
      # param_list$ro_surv_table$ro_surv_high > 1)
    toohigh_cols <- 1
    # toohigh_cols <- which(
    #   c(any(param_list$ro_surv_table$ro_surv_low > 1),
    #     any(param_list$ro_surv_table$ro_surv_high > 1)) == TRUE)
    rhandsontable(data = param_list$turb_surv_table, digits = 5, 
      colHeaders = c("Flow per gate (cfs)", 
        "Survival rate per gate"),
      rowHeaders = FALSE, 
      ### For highlighting:
      col_highlight = toohigh_cols,
      row_highlight = toohigh_rows - 1, 
      overflow = 'visible') %>%
      hot_cols(renderer = "
          function(instance, td, row, col, prop, value, cellProperties) {
              Handsontable.renderers.NumericRenderer.apply(this, arguments);
              if (instance.params) {
                  hcols = instance.params.col_highlight
                  hcols = hcols instanceof Array ? hcols : [hcols]
                  hrows = instance.params.row_highlight
                  hrows = hrows instanceof Array ? hrows : [hrows]
              }
              if (instance.params && hrows.includes(row) && hcols.includes(col)) td.style.color = 'red';
          }") 
  })
  
  # Spill
  output$spillsurv_hot <- renderRHandsontable({
    req(param_list$spill_surv_table)
    # the RO has multiple elevations
    toohigh_rows <- which(
      param_list$spill_surv_table$spill_surv > 1)
      # param_list$ro_surv_table$ro_surv_high > 1)
    toohigh_cols <- 1
    # toohigh_cols <- which(
    #   c(any(param_list$ro_surv_table$ro_surv_low > 1),
    #     any(param_list$ro_surv_table$ro_surv_high > 1)) == TRUE)
    rhandsontable(data = param_list$spill_surv_table, digits = 5, 
      colHeaders = c("Flow per gate (cfs)", 
        "Survival rate per gate"),
      rowHeaders = FALSE, 
      ### For highlighting:
      col_highlight = toohigh_cols,
      row_highlight = toohigh_rows - 1, 
      overflow = 'visible') %>%
      hot_cols(renderer = "
          function(instance, td, row, col, prop, value, cellProperties) {
              Handsontable.renderers.NumericRenderer.apply(this, arguments);
              if (instance.params) {
                  hcols = instance.params.col_highlight
                  hcols = hcols instanceof Array ? hcols : [hcols]
                  hrows = instance.params.row_highlight
                  hrows = hrows instanceof Array ? hrows : [hrows]
              }
              if (instance.params && hrows.includes(row) && hcols.includes(col)) td.style.color = 'red';
          }") 
  })
  # FPS
  output$fpssurv_hot <- renderRHandsontable({
    req(param_list$fps_surv_table)
    # the RO has multiple elevations
    toohigh_rows <- which(
      param_list$fps_surv_table$fps_surv > 1)
      # param_list$ro_surv_table$ro_surv_high > 1)
    toohigh_cols <- 1
    # toohigh_cols <- which(
    #   c(any(param_list$ro_surv_table$ro_surv_low > 1),
    #     any(param_list$ro_surv_table$ro_surv_high > 1)) == TRUE)
    rhandsontable(data = param_list$fps_surv_table, digits = 5, 
      colHeaders = c("Flow per gate (cfs)", 
        "Survival rate per gate"),
      rowHeaders = FALSE, 
      ### For highlighting:
      col_highlight = toohigh_cols,
      row_highlight = toohigh_rows - 1, 
      overflow = 'visible') %>%
      hot_cols(renderer = "
          function(instance, td, row, col, prop, value, cellProperties) {
              Handsontable.renderers.NumericRenderer.apply(this, arguments);
              if (instance.params) {
                  hcols = instance.params.col_highlight
                  hcols = hcols instanceof Array ? hcols : [hcols]
                  hrows = instance.params.row_highlight
                  hrows = hrows instanceof Array ? hrows : [hrows]
              }
              if (instance.params && hrows.includes(row) && hcols.includes(col)) td.style.color = 'red';
          }") 
  })
  
  # Update param_list from tables
  # RO
  observeEvent(input$rosurv_hot, {
    rosurv_mod <- hot_to_r(input$rosurv_hot)
    param_list$ro_surv_table <- rosurv_mod
    message(param_list$ro_surv_table[1,])
  })
  # Turb
  observeEvent(input$turbsurv_hot, {
    surv_mod <- hot_to_r(
      input$turbsurv_hot)
    param_list$turb_surv_table <- 
      surv_mod
  })
  # Spill
  observeEvent(input$spillsurv_hot, {
    surv_mod <- hot_to_r(
      input$spillsurv_hot)
    param_list$spill_surv_table <- 
      surv_mod
  })
  # FPS
  observeEvent(input$fpssurv_hot, {
    surv_mod <- hot_to_r(
      input$fpssurv_hot)
    param_list$fps_surv_table <- 
      surv_mod
  })
  
  # Table modification buttons
  # RO
  observeEvent(input$rosurv_addrow, {
    DF <- param_list$ro_surv_table
    param_list$ro_surv_table <- rbind(DF,
      rep(NA, ncol(DF)))
  })
  observeEvent(input$rosurv_rmrow, {
    DF <- param_list$ro_surv_table
    param_list$ro_surv_table <- DF[-nrow(DF), ] 
  })
  # Turb
  observeEvent(input$turbsurv_addrow, {
    DF <- param_list$turb_surv_table
    param_list$turb_surv_table <- rbind(DF,
      rep(NA, ncol(DF)))
  })
  observeEvent(input$turbsurv_rmrow, {
    DF <- param_list$turb_surv_table
    param_list$turb_surv_table <- DF[-nrow(DF), ] 
  })
  # Spill
  observeEvent(input$spillsurv_addrow, {
    DF <- param_list$spill_surv_table
    param_list$spill_surv_table <- rbind(DF,
      rep(NA, ncol(DF)))
  })
  observeEvent(input$spillsurv_rmrow, {
    DF <- param_list$spill_surv_table
    param_list$spill_surv_table <- DF[-nrow(DF), ] 
  })
  # FPS
  observeEvent(input$fpssurv_addrow, {
    DF <- param_list$fps_surv_table
    param_list$fps_surv_table <- rbind(DF,
      rep(NA, ncol(DF)))
  })
  observeEvent(input$fpssurv_rmrow, {
    DF <- param_list$fps_surv_table
    param_list$fps_surv_table <- DF[-nrow(DF), ] 
  })

  # Warning text for when the elevations of the two ROs are incorrect
  ro_elev_text <- reactive({
    req(input$ro_elev_low)
    req(input$ro_elev_high)
    low.elev <- input$ro_elev_low
    high.elev <- input$ro_elev_high
    ifelse(high.elev < low.elev, 
      paste0("<font color=\"#e05959\"><b>",
        "Error: High pool elevation is less than low pool elevation!\n",
        "</b></font>"),
      paste0("")
    )
  })
  output$ro_elev_warning <- renderText({
    warning <- ro_elev_text()
    HTML(warning)
  })

  ### Run FBW warning
  # Warning text for when the elevations of the two ROs are incorrect
  fbw_warning_text <- reactive({
    missing_params <- c()
    # Alt descriptions
    if (is.null(param_list$alt_desc[["fp_alternative"]]) | 
      is.na(param_list$alt_desc[["fp_alternative"]])) {
        missing_params <- append(missing_params, 
          "Alt description: FP alternative")
    }
    if (is.na(param_list$alt_desc[["collector"]]) | 
      !(param_list$alt_desc[["collector"]] %in% c("FSC", "FSS", "FISH WEIR",
        "FSO", "NONE"))) {
        missing_params <- append(missing_params, 
          "Alt description: Collector type (must be one of FSC, FSS, FSO, weir, or 'None')")
      }
    if (is.na(param_list$alt_desc[["dpe_column_name"]]) | 
      !(param_list$alt_desc[["dpe_column_name"]]) %in% 
      c("baseline_dpe", "fss_dpe", "fsc_dpe", "weir_dpe")) {
        missing_params <- append(missing_params, 
          "DPE column selector (indicates which column of the DPE table to use)")
    }
    if (any(dim(param_list$route_dpe) == 0)) {
      missing_params <- append(missing_params, 
        "There are 0 rows in the DPE table")
    }

    if (is.na(param_list$alt_desc[["fp_alternative"]]) | 
      !(param_list$alt_desc[["fp_alternative"]] %in% c("Y", "N"))) {
        missing_params <- append(missing_params, 
          "Fish run timing - is it affected by a fish passage structure? (Should be Y/N)")
      }

    if (is.na(param_list$alt_desc[["fish_with_flow"]]) | 
      !(param_list$alt_desc[["fish_with_flow"]] %in% c("Y", "N"))) {
        missing_params <- append(missing_params, 
          "Should fish timing be influenced by flow through the dam? (Should be Y/N)")
      }
    # ROUTE EFFECTIVENESS
    if (any(dim(param_list$route_eff) == 0)) {
      missing_params <- append(missing_params, 
        "There are 0 rows or 0 columns in the route effectiveness table")
    }

    # Monthly run timing should sum to 1
    message(colSums(param_list$monthly_runtiming[, 2:3]))
    if (any(is.na(colSums(param_list$monthly_runtiming[, 2:3]))) | 
      any (colSums(param_list$monthly_runtiming[, 2:3]) != 1)) {
      missing_params <- append(missing_params, 
        "At least one column in the monthly run timing table does not add to 1")
    }

    # SURVIVAL PARAMS

    if (length(missing_params) == 0) {
      outtext <- ""
    } else {
      outtext <- paste0("<font color=\"#e05959\"><b>",
        "The following parameters are missing or incorrectly specified:<ul><li>",
        paste0(missing_params, collapse = "</li><li>"), 
        "</li></ul></b></font>")
    }
  })

  output$runfbw_warnings <- renderText({
    warning <- fbw_warning_text()
    HTML(warning)
  })

  ### Graphical outputs --------------------------------------------------------
  ### ResSim hydrological trends over time, according to selected inputs
  observeEvent({
      # input$ressim_plot_facets
      input$ressim_plot_select
    },
    {
      output$ressim_plots <- renderUI({
        fluidRow(
          column(12,
            hr(),
            # dataTableOutput(outputId = "ressimsubset_preview"),
            plotlyOutput(outputId = "ressim_elev_vs_outflow") %>% 
              withSpinner()
          ),
          column(4,
            plotlyOutput(outputId = "ressim_elev_vs_powerhouse") %>% 
              withSpinner()
          ),
          column(4,
            plotlyOutput(outputId = "ressim_elev_vs_ro")  %>% 
              withSpinner()
          ),
          column(4,
            plotlyOutput(outputId = "ressim_elev_vs_spill") %>% 
              withSpinner()
          )
        )
      })
      # Sub-plots
      output$ressim_elev_vs_outflow <- renderPlotly({
        # First, plot elevation vs. outflow
        p <- ggplot(ressim_dataframe_plotting_subset(),
          aes(x = Date, y = outflow_flow, 
          # group=1 required to make text work?
          group = 1,
          text = paste(
            "Date: ", Date, "<br>", 
            "Pool elev. (f): ", round(elev, 0), "<br>",
            "Total outflow (cfs): ", round(outflow_flow, 3), sep = ""))) + 
          scale_x_datetime(breaks = "2 months", date_labels = "%b") +
          geom_line(linewidth = 1.1,
            aes(y = elev, color = "Pool elevation (ft)")) +
          geom_line(linewidth = 1.1, aes(color = "Total outflow (cfs)")) +
          # scale_x_continuous(breaks = seq(0, 336, 24)) +
          labs(x = "", y = "Total outflow (cfs)", color = "") +
          scale_color_manual(values = c("gray50", sankeycols$approach)) +
          scale_y_continuous(
            # Features of the first axis
            name = "Total outflow (cfs)",
            # Add a second axis and specify its features
            sec.axis = sec_axis(~.*1, name = "Pool elevation (ft)")) +
          theme_classic() +
          theme(
            axis.title.y = element_text(color = sankeycols$approach),
            axis.title.y.right = element_text(color = "gray50")) +
          ggtitle("Total outflow vs Pool elevation")
        ## ggplotly legend modification
        ggplotly(p, tooltip = "text") %>%
          layout(legend = list(orientation = 'h'))
      })

      # if ("turb" %in% input$ressim_plot_facets) {
        output$ressim_elev_vs_powerhouse <- renderPlotly({
          # This is used to create a second axis
          # yaxis_ratio <- max(ressim_dataframe_plotting_subset()$turb_flow, na.rm = T) /
          #   max(ressim_dataframe_plotting_subset()$elev, na.rm = T)
          # First, plot elevation vs. outflow
          p <- ggplot(ressim_dataframe_plotting_subset(), aes(x = Date,
            y = turb_flow, 
            # group=1 required to make text work?
            group = 1,
            text = paste(
              "Date: ", Date, "<br>",
              "Pool elev. (f): ", round(elev, 0), "<br>",
              "Powerhouse outflow (cfs): ", round(turb_flow, 3), sep = ""))) +
            scale_x_datetime(breaks = "2 months", date_labels = "%b") +
            geom_line(linewidth = 1.1, aes(color = "Powerhouse outflow (cfs)")) +
            geom_line(linewidth = 1.1,
              aes(y = elev, color = "Pool elevation (ft)"))+
            # scale_x_continuous(breaks = seq(0, 336, 24)) +
            labs(x = "", y = "Powerhouse outflow (cfs)", color = "") +
            scale_color_manual(values = c("gray50", sankeycols$turb)) +
            scale_y_continuous(
              # Features of the first axis
              name = "Powerhouse outflow (cfs)",
              # Add a second axis and specify its features
              sec.axis = sec_axis(~.*1, name = "Pool elevation (ft)")) +
            theme_classic() +
            theme(
              legend.position = "bottom",
              axis.title.y = element_text(color = sankeycols$turb),
              axis.title.y.right = element_text(color = "gray50")) +
            ggtitle("Powerhouse outflow vs Pool elevation")
        ## ggplotly legend modification
        ggplotly(p, tooltip = "text") %>%
          layout(legend = list(orientation = 'h'))
        })
      # } else {
        # output$ressim_elev_vs_powerhouse <- NULL
      # }
    # if ("ro" %in% input$ressim_plot_facets) {
      output$ressim_elev_vs_ro <- renderPlotly({
        # yaxis_ratio <- max(ressim_dataframe_plotting_subset()$RO_flow, na.rm = T) /
        #   max(ressim_dataframe_plotting_subset()$elev, na.rm=T)
        # First, plot elevation vs. outflow
        p <- ggplot(ressim_dataframe_plotting_subset(), aes(x = Date, y = RO_flow, 
            # group=1 required to make text work?
            group = 1,
            text = paste(
              "Date: ", Date, "<br>", 
              "Pool elev. (f): ", round(elev, 0), "<br>",
              "RO outflow (cfs): ", round(RO_flow, 3), sep = ""))) +
          scale_x_datetime(breaks = "2 months", date_labels = "%b") +
          geom_line(linewidth = 1.1, aes(color = "Regulating outlet outflow (cfs)")) +
          geom_line(linewidth = 1.1,
            aes(y = elev, color = "Pool elevation (ft)"))+
          # scale_x_continuous(breaks = seq(0, 336, 24)) +
          labs(x = "", y = "Regulating outlet outflow (cfs)", color = "") +
          scale_color_manual(values = c("gray50",sankeycols$ro)) +
          scale_y_continuous(
            # Features of the first axis
            name = "Regulating outlet outflow (cfs)",
            # Add a second axis and specify its features
            sec.axis = sec_axis(~.*1, name = "Pool elevation (ft)")) +
          theme_classic() +
          theme(
            legend.position = "bottom",
            axis.title.y = element_text(color = sankeycols$ro),
            axis.title.y.right = element_text(color = "gray50")) +
          ggtitle("Regulating outlet vs Pool elevation")
          ## ggplotly legend modification
          ggplotly(p, tooltip = "text") %>%
            layout(legend = list(orientation = 'h'))
        })
    # } else {
      # output$ressim_elev_vs_ro <- NULL
    # }
    # if ("spill" %in% input$ressim_plot_facets) {
      output$ressim_elev_vs_spill <- renderPlotly({
        # First, plot elevation vs. outflow
        p <- ggplot(ressim_dataframe_plotting_subset(), aes(x = Date, 
          y = spill_flow, 
            # group=1 required to make text work?
            group = 1,
            text = paste(
              "Date: ", Date, "<br>", 
              "Pool elev. (f): ", round(elev, 0), "<br>",
              "Spillway outflow (cfs): ", round(spill_flow, 3), sep = ""))) +
          scale_x_datetime(breaks = "2 months", date_labels = "%b") +
          geom_line(linewidth = 1.1, aes(color = "Spillway outflow (cfs)")) +
          geom_line(linewidth = 1.1,
            aes(y = elev, color = "Pool elevation (ft)"))+
          # scale_x_continuous(breaks = seq(0, 336, 24)) +
          labs(x = "", y = "Spillway outflow (cfs)", color = "") +
          scale_color_manual(values = c("gray50", sankeycols$spill)) +
          scale_y_continuous(
            # Features of the first axis
            name = "Spillway outflow (cfs)",
            # Add a second axis and specify its features
            sec.axis = sec_axis(~.*1, name = "Pool elevation (ft)")) +
          theme_classic() +
          theme(
            legend.position = "bottom",
            axis.title.y = element_text(color = sankeycols$spill),
            axis.title.y.right = element_text(color = "gray50")) +
          ggtitle("Spillway vs Pool elevation")
          ## ggplotly legend modification
          ggplotly(p, tooltip = "text") %>%
            layout(legend = list(orientation = 'h'))
        })
    # } else {
      # output$ressim_elev_vs_spill <- NULL
    # }
  })
# # Plot DPE output
#     output$dpe_plotly <- renderPlotly({
#       # Create the DPE from the hottable
#       dpe_df_o <- param_list$route_dpe
#       dpe_df <- dpe_df_o %>%
#         tidyr::pivot_longer(cols = -c(elev, elev_description), names_to = "DPEtype",
#           values_to = "DPE") %>%
#         dplyr::mutate(
#           labelstring = case_when(
#             DPEtype == "baseline_dpe" ~ "Baseline",
#             DPEtype == "fss_dpe" ~ "FSS",
#             DPEtype == "fsc_dpe" ~ "FSC",
#             DPEtype == "weir_dpe" ~ "Weir")
#           )
#       dpe_hot_whichCol <- which(colnames(param_list$route_dpe) == 
#         param_list$alt_desc[["dpe_column_name"]])
#       dpe_hot_whichRows <- which(
#         # Identify which rows are within the range 
#         (param_list$route_dpe[["elev"]] >=
#         ifelse(
#           is.na(param_list$route_specs[
#             which(rownames(param_list$route_specs) == "FPS"),]$bottom_elev), 
#           -Inf, 
#           param_list$route_specs[which(rownames(param_list$route_specs) == "FPS"),]$bottom_elev)) & 
#         (param_list$route_dpe[["elev"]] <=
#         ifelse(
#           is.na(param_list$alt_desc[["fps_max_elev"]]),
#           Inf,
#           param_list$alt_desc[["fps_max_elev"]]))
#       )
#       dpe_realized_df <- data.frame(
#         elev = dpe_df_o$elev,
#         DPE = as.numeric(NA)
#       )
#       # Overwrite DPE to be the "realized" value at each elevation
#       dpe_realized_df[dpe_hot_whichRows, 2] <- dpe_df_o[dpe_hot_whichRows,
#         dpe_hot_whichCol]
#       dpe_realized_df[which(!(
#         (1:nrow(dpe_realized_df) %in% dpe_hot_whichRows))), 2] <- dpe_df_o[
#           which(!(
#         (1:nrow(dpe_realized_df) %in% dpe_hot_whichRows))),
#         # Use column 3
#         3]
#     dpeplotly <- ggplot(dpe_df, aes(x = as.numeric(elev), y = as.numeric(DPE), color = labelstring, 
#       group = 1,
#       text = paste(
#         "Elevation: ", round(as.numeric(elev), 0), 
#           ifelse(is.na(elev_description), "ft.<br>", paste0("ft. (", 
#             elev_description, ")<br>")),
#         "DPE at ", labelstring, ": ", round(DPE, 3), sep = ""))) + 
#       geom_point(size = 2) + 
#       geom_line() + 
#       theme_classic() + 
#       labs(x = "Pool elevation (ft)", y = "DPE") + 
#       scale_color_manual("DPE for ...", 
#         values = c("black", sankeycols$spill, "turquoise4", "orange2")) + 
#       geom_line(data = dpe_realized_df, inherit.aes = FALSE, 
#         aes(x = as.numeric(elev), y = as.numeric(DPE)), lwd = 4, alpha = 0.5, col = "orange2") + 
#       ggtitle("DPE through different structures")
#     ggplotly(dpeplotly, tooltip = "text")
#     })
  
    ### Parameters: Interactive handsontables ------------------------------------
    
    output$routerules_hot <- renderRHandsontable({
      allowed_gate_methods <- c("", NA, "Equal Q", "Min Q to equal", "Unit to Max Q", "Target Q", 
            "Peaking Performance")
      allowed_normused <- c("Y", "N", "", NA)
      rhandsontable(data = param_list$route_specs %>%
        select(-passage_surv_rate) %>%
        select(c(bottom_elev, n_gates, gate_method, min_flow, target_flow, max_flow, normally_used)) %>%
        mutate(
          gate_method = factor(gate_method, levels = allowed_gate_methods),
          n_gates = as.integer(n_gates),
          normally_used = factor(normally_used, levels = allowed_normused)
        ), digits = 2, colHeaders = c(
      "Bottom elevation (f)",
      "# of gates",
      "How to split flow between gates?",
      # INSTEAD: Have this on another tab to help with visualization
      # "Passage surv. rate ('table', rate, or distribution)", 
      "Min. flow (cfs)", 
      "Target flow (cfs)", 
      "Max. flow (cfs)",
      "Normally used? (Y/N)"
      ),
      rowHeaders = c("Regulating Outlet",
        "Turbines", "Spillway", "Fish passage structure"),
        rowHeaderWidth = 200, colHeaderWidth = 300,
        height = 250, 
        overflow = 'visible') %>%
      # New feature: validation
      hot_validate_numeric(cols = c(1:2, 4:6), min = 0) %>%
      # # Suvival
      # hot_validate_numeric(col = 3, min = 0, max = 1) %>%
      # Gate method
      hot_validate_character(3, 
        choices = allowed_gate_methods, allowInvalid = TRUE) %>%
      hot_validate_character(7, choices = c("Y", "N", ""), allowInvalid = TRUE)
    })
    observeEvent(input$routerules_hot, {
      mod_routerules <- hot_to_r(input$routerules_hot)
      original_routerules <- param_list$route_specs
      # param_list$route_specs <- mod_routerules
      param_list$route_specs$bottom_elev <- mod_routerules$bottom_elev
      param_list$route_specs$bottom_elev <- mod_routerules$bottom_elev
      param_list$route_specs$n_gates <- mod_routerules$n_gates
      param_list$route_specs$gate_method <- mod_routerules$gate_method
      param_list$route_specs$min_flow <- mod_routerules$min_flow
      param_list$route_specs$target_flow <- mod_routerules$target_flow
      param_list$route_specs$max_flow <- mod_routerules$max_flow
      param_list$route_specs$normally_used <- mod_routerules$normally_used
    })
    
    output$routeeff_hot <- renderRHandsontable({
      rhandsontable(data = param_list$route_eff %>%
        select("RO", "Turb", "Spill", "FPS"),
        # rowwise(),
        # mutate(q_ratio = paste0("Q ratio: ", q_ratio)),
      colHeaders = c(# "Q Ratio", 
        "Regulating\noutlet R.E.", 
        "Turbine R.E.", 
        "Spillway R.E.",
        "FPS R.E."), 
      rowHeaders = paste("Q Ratio:  \t", seq(from = 0, to = 1, by = 0.1)),
      rowHeaderWidth = 200, colHeaderWidth = 500, 
      overflow = 'visible') %>%
      # Turn off editing for Qratio = 0 and Qratio = 1
      # hot_col(col = 1, readOnly = TRUE, format = '0') %>%
      hot_col(col = 1:4, format = '0.00') %>%
      hot_row(c(1, 11), readOnly = TRUE) %>%
      hot_heatmap(
        renderer = renderer_heatmap_custom(
          color_scale = c('#576871', '#fafa6e')
          ))
    })
    observeEvent(input$routeeff_hot, {
      mod_route_eff <- hot_to_r(input$routeeff_hot) %>%
        select("Spill", "FPS", "RO", "Turb")
      # The q-ratio column has been de-selected, so only update the other cols
      param_list$route_eff[, 2:5] <- mod_route_eff[, 1:4]
    })

    output$dpe_hot <- renderRHandsontable({
      # For highlighting: 
      dpe_hot_whichCol <- which(colnames(param_list$route_dpe) == 
        param_list$alt_desc[["dpe_column_name"]])
      dpe_hot_whichRows <- which(
        # Identify which rows are within the range 
        (param_list$route_dpe[["elev"]] >=
        ifelse(
          is.na(param_list$route_specs[
            which(rownames(param_list$route_specs) == "FPS"),]$bottom_elev), 
          -Inf, 
          param_list$route_specs[which(rownames(param_list$route_specs) == "FPS"),]$bottom_elev)) & 
        (param_list$route_dpe[["elev"]] <=
        ifelse(
          is.na(param_list$alt_desc[["fps_max_elev"]]),
          Inf,
          param_list$alt_desc[["fps_max_elev"]]))
      )
      rhandsontable(data = param_list$route_dpe, digits = 4,
        colHeaders = c("Pool elevation (feet)", "Elevation description", "Baseline DPE", 
        "FSS DPE", "FSC DPE", "Weir DPE"),
        rowHeaders = NULL, 
        colHeaderWidth = 200,
        # height = 100,
        ### New for highlighting:
        col_highlight = dpe_hot_whichCol - 1,
        row_highlight = dpe_hot_whichRows - 1, 
        overflow = 'visible') %>%
      hot_col("Pool elevation (feet)", format = "0.") %>% 
        # Modified from: https://stackoverflow.com/questions/41595026/correct-way-to-customize-color-of-rhandsontable-inside-a-shiny-app
      hot_cols(renderer = "
        function(instance, td, row, col, prop, value, cellProperties) {
            Handsontable.renderers.NumericRenderer.apply(this, arguments);
            if (instance.params) {
                hcols = instance.params.col_highlight
                hcols = hcols instanceof Array ? hcols : [hcols]
                hrows = instance.params.row_highlight
                hrows = hrows instanceof Array ? hrows : [hrows]
            }
            if (instance.params && hrows.includes(row) && hcols.includes(col)) td.style.background = '#b9f1e6';
            if (instance.params && !hrows.includes(row) && col==2) td.style.background = '#b9f1e6';
        }"
      )
    }) %>% bindEvent(
      input$dpe_colselector,
      input$fps_minelev,
      input$fps_maxelev,
      param_list$route_dpe, 
      input$dpe_addrow, 
      input$dpe_rmrow)  
    observeEvent(input$dpe_hot, {
      mod_dpe_df <- hot_to_r(input$dpe_hot)
      param_list$route_dpe <- mod_dpe_df
    })

output$monthly_runtiming <- renderRHandsontable({
    timinghot_whichCol <- ifelse(
      param_list$alt_desc[["fp_alternative"]] == "Y", 3, 2)
    rhandsontable(data = param_list$monthly_runtiming %>%
        mutate(Date = factor(format(Date, "%B"))),
      digits = 4,
      colHeaders = c("Month",
        "% Approaching (baseline)", "% Approaching (alternative)"),
      rowHeaders = NULL,
      ### New for highlighting; -1 to index properly
      col_highlight = timinghot_whichCol - 1,
      row_highlight = 0:11, # Jan-Dec, 12 entries
      colHeaderWidth = 100, 
      overflow = 'visible') %>%
      hot_col(1, dateFormat = "B", type = "date",
          readOnly = TRUE) %>%
      hot_cols(renderer = "
          function(instance, td, row, col, prop, value, cellProperties) {
              Handsontable.renderers.NumericRenderer.apply(this, arguments);
              if (instance.params) {
                  hcols = instance.params.col_highlight
                  hcols = hcols instanceof Array ? hcols : [hcols]
                  hrows = instance.params.row_highlight
                  hrows = hrows instanceof Array ? hrows : [hrows]
              }
              if (instance.params && hcols.includes(col)) td.style.background = '#b9f1e6';
          }")
  })

  # Plot the DPE relationship from the table
  output$dpe_plotly <- renderPlotly({
      # Create the DPE from the hottable
      dpe_df_o <- param_list$route_dpe
      dpe_df <- dpe_df_o %>%
        tidyr::pivot_longer(cols = -c(elev, elev_description), names_to = "DPEtype",
          values_to = "DPE") %>%
        dplyr::mutate(
          labelstring = case_when(
            DPEtype == "baseline_dpe" ~ "Baseline",
            DPEtype == "fss_dpe" ~ "FSS",
            DPEtype == "fsc_dpe" ~ "FSC",
            DPEtype == "weir_dpe" ~ "Weir")
          )
      dpe_hot_whichCol <- which(colnames(param_list$route_dpe) == 
        param_list$alt_desc[["dpe_column_name"]])
      dpe_hot_whichRows <- which(
        # Identify which rows are within the range 
        (param_list$route_dpe[["elev"]] >=
        ifelse(
          is.na(param_list$route_specs[
            which(rownames(param_list$route_specs) == "FPS"),]$bottom_elev), 
          -Inf, 
          param_list$route_specs[which(rownames(param_list$route_specs) == "FPS"),]$bottom_elev)) & 
        (param_list$route_dpe[["elev"]] <=
        ifelse(
          is.na(param_list$alt_desc[["fps_max_elev"]]),
          Inf,
          param_list$alt_desc[["fps_max_elev"]]))
      )
      dpe_realized_df <- data.frame(
        elev = dpe_df_o$elev,
        DPE = as.numeric(NA)
      )
      # Overwrite DPE to be the "realized" value at each elevation
      dpe_realized_df[dpe_hot_whichRows, 2] <- dpe_df_o[dpe_hot_whichRows,
        dpe_hot_whichCol]
      dpe_realized_df[which(!(
        (1:nrow(dpe_realized_df) %in% dpe_hot_whichRows))), 2] <- dpe_df_o[
          which(!(
        (1:nrow(dpe_realized_df) %in% dpe_hot_whichRows))),
        # Use column 3
        3]

    dpeplotly <- ggplot(dpe_df, aes(x = as.numeric(elev), y = as.numeric(DPE), color = labelstring, 
      group = 1,
      text = paste(
        "Elevation: ", round(as.numeric(elev), 0), 
          ifelse(is.na(elev_description), "ft.<br>", paste0("ft. (", 
            elev_description, ")<br>")),
        "DPE at ", labelstring, ": ", round(DPE, 3), sep = ""))) + 
      geom_path(data = dpe_realized_df, inherit.aes = FALSE, 
        aes(x = as.numeric(elev), y = as.numeric(DPE)),
        lineend = "round",
        linejoin = "miter",
        linemitre = 20,
        lwd = 5, alpha = 0.5, 
        col = "#b9f1e6") + 
      geom_point(size = 2) + 
      geom_line() + 
      theme_classic() + 
      labs(x = "Pool elevation (ft)", y = "DPE") + 
      scale_color_manual("DPE for ...", 
        values = c(sankeycols$approach, sankeycols$spill, sankeycols$turb, 
          sankeycols$fps)) + 
      ggtitle("DPE through different structures")
    ggplotly(dpeplotly, tooltip = "text")
    }) %>% bindEvent({
      param_list$route_dpe
      param_list$alt_desc[["dpe_column_name"]]
      # input$make_dpe_plot
    })

    # Survival rate through each outlet: plots/text
    # RO 
    output$rosurv_plot <- renderPlotly({
      req(input$rosurv_type)
      if (input$rosurv_type == "rate") {
        req(input$rosurv_rate)
        # req(ressim_dataframe())
        # message("rate based RO figure")
        # message(colnames(ressim_dataframe()))
        example_df <- data.frame(
          flow = NA,
          survival = NA
        )
        # message(example_df[1,])
        ggpl <- ggplot(example_df, aes(x = flow, y = survival,
          group = 1, text = paste0("Flow: ", flow, "<br>Survival rate: ", survival))) +
          geom_line(lwd = 1.2) + 
          geom_point(size = 2) + 
          theme_classic() + 
          labs(title = "Regulating outlet: flat survival rate", 
            x = "Flow rate per gate (cfs); values were chosen for demonstration", 
            y = "Survival rate")
        ggplotly(ggpl, tooltip = "text")
    } else if(input$rosurv_type == "sampler") {
      rosurv_sampler_params <- estBetaParams(mu = input$rosurv_beta_mean,
        var = (input$rosurv_beta_var))
      # output$spillsurv_beta_preview <- renderPlot({
        sampled_beta <- data.frame(x = 1:1000, sample = rbeta(n = 1000, 
          shape1 = rosurv_sampler_params$alpha,
          shape2 = rosurv_sampler_params$beta))
        ggpl <- ggplot(sampled_beta, aes(x=sample)) + 
          theme_classic() +
          geom_density(aes(text = sample), alpha = 0.2,
            fill = "#00868B", color = sankeycols$ro) +
          geom_vline(aes(xintercept=mean(sample, na.rm=T),
            text = paste0("Mean of the sample")),   # Ignore NA values for mean
            color="#cc7000", linetype="dotted", size=1) +
          geom_errorbarh(aes(y = 0.5, xmin = (mean(sample, na.rm = T) - 
            var(sample, na.rm = T)), xmax = (mean(sample, na.rm = T) + 
            var(sample, na.rm = T)),
            text = "Variance of the sample"), size = 1, height = 0.1,
            color="#cc7000") +
          labs(title = "Regulating outlet: sampled survival rate", x = "Survival rate",
            y = "Probability density") +
          xlim(c(0,1))
        ggplotly(ggpl, tooltip = "text")
    } else if(input$rosurv_type == "table") {
      if (nrow(example_df) == 0) {
        example_df <- data.frame(
          flow = numeric(NA),
          ro_surv_low = numeric(NA),
          ro_surv_high = numeric(NA)
        )
      } else {
        example_df <- example_df %>%
          rename(`Low pool` = ro_surv_low,
            `High pool` = ro_surv_high)
      }
      ggpl <- ggplot(example_df, aes(x = flow,
        group = 1, text = paste0("Flow: ", flow, "<br>Low pool survival rate: ", `Low pool`, "<br>High pool survival rate: ", `High pool`))) +
        geom_line(lwd = 1.2, aes(y = `Low pool`), color = "#00868B") + 
        geom_point(size = 2, aes(y = `Low pool`), color = "#00868B") + 

        geom_line(lwd = 1.2, aes(y = `High pool`), color = "#cc7000") + 
        geom_point(size = 2, aes(y = `High pool`), color = "#cc7000") + 
        
        theme_classic() + 
        labs(title = "Regulating outlet: flow-based survival rate", 
          x = "Flow rate per gate (cfs)",
          y = "Survival rate")
      ggplotly(ggpl, tooltip = "text")    
    }
  })

  output$rosurv_plot_text <- renderText({
    if (input$rosurv_type == "sampler") {
      paste0("If no plot appears, check that the mean and variance parameters", 
      "you input are compatible with a beta distribution.\nThe orange dashed line represents the mean of the sample from the beta distribution, the bars represent the variance.")
    } else {
      paste0("")
    }
  })
  
  # Turbines 
  output$turbsurv_plot <- renderPlotly({
      req(input$turbsurv_type)
      if (input$turbsurv_type == "rate") {
        req(input$turbsurv_rate)
        example_df <- data.frame(
          flow = NA,
          survival = NA
        )
        # message(example_df[1,])
        ggpl <- ggplot(example_df, aes(x = flow, y = survival,
          group = 1, text = paste0("Flow: ", flow, "<br>Survival rate: ", survival))) +
          geom_line(lwd = 1.2) + 
          geom_point(size = 2) + 
          theme_classic() + 
          labs(title = "Turbines: flat survival rate", 
            x = "Flow rate per gate (cfs); values were chosen for demonstration", 
            y = "Survival rate")
        ggplotly(ggpl, tooltip = "text")
    } else if(input$turbsurv_type == "sampler") {
      turbsurv_sampler_params <- estBetaParams(mu = input$turbsurv_beta_mean,
        var = (input$turbsurv_beta_var))
      # output$spillsurv_beta_preview <- renderPlot({
        sampled_beta <- data.frame(x = 1:1000, sample = rbeta(n = 1000, 
          shape1 = turbsurv_sampler_params$alpha,
          shape2 = turbsurv_sampler_params$beta))
        ggpl <- ggplot(sampled_beta, aes(x=sample)) + 
          theme_classic() +
          geom_density(aes(text = sample), alpha = 0.2,
            fill = "#00868B", color = sankeycols$ro) +
          geom_vline(aes(xintercept=mean(sample, na.rm=T),
            text = paste0("Mean of the sample")),   # Ignore NA values for mean
            color="#cc7000", linetype="dotted", size=1) +
          geom_errorbarh(aes(y = 0.5, xmin = (mean(sample, na.rm = T) - 
            var(sample, na.rm = T)), xmax = (mean(sample, na.rm = T) + 
            var(sample, na.rm = T)),
            text = "Variance of the sample"), size = 1, height = 0.1,
            color="#cc7000") +
          labs(title = "Turbines: sampled survival rate", x = "Survival rate",
            y = "Probability density") +
          xlim(c(0,1))
        ggplotly(ggpl, tooltip = "text")
    } else if(input$turbsurv_type == "table") {
      if (nrow(example_df) == 0) {
        example_df <- data.frame(
          flow = numeric(NA),
          survival = numeric(NA)
        )
      } else {
        example_df <- example_df %>%
          rename(survival = turb_surv)
      }
      ggpl <- ggplot(example_df, aes(x = flow, y = survival,
        group = 1, 
          text = paste0("Flow: ", flow, "<br>Survival: ", survival))) +
        geom_line(lwd = 1.2, color = "black") + 
        geom_point(size = 2, color = "black") +
        # geom_point(size = 2, aes(y = `Low pool`), color = "#00868B") +         
        theme_classic() + 
        labs(title = "Turbines: flow-based survival rate", 
          x = "Flow rate per gate (cfs)",
          y = "Survival rate")
      ggplotly(ggpl, tooltip = "text")    
    }
  })

  output$turbsurv_plot_text <- renderText({
    if (input$turbsurv_type == "sampler") {
      paste0("If no plot appears, check that the mean and variance parameters", 
      "you input are compatible with a beta distribution.\nThe orange dashed line represents the mean of the sample from the beta distribution, the bars represent the variance.")
    } else {
      paste0("")
    }
  })

  # Spill
    output$spillsurv_plot <- renderPlotly({
      req(input$spillsurv_type)
      if (input$spillsurv_type == "rate") {
        req(input$spillsurv_rate)
        example_df <- data.frame(
          flow = NA,
          survival = NA
        )
        # message(example_df[1,])
        ggpl <- ggplot(example_df, aes(x = flow, y = survival,
          group = 1, text = paste0("Flow: ", flow, "<br>Survival rate: ", survival))) +
          geom_line(lwd = 1.2) + 
          geom_point(size = 2) + 
          theme_classic() + 
          labs(title = "Spillway: flat survival rate", 
            x = "Flow rate per gate (cfs); values were chosen for demonstration", 
            y = "Survival rate")
        ggplotly(ggpl, tooltip = "text")
    } else if(input$spillsurv_type == "sampler") {
      spillsurv_sampler_params <- estBetaParams(mu = input$spillsurv_beta_mean,
        var = (input$spillsurv_beta_var))
      # output$spillsurv_beta_preview <- renderPlot({
        sampled_beta <- data.frame(x = 1:1000, sample = rbeta(n = 1000, 
          shape1 = spillsurv_sampler_params$alpha,
          shape2 = spillsurv_sampler_params$beta))
        ggpl <- ggplot(sampled_beta, aes(x=sample)) + 
          theme_classic() +
          geom_density(aes(text = sample), alpha = 0.2,
            fill = "#00868B", color = "turquoise4") +
          geom_vline(aes(xintercept=mean(sample, na.rm=T),
            text = paste0("Mean of the sample")),   # Ignore NA values for mean
            color="#cc7000", linetype="dotted", size=1) +
          geom_errorbarh(aes(y = 0.5, xmin = (mean(sample, na.rm = T) - 
            var(sample, na.rm = T)), xmax = (mean(sample, na.rm = T) + 
            var(sample, na.rm = T)),
            text = "Variance of the sample"), size = 1, height = 0.1,
            color="#cc7000") +
          labs(title = "Spillway: sampled survival rate", x = "Survival rate",
            y = "Probability density") +
          xlim(c(0,1))
        ggplotly(ggpl, tooltip = "text")
    } else if(input$spillsurv_type == "table") {
      example_df <- param_list$spill_surv_table
      if (nrow(example_df) == 0) {
        example_df <- data.frame(
          flow = numeric(NA),
          survival = numeric(NA)
        )
      } else {
        example_df <- example_df %>%
          rename(survival = spill_surv)
      }
      ggpl <- ggplot(example_df, aes(x = flow, y = survival,
        group = 1, 
          text = paste0("Flow: ", flow, "<br>Survival: ", survival))) +
        geom_line(lwd = 1.2, color = "black") + 
        geom_point(size = 2, color = "black") +
        # geom_point(size = 2, aes(y = `Low pool`), color = "#00868B") +         
        theme_classic() + 
        labs(title = "Spillway: flow-based survival rate", 
          x = "Flow rate per gate (cfs)",
          y = "Survival rate")
      ggplotly(ggpl, tooltip = "text")    
    }
  }) 

  output$spillsurv_plot_text <- renderText({
    if (input$spillsurv_type == "sampler") {
      paste0("If no plot appears, check that the mean and variance parameters", 
      "you input are compatible with a beta distribution.\nThe orange dashed line represents the mean of the sample from the beta distribution, the bars represent the variance.")
    } else {
      paste0("")
    }
  })

  # FPS
  output$fpssurv_plot <- renderPlotly({
      req(input$fpssurv_type)
      if (input$fpssurv_type == "rate") {
        req(input$fpssurv_rate)
        example_df <- data.frame(
          flow = numeric(NA),
          survival = numeric(NA)
        )
        # message(example_df[1,])
        ggpl <- ggplot(example_df, aes(x = flow, y = survival,
          group = 1, text = paste0("Flow: ", flow, "<br>Survival rate: ", survival))) +
          geom_line(lwd = 1.2) + 
          geom_point(size = 2) + 
          theme_classic() + 
          labs(title = "Fish passage structure: flat survival rate", 
            x = "Flow rate per gate (cfs); values were chosen for demonstration", 
            y = "Survival rate")
        ggplotly(ggpl, tooltip = "text")
    } else if(input$fpssurv_type == "sampler") {
      fpssurv_sampler_params <- estBetaParams(mu = input$fpssurv_beta_mean,
        var = (input$fpssurv_beta_var))
      # output$fpssurv_beta_preview <- renderPlot({
        sampled_beta <- data.frame(x = 1:1000, sample = rbeta(n = 1000, 
          shape1 = fpssurv_sampler_params$alpha,
          shape2 = fpssurv_sampler_params$beta))
        ggpl <- ggplot(sampled_beta, aes(x=sample)) + 
          theme_classic() +
          geom_density(aes(text = sample), alpha = 0.2,
            fill = "#00868B", color = "turquoise4") +
          geom_vline(aes(xintercept=mean(sample, na.rm=T),
            text = paste0("Mean of the sample")),   # Ignore NA values for mean
            color="#cc7000", linetype="dotted", size=1) +
          geom_errorbarh(aes(y = 0.5, xmin = (mean(sample, na.rm = T) - 
            var(sample, na.rm = T)), xmax = (mean(sample, na.rm = T) + 
            var(sample, na.rm = T)),
            text = "Variance of the sample"), size = 1, height = 0.1,
            color="#cc7000") +
          labs(title = "Fish passage structure: sampled survival rate", x = "Survival rate",
            y = "Probability density") +
          xlim(c(0,1))
        ggplotly(ggpl, tooltip = "text")
    } else if(input$fpssurv_type == "table") {
      example_df <- param_list$fps_surv_table 
      if (nrow(example_df) == 0) {
        example_df <- data.frame(
          flow = NA,
          survival = NA
        )
      } else {
        example_df <- example_df %>%
          rename(survival = fps_surv)
      }
      ggpl <- ggplot(example_df, aes(x = flow, y = survival,
        group = 1, 
          text = paste0("Flow: ", flow, "<br>Survival: ", survival))) +
        geom_line(lwd = 1.2, color = "black") + 
        geom_point(size = 2, color = "black") + 
        # geom_point(size = 2, aes(y = `Low pool`), color = "#00868B") +         
        theme_classic() + 
        labs(title = "Fish passage structure: flow-based survival rate", 
          x = "Flow rate per gate (cfs)",
          y = "Survival rate")
      ggplotly(ggpl, tooltip = "text")    
    }
  }) 

  output$fpssurv_plot_text <- renderText({
    if (input$fpssurv_type == "sampler") {
      paste0("If no plot appears, check that the mean and variance parameters", 
      "you input are compatible with a beta distribution.\nThe orange dashed line represents the mean of the sample from the beta distribution, the bars represent the variance.")
    } else {
      paste0("")
    }
  })

  ### Run the FBWR model and generate results
  
  # Generate the initial reactive values object, including both raw/summarized
  fbw_results <- reactiveValues(
    full_results = NA,
    summary_results = NA
  )

  observeEvent({
    input$run_fbw_button
  }, {
    message("Running fbwR...")
    saveRDS(list(
      param_list = reactiveValuesToList(param_list), 
      ressim_dataframe = isolate(ressim_dataframe())),
      file = "nov192024_runFBW_paramList.Rds")
    fbw_results$full_results <- 
      # Call the runFBW function and populate the reactive value
      fbwR::runFBW(
        param_list = reactiveValuesToList(param_list),
        ressim = isolate(ressim_dataframe()),
        summarize = FALSE
      )
    # message("Run fbwR complete")
    # message(class(fbw_results$full_results))
    message("Done running fbwR...")
    updateCollapse(session, id = "fbw_results_bs",
      open = c("summary_res", "wyt_res")
    )
    fbw_results$summary_results <- fbwR::summarizeFBW(
      fbw_results$full_results)
    # message(head(fbw_results$summary_results[[1]]))
  })

  output$fbw_res_summary_monthly_surv <- renderFormattable({
    dt <- data.frame(fbw_results$summary_results[["monthly_summary"]])
    colnames(dt) <- c(
      "Month",
      "Exceedence: WSE (f)", "Exceedence: flow",
      "% Approaching (baseline)", "% Approaching (calculated)",
      "FPS flow", "Turbine flow", "RO flow", "Spillway flow", 
      "Population: Forebay", "Population: FPS", "Population: Turbines", 
      "Population: RO", "Population: Spillway", "Route Survival: FPS", 
      "Route Survival: Turbines", "Route Survival: RO",
      "Route Survival: Spillway"
    )
    dt <- dt %>%
      select(c("Month",
        "% Approaching (baseline)", 
        "% Approaching (calculated)",
        "Population: Forebay", "Population: FPS", "Population: Turbines", 
        "Population: RO", "Population: Spillway", 
        "Route Survival: FPS", 
        "Route Survival: Turbines", "Route Survival: RO",
        "Route Survival: Spillway")) 
        # %>%
      # mutate_at(c(
      #   "% Approaching (baseline)", 
      #   "% Approaching (calculated)",
      #   "Population: Forebay", "Population: FPS", "Population: Turbines", 
      #   "Population: RO", "Population: Spillway", 
      #   "Route Survival: FPS", 
      #   "Route Survival: Turbines", "Route Survival: RO",
      #   "Route Survival: Spillway"), round, digits = 3)

        # "% Approaching (baseline)" =, 
        # "% Approaching (calculated)",
        # "Population: Forebay", 
        # "Population: FPS", 
        # "Population: Turbines", 
        # "Population: RO", 
        # "Population: Spillway", 
        # "Route Survival: FPS", 
        # "Route Survival: Turbines", "Route Survival: RO",
        # "Route Survival: Spillway"))
      # options = list(
      #   pageLength = 12,
      #   # Allows the user to download the data
      #   dom = 'Bfrtip',
      #   buttons = c('csv', 'excel')), 
      # rownames = FALSE,
      # escape = FALSE,
      # extensions = 'Buttons')
      # ) %>%
      # formatPercentage(2:12, 2)
    #    %>%
    # gt() %>%
    # data_color(columns = 2:8, 
    #           colors = col_numeric(palette = c("white","navy"),
    #                                 domain = c(0,1))) %>%
    # data_color(columns = 9:12, 
    #           colors = col_numeric(palette = c("#fc8d59","#ffffbf","#91bfdb"),
    #                                 domain = c(0,1))) %>%
    # fmt_percent(columns = 2:12, scale_values = FALSE, decimals = 1)
    # formatRound(c(2:9), 2)
    formattable(
      x = dt,
      list(
        # `50% elevation exceed.` = color_bar(formattable_fill),
        # ,
        # Approaching
        area(col = 2:12) ~ function(x) percent(x / 100, digits = 3),
        `% Approaching (baseline)` = color_tile("white", 
          colorspace::lighten(sankeycols$approach, 0.3)),
        `% Approaching (calculated)` = color_tile("white", 
          colorspace::lighten(sankeycols$approach, 0.3)),
        # Population division
        `Population: Forebay` = color_tile("white", 
          colorspace::lighten(sankeycols$remain, 0.3)),
        `Population: FPS` = color_tile("white", 
          colorspace::lighten(sankeycols$fps, 0.3)),
        `Population: Turbines` = color_tile("white", 
          colorspace::lighten(sankeycols$turb, 0.3)),
        `Population: RO` = color_tile("white", 
          colorspace::lighten(sankeycols$ro, 0.3)),
        `Population: Spillway` = color_tile("white", 
          colorspace::lighten(sankeycols$spill, 0.3)),

        `Route Survival: FPS` = color_tile("white", 
          colorspace::lighten(sankeycols$fps, 0.3)),
        `Route Survival: Turbines` = color_tile("white", 
          colorspace::lighten(sankeycols$turb, 0.3)),
        `Route Survival: RO` = color_tile("white", 
          colorspace::lighten(sankeycols$ro, 0.3)),
        `Route Survival: Spillway` = color_tile("white", 
          colorspace::lighten(sankeycols$spill, 0.3))
        # `` = color_tile("white", sankeycols$surv),
        # `` = color_tile("white", sankeycols$surv),
        # `` = color_tile("white", sankeycols$surv),
        # `` = color_tile("white", sankeycols$surv)
      )
    )
  }) %>% bindEvent(input$run_fbw_button)

  output$fbw_res_summary_monthly_flow <- renderFormattable({
    dt <- data.frame(fbw_results$summary_results[["monthly_summary"]])
    colnames(dt) <- c(
      "Month",
      # stringr::str_wrap(
        "50% elevation exceed.",
      # stringr::str_wrap(
        "50% flow exceed.",
      "% Approaching (baseline)", "% Approaching (calculated)",
      "FPS flow", "Turbine flow", "RO flow", "Spillway flow", 
      "Population: Forebay", "Population: FPS", "Population: Turbines", 
      "Population: RO", "Population: Spillway", "Route Survival: FPS", 
      "Route Survival: Turbines", "Route Survival: RO",
      "Route Survival: Spillway"
    )
    dt <- dt %>%
      select(c("Month",
      "50% elevation exceed.",
      "50% flow exceed.",
      "FPS flow", "Turbine flow", "RO flow", "Spillway flow")) %>%
      mutate(
        "50% elevation exceed." = round(`50% elevation exceed.`, 2), 
        "50% flow exceed." = round(`50% flow exceed.` , 2),
        "FPS flow" = round(`FPS flow`), 
        "Turbine flow" = round(`Turbine flow`), 
        "RO flow" = round(`RO flow`), 
        "Spillway flow" = round(`Spillway flow`))
      
    formattable(
      x = dt,
      list(
        # area(col = 4:7) ~ color_tile("white", formattable_fill),
        area(col = 2) ~ color_tile("white", "orange"),
        area(col = 3) ~ color_tile("white", formattable_fill),
        # `50% elevation exceed.` = color_bar(formattable_fill),
        # ,
        area(col = 4) ~ color_tile("white", sankeycols$fps),
        area(col = 5) ~ color_tile("white", sankeycols$turb),
        area(col = 6) ~ color_tile("white", sankeycols$ro),
        area(col = 7) ~ color_tile("white", sankeycols$spill)
      )
    )
    # as.datatable(
    #   rownames = FALSE,
    #   escape = FALSE,
    #   options = list(pageLength = 12)) %>%
    # # formatPercentage(c(4,5, 10:18), 2) %>%
    # formatRound(c(2:7), 2)
  }) %>% bindEvent(input$run_fbw_button)

  # # Sankey diagrams
  # sankey_diagrams <- reactive({
    
  # }) 

  output$summary_sankey_flow <- renderPlotly({
    summary_sankey(
      fbwR_summary = fbw_results$summary_results[["monthly_summary"]],
      sankeycols = sankeycols)[[1]]
  })

  output$summary_sankey_surv <- renderPlotly({
    summary_sankey(
      fbwR_summary = fbw_results$summary_results[["monthly_summary"]],
      sankeycols = sankeycols)[[2]]
  })

  output$fbw_res_summary_wyt <- renderFormattable({
    dt <- data.frame(fbw_results$summary_results[["wyt_surv_summary"]]) %>%
      mutate(
        avg_surv = round(avg_surv, 3),
        p05_surv = round(p05_surv, 3),
        p10_surv = round(p10_surv, 3),
        p25_surv = round(p25_surv, 3),
        p50_surv = round(p50_surv, 3),
        p75_surv = round(p75_surv, 3),
        p90_surv = round(p90_surv, 3),
        p95_surv = round(p95_surv, 3)
      )
    colnames(dt) <- c(
      "Water year type", # "type"
      "Average survival", # "avg_surv",
      "5% quantile", # "p05_surv",
      "10% quantile", # "p10_surv",
      "25% quantile", # "p25_surv",
      "50% quantile", # "p50_surv",
      "75% quantile", # "p75_surv",
      "90% quantile", # "p90_surv",
      "95% quantile" # "p95_surv"
    )

    formattable(
      x = dt,
      list(
        `Average survival` = formatter("span", font.weight = "bold"),
        # `5% quantile` = formatter("span", x ~ percent(x / 100)),
        # `10% quantile` = formatter("span", x ~ percent(x / 100)),
        # `25% quantile` = formatter("span", x ~ percent(x / 100)),
        # `50% quantile` = formatter("span", x ~ percent(x / 100)),
        # `75% quantile` = formatter("span", x ~ percent(x / 100)),
        # `90% quantile` = formatter("span", x ~ percent(x / 100)),
        # `95% quantile` = formatter("span", x ~ percent(x / 100)),
        area(col = 2:9) ~ function(x) percent(x / 100, digits = 3),
        area(col = 2) ~ color_tile("white", formattable_fill_highlight),
        area(col = 3:9) ~ color_tile("white", formattable_fill)
      )
    )
  }) # %>% bindEvent(fbw_results$summary_results)

  output$fbw_res_full <- DT::renderDT(
    data.frame(fbw_results$full_results) %>%
      mutate(
        Date = format(Date, "%d-%b-%Y")
      ) %>%
      relocate(Month, .before = elev) %>%
      relocate(WaterYearType, .before = elev) %>%
      relocate(FPS_flow, .before = approaching_monthly) %>%
      relocate(Q.Tot, .after = FPS_flow) %>%
      relocate(F.NoPass, .before = approaching_daily_postDPE),
      # # %>%
      # rename(
      #   "Pool elevation" = elev,
      #   "Outflow (cfs)" = outflow_flow,
      #   "Turbine flow" = turb_flow,
      #   "RO flow" = RO_flow,
      #   "Spillway flow" = spill_flow,
      #   "Proportion of fish population approaching (month)" = approaching_monthly,
      #   "Proportion of fish population approaching (day)" = approaching_daily,
      #   "Daily DPE" = dpe, 
      #   "Proportion of fish entering the dam" = approaching_daily_postDPE,
      #   "Total calculated outflow (including the FPS)" = Q.Tot,
      #   "Proportion of fish that do not pass the dam (1-DPE)" = F.NoPass,
      #   "Proportion of fish passing via spillway" = F.spill,
      #   "Fish-bearing flow passing via turbines" = F.turb,
      #   "Fish-bearing flow passing via ROs" = F.RO,
      #   "Fish-bearing flow passing via FPS" = F.FPS,

      # ),
    options = list(
      lengthMenu = list(c(10, 40, 100), c('10', '40', '100')),
        pageLength = 10,
        scrollX = TRUE,
        # digits = 5,
        format = "0.000"
      ),
    rownames= FALSE
  ) %>% bindEvent(input$run_fbw_button)
  
  output$download_full_results <- downloadHandler(
    filename = function() {
      paste("fbwR_fullresults_", format(Sys.Date(), "%y_%m_%d"), ".csv", 
        sep = "")
    }, 
    content = function(file) {
      write.csv(x= fbw_results$full_results,
        file, row.names = FALSE)
    }
  )

  output$download_monthly_results <- downloadHandler(
    filename = function() {
      paste("fbwR_monthlySummarizedResults_", format(Sys.Date(), "%y_%m_%d"), ".csv", 
        sep = "")
    }, 
    content = function(file) {
      write.csv(x= fbw_results$summary_results[["monthly_summary"]],
        file, row.names = FALSE)
    }
  )

  output$download_wyt_results <- downloadHandler(
    filename = function() {
      paste("fbwR_WYTSummarizedResults_", format(Sys.Date(), "%y_%m_%d"), ".csv", 
        sep = "")
    }, 
    content = function(file) {
      write.csv(x= fbw_results$summary_results[["wyt_surv_summary"]],
        file, row.names = FALSE)
    }
  )

  ### Reactive navigation: ResSim compilation ----------------------------------
  observeEvent({
      input$ressim_create
    }, {
    updateCollapse(session, id = "ressim_bs",
      # Force closed the compiled inputs
      open = "ressim_preview_panel",
      close = c("ressim_about", "ressim_upload", "ressim_compile"))
  })

  observeEvent(input$ressim_input, {
    updateCollapse(session, id = "ressim_bs",
      # Force closed the compiled inputs
      open = "ressim_compile")
  })

  ### Reactive navigation: next buttons ----------------------------------------

  observeEvent(input$about_next, {
    updateNavbarPage(session, inputId = "fbw_navbar",
      selected = "ressim")
  })
  
  observeEvent(input$ressim_back, {
    updateNavbarPage(session, inputId = "fbw_navbar",
      selected = "app_howto")
  })
  observeEvent(input$ressim_next, {
    # if (!input$ressim_create) {
    # }
    # shinyjs::click("ressim_create")
    updateNavbarPage(session, inputId = "fbw_navbar",
      selected = "ressim_prev")
    shinyjs::click("ressim_create")
  })

  observeEvent(input$ressim_prev_back, {
    updateNavbarPage(session, inputId = "fbw_navbar",
      selected = "ressim")
  })
  observeEvent(input$ressim_prev_next, {
    updateNavbarPage(session, inputId = "fbw_navbar",
      selected = "fbwtemplate")
  })

  observeEvent(input$template_back, {
    updateNavbarPage(session, inputId = "fbw_navbar",
      selected = "ressim_prev")
  })
  observeEvent(input$template_next, {
    updateNavbarPage(session, inputId = "fbw_navbar",
      selected = "params_outlets")
  })

  observeEvent(input$outlets_back, {
    updateNavbarPage(session, inputId = "fbw_navbar",
      selected = "fbwtemplate")
  })
  observeEvent(input$outlets_next, {
    updateNavbarPage(session, inputId = "fbw_navbar",
      selected = "params_dpe")
  })

  observeEvent(input$dpe_back, {
    updateNavbarPage(session, inputId = "fbw_navbar",
      selected = "params_outlets")
  })
  observeEvent(input$dpe_next, {
    updateNavbarPage(session, inputId = "fbw_navbar",
      selected = "params_survival")
  })

  observeEvent(input$surv_back, {
    updateNavbarPage(session, inputId = "fbw_navbar",
      selected = "params_dpe")
  })
  observeEvent(input$surv_next, {
    updateNavbarPage(session, inputId = "fbw_navbar",
      selected = "run_fbw")
  })

  observeEvent(input$fbwrun_back, {
    updateNavbarPage(session, inputId = "fbw_navbar",
      selected = "params_survival")
  })
})

shinyApp(ui, server)