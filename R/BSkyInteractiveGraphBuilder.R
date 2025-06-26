
BSkyGraphBuilderInternalCore <- function(tempDatasetRDataFilePath = c(), graph_area_height = 600, graphBuilderTitle = c("BlueSky Statistics Interactive Graph Builder")){
	library(shiny)
	library(ggplot2)
	library(ggthemes)
	library(dplyr)
	library(plotly)
	library(scales)
	library(shinyFiles)
	library(shinyjs)
	


	tempDatasetRDataFilePath_to_load = tempDatasetRDataFilePath
	# Helper: Only the dataset object that are opened in Bsky datagrid at the time of 
	# BluSky Graphic Builder is launched into a R subprocess (write to .RData file from the 
	# BlueSky app parent R process)
	get_dataframes <- function(load_RData_file = TRUE) { 
		if(load_RData_file){
			load_tmp_dataset_file = if(length(tempDatasetRDataFilePath_to_load) == 0)paste0(tempdir(),'/bsky_open_dataframes.RData')else tempDatasetRDataFilePath_to_load
			loaded_dfs = load(load_tmp_dataset_file, envir = .GlobalEnv)
			assign("bsky_already_loaded_df_names", loaded_dfs, envir = .GlobalEnv)
		}else{
			loaded_dfs = bsky_already_loaded_df_names
		}
		return(loaded_dfs)
	}

	ui <- fluidPage(
		shinyjs::useShinyjs(),
		tags$head(
		  tags$style(HTML("
			/* Scroll style for the Plot tab */
			#sidebar-scroll {
			  max-height: 90vh;
			  overflow-y: auto;
			  padding-right: 10px;
			}
			
			/* Scroll style for the Plot tab */
			#themes-sidebar-scroll {
			  max-height: 90vh;
			  overflow-y: auto;
			  padding-right: 10px;
			} 
			
			#download-options-sidebar-scroll { 
			  max-height: 90vh;
			  overflow-y: auto;
			  padding-right: 10px;
			}
			
			#data-filter-sidebar-scroll {
			  max-height: 90vh;
			  overflow-y: auto;
			  padding-right: 10px;
			}

			/* Bold tab titles */
			.nav-tabs > li > a {
			  font-weight: bold;
			}

			/* Debug text box styling */
			#debug_text {
			  font-size: 16px;
			  font-family: 'Courier New', monospace;
			  background-color: transparent;
			  border: none;
			  padding: 0;
			  border-radius: 0;
			  overflow-x: auto;
			}
		  "))
		),

	titlePanel(graphBuilderTitle),
  sidebarLayout(
    sidebarPanel(
      width = 4,  # optional: set sidebar width
		# Arrange the button and tabsetPanel in a fluidRow
		#column(3,  # Allocate 3 columns for the button
               # div(
                 # style = "text-align: right; margin-top: 0px; padding-top: 0px; padding-bottom: 0px; line-height: 1;",
                 # actionButton(
                   # "quit_app",
                   # strong("Close and Quit"),
                   # style = "margin: 0px; padding: 2px 8px; font-weight: bold;
                                    # box-shadow: 2px 2px 5px rgba(0,0,0,0.3);"
                 # )
               # ),
			   
			  style = "display: flex; flex-direction: column; height: 100vh; padding-bottom: 0;",
			  
			  # Quit Button stays top
			  div(
				style = "text-align: right; margin: 0;",
				actionButton(
				  "quit_app",
				  strong("Close and Quit"),
				  style = "margin: 0px; padding: 2px 8px; font-weight: bold;
						   box-shadow: 2px 2px 5px rgba(0,0,0,0.3);"
				)
			  ),
        #)
		fluidRow(
			column(12,  # Allocate 12 out of 12 columns for the tabsetPanel for tab content to take the full width of the side pannel
               tabsetPanel(
                 id = "sidebar_tabs",
                 # ---- Tab 1: Plot Options ----
					tabPanel("Plots",
                          div(id = "sidebar-scroll",
                              tags$head(
                                tags$script(HTML("
                                Shiny.addCustomMessageHandler('closeWindow', function(x) {
                                window.close();
                                });
                            "))
                              ),

                              selectInput("plot_type", "Select Plot Type:",
							  choices = c("Scatter" = "scatter", "Line" = "line", "Boxplot" = "box",
										  "Histogram" = "histogram", "Bar" = "bar", "Dot" = "dot", "Pie" = "pie"),
							  selected = "scatter"
							),

							selectInput("dataset", "Choose Dataset", choices = get_dataframes()),

							uiOutput("y_ui"),
							uiOutput("x_ui"),
							uiOutput("group_ui"),

							uiOutput("position_ui"),  # for histogram and bar chart

							uiOutput("change_outlier_red_ui"),  # for boxplot
							uiOutput("jitter_ui"),  # for scatter and boxplot
							div(style = "margin-left: 30px;", uiOutput("jitter_spread_width_ui")),

							uiOutput("scatter_smooth_ui"),
							div(style = "margin-left: 30px;", uiOutput("loess_span_ui")),
							uiOutput("smooth_se_ui"),
							div(style = "margin-left: 30px;", uiOutput("smooth_confInt_ui")),

							hr(),

							uiOutput("facet_row_ui"),
							uiOutput("facet_col_ui"),
							uiOutput("facet_wrap_ui"),
							uiOutput("facet_scales_ui"),

							conditionalPanel(
							  condition = "['scatter', 'line', 'box', 'histogram', 'bar', 'dot'].includes(input.plot_type)",
							  actionButton("reset_facet_values", "Clear All Facet Values")
							),

							hr(),
							uiOutput("size_ui"),
							uiOutput("shape_ui"),
							div(style = "margin-left: 30px;", uiOutput("stackratio_ui")),  # for dot plot

							hr(),
							uiOutput("x_scale_ui"),
							uiOutput("y_scale_ui"),

							hr(),
							uiOutput("hlines_ui"),
							uiOutput("hline_labels_ui"),
							uiOutput("vlines_ui"),
							uiOutput("vline_labels_ui"),

							hr(),
							hr()
                          )
					),
					
					# ---- Tab 2: Themes & Options ----
					tabPanel("Themes & Options",
					  div(
						id = "themes-sidebar-scroll",
						selectInput("plot_theme", "Select Plot Theme:",
						  choices = c(
							"Default (Grey)" = "default",

							# ggplot2 themes
							"Classic" = "ggplot2::theme_classic",
							"Minimal" = "ggplot2::theme_minimal",
							"Light" = "ggplot2::theme_light",
							"Void" = "ggplot2::theme_void",
							"Black & White" = "ggplot2::theme_bw",
							"Linedraw" = "ggplot2::theme_linedraw",
							"Grey" = "ggplot2::theme_grey",

							# ggthemes
							"Economist" = "ggthemes::theme_economist",
							"Tufte" = "ggthemes::theme_tufte",
							"Excel" = "ggthemes::theme_excel",
							"Few" = "ggthemes::theme_few",
							"WSJ" = "ggthemes::theme_wsj",
							"Solarized Light" = "solarized_light",
							#"Solarized Dark" = "solarized_dark",
							"Stata" = "ggthemes::theme_stata",
							"Base" = "ggthemes::theme_base",
							"Highcharts" = "ggthemes::theme_hc"
						  ),
						  selected = "default"
						),
						
						hr(),
						
						# Slant Angles for Axis Text
						numericInput("x_angle", "Angle to slant x-axis text values (degrees - default 0 for no slanting):", value = 0, min = 0, max = 360, step = 5),
						numericInput("y_angle", "Angle to slant y-axis text values (degrees - default 0 for no slanting):", value = 0, min = 0, max = 360, step = 5),

						hr(),
						
						# Optional Plot Title and Axis Labels
						textInput("plot_title", "Plot Title (optional):", ""),
						textInput("x_axis_label", "X-axis Label (optional):", ""),
						textInput("y_axis_label", "Y-axis Label (optional):", ""),

						hr(),
						
						selectInput("axis_date_format", "Date Format for Date/Time axis",
						  choices = c("", 
								  "%Y-%m-%d (fullyear-month-day)" = "%Y-%m-%d", 
					              "%m-%d-%Y (month-day-fullyear)" = "%m-%d-%Y", 
								  "%d-%m-%Y (day-month-fullyear)" = "%d-%m-%Y", 
								  "%Y-%m (fullyear-month)" = "%Y-%m", 
								  "%m-%Y (month-fullyear)" = "%m-%Y", 
								  "%m-%d (month-day)" = "%m-%d", 
								  "%d-%m (day-month)" = "%d-%m",
								  "%b-%d (short month name-day of the month)" = "%b-%d", 
								  "%d-%b (day of the month-short month name)" = "%d-%b",
								  "%m (month only as number)" = "%m",
								  "%b (month only as short month name)" = "%b",
								  "%B (month only as full month name)" = "%B",
								  "%Y (year only as four digit year)" = "%Y",
								  "%y (year only as two digit year)" = "%y",
								  "%d (day only as zero-padded number)" = "%d",
								  "%a (weekday short name)" = "%a",
								  "%A (weekday full name)" = "%A"
							),
							 selected = ""
						),
						hr(),
						selectInput("axis_time_format", "Time Format for Date/Time axis",
						  choices = c("", 
								  "%H:%M", 
								  "%H:%M:%S", 
								  "%H"
							),
							 selected = ""
						),
						hr(),
						hr(),
						hr(),
						hr()
					  )
					),

					# ---- Tab 3: Data filters ----
					tabPanel("Download Options",
					  div(
					    id = "download-options-sidebar-scroll",
						#style = "height: 100%; display: flex; flex-direction: column;",
						# tags$h2("Static Labels"),
						# tags$p("This is a simple paragraph label."),
						# tags$strong("This is a bold label."),
						# tags$em("This is an italicized label."),
						
						tags$strong("Select directory to save (plot image, clicked points data table, etc.)"),
						shinyDirButton("directory", "Click to Browse Directory", "Choose Dir"),
						verbatimTextOutput("chosen_directory"),
						
					  conditionalPanel(
						condition = "['scatter'].includes(input.plot_type)",
						hr(),
						textInput("file_name", "File name to save the clicked points data (only for scatter plot):", "clicked_data_"),
						radioButtons("file_type", "Choose File Type:",
						  choices = c("CSV" = "csv", "RData" = "rdata"),
						  selected = "csv", inline = TRUE
						)
					  ),
					  
					  # Default width = 8, height = 6, dpi = 300
					    hr(),
						tags$strong("The following options are only applicable for the downloaded plot image"),
						hr(),
						numericInput("downloaded_plot_width", "Downloaded plot width", value = 8, min = 0.1), #, step = 0.1)
						numericInput("downloaded_plot_height", "Downloaded plot height", value = 6, min = 0.1), #, step = 0.1)
						#textInput("downloaded_plot_width", "Downloaded plot width", "8"),
						#textInput("downloaded_plot_height", "Downloaded plot height", "6"),
						selectInput(
							inputId = "downloaded_plot_height_width_units",
							label = "Downloaded plot width height unit",
							choices = c("in", "cm", "mm", "px"),
							selected = "in"
						),
						#textInput("downloaded_plot_resolution", "Downloaded plot resolution (dpi)", "300"),
						#textInput("downloaded_plot_scale", "Downloaded plot scale", "1"),
						numericInput("downloaded_plot_resolution", "Downloaded plot resolution (dpi)", value = 300, min = 0.1), #, step = 0.1)
						numericInput("downloaded_plot_scale", "Downloaded plot scale", value = 1, min = 0.1), #, step = 0.1)
						
						hr(),
						hr()
					  )
					),
					
					# ---- Tab 4: Data filters ----
					tabPanel("Data Filters",
					  div(
					    id = "data-filter-sidebar-scroll",
						#style = "height: 100%; display: flex; flex-direction: column;",
						hr(),
						uiOutput("filter_column_selector"),
						div(style = "flex-grow: 1; overflow-y: auto;",
							uiOutput("filters_ui")
						),
						hr(),
						actionButton("reset_filters", "Clear Filters"),
						hr(),
						hr(),
						hr(),
						hr(),
						hr()
					  )
					)

               )

			)
		)
    ),

	mainPanel(
		plotlyOutput("plot", height = paste0(graph_area_height,"px")), #"600px" 
		tags$div( # Container for the flex items
			style = "display: flex; justify-content: flex-end; align-items: center;", # Align download elements to the right and center vertically
			tags$div( # Container for conditional header (left-aligned)
				style = "margin-right: auto;", # Push this div to the left
				conditionalPanel(
					condition = "['scatter'].includes(input.plot_type) && output.has_selected_rows",
					tags$div( # Conditional header content
						style = "display: flex; align-items: center; gap: 10px;",
						tags$strong(textOutput("clicked_rows_label", inline = TRUE)),
						actionButton("clear_table", "Clear Click Table"),
						actionButton("save_table", "Save Clicked Data")
					)
				)
			),
			uiOutput("download_section")
			# tags$div( # Container for download elements (right-aligned)
				# style = "display: flex; align-items: center; gap: 10px;",
				# #downloadButton("download_plot", "Download Plot"),
				# #downloadButton("download_plot", "Download Plot", disabled = TRUE),
				# # Tooltip-enabled wrapper
				  # tags$div(
					# title = "Please select a directory to enable download",  # tooltip text
					# downloadButton("download_plot", "Download Plot")
				  # ),
				# selectInput(
					# inputId = "image_format",
					# label = "",
					# choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg", "JPEG" = "jpeg"),
					# selected = "png",
					# width = "100px" # Adjust as needed
				# )
			# )
		),
		conditionalPanel( # Place the table separately
			condition = "['scatter'].includes(input.plot_type) && output.has_selected_rows",
			tableOutput("click_table")
		),
		verbatimTextOutput("debug_text"),
		#actionButton("save_full_html", "Save Output as HTML"),
		# Add CSS to adjust the height of the select input AND the download button
		tags$head(
		  tags$style(HTML("
			.selectize-input {
			  height: 34px;  /* Adjust this value to match your desired height */
			}
			#download_plot { /* Target the download button by its ID */
			   height: 34px; /* Set the same height as the select input */
			}
		  "))
		)
	)
  )
)



	server <- function(input, output, session) {
		
		# This handles manual X click on browser window/tab to close
		session$onSessionEnded(function() {
			stopApp()
		})
		
		# This handles manual quit button close
		observeEvent(input$quit_app, {
			session$sendCustomMessage("closeWindow", list())
			stopApp()
		})
		
		###########################################################################
		
		current_plot_obj <- reactiveVal(NULL)  # holder
		
		selected_rows <- reactive({
		  df <- filtered_data()
		  sel <- df[df$row_id %in% selected_ids(), ]  # selected_ids() should already be defined in your app
		  if (nrow(sel) == 0) return(NULL)
		  sel
		})
		
		observeEvent(input$save_full_html, {
		  #req(current_plot_obj())  # Your reactive plot and table
		  req(current_plot_obj())  # Require the plot only — that's essential

		  # Allow clicked_table to be NULL
		  clicked_tbl <- tryCatch({
			selected_rows()
		  }, error = function(e) NULL)
		  
		  debug_txt <- tryCatch({
			  get_debug_text()
		  }, error = function(e) "")

		  #tmp_file <- tempfile(fileext = ".html")
		  output_report_file = "C:\\Users\\mashe\\OneDrive\\Documents\\Documents\\workfolder\\Graph builder files\\Bsky_Graph_Builder_output.html"

		 #showNotification(paste("Input file ", output_report_file))
		 
			Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/Pandoc")
			#rmarkdown::pandoc_available() # Should return TRUE
			#rmarkdown::pandoc_version()

		  rmarkdown::render(
			input = "C:\\Users\\mashe\\OneDrive\\Documents\\Documents\\workfolder\\Graph builder files\\Bsky_Graph_Builder_output_template.Rmd",
			output_file = output_report_file,
			params = list(
			  plot_obj = current_plot_obj(),
			  clicked_table = clicked_tbl,
			  debug_text = debug_txt
			),
			envir = new.env(parent = globalenv())
		  )

		  # Optional: Offer a download
		  showNotification(paste("Output HTML saved at", output_report_file), type = "message")
		  #browseURL(output_report_file)  # Automatically opens it in browser
		})
		
		output$has_selected_rows <- reactive({
		  df <- filtered_data()
		  nrow(df[df$row_id %in% selected_ids(), ]) > 0
		})
		
		outputOptions(output, "has_selected_rows", suspendWhenHidden = FALSE)
		
		# To capture the point details when clicked on the points on Scatter and Line plots
		clicked_points <- reactiveVal(data.frame())
		
		###########################################################################
		
		# Setup for shinyFiles directory selection
		shinyDirChoose(input, "directory", roots = c(home = normalizePath("~")), session = session)

		chosen_dir <- reactiveVal(NULL)

		observeEvent(input$directory, {
		  if (!is.integer(input$directory)) {
			path <- parseDirPath(c(home = normalizePath("~")), input$directory)
			chosen_dir(path)
		 }
		})

		output$chosen_directory <- renderText({
		  dir <- chosen_dir()
		  if (is.null(dir)) "No directory selected (the selected directory will appear here)"
		  else paste("Directory:", dir)
		})
		
		###########################################################################

		# The following steps are needed to reset the ref line UI elements
		# Since UI elements cannot be reset unless they are visible for a plot_type being
		# used to determine the different refline UI elements to be visible
		
		# Step 1: Internal storage (internal trick needed since 
		# standard reset event mechanism does not work
		current_hlines <- reactiveVal("")
		current_hline_labels <- reactiveVal("")
		current_vlines <- reactiveVal("")
		current_vline_labels <- reactiveVal("")

		# Step 2: Watch inputs if they exist
		observe({
		  if (!is.null(input$hlines)) {
			current_hlines(input$hlines)
		  } else {
			current_hlines("")
		  }
		  
		  if (!is.null(input$hline_labels)) {
			current_hline_labels(input$hline_labels)
		  } else {
			current_hline_labels("")
		  }
		  
		  if (!is.null(input$vlines)) {
			current_vlines(input$vlines)
		  } else {
			current_vlines("")
		  }
		  
		  if (!is.null(input$vline_labels)) {
			current_vline_labels(input$vline_labels)
		  } else {
			current_vline_labels("")
		  }
		})

		# Hard reset based on the plot_type - specially these plot type hide
		# one or more ref line UI elements whcih cannot be reset thouse
		# standard reset machanism if UI element(s) is not visible for a plot type
		observeEvent(input$plot_type, {
		  if (input$plot_type %in% c("box", "bar", "histogram", "dot", "pie")) {
			current_hlines("")
			current_vlines("")
			current_hline_labels("")
			current_vline_labels("")
		  }
		})

		
		get_debug_text <- function() {
		  df <- current_data()

		  capture.output({
			  cat("Current Selections:\n")
			  cat("plot_type =", input$plot_type)
			  cat(", dataset =", input$dataset, "\n")
			  cat(", dataset_class =", class(df), "\n")

			  if (!is.null(input$x_var) && input$x_var != "" && input$x_var %in% names(df)) {
				cat("x_variable =", input$x_var, ", x_type =", class(df[[input$x_var]]),"\n")
				if (is.numeric(df[[input$x_var]])) {
				  cat("  x_min =", min(df[[input$x_var]], na.rm = TRUE),
					  ", x_max =", max(df[[input$x_var]], na.rm = TRUE),
					  ", x_mean =", mean(df[[input$x_var]], na.rm = TRUE),
					  ", x_stdDev =", sd(df[[input$x_var]], na.rm = TRUE),
					  ", x_obs =", length(df[[input$x_var]]),
					  ", x_NAs =", sum(is.na(df[[input$x_var]])), "\n")
				} else if (is.character(df[[input$x_var]])|| is.factor(df[[input$x_var]])) {
				  cat("  x_unique =", length(unique(df[[input$x_var]][!is.na(df[[input$x_var]])])),
					  ", x_obs =", length(df[[input$x_var]]),
					  ", x_NAs =", sum(is.na(df[[input$x_var]])), "\n")
				}else if (inherits(current_data()[[input$x_var]], c("Date", "POSIXct"))) {
				  cat("  x_min =", format(min(df[[input$x_var]], na.rm = TRUE)),
					  ", x_max =", format(max(df[[input$x_var]], na.rm = TRUE)),
					  ", x_median =", format(median(df[[input$x_var]], na.rm = TRUE)),
					  ", x_obs =", length(df[[input$x_var]]), 
					  ", x_NAs =", sum(is.na(df[[input$x_var]])), "\n")
				}
			  }

			  if (!is.null(input$y_var) && input$y_var != "" && input$y_var %in% names(df)) {
				cat("y_variable =", input$y_var, ", y_type =", class(df[[input$y_var]]),"\n")
				if (is.numeric(df[[input$y_var]])) {
				  cat("  y_min =", min(df[[input$y_var]], na.rm = TRUE),
					  ", y_max =", max(df[[input$y_var]], na.rm = TRUE),
					  ", y_mean =", mean(df[[input$y_var]], na.rm = TRUE),
					  ", y_stdDev =", sd(df[[input$y_var]], na.rm = TRUE),
					  ", y_obs =", length(df[[input$y_var]]),
					  ", y_NAs =", sum(is.na(df[[input$y_var]])), "\n")
				} else if (is.character(df[[input$y_var]])|| is.factor(df[[input$y_var]])) {
				  cat("  y_unique =", length(unique(df[[input$y_var]][!is.na(df[[input$y_var]])])),
					  ", y_obs =", length(df[[input$y_var]]),
					  ", y_NAs =", sum(is.na(df[[input$y_var]])), "\n")
				}else if (inherits(current_data()[[input$y_var]],  c("Date", "POSIXct"))) {
					cat("  y_min =", format(min(df[[input$y_var]], na.rm = TRUE)),
						", y_max =", format(max(df[[input$y_var]], na.rm = TRUE)),
						", y_median =", format(median(df[[input$y_var]], na.rm = TRUE)),
						", y_obs =", length(df[[input$y_var]]),
						", y_NAs =", sum(is.na(df[[input$y_var]])), "\n")
				}
			  }

			if (!is.null(input$group_var) && input$group_var != "" && input$group_var %in% names(df)) {
			  cat("group_variable =", input$group_var, ", group_type =", class(df[[input$group_var]]), "\n")
			  cat("  group_obs =", length(df[[input$group_var]]),
				", group_NAs =", sum(is.na(df[[input$group_var]])), "\n")
			}
			  cat("row_facet_variable =", input$facet_row)
			  cat(", column_facet_variable =", input$facet_col)
			  cat(", wrap_facet_variable =", input$facet_wrap, "\n")
			  cat("x_axis_scale =", input$x_scale)
			  cat(", y_axis_scale =", input$y_scale)
		  })
		}
		
		output$debug_text <- renderPrint({
		  cat(get_debug_text(), sep = "\n")
		})  
	  

		# current_data holds the all the data for the dataset that has been curently selected 
		current_data <- reactive({
			req(input$dataset)
			df = get(input$dataset, envir = .GlobalEnv)
			df
		})

		observe({
			updateSelectInput(session, "dataset", choices = get_dataframes(load_RData_file = FALSE))
		})

		observeEvent(input$reset_filters, { 
			updateSelectInput(session, "filter_columns", selected = character(0))
		})
	  

		# For hsitogram and par chart to chow choices for stacked or side-by-side 
		# if a group variable is chosen
		output$position_ui <- renderUI({
		req(input$plot_type)
		  if (input$plot_type %in% c("bar", "histogram")) {
			selectInput(
			  "position", 
			  "Bar/Histogram Positioning (stacked or side-by-side)", 
			  choices = c("stack", "dodge"),
			  selected = "dodge"
			)
		  } else {
			return(NULL)
		  }
		}) 
		
		# Boxplot option - checkbox whether to remove outlier points
		output$change_outlier_red_ui <- renderUI({
		req(input$plot_type == 'box')
		  checkboxInput(
			"change_outlier_red", 
			"Show Outlier Points Red", 
			value = FALSE
		  ) 
		})
	  
		# First: render dropdown for smoothing method
		#Shiny does not show "" selection in the dropdown list unfortunately
		output$scatter_smooth_ui <- renderUI({
		req(input$plot_type)
		  if (input$plot_type %in% c("scatter")) {
			selectInput(
			  "scatter_smooth", 
			  "Select a Fit (Smoothing) model", 
			  choices = c("", "lm", "glm", "gam", "loess"),
			  #choices = c("lm", "glm", "gam", "loess", "None" = ""),
			  selected = ""
			  #choices = c("None" = "", "lm" = "lm", "glm" = "glm", "gam" = "gam", "loess" = "loess"),
			  #selected = input$scatter_smooth %||% ""
			)
		  } else {
			return(NULL)
		  }
		})
	  

		# Render LOESS span slider only when "loess" fitted line is selected
		output$loess_span_ui <- renderUI({
		req(input$plot_type == 'scatter', input$scatter_smooth == "loess")
		  sliderInput(
			"loess_span", 
			"Span", 
			min = 0.10, 
			max = 0.90, 
			value = 0.75, 
			step = 0.05
		  )
		}) 
	  
		# Render checkbox to show or not the ConfInt around the fitted line
		output$smooth_se_ui <- renderUI({
		req(input$plot_type == 'scatter', input$scatter_smooth != '')
		  checkboxInput(
			"std_error_band", 
			"Show ConfInt Band", 
			value = FALSE
		  ) 
		})
	  
		# Render the ConfInt selection slider around the fitted line if only the above checkbox is selected
		output$smooth_confInt_ui <- renderUI({
		req(input$plot_type == 'scatter', input$scatter_smooth != '', input$std_error_band == TRUE)
		   sliderInput(
			"smooth_confInt", 
			"ConfInt", 
			min = 0.50, 
			max = 0.9999, 
			value = 0.95, 
			step = 0.0001
		  )
		})
		
		# For Scatter and Box plot jitter points
		# Render checkbox to show or not the jitter points for box plot
		output$jitter_ui <- renderUI({
		req(input$plot_type %in% c('scatter','box'))
		  checkboxInput(
			"jitter", 
			"Show Jitter Points", 
			value = FALSE
		  ) 
		})
	  
		# Render the ConfInt selection slider around the fitted line if only the above checkbox is selected
		output$jitter_spread_width_ui <- renderUI({
		req(input$plot_type %in% c('scatter','box'), input$jitter == TRUE)
		   sliderInput(
			"jitter_spread_width", 
			"Jitter Point Spread Width", 
			min = 0, 
			max = 1, #0.4, 
			value = 0.2, 
			step = 0.05
		  )
		})
		
		# Render the spread selection slider for the dot plot points to spread vertically
		# output$stackratio_ui<- renderUI({
		# req(input$plot_type %in% c('dot'))
		   # sliderInput(
			# "stackratio", 
			# "Enter a positive number (default 6) to spread the points vertically", 
			# min = 0, 
			# max = 10, 
			# value = 6, 
			# step = 1
		  # )
		# })
		
		
		# Use the following if the slider to choose confint value to be changed to a input text box
		# output$smooth_confInt_ui <- renderUI({
		# req(input$plot_type == 'scatter', input$scatter_smooth != '', input$std_error_band == TRUE)
		  # numericInput(
			# "smooth_confInt_ui", 
			# "ConfInt", 
			# min = 0.50, 
			# max = 0.9999, 
			# value = 0.95, 
			# step = 0.0001
		  # )
		# })


		output$x_scale_ui <- renderUI({
		  req(input$plot_type)
		  if (input$plot_type %in% c("dot", "pie")) return(NULL)
		  selectInput("x_scale", 
					  "X Axis Scale", 
					  choices = c("continuous", "log10", "log2", "log"), 
					  selected = "continuous")
		})

		output$y_scale_ui <- renderUI({
		  req(input$plot_type)
		  if (input$plot_type %in% c("dot", "pie")) return(NULL)
		  selectInput("y_scale", 
					  "Y Axis Scale", 
					  choices = c("continuous", "log10", "log2", "log"), 
					  selected = "continuous")
		})

	  
		# Trigger the following code whenever a dataset selection change occurs from the dataset selection dropdown
		observeEvent(input$dataset, {
			data_vars <- names(current_data()) 
			
			#########################################################################
			# The following was put in place to overcome the issue of plot (besides x and y axis labels only) 
			# not being drawn after changing dataset but not changing the plot type the for histogram and bar i.e. 
			# for the plot_type only when x-var is used but no y-var. 
			# The following statement forces the plot type to reset to scatter for any dataset changes
			# so user is forced, not ideal user experience, to change the plot type to hsitogram, bar, or pie
			# for the plot to be shown correctly
			#########################################################################
			updateSelectInput(session, "plot_type", selected = "scatter")
			
			updateSelectInput(session, "facet_row", choices = c("", data_vars), selected = "")
			updateSelectInput(session, "facet_col", choices = c("", data_vars), selected = "")
			updateSelectInput(session, "facet_wrap", choices = c("", data_vars), selected = "")
			updateSelectInput(session, "facet_scales", selected = "fixed")
			#updateRadioButtons(session, "facet_type", selected = "grid")

			# Reset all the UI elements for the reflines (only works when any given UI element is visible
			# The hidden UI elements cannot be reset through this mechanism 
			updateTextInput(session, "hlines", value = "")
			updateTextInput(session, "hline_labels", value = "")
			updateTextInput(session, "vlines", value = "")
			updateTextInput(session, "vline_labels", value = "")

			# Reset axis scales when dataset selection changes
			updateSelectInput(session, "x_scale", selected = "continuous")
			updateSelectInput(session, "y_scale", selected = "continuous")
			updateSelectInput(session, "scatter_smooth", selected = "")
			#updateSelectInput(session, "scatter_smooth", choices = c("None" = "", "lm" = "lm", "glm" = "glm", "gam" = "gam", "loess" = "loess"), selected = input$scatter_smooth %||% "")

			if (!is.null(input$change_outlier_red)) {
				updateCheckboxInput(session, "change_outlier_red", value = FALSE) 
			}
			
			if (!is.null(input$jitter)) {
				updateCheckboxInput(session, "jitter", value = FALSE)
			}
			
			updateNumericInput(session, "x_angle", value = 0)
			updateNumericInput(session, "y_angle", value = 0)

			updateTextInput(session, "plot_title", value = "")
			updateTextInput(session, "x_axis_label", value = "")
			updateTextInput(session, "y_axis_label", value = "")
  
			updateSelectInput(session, "filter_columns", selected = character(0))

			#Reset the current selection print output
			verbatimTextOutput("debug_text")

			# Reset internal values that trigger plot and debug logic
			#output$plot <- renderPlotly({ return(NULL) })
			#output$debug_text <- renderPrint({ return(NULL) })
			
			#Clearing the clicked points table
			selected_ids(integer(0))
		})
	  
		# Trigger the following code whenever a plot type selection change occurs from the plot type selection dropdown
		observeEvent(input$plot_type, {
			data_vars <- names(current_data())

			updateSelectInput(session, "facet_row", choices = c("", data_vars), selected = "")
			updateSelectInput(session, "facet_col", choices = c("", data_vars), selected = "")
			updateSelectInput(session, "facet_wrap", choices = c("", data_vars), selected = "")
			updateSelectInput(session, "facet_scales", selected = "fixed")
			#updateRadioButtons(session, "facet_type", selected = "grid")

			updateTextInput(session, "hlines", value = "")
			updateTextInput(session, "hline_labels", value = "")
			updateTextInput(session, "vlines", value = "")
			updateTextInput(session, "vline_labels", value = "")

			if (!is.null(input$change_outlier_red)) {
			updateCheckboxInput(session, "change_outlier_red", value = FALSE)
			}
			
			if (!is.null(input$jitter)) {
				updateCheckboxInput(session, "jitter", value = FALSE)
			}

			# if (!(input$plot_type %in% c("scatter", "line", "box"))) {
			# updateTextInput(session, "hlines", value = "")
			# updateTextInput(session, "hline_labels", value = "")
			# }

			# if (!(input$plot_type %in% c("scatter", "line"))) {
			# updateTextInput(session, "vlines", value = "")
			# updateTextInput(session, "vline_labels", value = "")
			# }
			
			updateNumericInput(session, "x_angle", value = 0)
			updateNumericInput(session, "y_angle", value = 0)

			updateTextInput(session, "plot_title", value = "")
			updateTextInput(session, "x_axis_label", value = "")
			updateTextInput(session, "y_axis_label", value = "")

			# Reset internal values that trigger plot and debug logic
			verbatimTextOutput("debug_text")
			#output$plot <- renderPlotly({ return(NULL) })
			#output$debug_text <- renderPrint({ return(NULL) })
			
			#Clearing the clicked points table
			selected_ids(integer(0))
		})
	  
		#facet_resetting <- reactiveVal(FALSE)
		
		observeEvent(input$reset_facet_values, { 
			#facet_resetting(TRUE)
			
			# example best practice of a failsafe check. If it fails it will not proceed any further within this observeEvent() 
			validate(
				need(current_data(), "No dataset selected or available.")
			)
			
			data_vars <- names(current_data())
			
			updateSelectInput(session, "facet_row", choices = c("", data_vars), selected = "")
			updateSelectInput(session, "facet_col", choices = c("", data_vars), selected = "")
			updateSelectInput(session, "facet_wrap", choices = c("", data_vars), selected = "")
			updateSelectInput(session, "facet_scales", selected = "fixed")
			#updateRadioButtons(session, "facet_type", selected = "grid")
			
			# Delay to allow inputs to settle
			#later::later(function() facet_resetting(FALSE), 0.1)
		})
		
		gen_var_ui <- function(var_id, label) {
			renderUI({
				req(input$dataset, input$plot_type)
				
				########################
				# Custom logic: which inputs to skip/hide from the UI for specific plot types
				#############
				
				if (var_id %in% c("x_var", "y_var", "x_scale", "y_scale", "facet_row","facet_col","facet_wrap") && input$plot_type == "pie") {
				  return(NULL)
				}
				
				
				if (var_id %in% c("y_var") && (input$plot_type %in% c("histogram", "bar", "dot"))) {
				  return(NULL)
				}
				
				if (var_id %in% c("size_var", "shape_var") && input$plot_type != "scatter") {
				  return(NULL)
				}
				
				if (var_id %in% c("hlines", "hline_labels")) {
					if (!(input$plot_type %in% c("scatter", "line", "box"))) {
					  return(NULL)
					} else {
					  return(textInput(var_id, label))
					}
				}
			  
				if (var_id %in% c("vlines", "vline_labels")) {
					if (!(input$plot_type %in% c("scatter", "line"))) {
					  return(NULL)
					} else {
					  return(textInput(var_id, label))
					}
				}

				# Default for other variables
				choices <- c("", names(current_data()))
				selectInput(var_id, label, choices = choices, selected = "")
			})
		}

		# Even though the following code block is not wrapped in observe() or observeEvent(), 
		# those output$... <- renderUI(...) calls are reactive blocks evaluated and re-evaluated 
		# automatically by Shiny based on the reactive dependencies inside gen_var_ui()
		# i.e. effectively it is doing output$x_ui <- renderUI({ ... })
		output$x_ui <- gen_var_ui("x_var", "X Variable")
		output$y_ui <- gen_var_ui("y_var", "Y Variable")
		output$group_ui <- gen_var_ui("group_var", "Grouping Variable (Category)")
		output$facet_row_ui <- gen_var_ui("facet_row", "Facet Row (Category)")
		output$facet_col_ui <- gen_var_ui("facet_col", "Facet Column (Category)")
		output$facet_wrap_ui <- gen_var_ui("facet_wrap", "Facet Wrap (Category) if selected, Facet Row and Column will be ignored")
		output$size_ui <- gen_var_ui("size_var", "Point Size")
		output$shape_ui <- gen_var_ui("shape_var", "Point Shape(Category)")

		output$hlines_ui <- gen_var_ui("hlines", "Horizontal Lines comma separated values 5, 7 (ignored if y-axis non-numeric)")
		output$hline_labels_ui <- gen_var_ui("hline_labels", "Horizontal Line Labels (optional) comma separated values Lower, Upper")
		output$vlines_ui <- gen_var_ui("vlines", "Vertical Lines comma separated values 3, 8 (ignored if x-axis non-numeric)")
		output$vline_labels_ui <- gen_var_ui("vline_labels", "Vertical Line Labels (optional) comma separated values Low, High")

		#output$x_scale <- gen_var_ui("x_scale", "X Axis Scale")
		#output$y_scale <- gen_var_ui("y_scale", "Y Axis Scale")
	  
		output$facet_scales_ui <- renderUI({
		  if (!(input$plot_type %in% c("pie")))
			selectInput("facet_scales", "Facet Scales", choices = c("none" = "fixed", "free_x", "free_y", "free_x_and_y" = "free"), selected = "fixed")
		})

		output$filter_column_selector <- renderUI({
		#selectizeInput("filter_columns", paste0("currently selected dataset: ", input$dataset, hr(), "Choose one or more variables (after clicking on the empty box below) to filter the currently selected dataset before using the filtered data for plotting. The selection of variable filters, if any, will be removed automatically whenever the dataset selected in the Plots tab changes"),
		#	choices = names(current_data()), multiple = TRUE)   
			selectizeInput(
			  "filter_columns",
			  label = HTML(paste0(
				"Currently selected dataset: <strong>", input$dataset, "</strong><br><br>",
				"Choose one or more variables (after clicking on the empty box below) to filter the dataset before plotting.<br><br>",
				"Any filter selection will be removed automatically if you change the dataset"
			  )),
			  choices = names(current_data()), 
			  multiple = TRUE
			)
		})

		output$filters_ui <- renderUI({
		  req(input$filter_columns)
		  bsky_temp_df <- current_data()

		  lapply(input$filter_columns, function(col) {
			inputId <- paste0("filter_", col)
			col_data <- bsky_temp_df[[col]]
			
			# if (inherits(col_data, c("numeric", "integer", "POSIXct", "Date"))) {
				# # Use sliderInput for date range
				# rng <- range(col_data, na.rm = TRUE)
			# }
				
			if (inherits(col_data, "POSIXct")) {
				rng <- range(col_data, na.rm = TRUE)
				sliderInput(inputId, col,
						  min = rng[1], max = rng[2], value = rng,
						  timeFormat = "%Y-%m-%d %H:%M:%S")
			} else if (inherits(col_data, "Date")) {
				rng <- range(col_data, na.rm = TRUE)
				sliderInput(inputId, col,
						  min = rng[1], max = rng[2], value = rng,
						  timeFormat = "%Y-%m-%d")
			}else if (is.numeric(col_data)) {
				rng <- range(col_data, na.rm = TRUE)
				sliderInput(inputId, col, min = rng[1], max = rng[2], value = rng)
			} else {
				checkboxGroupInput(inputId, col,
						 choices = sort(unique(col_data)),
						 selected = unique(col_data))
			}
		  })
		})


		observeEvent(input$reset_filters, {
			lapply(input$filter_columns, function(col) {
			  inputId <- paste0("filter_", col)
			  updateCheckboxGroupInput(session, inputId, selected = unique(current_data()[[col]]))
			})
		})

		filtered_data <- reactive({
			bsky_temp_df = NULL
			bsky_temp_df <- current_data()
			
			if(!("row_id" %in% names(bsky_temp_df))){
				bsky_temp_df$row_id <- 1:nrow(bsky_temp_df)
				# Reorder columns to make 'row_id' the first
				bsky_temp_df <- bsky_temp_df[, c("row_id", setdiff(names(bsky_temp_df), "row_id"))]
			}
			
			#df_names = names(bsky_temp_df)
			
			# if (!is.null(input$filter_columns)) {
			  # for (col in input$filter_columns) {
				# #bsky_temp_df[[col]] <- as.factor(bsky_temp_df[[col]])
				# inputId <- paste0("filter_", col)
				# val <- input[[inputId]]
				# if (!is.null(val)) {
				  # if (is.numeric(current_data()[[col]])) {
					# bsky_temp_df <- bsky_temp_df[bsky_temp_df[[col]] >= val[1] & bsky_temp_df[[col]] <= val[2], ]
				  # } else {
					# bsky_temp_df <- bsky_temp_df[bsky_temp_df[[col]] %in% val, ]
				  # }
				# }
			  # }
			# }
			
			if (!is.null(input$filter_columns)) {
			  for (col in input$filter_columns) {
				inputId <- paste0("filter_", col)
				val <- input[[inputId]]
				if (!is.null(val)) {
				  col_data <- current_data()[[col]]

				  if (inherits(col_data, c("Date", "POSIXct"))) {
					# Date or POSIXct: use range filter
					bsky_temp_df <- bsky_temp_df[bsky_temp_df[[col]] >= val[1] & bsky_temp_df[[col]] <= val[2], ]

				  } else if (is.numeric(col_data)) {
					# Numeric: use range filter
					bsky_temp_df <- bsky_temp_df[bsky_temp_df[[col]] >= val[1] & bsky_temp_df[[col]] <= val[2], ]

				  } else {
					# Character, Factor, or anything else: exact match filter
					bsky_temp_df <- bsky_temp_df[bsky_temp_df[[col]] %in% val, ]
				  }
				}
			  }
			}

			
			###########################################################################
			# All UI elements are defined, launched conditionally, and also reset based
			# vaious conditions like dataset or plot type selection changes
			###########################################################################
			
			###########################################################################
			# Actual ggplot() for plotting execution logic begins here 
			###########################################################################
			
			# Remove all dataset rows where NA is present in Grouping variable if selected
			if (input$x_var != "") {
			  bsky_temp_df <- bsky_temp_df[!is.na(bsky_temp_df[[input$x_var]]), , drop = FALSE]
			}
			
			if (input$y_var != "") {
			  bsky_temp_df <- bsky_temp_df[!is.na(bsky_temp_df[[input$y_var]]), , drop = FALSE]
			}
			
			if (input$group_var != "") {
			  bsky_temp_df <- bsky_temp_df[!is.na(bsky_temp_df[[input$group_var]]), , drop = FALSE]
			}
			
			# Remove all dataset rows where NA is present in if either row or column facet variable if selected
			if (input$facet_row != "") {
			  bsky_temp_df <- bsky_temp_df[!is.na(bsky_temp_df[[input$facet_row]]), , drop = FALSE]
			}
			if (input$facet_col != "") {
			  bsky_temp_df <- bsky_temp_df[!is.na(bsky_temp_df[[input$facet_col]]), , drop = FALSE]
			}
			
			if (input$plot_type %in% c("bar","box") && input$x_var != "") {  
			  bsky_temp_df[[input$x_var]] <- as.factor(bsky_temp_df[[input$x_var]])
			}
			
			if (input$group_var != "") {
			  bsky_temp_df[[input$group_var]] <- as.factor(bsky_temp_df[[input$group_var]])
			}
			
			if (input$shape_var != "") {
			  bsky_temp_df[[input$shape_var]] <- as.factor(bsky_temp_df[[input$shape_var]])
			}
			
			# The following as.data.frame() needed when a data frame has only one column
			#bsky_temp_df = as.data.frame(bsky_temp_df)
			#names(bsky_temp_df) = df_names
			
			bsky_temp_df
		})
  
	
		###############################################################################
		
		# Track selected row_ids
		selected_ids <- reactiveVal(integer(0))

		# Single click
		observeEvent(event_data("plotly_click"), {
			click <- event_data("plotly_click")
			req(click)

			if (!is.null(click) && !is.null(click$customdata)) {
				id <- click$customdata
				current <- selected_ids()
			
				 # Toggle selection
				if (id %in% current) {
				  selected_ids(setdiff(current, id))
				} else {
				  selected_ids(c(current, id))
				}
			}else {
				# Optionally reset selection or do nothing
				#selected_ids(NULL)
				selected_ids(selected_ids())
			}
		})
		
		# Rubber Band Selection(box/lasso) of multiple points with Toggle Logic
		observeEvent(event_data("plotly_selected"), {
		  sel <- event_data("plotly_selected")
		  req(sel)

		  if (!is.null(sel$customdata)) {
			ids <- sel$customdata
			current <- selected_ids()

			# Toggle logic: if id is already selected, deselect it
			new_selection <- current

			for (id in ids) {
			  if (id %in% current) {
				new_selection <- setdiff(new_selection, id)  # Deselect
			  } else {
				new_selection <- c(new_selection, id)        # Select
			  }
			}

			selected_ids(new_selection)
		  }
		})


		# output$click_table <- renderTable({
			# #clicked_points()
			# #selected_ids()
			# df <- filtered_data()
			# df[df$row_id %in% selected_ids(), ]
		# })
		
		output$click_table <- renderTable({
			df <- filtered_data()
			sel_rows <- df[df$row_id %in% selected_ids(), ]
			
			if (nrow(sel_rows) == 0) return(NULL)
			
			# Format both POSIXct and Date columns as character strings
			sel_rows[] <- lapply(sel_rows, function(col) {
				if (inherits(col, "POSIXct")) {
				  # Format each value based on whether it has a non-zero time
				  formatted <- ifelse(
					format(col, "%H:%M:%S") == "00:00:00",
					format(col, "%Y-%m-%d"),
					format(col, "%Y-%m-%d %H:%M:%S")
				  )
				  return(formatted)
				} else if (inherits(col, "Date")) {
				  return(format(col, "%Y-%m-%d"))
				} else {
				  return(col)
				}
			})

			sel_rows
		})
		
		output$clicked_rows_label <- renderText({
			n <- length(selected_ids())
			paste0("Dataset (", input$dataset, " ) Rows of the Clicked Points (", n, ") ")
		})


		observeEvent(input$clear_table, {
			#clicked_points(data.frame())
			selected_ids(integer(0))
		})
		
		observe({
			req(input$dataset, input$plot_type == "scatter")

			default_name <- paste0("clicked_data_", input$dataset)

			# Only update if the user hasn't manually changed it
			if (input$file_name == "_clicked_data" || grepl("^clicked_data_", input$file_name)) {
				updateTextInput(session, "file_name", value = default_name)
			}
		})

		observeEvent(input$save_table, {
			df <- filtered_data()
			sel_rows <- df[df$row_id %in% selected_ids(), ]

			if (nrow(sel_rows) == 0) {
				showNotification("No rows selected to save.", type = "warning")
				return()
			}

			# Check for empty file name
			if (is.null(input$file_name) || trimws(input$file_name) == "") {
				showNotification("File name cannot be empty.", type = "error")
				return()
			}

			# Get and check chosen directory
			dir <- chosen_dir()
			if (is.null(dir) || !nzchar(dir)) {
				showNotification("Please choose a directory (see under 'Download Options' tab) to save the file.", type = "error")
				return()
			}

			# Check for write permissions
			if (file.access(dir, 2) != 0) {
				showNotification(paste("No write permission for directory:", dir), type = "error")
				return()
			}

			# Construct full file path
			file_name <- paste0(input$file_name, ifelse(input$file_type == "rdata", ".RData", ".csv"))
			path <- file.path(dir, file_name)

			# Try saving file
			tryCatch({
				if (tolower(input$file_type) == "rdata") {
				  save(sel_rows, file = path)
				} else {
				  write.csv(sel_rows, file = path, row.names = FALSE)
				}
				showNotification(paste("File saved successfully to:", path), type = "message")
				}, error = function(e) {
				showNotification(paste("Error saving file:", e$message, " - Check whether the file", input$file_name, "is open or locked"), type = "error")
			})
		})
		
		observe({
		  valid_plot <- !is.null(current_plot_obj())
		  valid_dir <- !is.null(chosen_dir()) && nzchar(chosen_dir())

		  shinyjs::toggleState("download_plot", condition = valid_plot && valid_dir)
		})
		
		
		output$download_section <- renderUI({
		  # Check if directory is valid and button should be enabled
		  dir_valid <- !is.null(chosen_dir()) && nzchar(chosen_dir())

		  tags$div(
			style = "display: flex; align-items: center; gap: 10px;",

			if (dir_valid) {
			  # No tooltip if directory is valid
			  downloadButton("download_plot", "Download Plot")
			} else {
			  # Tooltip shown if button should be disabled
			  tags$div(
				title = "Select a directory under Download Options tab to enable the download button",
				downloadButton("download_plot", "Download Plot", disabled = TRUE)
			  )
			},

			selectInput(
			  inputId = "image_format",
			  label = "",
			  choices = c("PNG" = "png", "JPEG" = "jpeg", "SVG" = "svg", "TIFF" = "tiff", "PDF" = "pdf", "BMP" = "bmp", "EPS" = "eps"),  #"ps", "tex" (pictex) 
			  selected = "png",
			  width = "100px"
			)
		  )
		})


		output$download_plot <- downloadHandler(
			filename = function() {
				if (is.null(current_plot_obj())) {
					showNotification("No plot available to download.", type = "error")
					return(invisible(NULL))
				}
				# This specifies the default filename for the user's download
				ext <- input$image_format
				#paste0("Bsky_Graph_Builder_plot_", Sys.Date(), ".", ext) #format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
				#paste0(input$plot_type,"_",input$dataset, "_", Sys.Date(), ".", input$image_format)
				paste0(input$plot_type,"_",input$dataset, "_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".", input$image_format)
			},
			content = function(file) {
				# Check if the plot is available
				if (is.null(current_plot_obj())) {
				  showNotification("No plot available to save. Please create a plot first.", type = "error")
					
					# Write dummy empty content to file to avoid error
					write("No plot available", file)

				  return()
				}
	
				# Get and check chosen directory
				dir <- chosen_dir()
				if (is.null(dir) || !nzchar(dir)) {
					showNotification("Please choose a directory (see under 'Download Options' tab) to save the plot", type = "error")
					
					# Write dummy empty content to file to avoid error
					write("No directory selected", file)
					
					return()
				}

				# Check for write permissions
				if (file.access(dir, 2) != 0) {
					showNotification(paste("No write permission for directory:", dir), type = "error")
					
					# Write dummy empty content to file to avoid error
					write("No write permission", file)
					
					return()
				}

				# Construct full file path
				#file_name <- paste0(input$file_name, ifelse(input$file_type == "rdata", ".RData", ".csv"))
				#path <- file.path(dir, file_name)

				# Construct the full file path for saving on the server
				#file_path <- file.path(dir, paste0(input$plot_type,"_",input$dataset, "_", Sys.Date(), ".", input$image_format))
				file_path <- file.path(dir, paste0(input$plot_type,"_",input$dataset, "_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".", input$image_format))

				# Try saving file
				tryCatch({
					# Save the plot to the specified directory on the server
					# Default width = 8, height = 6, units = "in", dpi = 300, scale = 1
					ggsave(file_path, plot = current_plot_obj(), 
										width = input$downloaded_plot_width, 
										height = input$downloaded_plot_height, 
										dpi = input$downloaded_plot_resolution, 
										units = input$downloaded_plot_height_width_units,
										scale = input$downloaded_plot_scale, 
										device = input$image_format)

					# Copy the saved file to the temporary file provided by downloadHandler
					file.copy(file_path, file, overwrite = TRUE)
				
					showNotification(paste("Plot saved successfully to:", file_path), type = "message")
					}, error = function(e) {
					showNotification(paste("Error saving Plot:", e$message, " - Check whether the file", file_path, "is open or locked"), type = "error")
					
					# Write fallback message to file to prevent browser error
					write(paste("Error:", e$message), file)
				})
			}
		)


	#########################################################
	# renderPlotly() is a Shiny server-side function that:
	# Takes R code that generates a Plotly object
	# Returns it as interactive output to be displayed in the 
	# UI using plotlyOutput
	# It’s the Plotly equivalent of renderPlot() (for base R plots) 
	# or renderPlot({ ggplot(...) }) (for ggplot2 plots).
	# observeEvent(list(input$dataset, input$plot_type), 
	##########################################################
	  output$plot <- renderPlotly({
		#bsky_temp_df <- filtered_data()
		clicked_df <- clicked_points()

		#req(input$dataset,input$plot_type)
		#req(input$dataset, input$plot_type)
		
		#bsky_temp_df <- filtered_data()
		#plot_type <- input$plot_type
		
		#if (facet_resetting()) return(NULL)  # skip plotting during reset
		#req(!facet_resetting())
		
		if (input$plot_type %in% c("scatter", "line", "histogram", "bar", "box","dot")) {
		  req(input$x_var)
		}
		if (input$plot_type %in% c("scatter", "line", "box")) {
		  req(input$y_var)
		}
		if (input$plot_type %in% c("pie")) {
		  req(input$group_var)
		}
		
		bsky_temp_df <- filtered_data()
		plot_type <- input$plot_type

		p = NULL
		
		
		if (input$plot_type %in% c("scatter", "line", "histogram","bar","box","dot")) {
			aes_args <- list(x = as.name(input$x_var))
			if (input$y_var != "" && !(input$plot_type %in% c("histogram", "bar", "dot")) ) aes_args$y <- as.name(input$y_var)
			if (input$group_var != "" && !(input$plot_type %in% c("dot"))) aes_args$color <- as.name(input$group_var)
			

			p <- ggplot(bsky_temp_df, do.call(aes, aes_args))
		}
		
		if(input$plot_type %in% c("histogram", "bar")){
			pos_type <- if (input$position == "dodge") "dodge" else "stack"
		}
		
		if (plot_type == "scatter") {
		  if (input$y_var != "") {
			bsky_temp_df$.clicked <- bsky_temp_df$row_id %in% selected_ids()

			# Build hover text
			bsky_temp_df$hover_text <- paste0(
			  "Row ID: ", bsky_temp_df$row_id, "<br>",
			  input$x_var, ": ", bsky_temp_df[[input$x_var]], "<br>",
			  input$y_var, ": ", bsky_temp_df[[input$y_var]]
			)

			# Protect against aesthetics using x/y vars
			base_vars_used <- c(input$x_var, input$y_var)
			size_var <- if (input$size_var %in% base_vars_used) "" else input$size_var
			shape_var <- if (input$shape_var %in% base_vars_used) "" else input$shape_var

			# Jitter logic
			show_jitter <- input$jitter %||% FALSE
			jitter_width <- input$jitter_spread_width %||% 0.2 
			
			if (show_jitter) {
			  set.seed(2469)
			  #bsky_temp_df$x_var_jit <- jitter(bsky_temp_df[[input$x_var]], amount = ifelse((class(bsky_temp_df[[input$x_var]]) %in% c("POSIXct","Date")), "(jitter_width*10)*60", jitter_width))
			  bsky_temp_df$x_var_jit <- jitter(bsky_temp_df[[input$x_var]], amount = jitter_width)
			  bsky_temp_df$y_var_jit <- jitter(bsky_temp_df[[input$y_var]], amount = jitter_width)
			} else {
			  bsky_temp_df$x_var_jit <- bsky_temp_df[[input$x_var]]
			  bsky_temp_df$y_var_jit <- bsky_temp_df[[input$y_var]]
			}

			# Set up color mapping if group_var is selected
			if (input$group_var != "") {
			  group_levels <- unique(bsky_temp_df[[input$group_var]])
			  group_colors <- setNames(scales::hue_pal()(length(group_levels)), group_levels)
			  bsky_temp_df$color <- group_colors[as.character(bsky_temp_df[[input$group_var]])]

			  # Add group to hover
			  bsky_temp_df$hover_text <- paste0(
				bsky_temp_df$hover_text, "<br>Group: ", bsky_temp_df[[input$group_var]]
			  )
			} else {
			  bsky_temp_df$color <- "black"  # default color
			}

			# === Build the ggplot ===
			# Base ggplot
			p <- ggplot(bsky_temp_df, aes(
			  x = x_var_jit,
			  y = y_var_jit,
			  customdata = row_id,
			  text = hover_text
			))
			
			# Add manual legend if group_var is selected
			if (input$group_var != "") {
			
			########################################################################################################
			# The following does not work to keep the top or bottom mosts within plotly canvas when legends are added
			
				# # Dynamically handle POSIXct on x and/or y axes
				# x_dummy <- if (inherits(bsky_temp_df[[input$x_var]], "POSIXct")) as.POSIXct(NA) else NA
				# y_dummy <- if (inherits(bsky_temp_df[[input$y_var]], "POSIXct")) as.POSIXct(NA) else NA

				# legend_df <- data.frame(
					# x = x_dummy,
					# y = y_dummy,
					# group = factor(names(group_colors), levels = names(group_colors))
				# )

			  # # Add manual legend with invisible geom_point
			  # p <- p +
				# geom_point(
				  # data = legend_df,
				  # aes(x = x, y = y, color = group),
				  # inherit.aes = FALSE,
				  # show.legend = TRUE
				# ) +
				# scale_color_manual(
				  # name = input$group_var,
				  # values = group_colors
				# ) +
				# guides(color = guide_legend(override.aes = list(size = 4)))
			########################################################################################################
			
				#y_range <- range(bsky_temp_df$y_var_jit, na.rm = TRUE)
				#y_pad <- 0.05 * diff(y_range)

				#p <- p + scale_y_continuous(
				#  name = bsky_temp_df$y_var_jit,
				#  limits = c(y_range[1] - y_pad, y_range[2] + y_pad)
				#)
				
				###########################
				# Smart dummy coordinates with invisble points for group color legends to work
				##########################
				
				x_mid <- median(bsky_temp_df$x_var_jit, na.rm = TRUE)
				y_mid <- median(bsky_temp_df$y_var_jit, na.rm = TRUE)

				legend_df <- data.frame(
				  x = rep(x_mid, length(group_colors)),
				  y = rep(y_mid, length(group_colors)),
				  group = factor(names(group_colors), levels = names(group_colors))
				)

				# Transparent legend points
				p <- p +
				  geom_point(
					data = legend_df,
					aes(x = x, y = y, color = group),
					inherit.aes = FALSE,
					show.legend = TRUE,
					alpha = 0 # make the dummy points invisible on the plot
				  ) +
				  scale_color_manual(
					name = input$group_var,
					values = group_colors
				  ) +
				  guides(color = guide_legend(override.aes = list(size = 4, alpha = 1)))
			}

			# === Main point layer with conditional aesthetics ===
			point_aes <- aes()
			if (size_var != "") point_aes <- modifyList(point_aes, aes(size = .data[[size_var]]))
			if (shape_var != "") point_aes <- modifyList(point_aes, aes(shape = .data[[shape_var]]))

			p <- p + geom_point(mapping = point_aes, color = bsky_temp_df$color) #size = 3, alpha = 0.7

			# === Overlay: clicked points with shape 22 ===
			p <- p + geom_point(
			  data = bsky_temp_df[bsky_temp_df$.clicked, ],
			  aes(
				x = x_var_jit,
				y = y_var_jit,
				customdata = row_id,
				text = hover_text
			  ),
			  shape = 22, size = 3, stroke = 0.5, color = "black", fill = NA, inherit.aes = FALSE
			)

			# === Smooth layer (optional) ===
			if (input$scatter_smooth != "") {
			  se_val <- input$std_error_band %||% FALSE
			  conf_int_val <- input$smooth_confInt %||% 0.95

			  if (input$group_var != "") {
				if (input$scatter_smooth == "loess") {
				  p <- p + geom_smooth(
				    #aes(x = x_var_jit, y = y_var_jit, color = bsky_temp_df$color),
					aes(x = x_var_jit, y = y_var_jit, color = .data[[input$group_var]]),
					method = "loess", se = se_val, level = conf_int_val, span = input$loess_span, #color = bsky_temp_df$color, 
					alpha = 0.3, #group = input$group_var
					show.legend = FALSE,
					inherit.aes = FALSE
				  )
				} else {
				  p <- p + geom_smooth(
				    aes(x = x_var_jit, y = y_var_jit, color = .data[[input$group_var]]), #color = .data[[input$group_var]] color = bsky_temp_df$color
					method = input$scatter_smooth, se = se_val, level = conf_int_val, #color = bsky_temp_df$color,
					alpha = 0.3,  #group = input$group_var
					show.legend = FALSE,
					inherit.aes = FALSE
				  )
				}
			  } else {
				if (input$scatter_smooth == "loess") {
				  p <- p + geom_smooth(
					aes(x = x_var_jit, y = y_var_jit),
					method = "loess", se = se_val, level = conf_int_val, span = input$loess_span,
					alpha = 0.3, color = "#727272", inherit.aes = FALSE
				  )
				} else {
				  # p <- p + geom_smooth(
					# method = input$scatter_smooth, se = se_val, level = conf_int_val,
					# alpha = 0.3, color = "#727272"
				  # )
				  p <- p + geom_smooth(
					  aes(x = x_var_jit, y = y_var_jit),
					  method = input$scatter_smooth, se = se_val, level = conf_int_val,
					  alpha = 0.3, color = "#727272", inherit.aes = FALSE
					)
				}
			  }
			}
			
			p <- p + labs(x = input$x_var, y = input$y_var)
			
		   }
		   
		  } else  if (plot_type == "box") {
		  if (input$y_var != "") {
			change_outlier_points_red <- input$change_outlier_red %||% FALSE
			
			show_jitter <- input$jitter %||% FALSE 
			jitter_spread_width_val <- input$jitter_spread_width %||% 0.2

			# Identify outliers
			plot_df <- bsky_temp_df %>%
			  filter(!is.na(.data[[input$x_var]]), !is.na(.data[[input$y_var]])) %>%
			  group_by(across(all_of(input$x_var))) %>%
			  mutate(
				q1 = quantile(.data[[input$y_var]], 0.25, na.rm = TRUE),
				q3 = quantile(.data[[input$y_var]], 0.75, na.rm = TRUE),
				iqr = q3 - q1,
				lower = q1 - 1.5 * iqr,
				upper = q3 + 1.5 * iqr,
				is_outlier = (.data[[input$y_var]] < lower) | (.data[[input$y_var]] > upper)
			  ) %>%
			  ungroup()

			# Split into main + outliers
			outlier_df <- plot_df %>% filter(is_outlier)
			non_outlier_df <- plot_df %>% filter(!is_outlier)

			# Base plot with boxplot
			if (input$group_var != "") {
			  p <- p + geom_boxplot(
				na.rm = TRUE,
				aes_string(fill = input$group_var)
			  )
			} else {
			  p <- p + geom_boxplot(na.rm = TRUE)
			}

			# Add red outliers manually
			if (change_outlier_points_red && nrow(outlier_df) > 0) {
			  p <- p + geom_point(
				data = outlier_df,
				aes_string(x = input$x_var, y = input$y_var),
				color = "red",
				size = 2,
				inherit.aes = FALSE
			  )
			}

			if(show_jitter){
				set.seed(2469) # fixed the seed so that jitter point pointsition is not randomized by ggplot()
				# Jitter non-outlier points only
				if (input$group_var != "") {
				  p <- p + geom_jitter(
					data = non_outlier_df,
					width = jitter_spread_width_val, 
					alpha = 0.5,
					aes_string(x = input$x_var, y = input$y_var, color = input$group_var),
					inherit.aes = FALSE
				  )
				} else {
				  p <- p + geom_jitter(
					data = non_outlier_df,
					width = jitter_spread_width_val, 
					alpha = 0.5,
					aes_string(x = input$x_var, y = input$y_var),
					inherit.aes = FALSE
				  )
				}
			}
		  }
		  
		} else if (plot_type == "histogram") {
		  if (input$group_var != "") {
			p <- p + geom_histogram(na.rm = TRUE, bins = 9, aes_string(fill = input$group_var), 
									alpha = 0.7, position = pos_type)
		  } else {
			p <- p + geom_histogram(na.rm = TRUE, bins = 9, fill = "#06b7db", alpha = 0.7)
		  }

		} else if (plot_type == "line") {
		  #req(input$y_var)
		  if (input$y_var != ""){
			  if (input$group_var != "") {
				p <- p + geom_line(na.rm = TRUE, aes_string(group = input$group_var))
			  } else {
				p <- p + geom_line(na.rm = TRUE, aes(group = 1))
			  }
		  }

		} else if (plot_type == "bar") {
		  if (input$group_var != "") {
			p <- p + geom_bar(na.rm = TRUE, aes_string(fill = input$group_var), position = pos_type)
		  } else {
			p <- p + geom_bar(na.rm = TRUE, fill = "#06b7db")
		  }

		}else if (plot_type == "dot") {
			  #req(input$x_var)
			  #stackratio_spread_points = ifelse(input$stackratio != '', as.numeric(input$stackratio), 4) 
			  #jitter_width <- input$jitter_spread_width %||% 0.2 stackratio
			  #stackratio_spread_points = input$stackratio %||% 4  
			  stackratio_spread_points = 6
			  
			  #showNotification(paste("Stack Ratio Slider Value1:", stackratio_spread_points), type = "message")
			  #showNotification(paste("Stack Ratio Slider Value2:", input$stackratio), type = "message")
			  
			  if(input$group_var != ""){
				p <- p + geom_dotplot(aes_string(fill = input$group_var), method = "histodot", binwidth = 0.1, stackdir = "up", dotsize = 0.8, binaxis = "x", stackratio = stackratio_spread_points, color = NA)
			  } else {
				p <- p + geom_dotplot(method = "histodot", binwidth = 0.1, stackdir = "up", dotsize = 0.8, binaxis = "x", stackratio = stackratio_spread_points)
			  }
			  
			  p <- p +
			  scale_x_continuous(breaks = pretty(bsky_temp_df[[input$x_var]], n = 10)) +  # More x ticks
			  scale_y_continuous(NULL, breaks = NULL) +  # Removes y-axis title and ticks
			  theme(
				axis.text.y = element_blank(),           # Removes y-axis labels
				axis.ticks.y = element_blank()           # Removes tick marks
			  )
		} else if(plot_type == "pie") { #pie chart does not get rendered by ggploty(). Hence fall back to plotly() directly
			#req(input$dataset)
			#req(input$group_var)
			
			#bsky_temp_df <- get(input$dataset)
			bsky_temp_df <- filtered_data()
			
			pie_df <- bsky_temp_df %>%
			  dplyr::count(!!sym(input$group_var)) %>%
			  dplyr::mutate(!!input$group_var := as.factor(!!sym(input$group_var)))

			return(
				plot_ly(
				  data = pie_df,
				  labels = ~get(input$group_var),
				  values = ~n,
				  type = 'pie'
				) #%>%
				  #layout(title = paste("Pie Chart of", input$group_var))
			)
		}

		#######################################################################
		# Facet variable management if specified. if specified Facet Wrap takes 
		# precedence and Facet row and column will be ignored
		#######################################################################
		
		if (input$facet_wrap != "") {
		  p <- p + facet_wrap(input$facet_wrap, scales = input$facet_scales)
		}else if (input$facet_row != "" || input$facet_col != "") {
		  row_facet <- ifelse(input$facet_row == "", ".", input$facet_row)
		  col_facet <- ifelse(input$facet_col == "", ".", input$facet_col)
		  facet_formula <- as.formula(paste(row_facet, "~", col_facet))
		  p <- p + facet_grid(facet_formula, scales = input$facet_scales)
		} 

		##########################################################################
		# Plot reference lines and line labels if any, skip/ignore reference lines 
		# for axis with time, factor, and character variables
		##########################################################################
		
		parse_coords <- function(input_str) {
		  suppressWarnings(as.numeric(unlist(strsplit(input_str, ","))))
		}

		parse_labels <- function(input_str) {
		  trimws(unlist(strsplit(input_str, ",")))
		}

		hlines <- reactive({
		  coords <- parse_coords(current_hlines()) #parse_coords(input$hlines)
		  if (length(coords) == 0 || all(is.na(coords))) {
			return(NULL)  # or return(data.frame(y = numeric(0), label = character(0)))
		  }

		  #coords = sort(coords)
		  labels <- parse_labels(current_hline_labels()) #parse_labels(input$hline_labels)
		  if(length(labels) > length(coords)) labels = labels[1:length(coords)]
		  data.frame(
			y = coords,
			label = if (length(labels) < length(coords)) {
			  c(labels, coords[(length(labels) + 1):length(coords)])
			} else {
			  labels
			},
			stringsAsFactors = FALSE
		  )
		})

		vlines <- reactive({
		  coords <- parse_coords(current_vlines()) #parse_coords(input$vlines)
		  if (length(coords) == 0 || all(is.na(coords))) {
			return(NULL)  # or return(data.frame(x = numeric(0), label = character(0)))
		  }

		  #coords = sort(coords)
		  labels <- parse_labels(current_vline_labels())
		  if(length(labels) > length(coords)) labels = labels[1:length(coords)]
		  data.frame(
			x = coords,
			label = if (length(labels) < length(coords)) {
			  c(labels, coords[(length(labels) + 1):length(coords)])
			} else {
			  labels
			},
			stringsAsFactors = FALSE
		  )
		})


		#plot_data <- current_data() 
		plot_data = bsky_temp_df

		if(current_hlines() != "" || current_vlines() != "")
		{
			if (!is.null(input$x_var) && input$x_var !='' && !is.null(input$y_var) && input$y_var !='' &&((!is.null(hlines()) && nrow(hlines()) > 0))||(!is.null(vlines()) && nrow(vlines()) > 0)) {
				x_range <- NULL
				y_range <- NULL

				# Check if x_var is numeric
				is_x_numeric <- !inherits(current_data()[[input$x_var]], c("Date", "POSIXct", "factor", "character"))

				# Check if y_var is numeric
				is_y_numeric <- !inherits(current_data()[[input$y_var]], c("Date", "POSIXct", "factor", "character"))

				# Calculate x_range and y_range for numeric variables
				if (is_x_numeric) {
				x_range <- range(plot_data[[input$x_var]], na.rm = TRUE)
				}
				if (is_y_numeric) {
				y_range <- range(plot_data[[input$y_var]], na.rm = TRUE)
				}

				# Handle horizontal lines if y is numeric
				if (is_y_numeric && !is.null(hlines()) && nrow(hlines()) > 0) {
					x_val <- if (!is.null(x_range)) {
								  x_range[2]
								} else {
								  x_data <- plot_data[[input$x_var]]
								  
								  if (inherits(x_data, "POSIXct") || inherits(x_data, "Date")) {
									max(x_data, na.rm = TRUE)
								  } else if (is.factor(x_data) || is.character(x_data)) {
									# Use the last level / value as fallback
									levels_or_unique <- if (is.factor(x_data)) levels(x_data) else unique(x_data)
									levels_or_unique[length(levels_or_unique)]
								  } else {
									Inf
								  }
								}
					
					y_coords <- hlines()$y
					y_range[2] <- max(y_range[2], max(y_coords, na.rm = TRUE))
						
					for (i in seq_len(nrow(hlines()))) {
					  p <- p + 
						geom_hline(yintercept = y_coords[i], linetype = "dashed", color = "red") +
						annotate("text", 
								 #x = if (!is.null(x_range)) x_range[2] else Inf,
								 x = x_val,
								 y = y_coords[i], 
								 label = hlines()$label[i], 
								 hjust = -0.1, vjust = -0.5)
					}
				}

				# Handle vertical lines if x is numeric
				if (is_x_numeric && !is.null(vlines()) && nrow(vlines()) > 0) {
					y_val <- if (!is.null(y_range)) {
								  y_range[2]
								} else {
								  y_data <- plot_data[[input$y_var]]
								  
								  if (inherits(y_data, "POSIXct") || inherits(y_data, "Date")) {
									max(y_data, na.rm = TRUE)
								  } else if (is.factor(y_data) || is.character(y_data)) {
									# Use the last level / value as fallback
									levels_or_unique <- if (is.factor(y_data)) levels(y_data) else unique(y_data)
									levels_or_unique[length(levels_or_unique)]
								  } else {
									Inf
								  }
								}
						
					x_coords <- vlines()$x
					x_range[2] <- max(x_range[2], max(x_coords, na.rm = TRUE))

					for (i in seq_len(nrow(vlines()))) {
					  p <- p + 
						geom_vline(xintercept = x_coords[i], linetype = "dashed", color = "red") +
						annotate("text", 
								 x = x_coords[i], 
								 #y = if (!is.null(y_range)) y_range[2] else Inf,
								 y = y_val,
								 label = vlines()$label[i], 
								 hjust = -0.1, vjust = -0.5)
					}
				}

				# Apply extended axis limits (accomodates if hline or vline coordinate values exceed 
				# the original xlimit and ylimit of the original plot x and y axis data
				if (!is.null(x_range) && !is.null(y_range)) {
				p <- p + coord_cartesian(xlim = x_range, ylim = y_range)
				} else if (!is.null(x_range)) {
				p <- p + coord_cartesian(xlim = x_range)
				} else if (!is.null(y_range)) {
				p <- p + coord_cartesian(ylim = y_range)
				}
			}
		}

		#################################################################################
		# Change to log scale log10, log2, or the natural log. The default is continuous.
		# The log transform will fail if there is zero or negative in the data
		#################################################################################
		if (input$x_scale != "continuous" && input$x_var != "" && is.numeric(bsky_temp_df[[input$x_var]])) {
		  p <- p + switch(input$x_scale,
			"log10" = scale_x_continuous(trans = log10_trans(), breaks = scales::pretty_breaks(n = 10)),
											#breaks = trans_breaks("log10", function(x) 10^x),
											#labels = trans_format("log10", math_format(10^.x))),
			"log2" = scale_x_continuous(trans = log2_trans(), breaks = scales::pretty_breaks(n = 10)),
											#breaks = trans_breaks("log2", function(x) 2^x),
											#labels = trans_format("log2", math_format(2^.x))),
			"log" = scale_x_continuous(trans = log_trans(), breaks = scales::pretty_breaks(n = 10)) #,
											#breaks = trans_breaks("log", function(x) exp(x)),
											#labels = trans_format("log", math_format(e^.x)))
			)
		}
		if (input$y_scale != "continuous" && input$y_var != "" && is.numeric(bsky_temp_df[[input$y_var]])) {
		  p <- p + switch(input$y_scale,
		   "log10" = scale_y_continuous(trans = log10_trans(), breaks = scales::pretty_breaks(n = 10)),
											#breaks = trans_breaks("log10", function(x) 10^x),
											#labels = trans_format("log10", math_format(10^.x))),
			"log2" = scale_y_continuous(trans = log2_trans(), breaks = scales::pretty_breaks(n = 10)),
											#breaks = trans_breaks("log2", function(x) 2^x),
											#labels = trans_format("log2", math_format(2^.x))),
			"log" = scale_y_continuous(trans = log_trans(), breaks = scales::pretty_breaks(n = 10)) #,
											#breaks = trans_breaks("log", function(x) exp(x)),
											#labels = trans_format("log", math_format(e^.x)))
		  )
		}
		
		if (input$x_scale == "continuous" && input$x_var != "" && is.numeric(bsky_temp_df[[input$x_var]])) {
			p <- p + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
		}
		
		# if (inherits(current_data()[[input$x_var]], "Date")) {
		  # p <- p + scale_x_date(date_labels = input$axis_date_format, breaks = scales::pretty_breaks(n = 10))
		# } else if (inherits(current_data()[[input$x_var]], "POSIXct")) {
		  # p <- p + scale_x_datetime(date_labels = paste(input$axis_date_format, input$axis_time_format), breaks = scales::pretty_breaks(n = 10))
		# }
		
		if (inherits(current_data()[[input$x_var]], "Date")) {
		  if (nzchar(input$axis_date_format)) {
			p <- p + scale_x_date(
			  date_labels = input$axis_date_format,
			  breaks = scales::pretty_breaks(n = 10)
			)
		  } else {
			p <- p + scale_x_date(
			  breaks = scales::pretty_breaks(n = 10)
			)
		  }
		} else if (inherits(current_data()[[input$x_var]], "POSIXct")) {
		  if (nzchar(input$axis_date_format) || nzchar(input$axis_time_format)) {
			combined_format <- trimws(paste(input$axis_date_format, input$axis_time_format))
			p <- p + scale_x_datetime(
			  date_labels = combined_format,
			  breaks = scales::pretty_breaks(n = 10)
			)
		  } else {
			p <- p + scale_x_datetime(
			  breaks = scales::pretty_breaks(n = 10)
			)
		  }
		}

		
		if (input$y_scale == "continuous" && input$y_var != "" && is.numeric(bsky_temp_df[[input$y_var]])) {
			p <- p + scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
		}
		
		# if (inherits(current_data()[[input$y_var]], "Date")) {
		  # p <- p + scale_y_date(breaks = scales::pretty_breaks(n = 10))
		# } else if (inherits(current_data()[[input$y_var]], "POSIXct")) {
		  # p <- p + scale_y_datetime(breaks = scales::pretty_breaks(n = 10))
		# }
		
		if (inherits(current_data()[[input$y_var]], "Date")) {
		  if (nzchar(input$axis_date_format)) {
			p <- p + scale_y_date(
			  date_labels = input$axis_date_format,
			  breaks = scales::pretty_breaks(n = 10)
			)
		  } else {
			p <- p + scale_y_date(
			  breaks = scales::pretty_breaks(n = 10)
			)
		  }
		} else if (inherits(current_data()[[input$y_var]], "POSIXct")) {
		  if (nzchar(input$axis_date_format) || nzchar(input$axis_time_format)) {
			combined_format <- trimws(paste(input$axis_date_format, input$axis_time_format))
			p <- p + scale_y_datetime(
			  date_labels = combined_format,
			  breaks = scales::pretty_breaks(n = 10)
			)
		  } else {
			p <- p + scale_y_datetime(
			  breaks = scales::pretty_breaks(n = 10)
			)
		  }
		}
		

		if (input$plot_type %in% c("scatter", "line", "histogram","bar","box", "dot")){
			if(!is.null(p)){
				# Apply themes 
				
				# selected_theme <- input$plot_theme
				# if (selected_theme == "default") {
				  # # No additional theme applied
				# } else if (selected_theme == "theme_economist") {
				  # p <- p + ggthemes::theme_economist()
				# } else if (selected_theme == "theme_tufte") {
				  # p <- p + ggthemes::theme_tufte()
				# } else if (selected_theme == "theme_minimal") {
				  # p <- p + theme_minimal()
				# } else if (selected_theme == "theme_classic") {
				  # p <- p + theme_classic()
				# } else if (selected_theme == "theme_void") {
				  # p <- p + theme_void()
				# } else if (selected_theme == "theme_solarized(light)") {
				  # p <- p + ggthemes::theme_solarized(light = TRUE)
				# } else if (selected_theme == "theme_solarized(dark)") {
				  # p <- p + ggthemes::theme_solarized(light = FALSE)
				# }
		
				if (input$plot_theme != "default") {
				  theme_str <- input$plot_theme

				  if (theme_str == "solarized_light") {
					p <- p + ggthemes::theme_solarized(light = TRUE)
				  } else if (theme_str == "solarized_dark") {
					p <- p + ggthemes::theme_solarized(light = FALSE)
				  } else {
					# Split the string into package and function name
					parts <- strsplit(theme_str, "::")[[1]]
					theme_func <- getFromNamespace(parts[2], ns = parts[1])
					p <- p + theme_func()
				  }
				}
				
				x_angle <- input$x_angle %||% 0
				y_angle <- input$y_angle %||% 0

				p <- p + theme(
				  axis.text.x = element_text(angle = x_angle, vjust = 1, hjust = 1),
				  axis.text.y = element_text(angle = y_angle, vjust = 1, hjust = 1)
				)
				

				if (input$plot_title != "") {
				  p <- p + ggtitle(input$plot_title)+
							theme(plot.title = element_text(hjust = 0.5)) #hjust = 0 → left, 0.5 → center, 1 → right
				}
				if (input$x_axis_label != "") {
				  p <- p + xlab(input$x_axis_label)
				}
				if (input$y_axis_label != "") {
				  p <- p + ylab(input$y_axis_label)
				}
				
				observe({
					#`p` is the ggplot object (not the plotly object yet)
					current_plot_obj(p)
				})

				if (input$plot_type %in% c("scatter")){
					#plotly_obj <- ggplotly(p, tooltip = "text", source = "myplot") %>%
					#	plotly::style(customdata = bsky_temp_df$row_id, traces = 1)
					
					#plotly_obj <- ggplotly(p, tooltip = "text", source = "myplot") #%>%
						#plotly::style(customdata = bsky_temp_df$row_id)
					
					#plotly_obj <- plotly::event_register(plotly_obj, "plotly_click")
					
					# ggplotly(p, tooltip = "text") %>%
						# style(customdata = bsky_temp_df$row_id, traces = 1)
					
					# ggplotly(p) %>% layout(dragmode = "select")
					# or use dragmode = "lasso"
					
					ggplotly(p, tooltip = "text") %>%
						  style(customdata = bsky_temp_df$row_id, traces = 1) %>%
							layout(dragmode = "select")
				}else{
					ggplotly(p)
				}
			}
		}
	  })

	  # Export button to save the dataset if any filtering of columns applied to the selected dataset
	  # output$export_data <- downloadHandler(
		# filename = function() paste0("filtered_data_", Sys.Date(), ".csv"),
		# content = function(file) {
		  # write.csv(filtered_data(), file, row.names = FALSE)
		# }
	  # )
	}

	shinyApp(ui, server)
}


BSkyGraphBuilderSubprocLaunchInternal <- function(tempDatasetRDataFilePath = c(), port_to_use = NULL, graph_area_height = 600, graphBuilderTitle = c("BlueSky Statistics Interactive Graph Builder")) {

library(future)

	num_workers_threads = 2 
	
	# Filter dataset names to include only those that are NOT all NA or all ''
	valid_datasets <- Filter(function(df_name) {
		df <- get(df_name, envir = .GlobalEnv)
		if (!is.data.frame(df)) return(FALSE)

		# For each column, check if it's all NA or blank (if character)
		all_empty <- sapply(df, function(col) {
			if (is.character(col)) {
				all(is.na(col) | col == "")
			} else {
				all(is.na(col))
			}
		})
	# Return TRUE if at least one column is not all empty
	!all(all_empty)
	}, uadatasets$name)

	# Save only valid datasets
	if (length(valid_datasets) > 0) {
		invalid_datasets <- setdiff(uadatasets$name, valid_datasets)
		if (length(invalid_datasets) > 0) {
			message("The following datasets (either all empty or all NAs) were excluded from the Graph Builder:\n", paste(invalid_datasets, collapse = ", "))
		}
		
		save_tmp_dataset_file = if(length(tempDatasetRDataFilePath) == 0)paste0(tempdir(),'/bsky_open_dataframes.RData')else tempDatasetRDataFilePath
		save(list = valid_datasets, file= save_tmp_dataset_file, envir = .GlobalEnv)
	} else {
		message("No valid datasets to launch the Graph Builder i.e. all open datasets are either all empty or NAs: ", paste0(uadatasets$name,collapse=', '))
		return(invisible())
	}
	
	#number_of_cores <- availableCores()
	#number_of_subprocess_active_workers <- nbrOfWorkers()
	
	if (!inherits(plan(), "multisession") || nbrOfWorkers() == 1) {
		plan(multisession, workers = num_workers_threads)
		#plan(multisession)
	}

	# Kill all workers if too many are running (likely leftovers)
	# if (number_of_subprocess_active_workers > (0.5 * number_of_cores)) {
		# #future::shutdownWorkers()
		# future::plan(future::sequential)
		# gc()
		# #future::plan(multisession, workers = num_workers_threads)
		# plan(multisession)
	# }

	# Port for the Graph Builder Shiny local web server app to coomunicate with the shiny Graph Builderbrowser client
	# port_to_use <- httpuv::randomPort()
	
	if(is.null(port_to_use)) port_to_use <- 2468
	
	proceed_to_launch_subprocess = TRUE
	
	if(exists("bsky_graphBuilder_subprocess_handle", envir = .GlobalEnv)) {
		old_handle <- get("bsky_graphBuilder_subprocess_handle", envir = .GlobalEnv)
		if (inherits(old_handle, "ClusterFuture")){
			tryCatch({
			  if (!resolved(old_handle)) {
				if (is.function(old_handle$cancel)) {
				  message("Cleaning up previously running BlueSky Statistics Graph Builder subprocess...")
				  old_handle$cancel()
				  bsky_graphBuilder_subprocess_handle <- NULL
				  future::plan(future::sequential)
				  gc()
				  future::plan(multisession, workers = num_workers_threads)
				} else {
				  message("There is a BlueSky Statistics Graph Builder already running. Close and quit that first before launching another one")
				  proceed_to_launch_subprocess = FALSE
				}
			  }
			}, error = function(e) {
				#Error handler hook
				#message("Error during subprocess resolution/cleanup: ", e$message)
				message("Initializing ...")
				
				bsky_graphBuilder_subprocess_handle <- NULL
				future::plan(future::sequential)
				gc()
				future::plan(multisession, workers = num_workers_threads)
				proceed_to_launch_subprocess = TRUE
			})
		}
	}

	if(proceed_to_launch_subprocess){
		tryCatch({
			# Launch new subprocess and store handle in global environment
			assign("bsky_graphBuilder_subprocess_handle", future({
				tempDatasetRDataFilePath_to_load = save_tmp_dataset_file
				
				library(shiny)
				library(ggplot2)
				library(ggthemes)
				library(dplyr)
				library(plotly)
				library(scales)

				#shiny::runApp(BSkyGraphBuilderInternalCore(tempDatasetRDataFilePath = tempDatasetRDataFilePath_to_load), port = port_to_use, launch.browser = FALSE)
				shiny::runApp(BSkyGraphBuilderInternalCore(tempDatasetRDataFilePath = tempDatasetRDataFilePath_to_load, graph_area_height = graph_area_height, graphBuilderTitle = graphBuilderTitle), port = port_to_use, launch.browser = TRUE)
			}), envir = .GlobalEnv)

			Sys.sleep(1)
			#browseURL(sprintf("http://127.0.0.1:%d", port_to_use))
			# Delay to allow server to bind before launching browser
			#Sys.sleep(1.5)
			#browseURL(sprintf("http://127.0.0.1:%d", port_to_use))
	
			cat(sprintf("BlueSky Statistics Graph Builder app launched in a browser. See http://127.0.0.1:%d in your browser.\n", port_to_use))
		}, error = function(e) {
				#Error handler hook
				message("Error launching BlueSky Statistics Graph Builder: ", e$message)
				proceed_to_launch_subprocess = FALSE
			}
		)
	}
	return(invisible())
}


BSkyGraphBuilderSubprocLaunchMain <- function(tempDatasetRDataFilePath = c(), port_to_use = NULL, graph_area_height = 600, graphBuilderTitle = c("BlueSky Statistics Interactive Graph Builder")) {
	BSkyGraphBuilderSubprocLaunchInternal(tempDatasetRDataFilePath = tempDatasetRDataFilePath, port_to_use =port_to_use, graph_area_height = graph_area_height, graphBuilderTitle = graphBuilderTitle)
}