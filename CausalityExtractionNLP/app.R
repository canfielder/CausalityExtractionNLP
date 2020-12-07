# Libraries -------------------------------------------------------------------
## R
library(DT)
library(reticulate)
library(shiny)
library(shinycssloaders)
library(shinydashboard)

## Python
PYTHON_DEPENDENCIES = c('joblib','numpy', 'pandas', 'sklearn', 'fasttext')

# UI --------------------------------------------------------------------------
ui <- dashboardPage(
    dashboardHeader(title = "Causal Knowledge Extractions"),
    dashboardSidebar(),
    dashboardBody()
)

# SERVER ----------------------------------------------------------------------
server <- function(input, output) {

  # VIRTUAL ENVIRONMENt SETUP -------------------------------------------------

  virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')
  python_path = Sys.getenv('PYTHON_PATH')

  # Create virtual env and install dependencies
  reticulate::virtualenv_create(envname = virtualenv_dir, python = python_path)
  reticulate::virtualenv_install(virtualenv_dir, packages = PYTHON_DEPENDENCIES, ignore_installed=TRUE)
  reticulate::use_virtualenv(virtualenv_dir, required = T)

  # FUNCTIONS -----------------------------------------------------------------
  # Display info about the system running the code
  output$sysinfo <- DT::renderDataTable({
    s = Sys.info()
    df = data.frame(Info_Field = names(s),
                    Current_System_Setting = as.character(s))
    table_output < - datatable(df, rownames = F, selection = 'none',
                               style = 'bootstrap', filter = 'none',
                               options = list(dom = 't'))

    return(table_output)
  })

  # Display system path to python
  output$which_python <- renderText({
    paste0('which python: ', Sys.which('python'))
  })

  # Display Python version
  output$python_version <- renderText({
    rr = reticulate::py_discover_config(use_environment = 'python35_env')
    paste0('Python version: ', rr$version)
  })

  # Display RETICULATE_PYTHON
  output$ret_env_var <- renderText({
    paste0('RETICULATE_PYTHON: ', Sys.getenv('RETICULATE_PYTHON'))
  })

  # Display virtualenv root
  output$venv_root <- renderText({
    paste0('virtualenv root: ', reticulate::virtualenv_root())
  })

}

# Run the application
shinyApp(ui = ui, server = server)
