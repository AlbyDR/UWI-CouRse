Rstudio
================
UWI-couRse
26/02/2021

# RStudio four main window ‘panes’:

The Source pane, for editing, saving, and dispatching R code to the console (top left). Note that this pane does not exist by default when you start RStudio: it appears when you open an R script, e.g. via File -> New File -> R Script. A common task in this pane is to send code on the current line to the console, via Ctrl/Cmd+Enter.

The Console pane. Any code entered here is processed by R, line by line. This pane is ideal for interactively testing ideas before saving the final results in the Source pane above.

The Environment pane (top right) contains information about the current objects loaded in the workspace including their class, dimension (if they are a data frame) and name. This pane also contains tabbed sub-panes with a searchable history that was dispatched to the console and (if applicable to the project) Build and Git options.

The Files pane (bottom right) contains a simple file browser, a Plots tab, Packages and Help tabs and a Viewer for visualising interactive R output such as those produced by the leaflet package and HTML ‘widgets’.
