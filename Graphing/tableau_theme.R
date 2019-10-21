library(ggthemr)

tableau_colours <- c('#1F77B4', '#FF7F0E', '#2CA02C', '#D62728', '#9467BD', '#8C564B', '#CFECF9', '#7F7F7F', '#BCBD22', '#17BECF')
tableau_colours <- c("#555555", tableau_colours)

ggthemr_reset() # clear history

# Define colours for your figures with define_palette
tableau <- define_palette(
  swatch = tableau_colours, # colours for plotting points and bars
  gradient = c(lower = tableau_colours[1L], upper = tableau_colours[2L]), #upper and lower colours for continuous colours
  background = "#fdfbfb" #defining a grey-ish background 
)
