library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

flowchart <- grViz(diagram = "digraph flowchart {
# define node aesthetics
      node [fontname = arial, shape = rectangle, color = Lavender, style = filled]
      tab6 [label = '@@7']
      tab8 [label = '@@9']

      # additional node (text-only)
      node [fontname = arial, shape = plaintext, color = white]
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5']
      tab7 [label = '@@7']
      tab9 [label = '@@9']

  
      # set up node layout
      edge[minlen = 2]
      tab1 -> tab2;
      tab2 -> tab3;
      tab3 -> tab4;
      tab4 -> tab5;
      tab5 -> tab6;
      tab3 -> tab7;
      tab7 -> tab8;
      tab3 -> tab9;
      tab9 -> tab5;

      { rank = same; tab3; tab7, tab8 }

    #  { rank = same; tab4; tab5 }

      }
      
      # add labels
      [1]: 'Identify common species'    
      [2]: 'Create 100 rarefied datasets'
      [3]: 'Split by fire history'
      [4]: 'Fit candidate models x 100 (with fire)'
      [5]: 'Identify errors and adapt model frameworks'
      [6]: 'Average coefficients for top models'
      [7]: 'Identify range edges from 95th percentiles'
      [8]: 'Calculate change in lower and upper edges'
      [9]: 'Fit candidate models x 100 (without fire)'
      ")

flowchart

# Export graph
setwd("figures") # For some reason, can't specify this in file name
export_svg(flowchart) %>%
  charToRaw() %>%
  rsvg() %>%
  png::writePNG("flowchart.png")

# Need to manually export in RStudio - can't figure out how to automate save


