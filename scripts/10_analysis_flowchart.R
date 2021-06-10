library(DiagrammeR)
library(DiagrammeRsvg)

flowchart <- grViz(diagram = "digraph flowchart {
# define node aesthetics
      node [fontname = arial, shape = rectangle, color = Lavender, style = filled]
      tab7 [label = '@@7']
      tab9 [label = '@@9']

      # additional node (text-only)
      node [fontname = arial, shape = plaintext, color = white]
      tab3 [label = '@@3']
      tab6 [label = '@@6']
      tab8 [label = '@@8']
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab5 [label = '@@5']
      tab4 [label = '@@4']
      tab10 [label = '@@10']

  
      # set up node layout
      edge[minlen = 2]
      tab1 -> tab2;
      tab2 -> tab3;
      tab3 -> tab4;
      tab4 -> tab5;
      tab5 -> tab6;
      tab6 -> tab7;
      tab4 -> tab8;
      tab8 -> tab9;
      tab4 -> tab10;
      tab10 -> tab6;

      { rank = same; tab4; tab8, tab9 }

    #  { rank = same; tab4; tab5 }

      }
      
      # add labels
      [1]: 'Data processing'
      [2]: 'Create 100 rarefied datasets'    
      [3]: 'Identify common species'
      [4]: 'Split by fire history'
      [5]: 'Fit candidate models x 100 (with fire)'
      [6]: 'Identify errors and adapt model frameworks'
      [7]: 'Average coefficients for top models'
      [8]: 'Identify range edges from 95th percentiles'
      [9]: 'Calculate change in lower and upper edges'
      [10]: 'Fit candidate models x 100 (without fire)'
      ")

flowchart

# Need to manually export in RStudio - can't figure out how to automate save


