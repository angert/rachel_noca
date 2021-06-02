library(DiagrammeR)

grViz(diagram = "digraph flowchart {
      # define node aesthetics
      node [fontname = arial, shape = rectangle, color = Lavender, style = filled]
      tab6 [label = '@@6']
      tab8 [label = '@@8']

      # additional node (text-only)
      node [fontname = arial, shape = plaintext, color = white]
      tab3 [label = '@@3']
      tab5 [label = '@@5']
      tab7 [label = '@@7']
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab4 [label = '@@4']
  
      # set up node layout
      edge[]
      tab1 -> tab2;
      tab2 -> tab3;
      tab3 -> tab4;
      tab4 -> tab5;
      tab5 -> tab6;
      tab3 -> tab7;
      tab7 -> tab8

    #  { rank = same; tab2; tab3 }

    #  { rank = same; tab4; tab5 }

      }
      
      # add labels
      [1]: 'Data processing'
      [2]: 'Create 100 rarefied datasets'    
      [3]: 'Identify common species'
      [4]: 'Fit candidate models for each species across 100 datasets'
      [5]: 'Identify errors and adapt model framework'
      [6]: 'Average coefficients for top models'
      [7]: 'Identify range edges from 95th percentiles'
      [8]: 'Calculate change in lower and upper edges'
      ")


