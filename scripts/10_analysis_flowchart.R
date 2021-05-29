library(DiagrammeR)

grViz(diagram = "digraph flowchart {
      # define node aesthetics
      node [fontname = arial, shape = oval, color = Lavender, style = filled]
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']

      # additional node (text-only)
      node [fontname = arial, shape = oval, color = black, style = invis]
      tab5 [label = '@@5']
  
      # set up node layout
      tab1 -> tab2;
      tab2 -> tab3;
      tab3 -> tab1;
      tab1 -> tab4;
      tab1 -> tab5
      }
      
      # add labels
      [1]: 'Data processing?'
      [2]: 'Create 100 rarefied datasets'    
      [3]: 'Then, either did this'
      [4]: 'Or we did this'
      [5]: 'Test label'
      ")


