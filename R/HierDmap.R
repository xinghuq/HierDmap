### hierarchical diversity plot


HierDplot=function(x,layout="circlepack",...){
hiergraph <- igraph::graph_from_data_frame(x$HierStr, vertices=x$HierDval)
p=ggraph::ggraph(hiergraph, layout = layout, weight=size) +
  ggraph::geom_node_circle(ggplot2::aes(fill = depth)) +
  ggraph::geom_node_text(ggplot2::aes(label=Shortnames, filter=leaf, fill=depth, size=size)) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position="FALSE") +
  ggraph::scale_fill_viridis() 
return(p)
}


##example

#HierDisland=HierDq(infile,q=2,ncode=3,nreg = 4,r=c(7,4,2,3))
#hie_mapdata=HierDstr(HierDisland)
#HierDplot(hie_mapdata,layout = 'circlepack')



