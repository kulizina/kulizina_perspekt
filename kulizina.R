# Установите и подключите библиотеку igraph
# install.packages("igraph")
library(igraph)

# 1. R-функция для создания матрицы смежности из случайных данных
create_adjacency_matrix <- function(nodes, edges) {
  adjacency_matrix <- matrix(0, nrow = nodes, ncol = nodes)
  for (i in 1:edges) {
    from <- sample(1:nodes, 1)
    to <- sample(1:nodes, 1)
    if (from != to) {
      adjacency_matrix[from, to] <- 1
      adjacency_matrix[to, from] <- 1
    }
  }
  rownames(adjacency_matrix) <- colnames(adjacency_matrix) <- 1:nodes
  return(adjacency_matrix)
}

# 2. Создание графа на основе матрицы смежности
create_graph <- function(adjacency_matrix) {
  g <- graph_from_adjacency_matrix(adjacency_matrix, mode = "undirected")
  return(g)
}

# 3. Визуализация графа с разными вариантами укладки
visualize_graph <- function(graph) {
  par(mfrow=c(1, 3))  # Разбиваем графику на три части
  plot(graph, layout = layout_with_fr(graph), main = "Force-Directed Layout", vertex.color = "blue")
  legend("topleft", legend = "Группа A", fill = "blue")
  
  plot(graph, layout = layout_in_circle(graph), main = "Circular Layout", vertex.color = "red")
  legend("topleft", legend = "Группа B", fill = "red")
  
  plot(graph, layout = layout_randomly(graph), main = "Random Layout")
}

# Вызов функций
nodes <- 10
edges <- 15
adjacency_matrix <- create_adjacency_matrix(nodes, edges)
graph <- create_graph(adjacency_matrix)
visualize_graph(graph)

write.csv(adjacency_matrix, file = "adjacency_matrix.csv", row.names = FALSE)
png("payment_graph.png", width = 800, height = 800)
plot(graph, layout = layout_fr, vertex.color = "lightblue")
dev.off()
