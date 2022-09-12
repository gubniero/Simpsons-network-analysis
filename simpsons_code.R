######################## Inicialização do programa ########################
# Instalação e ativação do pacote igraph
install.packages('igraph')
install.packages("xlsx")
library(igraph)
library(xlsx)
getwd()

# Abrindo arquivos
#abrir arquivo "SimpsonsEdgesNames.csv"
arestas <- read.csv(file = file.choose(), header = TRUE, sep = ';', dec = ',')#edge

# Visualisando variaveis
arestas

# Conferindo se são dataframes
is.data.frame(arestas)

# Criando e plotando grafo
grafo = graph_from_data_frame(d=arestas, directed=F)

deg = degree(grafo, mode="all")

tkplot(grafo,layout=layout.lgl, vertex.size=deg*0.4, vertex.color='Yellow' )

######################## GRAU ########################
grau = degree(grafo)
grau
sort(grau)[54:45]

# MAIOR GRAU
maiorgrau = which(max(grau) == grau)
maiorgrau
max(grau)

# MENOR GRAU
menorgrau = which(min(grau) == grau)
menorgrau
min(grau)

# MEDIA GRAU
mean(degree(grafo))

######################## PROXIMIDADE ########################
proximidade = closeness(grafo)
1/proximidade
# MENOR PROXIMIDADE
menorprox = which(max(proximidade) == proximidade)
menorprox
1/max(proximidade)

# MAIOR PROXIMIDADE
maiorprox = which(min(proximidade) == proximidade)
maiorprox
1/min(proximidade)

######################## INTERMEDIACAO DE VERTICE ########################
interdvert = betweenness(grafo)
interdvert
mean(interdvert)
# MAIOR INTERMEDIACAO
maioridv = which(max(interdvert) == interdvert)
maioridv
max(interdvert)

# MENOR INTERMEDIACAO
menoridv = which(min(interdvert) == interdvert)
menoridv
min(interdvert)

# FUNCAO PARA PEGAR MENOR VALOR DE INTERMEDIACAO DIFERENTE DE 0
y = 100000
for(x in interdvert){
  if(x != 0){
    if(x < y){
      y = x
    }
  }
}

# MENOR VALOR DE INTERMEDIACAO DIFERENTE DE -
menordfz = which(interdvert == y)
menordfz
y

# PERSONAGENS COM INTERMEDIACAO DIFERENTE DE 0
diferenteDeZero <- which(interdvert != 0)
diferenteDeZero

######################## INTERMEDIACAO DE ARESTAS ########################

interdarest = edge.betweenness(grafo)
interdarest



# MAIOR INTERMEDIACAO
maiorarest = which(max(interdarest) == interdarest)
maiorarest
E(grafo)[49]
E(grafo)[11]
max(interdarest)

# MENOR INTERMEDIACAO
menorarest = which(min(interdarest) == interdarest)
menorarest
E(grafo)[213]
min(interdarest)

######################## VERTICES E ARESTAS ########################

# N° DE VERTICES DO GRAFO
nvert = vcount(grafo)
nvert
# N° DE ARESTAS DE ARESTAS
narest = ecount(grafo)
narest

######################## DIAMETRO ########################

diametro = diameter(grafo)
diametro

# CAMINHO DO DIAMETRO
caminhodiametro = get_diameter(grafo)
caminhodiametro

# PLOT DO CAMINHO
vcol = rep("gray40",vcount(grafo))
vcol[caminhodiametro] = "gold"
ecol = rep("gray80",ecount(grafo))
ecol[E(grafo,path=caminhodiametro)] = 'orange'
tkplot(grafo, vertex.color=vcol,layout=layout.lgl, vertex.size=deg*0.4, edge.color=ecol, edge.arrow.mode=0)

######################## DISTANCIA DO GRAFO ########################

distancias = distances(grafo)
#write.xlsx(distancias,"C:/Users/Fox//Documents/matriz.xlsx", row.names = FALSE)

######################## DENSIDADE ########################

densidade = edge_density(grafo)
densidade

######################## CENTRALIDADE DE AUTOVETOR ########################

eigen_centrality(grafo)
sort(eigen_centrality(grafo)$vector, decreasing = TRUE)[1:10]

# ######################## VIZINHOS ########################

neighbors(grafo,'Homer Simpson')
neighbors(grafo, 'Agnes Skinner')
neighborhood(grafo, order=1, 'Milhouse Van Houten')
neighborhood(grafo, order =2, "Agnes Skinner")
neighbors(grafo, 'Troy McClure')
neighborhood(grafo, order =2, "Troy McClure")
neighbors(grafo, 'Apu Nahasapeemapetilon')

# plot dos grafos
grafosub = subgraph.edges(grafo,E(grafo)[inc(c('Apu Nahasapeemapetilon'))])
tkplot(grafosub, vertex.color='Orange')

grafosub = subgraph.edges(grafo,E(grafo)[inc(c('Troy McClure', 'Homer Simpson'))])
tkplot(grafosub)

######################## MODULARIDADE ########################

comunidade = cluster_edge_betweenness(grafo)

modularity(comunidade)

plot(comunidade,grafo,vertex.label = NA, vertex.size = 3)

# vizualizar comunidades
membros = membership(comunidade)
table(membros)

comunidade4= comunidade[4]

# plot da comunidade
plot(comunidade
     ,grafo
     ,vertex.label = as.character(membros)
     , vertex.size = 3)


# plots e analises sem os simpsons
#abrir arquivo "sem_simpsons.csv"
arestas_sem_simp <- read.csv(file = file.choose(), header = TRUE, sep = ';', dec = ',')#edge

sem_simp = graph_from_data_frame(d=arestas_sem_simp, directed=F)
tkplot(sem_simp)

sort(degree(sem_simp.decreasing=TRUE))

mean(degree(sem_simp))

narest_sem_simp = ecount(sem_simp)
narest_sem_simp

inter_sem_simp = betweenness(sem_simp)
inter_sem_simp

mean(inter_sem_simp)


diametro_sem_simp = diameter(sem_simp)
diametro_sem_simp