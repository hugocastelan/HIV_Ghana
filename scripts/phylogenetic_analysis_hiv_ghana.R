##This script is for count the introductions of Subtype HIV to Ghana 
##This script is a modification of the original script by Simon Dellicour for calculating the introduction of SARS-CoV-2.

##Read libraries 
library(seraphim)  
library(lubridate) 
library(diagram)
library(treeio)
library(ape)

writingFiles = FALSE
showingPlots = FALSE

analysis = "virus_vih"

# 1. Preparation of the discrete phylogeographic analysis

tree = readAnnotatedNexus(paste0(analysis,".tre"))
data = read.csv(paste0(analysis,".csv"), head=T)
seqIDs1 = tree$tip.label
seqIDs2 = c()
locations = rep(NA, length(seqIDs1))
collection_dates = rep(NA, length(seqIDs1))

for (i in 1:length(seqIDs1)) {
  # Dividimos el ID por los guiones bajos
  parts = unlist(strsplit(seqIDs1[i], "_"))
  
  # Verificamos si alguna parte contiene "GH"
  if ("GH" %in% parts) {
    locations[i] = "GH"
    seqIDs2 = c(seqIDs2, seqIDs1[i])
  } else {
    locations[i] = "other"
  }
  
  # data is the last part 
  collection_dates[i] = parts[length(parts)]
}

# Creating the table with the processed data
tab = cbind(gsub("'","",seqIDs1), locations, collection_dates)
colnames(tab) = c("sequence_ID", "location", "collection_date")

# Escritura de los archivos de salida
txt = c()
for (i in 1:length(seqIDs1)) {
  txt = c(txt, paste0(">", gsub("'","",seqIDs1[i])), "NNNN")
}
write.table(tab, paste0(analysis, ".txt"), row.names = FALSE, quote = FALSE, sep = "\t")
write(txt, paste0(analysis, ".fasta"))

burnIn <- 101 
collection_dates_numeric <- as.numeric(collection_dates)
trees <- scan(paste0(analysis, ".trees"), what = "", sep = "\n", quiet = TRUE, blank.lines.skip = FALSE)

# Asegurarse de que los índices no contengan NA
indices1 <- which(!grepl("tree STATE_", trees))
indices2 <- which(grepl("tree STATE_", trees))

if (length(indices2) <= burnIn) {
  stop("Error: No hay suficientes árboles después del burn-in.")
}

mostRecentSamplingDate <- max(collection_dates_numeric, na.rm = TRUE)
ghanaBranches_list <- rep(NA, length(trees))
ghanaIntroductions_list <- rep(NA, length(trees))
ghanaTipBranches_list <- rep(NA, length(trees))
ghana_tMRCAs_list <- list()

for (i in (burnIn + 1):length(indices2)) {
  cat("Processing tree ", i, " of ", length(indices2), "\n")
  
  # Seleccionar los índices para el subárbol
  selected_indices <- c(indices1[1:(length(indices1) - 1)], indices2[i], indices1[length(indices1)])
  
  # Verificar si los índices están dentro de los límites
  if (any(is.na(selected_indices)) || any(selected_indices > length(trees) | selected_indices < 1)) {
    cat("Error: Index out of bounds detected or NA in selected indices. Skipping iteration.\n")
    next
  }
  
  tree1 <- trees[selected_indices]
  temp_file <- paste0("TEMP_sampled_tree_", i, ".tree")
  
  # save the tree in temp 
  write(tree1, temp_file)
  
  # Read the tree using seraphim
  tree2 <- tryCatch({
    readAnnotatedNexus(temp_file)
  }, error = function(e) {
    cat("Error reading tree file for index ", i, ": ", e$message, "\n")
    return(NULL)
  })
  
  #Read the tree 
  if (is.null(tree2)) next
  
  # Initialization of counters
  ghanaBranches <- 0
  ghanaIntroductions <- 0
  ghanaTipBranches <- 0
  ghana_tMRCAs <- c()
  
  for (j in 1:dim(tree2$edge)[1]) {
    if (!is.null(tree2$annotations[[j]]) && tree2$annotations[[j]]$location == "GH") {
      ghanaBranches <- ghanaBranches + 1
      index <- which(tree2$edge[, 2] == tree2$edge[j, 1])
      
      if (length(index) > 0 && !is.null(tree2$annotations[[index]]) && tree2$annotations[[index]]$location != "GH") {
        ghanaIntroductions <- ghanaIntroductions + 1
        tMRCA <- mostRecentSamplingDate - nodeheight(tree2, tree2$edge[j, 1])
        ghana_tMRCAs <- c(ghana_tMRCAs, tMRCA)
      }
      
      if (!tree2$edge[j, 2] %in% tree2$edge[, 1]) {
        ghanaTipBranches <- ghanaTipBranches + 1
      }
    }
  }
  
  # Save results 
  ghanaBranches_list[i] <- ghanaBranches
  ghanaIntroductions_list[i] <- ghanaIntroductions
  ghanaTipBranches_list[i] <- ghanaTipBranches
  ghana_tMRCAs_list[[i]] <- ghana_tMRCAs
  
  # remove temporal file 
  file.remove(temp_file)
}

# Ajustar conteo de muestras de Ghana
total_ghana_samples <- 47

# Calcular percentiles y medianas
quantiles <- quantile(ghanaIntroductions_list[!is.na(ghanaIntroductions_list)], probs = c(0.025, 0.975))
median_introductions <- median(ghanaIntroductions_list[!is.na(ghanaIntroductions_list)])

cat("A minimum number of ", median_introductions, 
    " VIH subtype CRF02_AG introduction event (95% HPD interval = [", quantiles[1], "-", quantiles[2], 
    "]) identified from the phylogenetic analysis of ", total_ghana_samples, 
    " samples collected in Ghana", sep = "")



if (showingPlots)
{
  tree = readAnnotatedNexus(paste0(analysis,".tree"))
  rootHeight = max(nodeHeights(tree)); root_time = mostRecentSamplingDate-rootHeight
  selectedLabels = c("01-11-1960","01-01-1970","01-01-1980","01-01-1990","01-01-2000","01-01-2010","01-01-2020","01-01-2024")
  selectedDates = decimal_date(dmy(selectedLabels))
  cols = rep("gray30",dim(tree$edge)[1]); lwds = rep(0.1,dim(tree$edge)[1])
  for (i in 1:dim(tree$edge)[1])
  {
    if (tree$edge[i,1]%in%tree$edge[,2])
    {
      index = which(tree$edge[,2]==tree$edge[i,1])
      if ((tree$annotations[[index]]$location=="GH")&(tree$annotations[[i]]$location=="GH"))
      {
        cols[i] = rgb(0,104,71,255,maxColorValue=255); lwds[i] = 0.6 # green of the Ghana flag
      }
    }
  }
  pdf("Figure_1_NEW.pdf", width=15, height=15); par(oma=c(0,0,0,0), mar=c(0,0,0,0.0), lwd=0.1)
  plot(tree, type="fan", show.tip.label=T, show.node.label=F, edge.width=lwds, cex=0.4, align.tip.label=0.6, col="gray30", edge.color=cols)
  for (i in 1:dim(tree$edge)[1])
  {
    if (tree$annotations[[i]]$location == "GH")
    {
      index = which(tree$edge[,2]==tree$edge[i,1])
      if (tree$annotations[[index]]$location != "GH")
      {
        nodelabels(node=tree$edge[i,2], pch=16, cex=1, col=rgb(206,17,38,255,maxColorValue=255)) # red of the ghana flag
        nodelabels(node=tree$edge[i,2], pch=1, cex=1, col="gray30", lwd=0.5)
      }
    }
  }
  add.scale.bar(x=0.3, y=0.15, length=NULL, ask=F, lwd=0.5 , lcol ="gray30", cex=0.7)
  dev.off()
  
  pdf("Figure_2_NEW.pdf", width=7, height=7); par(oma=c(0,0,0,0), mar=c(0,0,0,0.0), lwd=0.1)
  plot(tree, show.tip.label=F, show.node.label=F, edge.width=lwds, cex=0.6, align.tip.label=3, col="gray30", edge.color=cols)
  for (i in 1:dim(tree$edge)[1])
  {
    if (tree$annotations[[i]]$location == "GH")
    {
      index = which(tree$edge[,2]==tree$edge[i,1])
      if (tree$annotations[[index]]$location != "GH")
      {
        nodelabels(node=tree$edge[i,2], pch=16, cex=0.6, col=rgb(206,17,38,255,maxColorValue=255)) # red of the Mexico flag
        nodelabels(node=tree$edge[i,2], pch=1, cex=0.6, col="gray30", lwd=0.5)
      }
    }
  }
  axis(lwd=0.2, at=selectedDates-root_time, labels=selectedLabels, cex.axis=0.5, mgp=c(0,0.05,-0.9), lwd.tick=0.2, 
       col.lab="gray30", col="gray30", tck=-0.005, side=1)
  dev.off()
}


# Plotea el árbol en forma de abanico
plot(tree, type="fan", show.tip.label=FALSE, show.node.label=FALSE, edge.width=lwds, cex=0.5, align.tip.label=0.5, col="gray30", edge.color=cols)

# Ajustar el radio para las etiquetas en forma de círculo
angle <- seq(0, 2*pi, length.out = length(leaf_labels) + 1)[-1]
radius <- max(c(tree$edge.length)) * 1.2  # Ajustar el radio para que las etiquetas estén más afuera

# Mover los círculos 4 posiciones en el sentido de las agujas del reloj
shift <- 2 * pi / length(leaf_labels) * 1  # Calcula el ángulo de desplazamiento para cuatro posiciones
angle <- (angle - shift) %% (2 * pi)  # Desplaza los ángulos hacia la derecha (sentido de las agujas del reloj)

tip_coords <- data.frame(
  x = radius * cos(angle),
  y = radius * sin(angle)
)

# Añadir círculos de colores correspondientes en las hojas
for (i in 1:length(tip_coords$x)) {
  points(tip_coords$x[i], tip_coords$y[i], pch=21, bg=leaf_colors[i], col="black", cex=1.5)
}

# Añadir triángulos en los nodos internos con el color de la bandera de Ghana
for (i in 1:nrow(tree$edge)) {
  if (tree$annotations[[i]]$location == "GH") {
    index <- which(tree$edge[, 2] == tree$edge[i, 1])
    if (tree$annotations[[index]]$location != "GH") {
      nodelabels(node=tree$edge[i, 2], pch=24, cex=1.5, col=rgb(206, 17, 38, 255, maxColorValue=255), bg=rgb(206, 17, 38, 255, maxColorValue=255))  # Triángulo rojo
      nodelabels(node=tree$edge[i, 2], pch=2, cex=1.5, col="gray30", lwd=0.5, lty=2)  # Triángulo contorno gris
    }
  }
}

# Asegurar que las ramas tengan la misma longitud para lograr un efecto de cladograma
for (i in 1:nrow(tree$edge)) {
  if (tree$edge[i, 2] > Ntip(tree)) {
    node_height <- max(branching.times(tree)) - branching.times(tree)[tree$edge[i, 2] - Ntip(tree)]
    segments(tree$edge[i, 1], tree$edge[i, 2], node_height, tree$edge[i, 2], col="gray30", lwd=0.5, lty=2)
  }
}



