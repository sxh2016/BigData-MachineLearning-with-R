this.file <- parent.frame(2)$ofile
this.dir <- dirname(this.file)

setwd(this.dir)

directory = list.dirs('./maildir',recursive = TRUE)
NUM_PEOPLE = 26

content <- matrix(" ",nrow = 0,ncol = 5,byrow = FALSE)
colnames(content) = c("From","To","Date","Subject","MessageID")
link <- matrix(" ",nrow = 0,ncol = 2,byrow = FALSE)
linkFrom <- matrix(" ",nrow = 0,ncol = 1,byrow = FALSE)
linkTo <- matrix(" ",nrow = 0,ncol = 1,byrow = FALSE)
colnames(link) = c("From","To")

# read files in all directories
for (direc in directory) {
  files <- list.files(direc)
  
  for (file in files) {
    if (file == "1") {
      fileName = paste0(direc,"/1")
      conn = file(fileName, open = "r")
      line <- readLines(conn)
      
      #retrieve From, To, Subject, Date and Message-ID from all emails and save them into matrix
      x1 <- line[grepl('From',line)]
      from <- substr(x1[1], 6, 1000000L)
      from <- trimws(from)
      
      x2 <- line[grepl('^To:',line)]
      to <- substr(x2[1], 4, 1000000L)
      if (grepl('Subject',line) - grepl('^To:',line) > 1) {
        for (i in grepl('^To:',line) + 1:grepl('Subject',line) - 1) {
          to <- paste(to,line[i])
        }
      }
      
      str <- as.list(strsplit(to, ", ")[[1]])
      
      for (su in str) {
        su <- trimws(su)
        link <- rbind(link, c(from,su))
        linkFrom <- rbind(linkFrom, c(from))
        linkTo <- rbind(linkTo, c(su))
      }
      
      x3 <- line[grepl('Date',line)]
      date <- substr(x3[1], 6, 1000000L)
      
      x4 <- line[grepl('Subject',line)]
      subject <- substr(x4[1], 9, 1000000L)
      
      x5 <- line[grepl('Message-ID',line)]
      id <- substr(x5, 12, 1000000L)
      content <- rbind(content, c(from,to,date,subject,id))
      
      close(conn)
    }
  }
}


exampleEmail <- unique(linkFrom)
email <- list()

#select 26 emails as target and find their relationship
n = 1
for (ele in exampleEmail) {
  if(length(grep("@enron.com",ele))>0) 
    email[n] <- ele
  else
    next
  n = n + 1
  if (n > NUM_PEOPLE)
    break
}

#build connection table
connections <- matrix(0,nrow = NUM_PEOPLE,ncol = NUM_PEOPLE)
#connection1 equals connection square
connections1 <- matrix(0,nrow = NUM_PEOPLE,ncol = NUM_PEOPLE)
colnames(connections) <- email
rownames(connections) <-  email

i = 1
for (ele1 in email) {
  j = 0
  
  for (ele2 in email) {
    j = j + 1
    if (ele1 == ele2)
      next
    
    t <- which(linkFrom == ele1,arr.ind = T)
    count = 0
    for (x in as.list(t)) {
      count = count + 1
      if (count > length(t) / 2)
        break
      
      if (is.null(linkTo[x]) || is.na(linkTo[x])) {
        next
      }
      
      if (linkTo[x] == ele2) {
        connections[j,i] <- 1
      }
    }
  }
  i = i + 1
}
connections1 <- connections %*% connections
library(sna)
library(igraph)
#calculate betweenness
ig <- graph.adjacency(connections, mode = "directed", weighted = TRUE)
str(ig)
plot(ig)
#plot(ig, edge.label=round(E(ig)$weight, 3))

print("betweenness matrix is: ")
print(betweenness(ig))
#calculate indegree
indegreeG <- degree(ig, mode = "in")
print("indegree matrix is :")
print(indegreeG)
#calculate outdegree
outdegreeG <- degree(ig, mode = "out")
print("outdegree matrix is :")
print(outdegreeG)
#calculate number of edges
print("the total number of edges")
print(ecount(ig))
#calculate n
n <- nrow(connections)
print("The total number of nodes")
print(n)
# calculate total number of nodes of set li
# set li means the set of nodes that can reach node l
print("li")
li <- prestige(connections, gmode = "digraph", cmode = "domain")
print(li)
#calculate distance from every two nodes
tmp <- shortest.paths(ig, mode = c("in"))
print("every distance")
print(tmp)
res <- vector(mode = "numeric", length = n)
for (i in 1:n) {
  for (j in 1:n) {
    if (tmp[i,j] != Inf) {
      res[i] <- res[i] + tmp[i,j]
    }
  }
}
print("total distance")
print(res)
# calculate proximity prestige
ans <- vector()
ans <- res / li
print("ans")
print(ans)