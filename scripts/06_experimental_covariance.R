#Setup----
#Updated April 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Estimate experimental covariance in space and time
#NOTE: updated code from Marc Serre's BMElib Bayesian Maximum Entropy library written in MATLAB. https://mserre.sph.unc.edu/BMElib_web/
#updated functions include: 
# coord2dist
# pairsindex
# stcov
#this file conducts a parallelized calculation of experimental covariance from daily heat index and hourly precipitation at a monthly chunk

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/06_experimental_covariance.R")
source(here::here("scripts", "01_library.R"))

## Functions ----
coord2dist <- function(c1, c2) {
  
  # 1. Handle “cell‐array” input. If you pass in c1 and c2 as lists (to mimic MATLAB’s 
  # cell arrays carrying an extra index vector), it will extract the first element of 
  # each list to get back to the raw coordinate matrices.
  
  # 2. Compute pairwise Euclidean distances. For each row𝑖of c1 and each row 𝑗of c2, 
  # it computes𝐷[𝑖,𝑗]=∑𝑘=1𝑑(𝑐1[𝑖,𝑘]−𝑐2[𝑗,𝑘])2
  
  if (is.data.frame(c1)) c1 <- as.matrix(c1)
  if (is.data.frame(c2)) c2 <- as.matrix(c2)
  
  # If inputs are lists (MATLAB‐style cell arrays), pull out the coordinate matrices
  if (is.list(c1)) {
    c1 <- c1[[1]]
    c2 <- c2[[1]]
  }
  
  # Ensure we now have plain numeric matrices
  if (!is.matrix(c1) || !is.matrix(c2)) {
    stop("coord2dist: both inputs must be numeric matrices (or single‐element lists of matrices).")
  }
  
  # Dimensions
  n1 <- nrow(c1)
  n2 <- nrow(c2)
  
  # Efficient vectorized Euclidean distance via cross‐product identity:
  #   ||x - y||^2 = ||x||^2 + ||y||^2 - 2 x·y
  x1_sq <- rowSums(c1^2)           # length‐n1
  x2_sq <- rowSums(c2^2)           # length‐n2
  
  # Build matrix of squared distances
  #   outer(x1_sq, rep(1,n2)) + outer(rep(1,n1), x2_sq)  – 2 * c1 %*% t(c2)
  D2 <- matrix(x1_sq, nrow = n1, ncol = n2) +
    matrix(x2_sq, nrow = n1, ncol = n2, byrow = TRUE) -
    2 * tcrossprod(c1, c2)
  
  # Numerical safety: negative tiny values → zero
  D2[D2 < 0] <- 0
  
  # Return the sqrt of the squared distances
  sqrt(D2)
}

pairsindex <- function(
    c1,      # n1 × 2 matrix or data.frame of coords
    c2,      # n2 × 2 matrix or data.frame of coords
    rLag,    # numeric vector length nr
    rLagTol, # numeric vector length nr
    dist     = list("coord2dist"),  # list: [[1]] = name or function
    method   = NULL
) {
  
  if (is.data.frame(c1)) c1 <- as.matrix(c1)
  if (is.data.frame(c2)) c2 <- as.matrix(c2)
  
  # 1. dimensions and defaults
  n1 <- nrow(c1);  n2 <- nrow(c2);  nr <- length(rLag)
  if (!is.list(dist)) stop("dist must be a list")
  if (is.null(method)) {
    method <- if (n1 * n2 < 500*500) "kron" else "kronloop"
  }
  if ((n1 < 2 || n2 < 2) && method == "superblock") {
    warning("c1 or c2 has only one point: switching method to 'kron'")
    method <- "kron"
  }
  
  # prepare output: a list of length nr
  idxpairs <- vector("list", nr)
  
  # helper to call your distance function
  call_dist <- function(A, B) {
    if (length(dist) == 1) {
      do.call(dist[[1]], list(A, B))
    } else {
      do.call(dist[[1]], list(A, B, dist[[2]]))
    }
  }
  
  # 2. three methods
  switch(method,
         
         kron = {
           # compute full n1×n2 distance matrix D
           D <- call_dist(c1, c2)
           for (ir in seq_len(nr)) {
             mask <- (D >= rLag[ir] - rLagTol[ir]) &
               (D <= rLag[ir] + rLagTol[ir])
             # which(..., arr.ind=TRUE) returns matrix of (i,j) pairs
             idxpairs[[ir]] <- which(mask, arr.ind = TRUE)
           }
         },
         
         kronloop = {
           # initialize each cell
           for (ir in seq_len(nr)) idxpairs[[ir]] <- matrix(numeric(0), ncol = 2)
           for (i in seq_len(n1)) {
             # distances from c1[i,] to all of c2
             d_i <- call_dist(matrix(c1[i,], nrow = 1), c2)
             for (ir in seq_len(nr)) {
               js <- which(d_i >= rLag[ir] - rLagTol[ir] &
                             d_i <= rLag[ir] + rLagTol[ir])
               if (length(js) > 0) {
                 idxpairs[[ir]] <- rbind(idxpairs[[ir]], cbind(i, js))
               }
             }
           }
         },
         
         superblock = {
           # subdivide domain into n×n superblocks
           n <- 10
           # helper to compute block ID from coords
           make_sb <- function(coords, minxy, dxy) {
             # zero‐based block indices
             sb <- pmin(n-1, floor((coords - minxy) / dxy))
             sb[,1] + n * sb[,2]
           }
           # 2D bounds & increments
           x1min <- min(c1[,1]); x1max <- max(c1[,1])
           y1min <- min(c1[,2]); y1max <- max(c1[,2])
           dx1 <- (x1max - x1min) / n;  dy1 <- (y1max - y1min) / n
           x2min <- min(c2[,1]); x2max <- max(c2[,1])
           y2min <- min(c2[,2]); y2max <- max(c2[,2])
           dx2 <- (x2max - x2min) / n;  dy2 <- (y2max - y2min) / n
           
           # assign superblock IDs
           sb1 <- make_sb(c1, c(x1min, y1min), c(dx1, dy1))
           sb2 <- make_sb(c2, c(x2min, y2min), c(dx2, dy2))
           # group point indices by block
           sb_idx1 <- sort(unique(sb1))
           sb_set1 <- lapply(sb_idx1, function(b) which(sb1 == b))
           sb_idx2 <- sort(unique(sb2))
           sb_set2 <- lapply(sb_idx2, function(b) which(sb2 == b))
           
           # check we didn't lose any points
           if (sum(lengths(sb_set1)) != n1 ||
               sum(lengths(sb_set2)) != n2) {
             stop("superblock: mismatch in point counts")
           }
           
           # compute center coords of each superblock
           center <- function(minxy, dxy, idx) {
             # idx zero‐based; row = idx %% n, col = idx %/% n
             r <- idx %% n
             c <- idx %/% n
             cbind(minxy[1] + (r + 0.5) * dxy[1],
                   minxy[2] + (c + 0.5) * dxy[2])
           }
           ctr1 <- center(c(x1min, y1min), c(dx1, dy1), sb_idx1)
           ctr2 <- center(c(x2min, y2min), c(dx2, dy2), sb_idx2)
           
           # buffer on block‐center distances
           dr <- sqrt(dx1^2 + dy1^2) + sqrt(dx2^2 + dy2^2)
           
           for (ir in seq_len(nr)) {
             idxpairs[[ir]] <- matrix(numeric(0), ncol = 2)
             # find block‐pairs whose centers might lie in [rLag±tol] ± dr
             for (i1 in seq_along(sb_idx1)) {
               d_block <- call_dist(matrix(ctr1[i1,], nrow=1), ctr2)
               j2 <- which(d_block >= rLag[ir] - rLagTol[ir] - dr &
                             d_block <= rLag[ir] + rLagTol[ir] + dr)
               for (j1 in j2) {
                 pts1 <- sb_set1[[i1]]
                 pts2 <- sb_set2[[j1]]
                 Dk <- call_dist(c1[pts1, ,drop=FALSE],
                                 c2[pts2, ,drop=FALSE])
                 pairs_ij <- which(Dk >= rLag[ir] - rLagTol[ir] &
                                     Dk <= rLag[ir] + rLagTol[ir],
                                   arr.ind = TRUE)
                 if (nrow(pairs_ij)>0) {
                   idxpairs[[ir]] <- rbind(
                     idxpairs[[ir]],
                     cbind(pts1[pairs_ij[,1]], pts2[pairs_ij[,2]])
                   )
                 }
               }
             }
           }
         },
         
         stop("method must be 'kron', 'kronloop' or 'superblock'")
  )
  
  # 3. sort each pair list by (i,j)
  for (ir in seq_len(nr)) {
    if (nrow(idxpairs[[ir]]) > 0) {
      idxpairs[[ir]] <- idxpairs[[ir]][
        order(idxpairs[[ir]][,1], idxpairs[[ir]][,2]), , drop = FALSE
      ]
    }
  }
  
  idxpairs
}

stcov <- function(
    Zi,   # matrix nMSi × nMEi
    cMSi, # data.frame or matrix nMSi × 2
    tMEi, # numeric vector length nMEi
    Zj,   # matrix nMSj × nMEj
    cMSj, # data.frame or matrix nMSj × 2
    tMEj, # numeric vector length nMEj
    rLag,     # numeric vector length nr
    rLagTol,  # numeric vector length nr
    tLag,     # numeric vector length nt
    tLagTol,  # numeric vector length nt
    dist = list("coord2dist"),  # list whose [[1]] is name of an R function
    method = NULL
) {
  
  #---- 1. Input validation ----#
  nMSi <- nrow(Zi);  nMEi <- ncol(Zi)
  if (!all(dim(cMSi) == c(nMSi, 2)))
    stop("cMSi must be nMSi × 2")
  if (length(tMEi) != nMEi)
    stop("tMEi must be length nMEi")
  
  nMSj <- nrow(Zj);  nMEj <- ncol(Zj)
  if (!all(dim(cMSj) == c(nMSj, 2)))
    stop("cMSj must be nMSj × 2")
  if (length(tMEj) != nMEj)
    stop("tMEj must be length nMEj")
  
  nr <- length(rLag)
  if (length(rLagTol) != nr)
    stop("rLag and rLagTol must have same length")
  nt <- length(tLag)
  if (length(tLagTol) != nt)
    stop("tLag and tLagTol must have same length")
  
  if (!is.list(dist) || !(is.character(dist[[1]]) || is.function(dist[[1]]))) {
    stop("dist must be a list whose [[1]] is the name or function for distance")
  }
  
  #---- 2. Default method ----#
  if (is.null(method)) {
    method <- if (nMSi * nMSj < 500*500) "kron" else "kronloop"
  }
  
  #---- 3. Spatial‐pair indexing (user must supply pairsindex) ----#
  #   idxpairsMS is a list of length nr; each element is a two‑column matrix
  #   of station‑index pairs (i, j).
  idxpairsMS <- pairsindex(cMSi, cMSj, rLag, rLagTol, dist, method)
  
  #---- 4. Temporal‐difference matrix ----#
  #   tMat[a,b] = tMEj[b] - tMEi[a]
  tMat <- outer(tMEi, tMEj, function(ti, tj) tj - ti)
  
  # # helper mats to recover event indices
  # iMEmat <- matrix(rep(seq_len(nMEi), times = nMEj),
  #                  nrow = nMEi, ncol = nMEj)
  # jMEmat <- matrix(rep(seq_len(nMEj), each  = nMEi),
  #                  nrow = nMEi, ncol = nMEj)
  
  #---- 5. Preallocate output ----#
  C  <- matrix(NA_real_, nrow = nr, ncol = nt)
  np <- matrix(   0L, nrow = nr, ncol = nt)
  
  #---- 6. Main loops ----#
  for (ir in seq_len(nr)) {
    pairs_ij <- idxpairsMS[[ir]]  # p_r × 2 matrix
    if (nrow(pairs_ij) == 0L) {
      C[ir, ]  <- NA
      np[ir, ] <- 0L
      next
    }
    iMS <- pairs_ij[,1]
    jMS <- pairs_ij[,2]
    
    for (it in seq_len(nt)) {
      # boolean mask of event‐pairs in the time‐lag window
      mask <- (tLag[it] - tLagTol[it] <= tMat) &
        (tMat <= tLag[it] + tLagTol[it])
      if (!any(mask)) {
        C[ir,it]  <- NA
        np[ir,it] <- 0L
        next
      }
      
      # which() with arr.ind gives a two‑column matrix of (i_event, j_event)
      ev_idx <- which(mask, arr.ind = TRUE)
      # now build all combinations with the spatial pairs
      # replicate each station‐pair for each event‐pair
      nSP <- nrow(pairs_ij)
      nEP <- nrow(ev_idx)
      sp_rep   <- pairs_ij[rep(seq_len(nSP), times = nEP), , drop = FALSE]
      ep_rep   <- ev_idx[rep(seq_len(nEP), each = nSP), , drop = FALSE]
      
      # extract values
      head_vals <- Zi[cbind(sp_rep[,1], ep_rep[,1])]
      tail_vals <- Zj[cbind(sp_rep[,2], ep_rep[,2])]
      valid     <- !is.na(head_vals) & !is.na(tail_vals)
      
      np[ir,it] <- sum(valid)
      if (np[ir,it] == 0L) {
        C[ir,it] <- NA
      } else {
        h <- head_vals[valid]
        t <- tail_vals[valid]
        C[ir,it] <- mean(h * t) - mean(h) * mean(t)
      }
    }
  }
  
  #---- 7. Return ----#
  list(C = C, np = np)
}

readGeoEAS <- function(filepath) {
  # Read all lines
  lines <- readLines(filepath)
  
  # 1) First line is the file title
  filetitle <- lines[1]
  
  # 2) Second line is the number of rows (stations)
  nrows <- as.integer(lines[2])
  if (is.na(nrows) || nrows < 1) {
    stop("Invalid station count on line 2 of ", filepath)
  }
  
  # 3) Find where data begins: the first line (after line 2)
  #    whose first token is numeric (x coordinate)
  is_data_line <- grepl("^\\s*[-+]?[0-9]", lines)
  data_start   <- which(is_data_line)[1]
  if (is.na(data_start) || data_start <= 2) {
    stop("Cannot locate data lines in ", filepath)
  }
  
  # 4) All lines 3:(data_start-1) are header names, one per column
  header_lines <- lines[3:(data_start - 1)]
  # Trim whitespace and convert spaces to underscores for safe names
  col_names <- gsub("\\s+", "_", trimws(header_lines))
  
  # 5) Read the data block as a table
  dat <- read.table(
    filepath,
    skip    = data_start - 1,
    nrows   = nrows,
    header  = FALSE,
    as.is   = TRUE
  )
  if (ncol(dat) != length(col_names)) {
    stop("Mismatch between header count (", length(col_names),
         ") and data columns (", ncol(dat), ") in ", filepath)
  }
  colnames(dat) <- col_names
  
  # 6) Return the values matrix, the variable names (excluding coords), and title
  list(
    val       = as.matrix(dat),
    valname   = col_names[-(1:2)],  # drop x_deg and y_deg
    filetitle = filetitle
  )
}

## Main script ----
##— PACKAGES & PARALLEL SETUP —##
library(future)
library(future.apply)
library(progressr)

plan(multisession, workers = 19)
handlers("txtprogress")

##— HELPERS —##

estimate_dist_mem <- function(n1, n2, bytes_per = 8) {
  bytes <- as.numeric(n1) * as.numeric(n2) * bytes_per
  list(MB = bytes / 1024^2, GB = bytes / 1024^3)
}

quantest <- function(x, probs) {
  stats::quantile(x, probs = probs, na.rm = TRUE)
}

dist_mem_threshold_gb <- 2

##— PARAMETERS —##
filepath     <- "V:/…/heat_index/month/"
filestem     <- "heat_index_"
outdir_space <- "V:/…/heat_index/month/"
if (!dir.exists(outdir_space)) dir.create(outdir_space, recursive = TRUE)

startYear    <- 1940; startMonth <- 1
endYear      <- 2023; endMonth   <- 12

N_spatial       <- 15; lambda_spatial   <- 4
N_temporal      <- 15; lambda_temporal  <- 4

yearMonthSequence <- {
  tmp <- integer((endYear-startYear)*12 + (endMonth-startMonth) + 1)
  idx <- 1
  for (yr in startYear:endYear) {
    for (mo in 1:12) {
      if ((yr==startYear && mo<startMonth) ||
          (yr==endYear   && mo> endMonth)) next
      tmp[idx] <- yr*100 + mo
      idx <- idx + 1
    }
  }
  tmp[1:(idx-1)]
}

##— TIMING START —##
start_time <- Sys.time()
message("Processing started at: ", format(start_time, "%Y-%m-%d %H:%M:%S"))

##— PARALLEL LOOP with PROGRESS & MEM CHECK —##
with_progress({
  p <- progressor(along = yearMonthSequence)
  
  future_lapply(yearMonthSequence, function(current) {
    p()
    
    infile <- file.path(filepath, paste0(filestem, current, ".txt"))
    geo    <- readGeoEAS(infile)
    val    <- geo$val
    sMS    <- val[,1:2]
    Zh     <- val[,-c(1,2), drop=FALSE]
    
    nMS <- nrow(sMS)
    mem <- estimate_dist_mem(nMS, nMS)
    message(sprintf("[%d] Dist matrix ~ %.1f MB (%.2f GB)", 
                    current, mem$MB, mem$GB))
    if (mem$GB > dist_mem_threshold_gb) {
      stop(sprintf("Requires %.2f GB > %.1f GB threshold", 
                   mem$GB, dist_mem_threshold_gb))
    }
    
    tME <- seq_len(ncol(Zh))
    
    # Spatial
    D   <- coord2dist(sMS, sMS)
    d   <- D[D > 0]
    raw_s   <- 1 - exp(-lambda_spatial * (seq_len(N_spatial)/N_spatial))
    probs_s <- raw_s / max(raw_s)
    cl_s    <- c(0, quantest(d, probs_s))
    rLag    <- c(0, 0.5*(head(cl_s,-1)+tail(cl_s,-1)))
    rLagTol <- c(0, diff(cl_s)/2.01)
    sp      <- stcov(Zh, sMS, tME, Zh, sMS, tME, rLag, rLagTol, 0, 0)
    Cr      <- sp$C
    
    # Temporal
    tMat  <- abs(outer(tME, tME, `-`))
    d_t   <- tMat[tMat > 0]
    raw_t   <- 1 - exp(-lambda_temporal * (seq_len(N_temporal)/N_temporal))
    probs_t <- raw_t / max(raw_t)
    cl_t    <- c(0, quantest(d_t, probs_t))
    tLag    <- c(0, 0.5*(head(cl_t,-1)+tail(cl_t,-1)))
    tLagTol <- c(0, diff(cl_t)/2.01)
    tm      <- stcov(Zh, sMS, tME, Zh, sMS, tME, 0, 0, tLag, tLagTol)
    Ct      <- tm$C
    
    # Write outputs
    write.table(
      data.frame(rLag = rLag, Cov = Cr[,1]),
      file = file.path(outdir_space,
                       paste0("heat_index_experimental_covariance_space_", current, ".txt")),
      sep = "\t", row.names = FALSE, quote = FALSE
    )
    write.table(
      data.frame(tLag = tLag, Cov = Ct[1,]),
      file = file.path(outdir_space,
                       paste0("heat_index_experimental_covariance_time_", current, ".txt")),
      sep = "\t", row.names = FALSE, quote = FALSE
    )
    
    NULL
  })
})

##— TIMING END —##
end_time <- Sys.time()
message("Processing finished at: ", format(end_time, "%Y-%m-%d %H:%M:%S"))
message("Elapsed time: ", round(difftime(end_time, start_time, units="secs"), 1), " seconds")
