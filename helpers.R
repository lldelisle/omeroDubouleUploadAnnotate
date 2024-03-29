subsetObjectByOwner <- function(listObj, owner.id) {
  owner.ids <- sapply(listObj, function(obj) {
    obj@dataobject$getOwner()$getId()
  })
  return(lapply(which(owner.ids == owner.id), function(i) {
    listObj[[i]]
  }))
}

parseImportOutput <- function(importOutput) {
  lines.with.images.ids <- grep("^Image:", importOutput)
  images.ids <- strsplit(gsub("Image:", "", importOutput[lines.with.images.ids]),
    ",")
  files <- sapply(strsplit(importOutput[lines.with.images.ids - 1], "Imported file: "),
    "[[", 2)
  files.rep <- rep(files, sapply(images.ids, length))
  return(data.frame(id = unlist(images.ids), original_file = files.rep))
}

getKeyValueDFFromImage <- function(omero.server, my.i) {
  annots <- getAnnotations(my.i)
  if (nrow(annots) > 0 && "MapAnnotationData" %in% annots$Type) {
    keyvalues <- apply(subset(annots, Type == "MapAnnotationData"), 1, function(v) {
      loadObject(omero.server, v["Type"], v["ID"])@dataobject$getContentAsString()
    })
  } else {
    keyvalues <- NULL
  }
  if (length(keyvalues) > 0) {
    keyvalues <- unlist(strsplit(keyvalues, ";"))
    keyvalues.split <- strsplit(keyvalues, "=")
    values <- sapply(keyvalues.split, tail, 1)
    keys <- sapply(sapply(keyvalues.split, head, -1), paste, collapse = "=")
  } else {
    keys <- character(0)
    values <- character(0)
  }
  return(data.frame(key = keys, value = values))
}

initiateDF <- function(omero.server, dataset) {
  all.images <- getImages(dataset)
  one.per.line <- do.call(rbind, lapply(all.images, function(my.i) {
    # print(my.i@dataobject$getName())
    temp.df <- getKeyValueDFFromImage(omero.server, my.i)
    temp.df <- rbind(temp.df, data.frame(key = "image.name", value = my.i@dataobject$getName()))
    temp.df$id <- getOMEROID(my.i)
    return(temp.df)
  }))
  # In OMERO it is possible to set multiple values for a single key. Here we
  # take only the first value for each key.
  agg.fun <- function(v) {
    if (length(v) == 0) {
      return(NA)
    } else {
      return(v[1])
    }
  }
  summary.df <- reshape::cast(one.per.line, id ~ key, fun.aggregate = agg.fun)
  return(summary.df)
}

mergeNicely <- function(mainDF, newDF, verbose) {
  if (!"id" %in% colnames(mainDF)) {
    if (verbose) {
      cat(file = stderr(), "No id in mainDF\n")
      return(mainDF)
    }
  }
  if (!"id" %in% colnames(newDF)) {
    if (verbose) {
      cat(file = stderr(), "No id in newDF\n")
      return(mainDF)
    }
  }
  extra.cols <- setdiff(colnames(newDF), c("id", "image.name"))
  existing.extra.cols <- intersect(extra.cols, colnames(mainDF))
  if (verbose) {
    cat(file = stderr(), "MERGE\n")
    cat(file = stderr(), extra.cols, "\n")
    cat(file = stderr(), existing.extra.cols, "\n")
  }
  if (length(existing.extra.cols) == 0) {
    mainDF <- merge(mainDF, newDF, all.x = T)
  } else {
    if (verbose) {
      cat(file = stderr(), "MERGE COMMON COLS\n")
    }
    if (length(extra.cols) > length(existing.extra.cols)) {
      mainDF <- merge(mainDF, newDF[, setdiff(colnames(newDF), existing.extra.cols)],
        all.x = T)
    }
    # Handle cases where some images have been removed
    my.ids <- intersect(newDF$id, mainDF$id)
    for (my.col in existing.extra.cols) {
      if (verbose) {
        cat(file = stderr(), my.col, "\n")
      }
      mainDF[match(my.ids, mainDF$id), my.col] <- newDF[, my.col]
    }
  }
  return(mainDF)
}
