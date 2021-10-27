subsetObjectByOwner <- function(listObj, owner.id){
  owner.ids <- sapply(listObj, function(obj){
    obj@dataobject$getOwner()$getId()
  })
  return(lapply(which(owner.ids == owner.id), function(i){listObj[[i]]}))
}

parseImportOutput <- function(importOutput){
  lines.with.images.ids <- grep("^Image:", importOutput)
  images.ids <- strsplit(gsub("Image:", "", importOutput[lines.with.images.ids]), ",")
  files <- sapply(strsplit(importOutput[lines.with.images.ids - 1], "Imported file: "), "[[", 2)
  files.rep <- rep(files, sapply(images.ids, length))
  return(data.frame(id = unlist(images.ids), 'original_file' = files.rep))
}

getKeyValueDFFromImage <- function(omero.server, my.i){
  annots <- getAnnotations(my.i)
  if(nrow(annots) > 0 && "MapAnnotationData" %in% annots$Type){
    keyvalues <- apply(subset(annots, Type == "MapAnnotationData"), 1, function(v){
      loadObject(omero.server, v["Type"], v["ID"])@dataobject$getContentAsString()
    })
  } else {
    keyvalues <- NULL
  }
  if (length(keyvalues) > 0){
    keyvalues <- unlist(strsplit(keyvalues, ";"))
    keyvalues.split <- strsplit(keyvalues, "=")
    values <- sapply(keyvalues.split, tail, 1)
    keys <- apply(cbind(keyvalues, values), 1, function(v){gsub(paste0("=", v[2], "$"), "", v[1])})
  } else {
    keys <- character(0)
    values <- character(0)
  }
  return(data.frame(key = keys, value = values))
}

initiateDF <- function(omero.server, dataset){
  all.images <- getImages(dataset)
  # ids <- sapply(all.images, getOMEROID)
  # names <- sapply(all.images, function(my.i){my.i@dataobject$getName()})
  one.per.line <- do.call(rbind, lapply(all.images, function(my.i){
    # print(my.i@dataobject$getName())
    temp.df <- getKeyValueDFFromImage(omero.server, my.i)
    temp.df <- rbind(temp.df, data.frame(key = "image.name", value = my.i@dataobject$getName()))
    temp.df$id <- getOMEROID(my.i)
    return(temp.df)
  }))
  # In OMERO it is possible to set multiple values for a single key.
  # Here we take only the first value for each key.
  # print(one.per.line)
  agg.fun <- function(v){
    if (length(v) == 0){
      return(NA)
    } else {
      return(v[1])
    }
  }
  summary.df <- reshape::cast(one.per.line, id ~ key, fun.aggregate = agg.fun)
  return(summary.df)
}

# This is super slow while it is super quick in python
# getAllKeyValuesFromScratch <- function(omero.server, omero.projects){
#   all.datasets <- unlist(lapply(omero.projects, getDatasets))
#   all.keyvalues <- NULL
#   for (my.dataset in all.datasets){
#     temp.df <- unique(do.call(rbind, lapply(getImages(my.dataset),
#                                             function(my.i){
#                                               getKeyValueDFFromImage(omero.server, my.i)
#                                             }
#                                             )
#     ))
#     all.keyvalues <- unique(rbind(all.keyvalues, temp.df))
#   }
#   return(all.keyvalues)
# }

