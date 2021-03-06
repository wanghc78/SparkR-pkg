# Utility functions to serialize R objects so they can be read in Java.

# Type mapping from R to Java
#  
# integer -> Int
# character -> String
# logical -> Boolean
# double, numeric -> Double
# raw -> Array[Byte]
#
# list[T] -> Array[T], where T is one of above mentioned types
# environment -> Map[String, T], where T is a native type
# jobj -> Object, where jobj is an object created in the backend

writeObject <- function(con, object, writeType = TRUE) {
  # NOTE: In R vectors have same type as objects. So we don't support
  # passing in vectors as arrays and instead require arrays to be passed
  # as lists.
  if (writeType) {
    writeType(con, class(object))
  }
  switch(class(object),
         integer = writeInt(con, object),
         character = writeString(con, object),
         logical = writeBoolean(con, object),
         double = writeDouble(con, object),
         numeric = writeDouble(con, object),
         raw = writeRaw(con, object),
         list = writeList(con, object),
         jobj = writeJobj(con, object),
         environment = writeEnv(con, object),
         stop("Unsupported type for serialization"))
}

writeJobj <- function(con, value) {
  if (!isValidJobj(value)) {
    stop("invalid jobj ", value$id)
  }
  writeString(con, value$id)
}

writeString <- function(con, value) {
  writeInt(con, as.integer(nchar(value) + 1))
  writeBin(value, con, endian = "big")
}

writeInt <- function(con, value) {
  writeBin(as.integer(value), con, endian = "big")
}

writeDouble <- function(con, value) {
  writeBin(value, con, endian = "big")
}

writeBoolean <- function(con, value) {
  # TRUE becomes 1, FALSE becomes 0
  writeInt(con, as.integer(value))
}

writeRawSerialize <- function(outputCon, batch) {
  outputSer <- serialize(batch, ascii = FALSE, connection = NULL)
  writeRaw(outputCon, outputSer)
}

writeRaw <- function(con, batch) {
  writeInt(con, length(batch))
  writeBin(batch, con, endian = "big")
}

writeType <- function(con, class) {
  type <- switch(class,
                 integer = "i",
                 character = "c",
                 logical = "b",
                 double = "d",
                 numeric = "d",
                 raw = "r",
                 list = "l",
                 jobj = "j",
                 environment = "e",
                 stop("Unsupported type for serialization"))
  writeBin(charToRaw(type), con)
}

# Used to pass arrays where all the elements are of the same type
writeList <- function(con, arr) {
  # All elements should be of same type
  elemType <- unique(sapply(arr, function(elem) { class(elem) }))
  stopifnot(length(elemType) <= 1)

  # TODO: Empty lists are given type "character" right now.
  # This may not work if the Java side expects array of any other type.
  if (length(elemType) == 0) {
    elemType <- class("somestring")
  }

  writeType(con, elemType)
  writeInt(con, length(arr))

  if (length(arr) > 0) {
    for (a in arr) {
      writeObject(con, a, FALSE)
    }
  }
}

# Used to pass in hash maps required on Java side.
writeEnv <- function(con, env) {
  len <- length(env)

  writeInt(con, len)
  if (len > 0) {
    writeList(con, as.list(ls(env)))
    vals <- lapply(ls(env), function(x) { env[[x]] })
    writeList(con, as.list(vals))
  }
}

# Used to serialize in a list of objects where each
# object can be of a different type. Serialization format is
# <object type> <object> for each object
writeArgs <- function(con, args) {
  if (length(args) > 0) {
    for (a in args) {
      writeObject(con, a)
    }
  }
}

writeStrings <- function(con, stringList) {
  writeLines(unlist(stringList), con)
}
