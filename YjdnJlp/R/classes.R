setClass("YjdnJlp", 
         representation(
                        appid="character",
                        handle="CURLHandle",
                        ma.service="character",
                        da.service="character",
                        keyphrase="character"),
         prototype=list(handle=getCurlHandle(),
                        ma.service="http://jlp.yahooapis.jp/MAService/V1/parse",
                        da.service="http://jlp.yahooapis.jp/DAService/V1/parse",
                        keyphrase="http://jlp.yahooapis.jp/KeyphraseService/V1/extract"))


setClass("YjdnJlpMA",
         representation(
                        sentence="character",
                        total.count="numeric",
                        filtered.count="numeric",
                        word.list="list"))

setClass("YjdnJlpDA",
         representation(
                        sentence="character",
                        chunk.list="list"))

setClass("YjdnJlpKeyphrase",
         representation(
                        sentence="character",
                        keyphrase.list="list"))

setClass("Word",
         representation(
             surface="character",
             reading="character",
             pos="character",
             baseform="character",
             count="numeric"))


setClass("Morphem",
         representation(
             surface="character",
             reading="character",
             baseform="character",
             pos="character",
             feature="list"
         )
)

setClass("Chunk",
         representation(
             id="numeric",
             dependency="numeric",
             morphem.list="list"
         )
)

setClass("Keyphrase",
         representation(
             keyphrase="character",
             score="numeric"))

# method
setMethod("post", signature="YjdnJlp", function(object, url, params=list()){
    params[["appid"]] <- object@appid
    content <- paste(mapply(function(x,y){ 
                                y <- ifelse(.Platform$OS.type == "windows", iconv(y, from="SHIFT_JIS", to="UTF-8"), y)
                                paste(x, URLencode(y),sep="=")}, 
                     names(params), params), sep="", collapse="&")

    xml <- getURL(url,
                  customrequest="POST",
                  postfields=content,
                  postfieldsize=length(strsplit(content, "")[[1]]),
                  curl=object@handle,
                  .encoding="UTF-8")

    result <- xmlToList(xmlRoot(xmlParse(xml, T)))

    if( .Platform$OS.type == "windows" ){
      convUTF8 <- function(x){
        if( is.list(x) ){
          return( lapply(x, convUTF8) )
        }

        return( iconv(x, "UTF-8", "SHIFT_JIS") )
      }

      result <- convUTF8(result)
    }

    result
})


setMethod("MAService", signature="YjdnJlp", function(object, sentence, ...){
    result <- post(object, object@ma.service, list(sentence=sentence,results="uniq",response="surface,reading,pos,baseform"))
    new("YjdnJlpMA", 
        sentence=sentence,
        total.count=as.numeric(result$uniq_result$total_count),
        filtered.count=as.numeric(result$uniq_result$filtered_count),
        word.list=buildWordList(result$uniq_result$word_list))
})

setMethod("DAService", signature="YjdnJlp", function(object, sentence){
    result <- post(object, object@da.service, list(sentence=sentence))
    new("YjdnJlpDA", sentence=sentence, chunk.list=buildChunkList(result$Result$ChunkList))
})

setMethod("Keyphrase", signature="YjdnJlp", function(object, sentence){
    result <- post(object, object@keyphrase, list(sentence=sentence))
    new("YjdnJlpKeyphrase", sentence=sentence, keyphrase.list=buildKeyphraseList(result[-length(result)]))
})


setMethod("toDataFrame", signature="YjdnJlpMA", function(object){
    word.list <- object@word.list
    df <- data.frame(NULL)

    for(word in word.list){
        df <- rbind(df,
                    data.frame(surface=word@surface,
                               reading=word@reading,
                               pos=word@pos,
                               baseform=word@baseform,
                               count=word@count))
    }

    df
})

setMethod("toDataFrame", signature="YjdnJlpDA", function(object){
    chunk.list <- object@chunk.list
    df <- data.frame(NULL)

    for(chunk in chunk.list){
        chunk.id <- chunk@id
        dependency <- chunk@dependency
        for( m in chunk@morphem.list ){
            df <- rbind(df, 
                        data.frame(chunk.id=chunk.id,
                                   dependency=dependency,
                                   surface=m@surface,
                                   reading=m@reading,
                                   baseform=m@baseform,
                                   pos=m@pos,
                                   feature=paste(unlist(m@feature), sep="", collapse=",")
                        ))
        }
    }

    df
})

setMethod("toDataFrame", signature="YjdnJlpKeyphrase", function(object){
    keyphrase.list <- object@keyphrase.list
    df <- data.frame(NULL)

    for(keyphrase in keyphrase.list){
        df <- rbind(df,
                    data.frame(keyphrase=keyphrase@keyphrase,
                               score=keyphrase@score))
    }

    df
})

# private function
buildWord <- function(surface, reading, pos, baseform, count)
{
    new("Word",
        surface=as.character(surface),
        reading=ifelse(is.null(reading), "", reading),
        pos=as.character(pos),
        baseform=as.character(baseform),
        count=as.numeric(count))
}

buildWordList <- function(raw.word.list)
{
    word.list <- lapply(raw.word.list, function(x){ buildWord(x$surface,x$reading,x$pos,x$baseform, x$count) } )
    names(word.list) <- NULL
    word.list
}

buildMorphem <- function(surface,reading, baseform, pos, feature)
{
    new("Morphem", 
        surface=surface,
        reading=reading,
        baseform=baseform,
        pos=pos,
        feature=as.list(strsplit(feature, ",")[[1]])
       )
}

buildChunk <- function(id, dependency, morphem.list)
{
    new("Chunk",
        id=id,
        dependency=dependency,
        morphem.list=morphem.list
       )
}

buildChunkList <- function(raw.chunk.list)
{
    chunk.list <- list()

    for( node in raw.chunk.list ){
        morphem.list <- lapply(node$MorphemList, function(x){ buildMorphem(x$Surface,x$Reading,x$Baseform,x$POS,x$Feature)})
        id <- as.numeric(node$Id) + 1
        dependency <- as.numeric(node$Dependency) + 1
        dependency <- ifelse(dependency==0, -1, dependency)
        chunk.list[[id]] <- buildChunk(id, dependency, morphem.list)
    }

    chunk.list
}

buildKeyphrase <- function(keyphrase, score)
{
    new("Keyphrase",
        keyphrase=as.character(keyphrase),
        score=as.numeric(score))
}

buildKeyphraseList <- function(raw.keyphrase.list)
{
    keyphrase.list <- lapply(raw.keyphrase.list, function(x){ buildKeyphrase(x$Keyphrase, x$Score) } )
    names(keyphrase.list) <- NULL
    keyphrase.list
}

