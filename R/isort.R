#' Generalized sorting of R objects
#'
#' A generalized insertion sort that works on all data types.  Using this
#' as a replacement to sort or order would be an inefficient choice.
#' However, implementing complex distance algorithms allows for unusual
#' applications, such as arbitrary sorting, clustering, and solving the
#' travelling salesman problem.
#' @name isort
#' @docType package
#' @import rowr
NULL


#' Vectorize a scalar function to work on any R object.
#' 
#' Robust alternative to \code{\link{Vectorize}} function that accepts any function with two 
#' or more arguments.  Returns a function that will work an arbitrary number of vectors, lists or 
#' data frames, though output may be unpredicatable in unusual applications.  The 
#' results are also intended to be more intuitive than Vectorize.
#' 
#' @param fun a two or more argument function
#' @param type like \code{MARGIN} in \code{\link{apply}}, except that \code{c(1,2)} is
#'   represented as a \code{3} instead.  By default, will \code{Reduce} single dimensional
#'   data handle everything else row-wise.
#' @export
#' @examples
#' vectorize(`+`)(c(1,2,3))
#' vectorize(sum)(c(1,2,3),c(1,2,3))
#' # Compare these results to Vectorize, which does not vectorize sum at all.
#' Vectorize(sum)(c(1,2,3),c(1,2,3))
#' # Across data frame columns.
#' df<-data.frame(a=c(1,2,3),b=c(1,2,3))
#' vectorize(sum)(df$a,df$b)
#' # Once again, Vectorize gives a different result
#' Vectorize(sum)(df$a,df$b)
#' # Any combination of vectors, lists, matrices, or data frames an be used.
#' vectorize(`+`)(c(1,2,3),list(1,2,3),cbind(c(1,2,3)))
distances<-function()
{
  function(distance)
  {
    function(data,compare)
    {
      if(is.null(nrow(data))|len(data)==1)
      {
        results<-distance(data,compare)
      }
      else
      {
        results<-rowApply(data,function (x) distance(x,compare))
      }
      return(results)
    }
  }
}

neighborDistances<-function()
{
  function(distance)
  {
    function(data)
    {
      if(len(data)<2)
        result<-0
      else
        result<-rollApply(data,window=2,minimum=1,align='right', fun=function (data) distance(head(data,1),tail(data,1)))  
      return(buffer(result,length.out=len(data),fill=0))
    }
  }
}

insert<-function()
{
  function(distances)
  {
    function(data,insert)
    {
      ##Get old stances
      oldDistances<-c(data[['distances']],0)
      ##Get distances between new object and old list
      insertDistances<-c(0,distances(data,insert))
      ##Determine combined distance increase
      combinedDistances<-rollApply(insertDistances,window=2,minimum=1,align='left',sum)
      ##Determine combined change in distances
      deltaDistances<-combinedDistances-oldDistances
      ##Insert where delta is smallest
      insertIndex=which.min(deltaDistances)
      result<-insertRow(data,insert,insertIndex)
      ##Update distances
#       print('Next')
#       print(as.list(environment()))
      result[insertIndex,'distances']<-insertDistances[insertIndex]
      if(isTRUE(insertIndex<length(insertDistances)))
        result[insertIndex+1,'distances']<-insertDistances[insertIndex+1]
      return(result)      
    }
  }
}

insert<-function()
{
  function(distance)
  {
    function(data,insert)
    {
      ##Get old stances
      oldDistances<-c(data[['distances']],0)
      headInsertDistances<-c(0,distances()(distance)(data,head(insert,n=1)))
      tailInsertDistances<-c(distances()(distance)(data,tail(insert,n=1)),0)
      ##Determine combined distance increase
      combinedDistances<-headInsertDistances+tailInsertDistances
      ##Determine combined change in distances
      deltaDistances<-combinedDistances-oldDistances
      ##Insert where delta is smallest
      insertIndex=which.min(deltaDistances)
      result<-insertRows(data,insert,insertIndex)
      ##Update distances
      #       print('Next')
            
      result[insertIndex,'distances']<-headInsertDistances[insertIndex]
      if(isTRUE(insertIndex<len(result)))
        result[insertIndex+len(insert),'distances']<-tailInsertDistances[insertIndex]
#       print(as.list(environment()))
      return(result)      
    }
  }
}

#' Vectorize a scalar function to work on any R object.
#' 
#' Robust alternative to \code{\link{Vectorize}} function that accepts any function with two 
#' or more arguments.  Returns a function that will work an arbitrary number of vectors, lists or 
#' data frames, though output may be unpredicatable in unusual applications.  The 
#' results are also intended to be more intuitive than Vectorize.
#' 
#' @param fun a two or more argument function
#' @param type like \code{MARGIN} in \code{\link{apply}}, except that \code{c(1,2)} is
#'   represented as a \code{3} instead.  By default, will \code{Reduce} single dimensional
#'   data handle everything else row-wise.
#' @export
#' @examples
#'insertionSort()(function (x,y) abs(x$FTSE-y$FTSE))(EU[1:100,])
#'packages<-as.data.frame(available.packages())
#'packages$Depends<-substring(packages$Depends,0,60)
#'require(stringdist)
#'insertionSort()(function (x,y) stringdist(x$Suggests,y$Suggests,method='jaccard',q=1,useBytes=TRUE))(packages[1:400,c('Package','Suggests')])
insertionSort<-function()
{
  function(distance)
  {
    function(data)
    {
      ##Convert to standard data type
      data<-buffer(data,preserveClass=FALSE)
      ##Add distance column
      data$distances<-0
      dataList<-split(data,1:nrow(data))
      result<-Reduce(insert()(distance),dataList)
      return(result)
    }
  }
}

splitWhere<-function(condition)
{
  function(data)
  {
    matches<-(condition(data)*1)
    result<-split(data,cumsum(matches))
    return(result)
  }
}

insertRow <- function(existingDF, newrow, r) {
  existingDF <- rbind(existingDF,newrow)
  existingDF <- existingDF[order(c(1:(nrow(existingDF)-1),r-0.5)),]
  row.names(existingDF) <- 1:nrow(existingDF)
  return(existingDF)  
}

insertRows <- function(existingDF, newDF, r) {
  colnames(newDF)<-colnames(existingDF)
  result<-rbind(existingDF,newDF)
  sizeA<-len(existingDF)
  sizeB<-len(newDF)
  order1<-c(seq_len(sizeA),rep.int(r,sizeB))
  order2<-c(rep.int(sizeB+1,sizeA),seq_len(sizeB))
  return(result[order(order1,order2),,drop=FALSE])
}

binarySplit<-function(threshold)
{
  function(distance)
  {
    function(data,compare)
    {
      if(len(data<threshold))
        return(insertionSort()(distance)(data))
      splitDistances<-distance(data,compare)
      splits<-splitDistances>median(splitDistances)
      result<-split(data,splits)
      return(result)
    }
  }
}

splitRecurse<-function(threshold)
{
  function(distance)
  {
    function(data)
    {
      if(len(data)<threshold)
        return(insertionSort()(distance)(data))
      compare<-rows(data,sample(1:len(data),1))
      splitDistances<-distance(data,compare)
      splits<-splitDistances>median(splitDistances)
      dataList<-split(data,splits)
      result<-lapply(dataList,splitRecurse(threshold)(distance))
      #result<-mergeTree()(distance)(result[[1]],result[[2]])
      result<-rbind(result[[1]],result[[2]])
      return(result)
    }
  }
}

connectTree<-function()
{
  function(distance)
  {
    function(tree1,tree2)
    {
      head1<-head(tree1,n=1)
      head2<-head(tree2,n=1)
      tail1<-tail(tree1,n=1)
      tail2<-tail(tree2,n=1)
      distance1<-distance(head1,head2)
      distance2<-distance(head1,tail2)
      distance3<-distance(head2,tail1)
      #print(as.list(environment()))
      if(min(distance1,distance2,distance3)==distance1)
        result<-rbind(rev(tree1),tree2)
      if(min(distance1,distance2,distance3)==distance2)
        result<-rbind(tree2,tree1)
      if(min(distance1,distance2,distance3)==distance3)
        result<-rbind(tree1,tree2)
      return(result)
      
    }
  }
}

resort<-function(condition)
{
  function(distance)
  {
    function(data)
    {
      data$distances<-neighborDistances()(distance)(data)
      dataList<-splitWhere(condition)(data)
      result<-Reduce(insert()(distance),dataList)
      return(result)
    }
  }
}

binarySort<-function(threshold=100)
{
  function(distance)
  {
    function(data)
    {
      result<-splitRecurse(threshold)(distance)(data)
      return(result)
    }
  }
}
