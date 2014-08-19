distances<-function()
{
  function(distance)
  {
    function(data,compare)
    {
      #result<-apply(data,1,function (x) dist(x,compare))
      if(is.null(nrow(data))|len(data)==1)
      {
        results<-distance(data[[1]],compare[[1]])
      }
      else
      {
        results<-apply(data,1,function (x) distance(x[[1]],compare[[1]]))
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
        result<-rollApply(data,window=2,minimum=2,align='right', fun=function (data) distance(head(data,1),tail(data,1)))  
      return(buffer(result,size=len(data),fill=0))
    }
  }
}

combine<-function()
{
  
  function(distances)
  {
    function(data,insert)
    {
      
      dataPlus<-cbind(data,distances(data,insert))
      colnames(dataPlus)[length(colnames(dataPlus))]<-"newDist"
      pairs<-c(dataPlus$newDist,0)+c(0,dataPlus$newDist)
      dataPlus<-rbind(c(rep(0,times=(dim(dataPlus)[2]))),dataPlus)
      dataPlus<-cbind(dataPlus,pairs)
      colnames(dataPlus)[length(colnames(dataPlus))]<-"totalDist"
      dataPlus<-cbind(dataPlus,as.numeric(dataPlus$dist)-as.numeric(dataPlus$totalDist))
      colnames(dataPlus)[length(colnames(dataPlus))]<-"change"
      index<-which.max(dataPlus$change)
      dataPlus<-rbind(dataPlus,c(rep(0,times=(dim(dataPlus)[2]))))
      insertPlus<-cbind(insert[,-(which(colnames(insert)=='dist'))],dataPlus[(index+1),"newDist"])
      colnames(insertPlus)[length(colnames(insertPlus))]<-"dist"
      colnames(insertPlus)<-colnames(dataPlus[,(1:(dim(insertPlus))[2])])
      dataPlus[index,"dist"]<-dataPlus[index,"newDist"]
      result<-rbind(dataPlus[1:index,colnames(insertPlus)],insertPlus,dataPlus[(index+1):(dim(dataPlus)[1]),colnames(insertPlus)])
      return(result[2:(dim(result)[1]-1),])
    }
  }
}

insertionSort<-function()
{
  function(distance)
  {
    function(data)
    {
      if(class(data)="list")
        data<-matrix(data)
      data<-data.frame(data)
      colnames(data)[1]<-"label"
      dataList<-split(data,1:(dim(data)[1]))
      result<-Reduce_pb(combine()(distances()(distance)),dataList)
      return(result)
    }
  }
}