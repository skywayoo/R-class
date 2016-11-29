library(jmotif)
# load Cylinder-Bell-Funnel data
data("CBF")
w <- 60 # the sliding window size
p <- 6  # the PAA size
a <- 6  # the SAX alphabet size

# convert the train classes to wordb
dat = CBF[["data_train"]]


allbag = list()
for(i in 1:length(dat[,1])){
  allbag[[i]] = series_to_wordbag(dat[i,], w, p, a, "exact", 0.01)
}
names(allbag)=sprintf("fff%s",1:30)
tfidf = bags_to_tfidf(allbag)
sys
dm = list()
head(tfidf)
k_dat = t(tfidf[,-1])
k_dat = as.data.frame(k_dat)

head(k_dat)
k_res = as.numeric(kmeans(k_dat,3)$cluster)
table(k_res,CBF[["labels_train"]])

plot(dat[8,],t='l')
CBF[["train"]]
for(j in 1:length(dat[,1])){
  bag = series_to_wordbag(dat[j,],w,p,a,"exact", 0.01)
  dm[[j]] = cosine_sim(list("bag"=bag, "tfidf" = tfidf))$cosines
}
dm_new2 = do.call(rbind,dm)
dm_new2[1,]
dm_new[2,]
dm[[1]]
plot(dat[4,],t='l',lwd=3)
CBF[["labels_train"]]
kmeans(dm_new,3)$cluster

series_to_wordbag(dat[1,],w,p,a,"exact", 0) == 
  tfidf[[2]]
