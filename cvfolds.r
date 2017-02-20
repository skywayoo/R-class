vfold_cv = function(vfold,data,label){
  set.seed(7777)
  train_cvfold_data = list()
  train_cvfold_label = list()
  eval_cvfold_data = list()
  eval_cvfold_label = list()
  init_idx = list()
  for(v in 1:vfold){
    train_data = data.frame()
    eval_data = data.frame()
    train_label = list()
    eval_label = list()
    for(y in 1:length(levels(label))){
      if(v==1){
        eval_idx = sample(which(label==levels(label)[y]),length(which(label==levels(label)[y]))/vfold)
        train_idx = which(label==levels(label)[y])[which(!(which(label==levels(label)[y])%in%eval_idx))]
        train_data = rbind(train_data,data[train_idx,])
        eval_data = rbind(eval_data,data[eval_idx,])
        train_label[[y]] = label[train_idx]
        eval_label[[y]] = label[eval_idx]
        init_idx[[y]] = eval_idx
      }
      else{
        eval_idx = sample(which(label==levels(label)[y])[which(!(which(label==levels(label)[y]) %in% unlist(init_idx[[y]])))],length(which(label==levels(label)[y]))/vfold)
        train_idx =  which(label==levels(label)[y])[which(!(which(label==levels(label)[y])%in%eval_idx))]
        train_data = rbind(train_data,data[train_idx,])
        eval_data = rbind(eval_data,data[eval_idx,])
        train_label[[y]] = label[train_idx]
        eval_label[[y]] = label[eval_idx]
        init_idx[[y]] = c(init_idx[[y]],eval_idx)
        }
    }
    train_cvfold_data[[v]] = train_data
    eval_cvfold_data[[v]] = eval_data
    train_cvfold_label = unlist(train_label)
    eval_cvfold_label = unlist(eval_label)

  }
  return(list("training data"=train_cvfold_data,
              "train label"=train_cvfold_label,
              "eval data"=eval_cvfold_data,
              "eval label"=eval_cvfold_label))
}
