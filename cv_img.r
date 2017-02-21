

cv_img = function("vfold" = vfold,"image list" = data,"image label" = label){
  require(cvTools)
  train_cvfold_data = list()
  train_cvfold_label = list()
  eval_cvfold_data = list()
  eval_cvfold_label = list()
  set.seed(7777)
  for(y in 1:length(levels(label))){
    cv__all_idx = list()
    for(y in 1:length(levels(label))){
      if(y==1){
        cv__all = cvFolds(length(which(label==levels(label)[y])),K=vfold)
      }
      else{
        cv__all = cvFolds(length(which(label==levels(label)[y])),K=vfold)
        new_vec = list()
        for(n in 1:vfold){
          new_vec[[n]] = which(label==levels(label)[y])[cv__all$subsets[which(cv__all$which==n)]]
        }
        cv__all$subsets = matrix(unlist(new_vec))
      }
      for(i in 1:vfold){
        if(y==1){
          cv__all_idx[[i]] = cv__all$subsets[cv__all$which==i]
        }
        else{
          val = cv__all_idx[[i]]
          cv__all_idx[[i]] = c(val,cv__all$subsets[cv__all$which==i]) 
        }
      }
    }
  }
  for(v in 1:vfold){
    img_h= dim(data[[1]])[2]
    img_w= dim(data[[1]])[1]
    img_channel=3
    img_eval_num = length(cv__all_idx[[v]])
    eval_dat = data[cv__all_idx[[v]]]
    eval_dat = array(unlist(eval_dat))
    dim(eval_dat) = c(img_h,img_w,img_channel,img_eval_num)
    eval_cvfold_data[[v]] = eval_dat
    train_dat = data[-cv__all_idx[[v]]]
    train_dat = array(unlist(train_dat))
    img_train_num =length(label[-cv__all_idx[[v]]])
    dim(train_dat) = c(img_h,img_w,img_channel,img_train_num)
    train_cvfold_data[[v]] = train_dat
    eval_cvfold_label[[v]] = label[cv__all_idx[[v]]]
    train_cvfold_label[[v]] = label[-cv__all_idx[[v]]]
  }
  return(list("training data"=train_cvfold_data,
              "train label"=train_cvfold_label,
              "eval data"=eval_cvfold_data,
              "eval label"=eval_cvfold_label,
              "cvfold index" = cv__all_idx)
         )
}

