setwd("~/Documents/kaggle home depot/input")
train= read.csv("train.csv",stringsAsFactors=FALSE)
test= read.csv("test.csv",stringsAsFactors=FALSE)
dscrp=read.csv("product_descriptions.csv",stringsAsFactors = FALSE)
attr=read.csv("attributes.csv",stringsAsFactors = FALSE)

all=read.csv("all.csv",stringsAsFactors = FALSE)




txt_st=all$search_term
txt_all_ns=all[,c(3,7,8)]
txt_pt=all$product_title
txt_trn=train[,3:4]
txt_tst=test[,3:4]

txt_all=all[,c(3,6:8)] # has brand name 
txt_all_nb=all[,c(3,6:7)] # no brand name
txt_dsp=all[,7]
txt_pt_st=all[,c(3,6)]
txt_pd_st=all[,c(7,6)]
txt_bd=all[,8]
setwd("~/Documents/kaggle home depot/doc2vec")
write.csv(txt_trn,file = "txt_trn.csv",row.names=FALSE,col.names = F,sep=",")
write.csv(txt_tst,file = "txt_tst.csv",row.names=FALSE,col.names = F,sep=",")
write.csv(txt_dsp,file = "txt_ds.csv",row.names=FALSE,col.names = F,sep=",")
write.csv(txt_all_nb,file = "txt_all_nb.csv",row.names=FALSE,col.names = F,sep=",")
write.table(txt_all, file="txt_all.txt", row.names=FALSE, col.names=FALSE, sep=",")
write.table(txt_all_nb, file="txt_all_nb.txt", row.names=FALSE, col.names=FALSE, sep=",")
write.table(txt_st, file="txt_st.txt", row.names=FALSE, col.names=FALSE, sep=",")
write.table(txt_all_ns, file="txt_all_ns.txt", row.names=FALSE, col.names=FALSE, sep=",")
write.table(txt_pt, file="txt_pt.txt", row.names=FALSE, col.names=FALSE, sep=",")
write.table(txt_dsp, file="txt_ds.txt", row.names=FALSE, col.names=FALSE, sep=",")
write.table(txt_pt_st, file="txt_pt_st.txt", row.names=FALSE, col.names=FALSE, sep=",")
write.table(txt_pd_st, file="txt_pd_st.txt", row.names=FALSE, col.names=FALSE, sep=",")
write.table(txt_bd, file="txt_bd.txt", row.names=FALSE, col.names=FALSE, sep=",")
### ------------------------Cleaned data ---------------------------
trn_clean=read.csv("train_clean.csv",stringsAsFactors = FALSE)
tst_clean=read.csv("test_clean.csv",stringsAsFactors = FALSE)
all=rbind(trn_clean[,c(4:5,7)],tst_clean[,c(4:6)])
txt_st=all$search_term
txt_all_ns=all[,c(1,3)]
write.table(txt_st, file="txt_stc.txt", row.names=FALSE, col.names=FALSE, sep=",")
write.table(txt_all_ns, file="txt_all_nsc.txt", row.names=FALSE, col.names=FALSE, sep=",")

###-----------------------group by product id -----------------------
setwd("~/Desktop/Model1_clean/input")
trainc= read.csv("../data/train_c1.csv",stringsAsFactors=FALSE)[,c(2:4)]
testc= read.csv("../data/test_c1.csv",stringsAsFactors=FALSE)[,c(2:4)]
all=rbind(trainc,testc)
data <- subset(all, !duplicated(all[,2]))
txt_spt=data[,3]
write.table(txt_spt, file="txt_spt.txt", row.names=FALSE, col.names=FALSE, sep=",")
pd_id=data[,2]
write.table(pd_id, file="pd_id.csv", row.names=FALSE, col.names=FALSE, sep=",")
# sort by prduct id 
# then delete the duplicated product id and related product title
# build model
# build dimensional data
# clustering and return the index
# link to product id 
# find search term and relevance correspond to the product id 
# calculate the simality between search term
