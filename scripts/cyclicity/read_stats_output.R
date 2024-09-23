basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"

stats_files = list.files(paste0(basepath,"/results/results_slurm/2024-08-14/stats/"),full.names=T)
stats_files
p.values = data.frame(taxa=character(),p.val=numeric(),c.min=numeric(),c.max=numeric())
for(jj in 1:length(protist_tricho_labelC)){
  print(jj)
  load(stats_files[jj])
  
  #extract taxa name
 
  num_replicates= 50
  num.greater= length(which(shift.boot.season$t > orig.ts.stat))
  ###p-val is fraction of bootstrapped numbers that exceed the original
  p.val = num.greater/length(shift.boot.season$t)
  s.error = sqrt((p.val*(1-p.val))/num_replicates)
  c.interval = c(p.val-2*s.error,p.val+2*s.error)
  c.interval
  p.values[jj,] <- c("a",p.val,c.interval)
}

jj=1
load(stats_files[jj])
