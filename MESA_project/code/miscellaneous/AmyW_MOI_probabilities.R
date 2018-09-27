#............................
# Amy W. MOI Issue 
# Fall 2018
#............................

prob.by.chance<-0.5

moi.a<-2
obs.relate.a = 1
moi.b<-20
obs.relate.b = 10
n.runs = 100

values.a<-sapply(1:n.runs, function(x) sum(rbinom(moi.a, 1, prob = prob.by.chance)))
values.b<-sapply(1:n.runs, function(x) sum(rbinom(moi.b, 1, prob = prob.by.chance)))

boxplot(values.a-obs.relate.a, values.b-obs.relate.b, names = c('a', 'b'))
