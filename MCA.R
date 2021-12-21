library("FactoMineR")
library("factoextra")

categories = apply(brfss.df, 2, function(x) nlevels(as.factor(x)))



res.mca = MCA(brfss.df, graph = FALSE)


# data frame with variable coordinates
res.mca.vars = data.frame(res.mca$var$coord, Variable = rep(names(categories), categories))

# data frame with observation coordinates
res.mca.obs = data.frame(res.mca$ind$coord)

# plot of variable categories
ggplot(data=res.mca.vars, 
       aes(x = Dim.1, y = Dim.2, label = rownames(res.mca.vars))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) +
  ggtitle("MCA plot of variables")




# MCA plot of observations and categories
ggplot(data = res.mca.obs, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.7) +
  geom_density2d(colour = "gray80") +
  geom_text(data = res.mca.vars, 
            aes(x = Dim.1, y = Dim.2, 
                label = rownames(res.mca.vars), colour = Variable)) +
  ggtitle("MCA plot of variables") +
  scale_colour_discrete(name = "Variable")