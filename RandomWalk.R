Xt = 0; Yt = 0
for (i in 2:1000)
{
  Xt[i] = Xt[i-1] + rnorm(1,0,1)
  Yt[i] = Yt[i-1] + rnorm(1,0,1)
}
df <- data.frame(x = Xt, y = Yt)
ggplot(df, aes(x=x, y=y)) + geom_path() + theme_classic() + coord_fixed(1)