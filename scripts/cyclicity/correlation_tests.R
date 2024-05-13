x = seq(1,365*10,1)
period=365
y=sin(2*pi/period*x)

plot(x,y)

z = y + 10

print(paste("Correlation:",cor(y,z)))
print(paste("DTW Distance:",dtw(y,z)$normalizedDistance))


r = sin(2*pi/period*x-0.4)

plot(x,r)
points(x,y,add=TRUE,col="red")
print(paste("Correlation:",cor(y,r)))
print(paste("DTW Distance:",dtw(y,r)$normalizedDistance))
