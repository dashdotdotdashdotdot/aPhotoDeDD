rm(list=ls())

tmp = image_read("https://upload.wikimedia.org/wikipedia/commons/b/be/Ballet-Ballerina-1843.jpg")
tmp = image_read("https://upload.wikimedia.org/wikipedia/commons/8/89/Bob_Dylan_%E2%80%93_Knockin%27_On_Heaven%27s_Door.jpg")
#adjPicApp(tmp)

tmp2 = adjustPicture(tmp,
              pixels = 600,
              imageFlip = T,
              imageFlop = T,
              bsh = c(100,100,100),
              darkGreyIn = 0.12,
              lightGreyIn = 0.87,
#              crop = c(0.96,0.96,0.02,0.02),
              crop = c(1,1,0,0),
              return = 0,
              aspect = 1)

decayFactors = seq(-4,3,length=200)
for (i in 1:length(decayFactors)){
tmp3=makeTessalation(tmp2,rows=3,columns=3,center=c(2,2),option=1,decay=decayFactors[i])
tmp5= adjustPicture(tmp3,pixels=2000)
if (i ==1) {stack1 = tmp5} else {stack1 = c(stack1,tmp5)}
print(i)
}


stack1[2]
stack2 = stack1
for (i in 1:length(stack2)){
stack2[i] = adjustPicture(stack1[i],pixels=2000,enlarge=0.05,backCol1 = "grey10")
}

stack3=image_morph(stack2,frames=0)
image_write_gif(c(stack2[length(stack2):1],stack2),"knockingOnHeavensDoor.gif")
image_write_video(c(stack2[length(stack2):1],stack2),"knockingOnHeavensDoor.mp4")
