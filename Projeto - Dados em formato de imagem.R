
 # install.packages("magick")
library(magick)

library(rsvg)

magick_config()

x <- image_read('https://static1.purepeople.com.br/articles/7/36/66/17/@/4202918-gretchen-um-dos-memes-da-cantora-comeco-700x700-2.jpg')

tiger <- image_read_svg('http://jeroen.github.io/images/tiger.svg', width = 350)

image_info(tiger)

patolino <- image_read(
  "https://s2.glbimg.com/niKmZWnyym0htQy4OvGJxM1Jpq4=/0x0:445x571/smart/filters:strip_icc()/i.s3.glbimg.com/v1/AUTH_ba3db981e6d14e54bb84be31c923b00c/internal_photos/bs/2022/G/g/sfiAXTTUCZD0yYxu7nDg/calca-de-shopping-1.jpg")

image_write(tiger, path = "tiger.png", format = ".png")

frink <- image_read("https://jeroen.github.io/images/frink.png")
frink

image_border(image_background(frink, "green"), "#FDC087", "120x10")

image_trim(frink)

image_scale(frink, "300")

image_rotate(frink, 45)

image_flip(frink)
image_flop(frink)

a <- image_flip(x)
image_write(a, 'x1.jpg')

b <- image_flop(x)
image_write(b, 'x2.jpg')

image_modulate(patolino, brightness = 100, saturation = 130, hue = 40)
c <- image_modulate(x, brightness = 100, saturation = 130, hue = 40)
image_write(c, 'x3.jpg')

image_fill(frink, "orange", point = "+100+200", fuzz = 50)

image_blur(frink, 10, 5)
d <- image_blur(x, 10, 5)
image_write(d, 'x4.jpg')

image_noise(frink)
e<- image_noise(x)
image_write(e, 'x5.jpg')
image_charcoal(frink)
f <- image_charcoal(x)
image_write(f, 'x6.jpg')
image_oilpaint(frink)

image_negate(frink)

image_annotate(frink, "CONFIDENTIAL", size = 30, color = "red", boxcolor = "pink",
               degrees = 60, location = "+50+100")

test <- image_rotate(frink, 90) %>%
image_background("blue", flatten = TRUE) %>%
image_border("red", "10x10") %>%
image_annotate("This is how we combine transformations", color = "white", size = 30)

earth <- image_read("https://jeroen.github.io/images/earth.gif") %>%
  image_scale("200x") %>%
  image_quantize(128)

length(earth)

rev(earth) %>% 
  image_flip() %>% 
  image_annotate("meanwhile in Australia", size = 20, color = "white")

img <- c(patolino, frink, tiger)
image_mosaic(img)
image_flatten(img)
image_flatten(img, 'Add')
image_flatten(img, 'Modulate')
image_flatten(img, 'Minus')
image_append(image_scale(img, "x200"))
image_append(image_scale(img, "100"), stack = TRUE)

patolinofrink <- image_scale(image_rotate(image_background(frink, "none"), 300), "x200")
image_composite(image_scale(patolino, "x400"), patolinofrink, offset = "+180+100")

image_animate(image_scale(img, "200x200"), fps = 1, dispose = "previous")

newlogo <- image_scale(image_read("https://jeroen.github.io/images/Rlogo.png"))
oldlogo <- image_scale(image_read("https://jeroen.github.io/images/Rlogo-old.png"))
image_resize(c(oldlogo, newlogo), '200x150!') %>%
  image_background('white') %>%
  image_morph() %>%
  image_animate(optimize = TRUE)

banana <- image_read("https://jeroen.github.io/images/banana.gif")
banana <- image_scale(banana, "150")
image_info(banana)

background <- image_background(image_scale(logo, "200"), "white", flatten = TRUE)

# Combine and flatten frames
frames <- image_composite(background, banana, offset = "+70+30")

# Turn frames into animation
animation <- image_animate(frames, fps = 10, optimize = TRUE)
print(animation)

image_write(animation, "Rlogo-banana.gif")

library(gapminder)
library(ggplot2)
img <- image_graph(600, 340, res = 96)
datalist <- split(gapminder, gapminder$year)
out <- lapply(datalist, function(data){
  p <- ggplot(data, aes(gdpPercap, lifeExp, size = pop, color = continent)) +
    scale_size("population", limits = range(gapminder$pop)) + geom_point() + ylim(20, 90) + 
    scale_x_log10(limits = range(gapminder$gdpPercap)) + ggtitle(data$year) + theme_classic()
  print(p)
})
dev.off()
animation <- image_animate(img, fps = 2, optimize = TRUE)
print(animation)

install.packages("tesseract")
library(tesseract)

img <- image_read("http://jeroen.github.io/images/testocr.png")
cat(image_ocr(img))


d <- image_modulate(x, brightness = 34, saturation = 100, hue = 40)
image_write(d, 'x11.jpg')
