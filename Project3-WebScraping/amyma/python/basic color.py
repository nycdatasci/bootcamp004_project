
from colormath.color_objects import LabColor, sRGBColor
from colormath.color_conversions import convert_color
def colab(r,g,b):
    rgb = sRGBColor(r/255.0, g/255.0, b/255.0)
    lab = convert_color(rgb, LabColor)
    print lab

# black
print("black")
colab(0,0,0)

# grey
print("gray")
colab(128,128,128)

# silver
print("white")
colab(192,192,192)

# white
print("white")
colab(255,255,255)

# maroon
print("maroon")
colab(128, 0, 0)

# red
print("red")
colab(255,0,0)

# olive
print("olive")
colab(128, 128, 0)


# yellow
print("yellow")
colab(255,255,0)

# green
print("green")
colab(0,128,0)

# lime
print("lime")
colab(0,255,0)

# teal
print("teal")
colab(0,128,128)

# aqua
print("aqua")
colab(0,255,255)

# navy
print("navy")
colab(0,0,128)

# blue
print("blue")
colab(0,0,255)

# orange
print("orange")
colab(255,165,0)

# purple
print("purple")
colab(128, 0, 128)

# fuchsia
print("fuchsia ")
colab(255, 0, 255)

# brown
print("brown")
colab(139,69,19)


black=LabColor (lab_l=0.0000,lab_a=0.0000,lab_b=0.0000)
gray=LabColor (lab_l=53.5850,lab_a=-0.0003,lab_b=-0.0051)
sliver=LabColor (lab_l=77.7044,lab_a=-0.0004,lab_b=-0.0069)
white=LabColor (lab_l=100.0000,lab_a=-0.0005,lab_b=-0.0086)
maroon=LabColor (lab_l=25.5344,lab_a=48.0439,lab_b=38.0559)
red=LabColor (lab_l=53.2390,lab_a=80.0905,lab_b=67.2014)
olive=LabColor (lab_l=51.8687,lab_a=-12.9319,lab_b=56.6742)
yellow=LabColor (lab_l=97.1388,lab_a=-21.5578,lab_b=94.4773)
green=LabColor (lab_l=46.2276,lab_a=-51.6986,lab_b=49.8970)
lime=LabColor (lab_l=87.7350,lab_a=-86.1829,lab_b=83.1795)
teal=LabColor (lab_l=48.2545,lab_a=-28.8437,lab_b=-8.4814)
aqua=LabColor (lab_l=91.1140,lab_a=-48.0832,lab_b=-14.1386)
navy=LabColor (lab_l=12.9734,lab_a=47.5046,lab_b=-64.7053)
blue=LabColor (lab_l=32.2994,lab_a=79.1914,lab_b=-107.8655)
orange=LabColor (lab_l=74.9347,lab_a=23.9292,lab_b=78.9486)
purple=LabColor (lab_l=29.7843,lab_a=58.9285,lab_b=-36.4932)
fuchsia=LabColor (lab_l=60.3236,lab_a=98.2353,lab_b=-60.8350)
brown=LabColor (lab_l=37.4691,lab_a=26.4404,lab_b=40.9820)


