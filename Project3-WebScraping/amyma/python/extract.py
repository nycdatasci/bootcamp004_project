from collections import namedtuple
from math import sqrt
import random
try:
    import Image
except ImportError:
    from PIL import Image
from colormath.color_objects import LabColor, sRGBColor
from colormath.color_conversions import convert_color
from colormath.color_diff import delta_e_cie1976
import pandas as pd
import csv

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

Point = namedtuple('Point', ('coords', 'n', 'ct'))
Cluster = namedtuple('Cluster', ('points', 'center', 'n'))

def get_points(img):
    points = []
    w, h = img.size
    for count, color in img.getcolors(w * h):
        points.append(Point(color, 3, count))
    return points

rtoh = lambda rgb: '#%s' % ''.join(('%02x' % p for p in rgb))

def colorz(filename, n=5):
    img = Image.open(filename)
    img.thumbnail((200, 200))
    w, h = img.size
    
    points = get_points(img)
    clusters = kmeans(points, n, 1)
    rgbs = [map(int, c.center.coords) for c in clusters]
    col_dict={} # using dictionary to store the colors as keys
    # I will also store the lab difference into it
    # then output as a csv file
    col_dict['index']=['html_color','r,g,b','black','gray','sliver','white','maroon','red','olive','yellow','green','lime','teal','aqua','navy','blue','orange','purple','fuchsia','brown']
    counter=1
    for col in rgbs:
        col_index=filename+str(counter)
        html=rtoh(col)
        scale_rgbs= sRGBColor(col[0]/255.0,col[1]/255.0,col[2]/255.0)
        lab_n = convert_color(scale_rgbs, LabColor)
        
        #compared with other basic colors:
        de_black= delta_e_cie1976(lab_n, black)
        de_gray= delta_e_cie1976(lab_n, gray)
        de_sliver= delta_e_cie1976(lab_n, sliver)
        de_white= delta_e_cie1976(lab_n, white)
        de_maroon= delta_e_cie1976(lab_n, maroon)
        de_red= delta_e_cie1976(lab_n, red)


        de_olive= delta_e_cie1976(lab_n, olive)
        de_yellow= delta_e_cie1976(lab_n, yellow)
        de_green= delta_e_cie1976(lab_n, green)
        de_lime= delta_e_cie1976(lab_n, lime)
        de_teal= delta_e_cie1976(lab_n, teal)
        de_aqua= delta_e_cie1976(lab_n, aqua)
        de_navy= delta_e_cie1976(lab_n, navy)
        de_blue= delta_e_cie1976(lab_n, blue)
        
        de_orange= delta_e_cie1976(lab_n, orange)
        de_purple= delta_e_cie1976(lab_n, purple)
        de_fuchsia= delta_e_cie1976(lab_n, fuchsia)
        de_brown= delta_e_cie1976(lab_n, brown)

        # store the html color as the first col
        col_dict[col_index]=[html,tuple(col),de_black,de_gray,de_sliver,de_white,de_maroon,de_red,de_olive,de_yellow,de_green,de_lime,de_teal,de_aqua,de_navy,de_blue,de_orange,de_purple,de_fuchsia,de_brown]
        counter+=1

    # I can only store the difference 
    # col_dict[col]=tuple(lab_n)
    #col_dict[col]=lab_n
    # input another function to comapre between colors and basic colors
    #delta_e = delta_e_cie1976(color1, color2)
    return col_dict
#xyz
#colorsys.rgb_to_hsv(rgbs)
#map(rtoh, rgbs)# retrun rgbs directly
#rgbs # return the html color code
# I need  a big loop for all the images I got, with the key as the image name



def euclidean(p1, p2):
    return sqrt(sum([
                     (p1.coords[i] - p2.coords[i]) ** 2 for i in range(p1.n)
                     ]))

def calculate_center(points, n):
    vals = [0.0 for i in range(n)]
    plen = 0
    for p in points:
        plen += p.ct
        for i in range(n):
            vals[i] += (p.coords[i] * p.ct)
    return Point([(v / plen) for v in vals], n, 1)

def kmeans(points, k, min_diff):
    clusters = [Cluster([p], p, p.n) for p in random.sample(points, k)]
    
    while 1:
        plists = [[] for i in range(k)]
        
        for p in points:
            smallest_distance = float('Inf')
            for i in range(k):
                distance = euclidean(p, clusters[i].center)
                if distance < smallest_distance:
                    smallest_distance = distance
                    idx = i
            plists[idx].append(p)
        
        diff = 0
        for i in range(k):
            old = clusters[i]
            center = calculate_center(plists[i], old.n)
            new = Cluster(plists[i], center, old.n)
            clusters[i] = new
            diff = max(diff, euclidean(old.center, new.center))
        
        if diff < min_diff:
            break

    return clusters


## try get points
image_type = "comedy"
csv_name=image_type+".csv"

for i in range(28):
    img_name= image_type+"_"+str(i)+".jpg"
    if i==0:
        total=colorz(img_name)
    else:
        total.update(colorz(img_name))
writer = csv.writer(open(csv_name, 'wb'))
for key, value in total.items():
    writer.writerow([key, value])

## try get points
image_type = "action"
csv_name=image_type+".csv"

for i in range(28):
    img_name= image_type+"_"+str(i)+".jpg"
    if i==0:
        total=colorz(img_name)
    else:
        total.update(colorz(img_name))
writer = csv.writer(open(csv_name, 'wb'))
for key, value in total.items():
    writer.writerow([key, value])

## try get points
image_type = "animation"
csv_name=image_type+".csv"

for i in range(28):
    img_name= image_type+"_"+str(i)+".jpg"
    if i==0:
        total=colorz(img_name)
    else:
        total.update(colorz(img_name))
writer = csv.writer(open(csv_name, 'wb'))
for key, value in total.items():
    writer.writerow([key, value])

## try get points
image_type = "Horror"
csv_name=image_type+".csv"

for i in range(28):
    img_name= image_type+"_"+str(i)+".jpg"
    if i==0:
        total=colorz(img_name)
    else:
        total.update(colorz(img_name))
writer = csv.writer(open(csv_name, 'wb'))
for key, value in total.items():
    writer.writerow([key, value])



#total[str(i)]=[colorz(img_name)]
#total_df=pd.DataFrame(total)
#total_df
#csv_name=image_type+".csv"
#total_df.to_csv(csv_name,sep='\t',index=False)