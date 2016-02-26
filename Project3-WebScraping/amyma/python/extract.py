from collections import namedtuple
from math import sqrt
import random
try:
    import Image
except ImportError:
    from PIL import Image
from colormath.color_diff import delta_e_cie1976
import pandas as pd
import csv


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