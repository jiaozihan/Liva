from os import listdir
from os.path import isfile, join
onlyfiles = [f for f in listdir('test/') if isfile(join('test', f))]

fp = open("result.txt",'w') 
for  f in  onlyfiles:
    fp.write(f + ": \n")
    curFile = open("test/" + f, 'r')
    for lines in curFile.readlines():
        fp.write(lines)
    fp.write("\n")
    fp.write("______________")
    fp.write("\n")
    print("adding " + f + "...")