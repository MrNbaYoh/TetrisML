import sys
from PIL import Image

cmdargs = sys.argv

f = Image.open(cmdargs[1])
data = list(f.getdata())

string = 'let '+cmdargs[3]+'_h = ' + str(f.size[1]) + ' and ' + cmdargs[3] +'_w = ' + str(f.size[0]) + ' and ' + cmdargs[3] + ' = make_image [|'

for i in range(f.size[1]):
    string += "[|"
    l = data[i*f.size[0]:(i+1)*f.size[0]]
    for p in l:
        string += str(p[0]<<16 | p[1]<<8 | p[2]) + ';'
    string += '|];'
string += '|];;'

o = open(cmdargs[2], 'w')
o.write(string)
