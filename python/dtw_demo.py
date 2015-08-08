# Unfortuntely, results were not promising. The map images may have to be massaged
# a bit to remove noise.

from PIL import Image
from numpy import array
import mlpy
from collections import OrderedDict
import fnmatch
import os

data = {}
matches = []
for root, dirnames, filenames in os.walk('data/maps'):
  for filename in fnmatch.filter(filenames, '*.jpg'):
    matches.append(os.path.join(root, filename))

index = 0
for fn in matches:
  gray = Image.open(fn).convert('L')
  gray.save('data/maps/greyscale-' + str(index) + '.jpg')
  img = Image.open(fn)
  arr = array(img)

  list = []
  for n in arr: list.append(n[0][0])
  for n in arr: list.append(n[0][1])
  for n in arr: list.append(n[0][2])
  data[index] = {'filename': fn, 'data': list}
  index += 1

reference = data[2]
print "The reference image is: " + reference['filename']

result = {}
for x, y in data.items():
  dist = mlpy.dtw_std(reference['data'], y['data'], dist_only=True)
  result[x] = dist

sortedRes = OrderedDict(sorted(result.items(), key = lambda x: x[1]))

for a, b in sortedRes.items():
  print("{0} <=> {1}".format(data[a]['filename'], b))
