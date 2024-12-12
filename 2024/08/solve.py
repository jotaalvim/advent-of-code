from itertools import combinations
def printAnti(anti):
    with open("inputExample.txt") as f:
        text = f.read().strip().splitlines()
    xsize, ysize = len( text[0] ), len( text )
    antenas = {}
    for y, l in enumerate(text):
        for x, a in enumerate(l):
            if (x, y) in anti:     print("#", end="")
            else:                  print(a, end="") 
        print()
     
def parseInput( ):
    global xsize, ysize

    with open("inputExample.txt") as f:
        text = f.read().strip().splitlines()
    xsize, ysize = len( text[0] ), len( text )
    antenas = {}
    for y,l in enumerate(text):
        for x,a in enumerate(l):
            if a.isalnum() :
                if a in antenas:
                    antenas[a].append((x,y))
                else:
                    antenas[a] = [(x,y)]
    return antenas

def makeAntinode(antena1, antena2):
    global xsize, ysize
    x1  , y1   = antena1
    x2  , y2   = antena2
    xdif, ydif = x1 - x2, y1 - y2
    p1x , p1y  = x1 - xdif,y1 - ydif
    p2x , p2y  = x2 + xdif,y2 + ydif

    if (p2x < 0 or p2x > xsize - 1 or p2y < 0 or p2y > ysize - 1 and
        p1x < 0 or p1x > xsize - 1 or p1y < 0 or p1y > ysize - 1):
        return []
    if  p2x < 0 or p2x > xsize - 1 or p2y < 0 or p2y > ysize - 1: 
        return [(p1x, p1y)]
    if  p1x < 0 or p1x > xsize - 1 or p1y < 0 or p1y > ysize - 1: 
        return [(p2x, p2y)]
    else:
        return [(p1x, p1y), (p2x, p2y)]

def part1(inp):
    print(inp)
    # antinodes = set()
    antinodes = []
    for k in inp.keys():
        for p1,p2 in list(combinations(inp[k], 2)):
            antinodes +=  makeAntinode(p1,p2)
            #for a in makeAntinode(p1,p2):
            #    antinodes.add( a )
    printAnti (antinodes)
    print(antinodes)
    return len(antinodes)

def part2(inp):
    yield

inp     = parseInput()

print( "part 1:", part1(inp))
print( "part 2:", part2(inp))
