import re
import math

def parseInput(  ):
    with open("input.txt") as f:
        text = f.read().strip()
    pos = []
    for line in text.splitlines():
        print(line)
        pos.append( tuple(map(lambda a: int(a) ,re.findall(r'p=(-?\d+)\,(-?\d+)\s+v=(-?\d+)\,(-?\d+)',line)[0] )))
    return pos


def next ( state ):
    px,py,vx,vy = state
    nx = px
    ny = py
    for i in range(abs(vx)):
        nx += 1 if vx > 0 else -1
        if nx < 0 :
            nx = sizeX - 1
        if nx >= sizeX:
            nx = 0
    for i in range(abs(vy)):
            ny += 1 if vy > 0 else -1
            if ny < 0 :
                ny = sizeY - 1 
            if ny >= sizeY:
                ny = 0
    return (nx,ny,vx,vy)


def quadrant ( states ) :
    cols = math.floor(sizeX / 2)
    rows = math.floor(sizeY / 2)
    first = []
    second = []
    third = []
    fourth = []

    for px,py,_,_ in states:
        if px < cols and py < rows:
            first.append( px )
        if px < cols and py >= sizeY - rows:
            second.append( px )
        if px >= sizeX-cols and py <  rows:
            fourth.append(px)
        if px >= sizeX-cols and py >= sizeY - rows:
            third.append(px)
    return len( first) * len( second) * len( third ) * len( fourth )



def calculate_variance_2d(points):
    x_coords = [point[0] for point in points]
    y_coords = [point[1] for point in points]
    
    mean_x = sum(x_coords) / len(x_coords)
    mean_y = sum(y_coords) / len(y_coords)
    
    variance_x = sum((x - mean_x) ** 2 for x in x_coords) / len(x_coords)
    variance_y = sum((y - mean_y) ** 2 for y in y_coords) / len(y_coords)
    return variance_x, variance_y

def printGrid( m , i):
    newG = [(x,y) for x,y,_,_ in m ]
    print("ITERATION", i)
    for y in range(sizeY):
        line = ""
        for x in range(sizeX) :
            if (x,y) in newG:
                line += "#"
            else:
                line += " "
        line += "\n"
        print(line)

def part1( inp ):
    seconds = 100
    m = inp
    for i in range( seconds):
        m = list ( map ( lambda x :next(x), m))
    return quadrant ( m )

def part2( inp ):
    seconds = 10000
    m = inp
    small = (999999999999,99999999999)
    for i in range( seconds):
        m = list ( map ( lambda x :next(x), m))
        var = calculate_variance_2d(m)
        if var < small:
            small = var
            print("Iteration: ",i, var)

    return  small


positions = parseInput()
sizeX = 101
sizeY = 103

#print( "part 1:", part1(positions))
print( "part 2:", part2(positions))

