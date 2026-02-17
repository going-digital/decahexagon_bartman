# Clipping test

class Point:
    x:int
    y:int
    def __init__(self, x, y):
        self.x = x
        self.y = y

XMAX = 319
YMAX = 199

def drawline(a:Point, b:Point):
    pass

def fillfix(y0:int, y1:int):
    if (y1 < y0):
        y1, y0 = y0, y1
    if (y1 < 0) or (y0 > YMAX):
        return
    if (y0 < 0):
        y0 = 0
    if (y1 > YMAX):
        y1 = YMAX
    drawline(Point(XMAX, y0), Point(XMAX, y1))
    
def clipline(a:Point, b:Point, mxy:float, myx:float):
    outside_viewport = 4
    viewport_intersections = False
    if (a.y > b.y):
        b,a = a,b
        # Note that this doesn't change mxy or myx

    # Clip at y=0
    if (b.y < 0):
        # Entire line is off screen
        return
    elif (a.y < 0):
        mxy = (b.x - a.x) / (b.y - a.y) # Precalculated
        new_x = a.x - a.y * mxy
        if (0 <= new_x <= XMAX):
            a = Point(new_x, 0)
            viewport_intersections = True
    else:
        outside_viewport -= 1

    # Clip at y=YMAX
    if (a.y > YMAX):
        return
    elif (b.y > YMAX):
        mxy = (b.x - a.x) / (b.y - a.y) # Precalculated
        new_x = b.x + (YMAX - b.y) * mxy
        if (0 <= new_x <= XMAX):
            b = Point(new_x, YMAX)
            viewport_intersections = True
    else:
        outside_viewport -= 1

    if (a.x > b.x):
        b,a = a,b
        # Note that this doesn't change mxy or myx

    # Clip at x=0
    if (b.x < 0):
        return []
    elif (a.x < 0):
        myx = (b.y - a.y) / (b.x - a.x) # Precalculated
        new_y = a.y - a.x * myx
        if (0 <= new_y <= YMAX):
            a = Point(0, new_y)
            viewport_intersections = True
    else:
        outside_viewport -= 1

    # Clip at x=XMAX
    # Needs to be done last due to fill fixup
    if (a.x > XMAX):
        # Entire line is outside viewport
        fillfix(a.y, b.y)
        return
    elif (b.x > XMAX):
        myx = (b.y - a.y) / (b.x - a.x) # Precalculated
        new_y = b.y + (XMAX - b.x) * myx
        if (0 <= new_y <= YMAX):
            fillfix(new_y, b.y)
            b = Point(XMAX, new_y)
            viewport_intersections = True
    else:
        outside_viewport -= 1

    if (a.y > b.y):
        b,a = a,b
        # Draw in consistent y direction as this fixes polygon fill.

    if outside_viewport == 0 or viewport_intersections > 0:
        drawline(a, b)
    else:
        # Line is outside viewport
        return
