From [An efficient new algorithm for 2-D line clipping: Its development and analysis](https://dl.acm.org/doi/10.1145/37401.37432), Nicholl-Lee-Nicholl 1987

```c
void rotate90c(int* x, int* y) {
    int t = *x;
    *x = *y;
    *y = -t;
}

void rotate180c(int* x, int* y) {
    *x = -*x;
    *y = -*y;
}

void rotate270c(int* x, int* y) {
    int t = *x;
    *x = -*y;
    *y = t;
}

void reflectxminusy(int* x, int* y) {
    int t = *x;
    *x = -*y;
    *y = -t;
}

void reflectxaxis(int* x, int* y) {
    *x = -*x;
}

void clip(int xleft, int ytop, int xright, int ybottom, int *x1, int *y1, int *x2, int *y2) {
    bool *display;
    if (*x1 < xleft) {
        leftcolumn(xleft, ytop, xright, ybottom, x1, y1, x2, y2, display);
    } else if (*x1 > xright) {
        rotate180c(x1, y1);
        rotate180c(x2, y2);
        leftcolumn(-xright, -ybottom, -xleft, -ytop, x1, y1, x2, y2, display);
        rotate180c(x1, y1);
        rotate180c(x2, y2);
    } else {
        centrecolumn(xleft, ytop, xright, ybottom, x1, y1, x2, y2, display);
    }
    if (display) {
        line(x1, y1, x2, y2)
    }
}

void leftcolumn(int xleft, int ytop, int xright, int ybottom, int *x1, int *y1, int *x2, int *y2, bool display) {
    if (*x2 < xleft) {
        *display = false;
    } else if (*y1 > ytop) {
        topleftcorner(xleft, ytop, xright, ybottom, x1, y1, x2, y2, display);
    } else if (*y1 < ybottom) {
        reflectxaxis(x1, y1);
        reflectxaxis(x2, y2);
        topleftcorner(xleft, -ybottom, xright, -ytop, x1, y1, x2, y2, display);
        reflectxaxis(x1, y1);
        reflectxaxis(x2, y2);
    } else {
        leftedge(xleft, ytop, xright, ybottom, x1, y1, x2, y2, display);
    }
}

void topleftcorner(int xleft, int ytop, int xright, int ybottom, int *x1, int *y1, int *x2, int *y2, bool *display) {
    int relx2;
    int rely2;
    int topproduct;
    int leftproduct;
    if (y2 > ytop) {
        *display = false;
    } else {
        relx2 = x2 - x1;
        rely2 = y2 - y1;
        topproduct = (ytop - y1) * relx2;
        leftproduct = (xleft - x1) * rely2;
        if (topproduct > leftproduct) {
            leftbottomregion(xleft, ytop, xright, ybottom, x1, y1, x2, y2, display, relx2, rely2, leftproduct);
        } else {
            reflectxminusy(x1, y1);
            reflectxminusy(x2, y2);
            leftbottomregion(-ytop, -xleft, -ybottom, -xright, x1, y1, x2, y2, display, -rely2, -relx2, topproduct);
            reflectxminusy(x1, y1);
            reflectxminusy(x2, y2);
        }
    }
}

void leftbottomregion(int xleft, int ytop, int xright, int ybottom, int *x1, int *y1, int *x2, int* y2, bool *display, int relx2, int rely2, int leftproduct) {
    int bottomproduct;
    int rightproduct;
    if (*y2 >= ybottom) {
        if (*x2 > xright) {
            *y2 = *y1 + (xright - *x1) * rely2 / relx2;
            *x2 = xright;
        }
        *y1 = *y1 + leftproduct / relx2;
        *x1 = xleft;
        *display = true;
    } else {
        bottomproduct = (ybottom - y1) * relx2;
        if (bottomproduct > leftproduct) {
            *display = false;
        } else {
            if (*x2 > xright) {
                rightproduct = (xright - *x1) * rely2;
                if (bottomproduct > rightproduct) {
                    *x2 = *x1 + bottomproduct / rely2;
                    *y2 = ybottom;
                } else {
                    *y2 = *y1 + rightproduct / relx2;
                    *x2 = xright;
                }
            } else {
                *x2 = *x1 + bottomproduct / rely2;
                *y2 = ybottom;
            }
            *y1 = *y1 + leftproduct/relx2;
            *x1 = xleft;
            *display = true;
        }
    }
}

void leftedge(int xleft, int ytop, int xright, int ybottom, int *x1, int *y1, int *x2, int *y2, bool* display) {
    int relx2;
    int rely2;
    if (*x2 < xleft) {
        *display = false;
    } else if (*y2 < ybottom) {
        p2bottom(xleft, ytop, xright, ybottom, *x1, *y1, *x2, *y2, *display);
    } else if (y2 > ytop) {
        reflectaxis(*x1, *y1);
        reflectaxis(*x2, *y2);
        p2bottom(xleft, -ybottom, xright, -ytop, *x1, *y1, *x2, *y2, *display);
        reflectaxis(*x1, *y1);
        reflectaxis(*x2, *y2);
    } else {
        relx2 = *x2 - *x1;
        rely2 = *y2 - *y1;
        if (*x2 > xright) {
            *y2 = *y1 + rely2 * (xright-*x1)/relx2;
            *x2 = xright;
        }
        *y1 = *y1 + rely2 * (xleft-*x1)/relx2;
        *x1 = xleft;
        *display = true;
    } 
}

void p2bottom(int xleft, int ytop, int xright, int ybottom, int*x1, int*y1, int*x2, int*y2, bool*display) {
    int leftproduct;
    int bottomproduct;
    int rightproduct;
    int relx2;
    int rely2;
    relx2 = *x2 - *x1;
    rely2 = *y2 - *y1;
    leftproduct = (xleft - *x1) * relx2;
    bottomproduct = (ybottom - *y1) * rely2;
    if (bottomproduct > leftproduct) {
        *display = false;
    } else {
        if (*x2 <= xright) {
            x2 = x1 + bottomproduct/rely2;
            y2 = ybottom;
        } else {
            rightproduct = (xright - *x1) * rely2;
            if (bottomproduct > rightproduct) {
                *x2 = *x1 + bottomproduct/rely2;
                *y2 = ybottom;
            } else {
                *y2 = *y1 + rightproduct/relx2;
                *x2 = xright;
            }
        }
        *y1 = *y1 + leftproduct/relx2;
        *x1 = xleft;
        *display = true;
    }
}

void centrecolumn(int xleft, int ytop, int xright, int ybottom, int *x1, int* y1, int* x2, int* y2, bool* display) {
    if (*y1 > ytop) {
        rotate270c(*x1, *y1);
        rotate270c(*x2, *y2);
        leftedge(-ytop, xright, -ybottom, xleft, *x1, *y1, *x2, *y2, *display);
        rotate90c(*x1, *y1);
        rotate90c(*x2, *y2);
    } else if (*y1 < ybottom) {
        rotate90c(*x1, *y1);
        rotate90c(*x2, *y2);
        leftedge(ybottom, -xleft, ytop, -xright, *x1, *y1, *x2, *y2, *display);
        rotate270c(*x1, *y1);
        rotate270c(*x2, *y2);
    } else {
        inside(xleft, ytop, xright, ybottom, *x1, *y1, *x2, *y2, *display);
    }
}

void inside(xleft, ytop, xright, ybottom, *x1, *y1, *x2, *y2, *display) {
    *display = true;
    if (*x2 < xleft) {
        p2left(xleft, ytop, xright, ybottom, x1, y1, x2, y2);
    } else if (*x2 > xright) {
        rotate180c(x1, y1);
        rotate180c(x2, y2);
        p2left(-xright, -ybottom, -xleft, -ytop, x1, y1, x2, y2);
        rotate180c(x1, y1);
        rotate180c(x2, y2);
    } else if (*y2 > ytop) {
        *x2 = *x1 + (*x2 - *x1) * (ytop - *y1)/(*y2 - *y1);
        *y2 = ytop;
    } else if (*y2 < ybottom) {
        *x2 = *x1 + (*x2 - *x1) * (ybottom - *y1) / (y2 - y1);
        *y2 = ybottom;
    }
}

void p2left(xleft, ytop, xright, ybottom, *x1, *y1, *x2, *y2) {
    if (y2 > ytop) {
        p2lefttop(xleft, ytop, xright, ybottom, *x1, *y1, *x2, *y2);
    } else if (y2 < ybottom) {
        rotate90c(x1, y1);
        rotate90c(x2, y2);
        p2lefttop(ybottom, -xleft, ytop, -xright, x1, y1, x2, y2);
        rotate270c(x1, y1);
        rotate270c(x2, y2);
    } else {
        *y2 = *y1 + (*y2 - *y1) * (xleft - *x1)/(*x2 - *x1);
        *x2 = xleft;
    }
}

void p2lefttop(xleft, ytop, xright, ybottom, *x1, *y1, *x2, *y2) {
    int relx2;
    int rely2;
    int leftproduct;
    int rightproduct;
    relx2 = x2 - x1;
    rely2 = y2 - y1;
    leftproduct = rely2 * (xleft - x1);
    topproduct = relx2 * (ytop - y1);
    if (topproduct > leftproduct) {
        x2 = x1 + topproduct / rely2;
        y2 = ytop;
    } else {
        y2 = y1 + leftproduct / relx2;
        x2 = xleft;
    }
}
```