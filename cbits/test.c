#include <stdio.h>
#include "spiroentrypoints.h"
#include "bezctx.h"

void moveto(bezctx *bc, double x, double y, int is_open)
{
    printf("%lf %lf moveto\n", x, y);
}

void lineto(bezctx *bc, double x, double y)
{
    printf("%lf %lf lineto\n", x, y);
}

void quadto(bezctx *bc, double x1, double y1, double x2, double y2)
{
    printf("%lf %lf %lf %lf quadto\n", x1, y1, x2, y2);
}

void curveto(bezctx *bc, double x1, double y1, double x2, double y2, double x3, double y3)
{
    printf("%lf %lf %lf %lf %lf %lf curveto\n", x1, y1, x2, y2, x3, y3);
}

void mark_knot(bezctx *bc, int knot_idx)
{
}

void main()
{
    spiro_cp points[4];

    /* This defines something very like a circle, centered at the origin with radius 100 */
    points[0].x = -100; points[0].y =    0; points[0].ty = SPIRO_G4;
    points[1].x =    0; points[1].y =  100; points[1].ty = SPIRO_G4;
    points[2].x =   50; points[2].y =  -10; points[2].ty = SPIRO_G4;
    points[3].x =    0; points[3].y = -100; points[3].ty = SPIRO_G4;
    
    bezctx bc;
    bc.moveto = moveto;
    bc.lineto = lineto;
    bc.curveto = curveto;
    bc.quadto = quadto;
    bc.mark_knot = mark_knot;
    
    SpiroCPsToBezier(points,4,-1,&bc);
}