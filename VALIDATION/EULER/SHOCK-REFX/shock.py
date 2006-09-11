#! /usr/bin/env python
import math
gam = 1.4
ms  = 2.85
p1p0 = 2*gam/(gam+1.)*ms**2 - (gam-1)/(gam+1)
r1r0 = 1./(2./(gam+1.)/ms**2 + (gam-1)/(gam+1))
t1t0 = p1p0/r1r0
M1   = ms/math.sqrt(t1t0) - ms/r1r0/math.sqrt(t1t0)

print "Ms     :",ms
print "p1/p0  :",p1p0
print "T1/T0  :",t1t0
print "r1/r0  :",r1r0
print "M1     :",M1
print "Ti1/T0 :",t1t0*(1+(gam-1)/2*M1**2)
print "Pi1/p0 :",p1p0*(1+(gam-1)/2*M1**2)**(gam/(gam-1))
