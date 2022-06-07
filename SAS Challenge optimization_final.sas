proc optmodel;

var s1 >= 0;
var p1 >= 0;
var s2 >= 0;
var p2 >= 0;
var s3 >= 0;
var p3 >= 0;
var s4 >= 0;
var p4 >= 0;
var c1 >= 0 integer;
var c2 >= 0 integer;

impvar inventory1 = 62500 + 12000 - s1;
impvar inventory2 = (62500 + 12000 - s1) + 18000 - s2;
impvar inventory3 = (62500 + 12000 - s1) + 18000 - s2 + 20000 - s3;
impvar inventory4 = (62500 + 12000 - s1) + 18000 - s2 + 20000 - s3 + 22000 - s4;

min cost = (0.12*p1 + 0.18*s1 + 0.12*p2 + 0.18*s2 + 0.12*p3 + 0.1*s3 + 0.12*p4 + 0.1*s4)*c2 + (0.15*p1 + 0.18*s1 + 0.15*p2 + 0.18*s2 + 0.15*p3 + 0.1*s3 + 0.15*p4 + 0.1*s4)*c1;

con c1 + c2 = 1;

con s1 + p1 = 56151;
con s1 >= 56151*0.25;
con inventory1 >= 30000;
con p1 >= 35000*c2 + 25000*c1;

con s2 + p2 = 60610;
con s2 >= 60610*0.25;
con inventory2 >= 30000;
con p2 >= 35000*c2 + 25000*c1;

con s3 + p3 = 63729;
con s3 >= 63729*0.25;
con inventory3 >= 30000;
con p3 >= 35000*c2 + 25000*c1;

con s4 + p4 = 66464;
con s4 >= 66464*0.25;
con inventory4 >= 30000;
con p4 >= 35000*c2 + 25000*c1;

solve with LSO;

print s1 p1 s2 p2 s3 p3 s4 p4 c1 c2 inventory1 inventory2 inventory3 inventory4 cost;

