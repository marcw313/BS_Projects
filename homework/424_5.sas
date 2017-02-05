*Problem 1 - I went through the reading;


*Problem 2;

proc iml;
reset log print;

y={6, 11, 18, 27, 38, 51, 66, 83, 102, 123};

*Problem 3;

x={ 1    1    1,
    1    2    4,
    1    3    9,
    1    4   16,
    1    5   25,
    1    6   36,
    1    7   49,
    1    8   64,
    1    9   81,
    1   10  100};
    
*Problem 4;
    
xtx = x`*x;

*Problem 5;

invx = inv(xtx);

*Problem 6;

b = invx*(x`*y);

*Problem 7;

d = x||y;

*Problem 8;

d[5,2] = 7;

*Problem 9;

dd = (d[1:5,1:2]//d[9:10,1:2])||(y[1:5,]//y[9:10,]);

*Problem 10;

create work.myData from dd;
append from dd;
quit;

proc print data = myData;
run;

*Problem 11;

data odds;
input var1 var2 var3;
datalines;
1 3.55 5
7 52 11
13 15 17
;
run;

proc iml;
reset log print;
use work.odds;
read all INTO myIML;


*Problem 12;

myVar = var(myIML);
myCov = cov(myIML);
quit;


