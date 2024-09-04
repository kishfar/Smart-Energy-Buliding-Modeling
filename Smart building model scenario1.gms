sets
j day     /1*365/;
***********************************
***********************************
parameters
Cimp    Cost for electricity imported from the grid   /0.003/
EL      Electricity load   /105/
Q(j)    Heating and cooling load 
;
***********************************
***********************************
positive variable
Pimp(j)  Electricity power imported
;
***********************************
***********************************
variable
OF
;
equation
c1
c2
;
c1..  OF=e=sum(j,Cimp*Pimp(j)) ;
c2(j).. Pimp(j)=e=EL+Q(j)      ;

Model opt /all/;
Solve opt us LP min OF;
execute_unload "scenario1.gdx" ;
