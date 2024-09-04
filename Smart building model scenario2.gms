sets
i device  /1*22/
j day     /1*365/
t time(h) /1*24/
;
********************************************************************************
********************************************************************************
parameters
********* COSTS *********
COMPV       OM cost of PhotoVoltaic                               /0.005/
COMST       OM cost of Solar Thermal                              /0.005/
COMWT       OM cost of Wind Turbine                               /0.005/
COMGSHP     OM cost of Ground Source Heat Pump                    /0.01/
COMEES      OM cost of Electerical Energy Storage                 /0.005/
COMTES      OM cost of Thermal Energy Storage                     /0.001/
Cimp        Cost electricity imported from the grid               /0.005/
Cexp        Cost electricity exported to the grid at              /0.03/
*********PV & ST ********
PPVmin      Minimum power output PV                               /0/
PPVmax      Maximum power output PV                               /12/
PSTmin      Minimum power output ST                               /0/
PSTmax      Maximum power output ST                               /2.2/
APV         Installed photovoltaic area                           /70/
AST         Installed Solar Thermal area                          /12/
nPV         Efficiency PV                                         /0.15/
nST         Efficiency ST                                         /0.15/
H(j,t)      Solar irridiance
********* WT **********
PWTmin      Minimum power output WT                               /0/
PWTmax      Maximum power output WT                               /7/
Ad          Air density                                           /1.225/
AWT         Rotor Area                                            /28/
V(j,t)      Wind velocity
Vcutin      Cut in wind velocity                                  /3/
Vcutout     Cut out wind velocity                                 /10/
nWT         Efficiency Wind Turbine                               /0.26/
********* GSHP **********
Q(j,t)      Heat demand
********* EES & TES *******
SEESmin     Minimum saving level stored in the EES                /0/
SEESmax     Maximum saving level stored in the EES                /12/
STESmin     Minimum saving level stored in the TES                /0/
STESmax     Maximum saving level stored in the TES                /10/
PEESchmin   Minimum EES charge                                    /0.4/
PEESchmax   Maximum EES charge                                    /12/
PEESdchmin  Minimum EES discharge                                 /0.4/
PEESdchmax  Maximum EES discharge                                 /12/
PTESchmin   Minimum TES charge                                    /0/
PTESchmax   Maximum TES charge                                    /10/
PTESdchmin  Minimum TES discharge                                 /0/
PTESdchmax  Maximum TES discharge                                 /10/
nEESch      Charge Efficiency EES                                 /0.95/
nTESch      Charge Efficiency TES                                 /0.75/
nEESdch     Discharge Efficiency EES                              /0.95/
nTESdch     Discharge Efficiency TES                              /0.75/
LEES        Energy loss of EES                                    /0.02/
LTES        Energy loss of TES                                    /0.032/
********* Micro grid*******
Pimpmax     Maximum electricity imported from the grid to the building  /20/
Pexpmax     Maximum electricity exported from the grid to the building  /10/
;
********* Input data********
$call gdxxrw H.xlsx par=H rng=sheet1!a1 Cdim=1 Rdim=1
$gdxin H.gdx
$load H
$gdxin
$call gdxxrw V.xlsx par=V rng=sheet1!a1   Cdim=1 Rdim=1
$gdxin V.gdx
$load V
$gdxin
$call gdxxrw Q.xlsx par=Q rng=sheet1!a1 Cdim=1 Rdim=1
$gdxin Q.gdx
$load Q
$gdxin
;
********************************************************************************
********************************************************************************
****** Smart devices table *********
Table SmartTable(i,*)
      consumptionhour       start     end         energyconsumption
1           24               0         24               0.15
2           24               0         24               0.01
3           24               0         24               0.01
4           24               0         24               0.01
5           6                9         22               1
6           8                7         23               1.4
7           4                11        20               1.8
8           7                15        24               0.14
9           9                6         18               0.12
10          9                1         12               1.16
11          16               7         24               0.3
12          12               7         24               1.5
13          12               10        24               0.1
14          6                16        24               0.3
15          3                10        17               1.1
16          10               8         21               0.65
17          6                8         24               0.9
18          12               6         23               1
19          6                10        22               0.6
20          4                7         12               0.8
21          2                7         20               0.75
22          1                9         20               1.2
;
********************************************************************************
********************************************************************************
Binary variables
YEESch(j,t)  Charging mode EES
YEESdch(j,t) Discharging mode EES
YTESch(j,t)  Charging mode TES
YTESdch(j,t) Discharging mode TES
Yptd(i,j,t)  Part time device mode
Yimp(j,t)    Mode of electricity Purchase from the grid
Yexp(j,t)    Mode of electricity sales to the grid
;
********************************************************************************
********************************************************************************
positive Variables
PPV(j,t)     Power generated by PV
PST(j,t)     Power generated by ST
PWT(j,t)     Power generated by WT
Pimp(j,t)    Power imported from the grid
Pexp(j,t)    Power exported from the grid
PEESdch(j,t) Discharge of EES
PTESdch(j,t) Discharge of TES
PEESch(j,t)  charge of EES
PTESch(j,t)  charge of TES
SEES(j,t)    Saving level of energy stored in the EES
STES(j,t)    Saving level of energy stored in the TES
Psh(j,t)     Smart home power consumption
TPsd(j,t)    Total power consumption of smart devices
Psd(i,j,t)   Energy consumption of device i
PGSHP(j,t)   GSHP power consumption
;
********************************************************************************
********************************************************************************
variable
OF           Objective function
;
********************************************************************************
********************************************************************************
Equations
G
c1
c2
c3
c4
c5
c6
c7
c8
c9
c10
c11
c12
c13
c14
c15
c16
c17
c18
c19
c20
c21
c22
c23
c24
c25
c26
c27
c28
c29
c30
c31
c32
c33
c34
c35
c36
c37
c38
c39
c40
c41
c42
c43

;
G..OF=e=sum((j,t),COMPV*PPV(j,t))+sum((j,t),COMST*PST(j,t))+
        sum((j,t),COMWT*PWT(j,t))+sum((j,t),Cimp*Pimp(j,t))-
        sum((j,t),Cexp*Pexp(j,t))+sum((j,t),COMEES*PEESdch(j,t))+
        sum((j,t),COMTES*PTESdch(j,t))+sum((j,t),COMGSHP*PGSHP(j,t))
;
c1(j,t).. PPV(j,t)=e=nPV*APV*H(j,t);
c2(j,t).. PST(j,t)=e=nST*AST*H(j,t);
c3(j,t).. PPV(j,t)=l=PPVmax;
c4(j,t).. PPV(j,t)=g=PPVmin;
c5(j,t).. PST(j,t)=l=PSTmax;
c6(j,t).. PST(j,t)=g=PSTmin;
c7(j,t).. PWT(j,t)=l=PWTmax;
c8(j,t).. PWT(j,t)=g=PWTmin;
c9(j,t).. PWT(j,t)=e=((1/2000)*Ad*nWT*AWT*(V(j,t))**3);
c10(j,t).. PGSHP(j,t)=e=Q(j,t) ;
c11(j,t)$(ord(t)>1).. SEES(j,t)=e=SEES(j,t-1)+(nEESch*PEESch(j,t)-nEESdch*PEESdch(j,t)-LEES) ;
c12(j).. SEES(j+1,'1')=e=SEES(j,'24')-nEESdch*PEESdch(j+1,'1')-LEES+nEESch*PEESch(j+1,'1')  ;
c13(j,t).. PEESch(j,t)=g=YEESch(j,t)*PEESchmin  ;
c14(j,t).. PEESch(j,t)=l=YEESch(j,t)*PEESchmax  ;
c15(j,t).. PEESdch(j,t)=g=YEESdch(j,t)*PEESdchmin  ;
c16(j,t).. PEESdch(j,t)=l=YEESdch(j,t)*PEESdchmax  ;
c17(j,t).. SEES(j,t)=l=SEESmax  ;
c18(j,t).. SEES(j,t)=g=SEESmin   ;
c19(j,t).. YEESch(j,t)+YEESdch(j,t)=l=1  ;
c20(j,t)$(ord(t)>1).. STES(j,t)=e=STES(j,t-1)+(nTESch*PTESch(j,t)-nEESdch*PTESdch(j,t)-LTES) ;
c21(j).. STES(j+1,'1')=e=STES(j,'24')-nTESdch*PTESdch(j+1,'1')-LTES+nTESch*PTESch(j+1,'1')  ;
c22(j,t).. YTESch(j,t)*PTESchmin=l=PTESch(j,t)   ;
c23(j,t).. PTESch(j,t)=l=YTESch(j,t)*PTESchmax    ;
c24(j,t).. YTESdch(j,t)*PTESdchmin=l=PTESdch(j,t)   ;
c25(j,t).. PTESdch(j,t)=l=YTESdch(j,t)*PTESdchmax  ;
c26(j,t).. STES(j,t)=g=STESmin    ;
c27(j,t).. STES(j,t)=l=STESmax   ;
c28(j,t).. YTESch(j,t)+YTESdch(j,t)=l=1   ;
c29(j,t).. Psh(j,t)-TPsd(j,t)-PGSHP(j,t)=e=0     ;
c30(j,t).. PST(j,t)+PTESdch(j,t)=g=PTESch(j,t);
c31(j,t).. TPsd(j,t)-Pimp(j,t)-PPV(j,t)-PWT(j,t)-PEESdch(j,t)+PGSHP(j,t)+PEESch(j,t)+Pexp(j,t)=l=0  ;
c32(j,t).. TPsd(j,t)=l=Psh(j,t)  ;
c33(i,j,t)$(ord(i)>4)..Psd(i,j,t)=e=SmartTable(i,'energyconsumption')*Yptd(i,j,t)  ;
c34(i,j,t)$(ord(i)<5).. Psd(i,j,t)=e=SmartTable(i,'energyconsumption')  ;
c35(i,j)$(ord(i)>4)..sum(    t$(ord(t)<=SmartTable(i,'end') and ord(t)>=SmartTable(i,'start')),Psd(i,j,t))=e=SmartTable(i,'energyconsumption')*SmartTable(i,'consumptionhour') ;
c36(j,t).. TPsd(j,t)=e=sum(i,Psd(i,j,t));
c37(j,t).. Psh(j,t)=l=PPV(j,t)+PWT(j,t)+Pimp(j,t);
c38(j,t).. Pimp(j,t)=g=0     ;
c39(j,t).. Pimp(j,t)=l=Yimp(j,t)*Pimpmax   ;
c40(j,t).. Pexp(j,t)=g=0   ;
c41(j,t).. Pexp(j,t)=l=Yexp(j,t)*Pexpmax  ;
c42(j,t).. Yimp(j,t)+Yexp(j,t)=l=1   ;
c43(j,t).. Pexp(j,t)=l=PPV(j,t)+PWT(j,t)  ;

Model opt /all/;
Solve opt us MIP min OF;
execute_unload "scenario2.gdx" ;
