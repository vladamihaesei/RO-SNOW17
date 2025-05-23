
## SNOW-17

Snow-17 accumulation and ablation model. This code is writen in R programming language, converted from MATLAB code written by Mark Raleigh (mraleig1\@uw.edu) based on Anderson (2006).

This version of Snow-17 is intended for both use at a point location and gridded data (raster file). 

The time steps for precipitation and temperature must be equal for this code. This version provides detailed outputs, and includes the compaction module to compute snow depth and bulk density.

### SYNTAX

SNOWout = snow17c(sINPUTS)

SNOWout = snow17c(sINPUTS, sIC)

SNOWout = snow17c(sINPUTS, sIC, sSETTINGS)

SNOWout = snow17c(sINPUTS, sIC, sSETTINGS, sPARAMS)

### INPUTS

-   **sINPUTS**

structure includes these fields. This is the only required input

1.  T - Lx1 array of air temperature data (deg C)

2.  P - Lx1 array of incremental precipitation data (mm/timestep)

3.  TIME - Lx7 time matrix (see time_builder.R code)

4.   elev = 1x1 value of station elevation (meters)

-   **sIC** structure includes these fields for initial conditions (enter 0 for all if no snowpack, or set to empty)

1.  ATI = initial antecedent temperature index (C)

2.  W_q = initial SWE (liquid component), mm

3.  W_i = initial SWE (ice component), mm

4.  Deficit = initial heat deficit, aka NEGHS, Negative Heat Storage

5.  SNDEN = initial bulk snow density (kg m\^-3)

6.  SNTMP = initial bulk snow temperature (C)

-   **sSETTINGS** structure includes these fields - optional. will use defaults if not specified.

1.   RvS = control of how rain and snow are divided. Enter 0 if you want a single temperature (PXTEMP) that divides rain and snow --- default Enter 1 if you want a linear transition between 2 temperatures (PXTEMP1 and PXTEMP2) Enter 2 if you want no adjustments made to the precipitation data. All precipitation is considered snowfall. Enter 3 if you want to use an S-shaped transition \[Kienzle 2008\]

2.   mDEC_RHO_COMPACTION = modeling decision. specify whether compaction routine is included Enter 0 to turn off Enter 1 to turn on (default) 3

3.   mDEC_RHO_DM_METAMORPH = modeling decision. specify whether densification from destructive metamorphism and melt metamorphism is represented Enter 0 to turn off Enter 1 to turn on (default)
4.   mDEC_RHO_NEW = method for new snow density Enter 0 for Anderson (default) Enter 1 for Hedstrom and Pomeroy Enter 2 for constant value
5.   mDEC_RHO_METHOD = method for snowpack densification Enter 0 for Anderson (default) Enter 1 for density-time curve Enter 2 for Snobal-like density-time curves Enter 3 for constant value Enter 4 for Snobal updated

-   **sPARAMS** structure includes any or all or none of these fields. Traditional Params

1.   SCF = gauge under-catch snow correction factor

2.   UADJ = average wind function during rain on snow (mm/mb)

3.   MBASE = base temperature above which melt typically occurs (deg C)

4.  MFMAX = maximum melt factor during non-rain periods (mm/deg C 6 hr) - in western facing slope assumed to occur on June 21

5.  MFMIN = minimum melt factor during non-rain periods (mm/deg C 6 hr) - in western facing slope assumed to occur on December 21

6.  TIPM = model parameter (\>0.0 and \<1.0) - Anderson Manual recommends 0.1 to 0.2 for deep snowpack areas

7.  NMF = maximum negative melt factor (mm/deg C 6 hr)

8.  PLWHC = percent liquid water holding capacity of the snow pack - max is 0.4

-    **sPARAMS1 (other Params)**

1.  PXTEMP = **USED IF RvS=0**: temperature dividing rain from snow (deg C) - if temp is less than or equal to PXTEMP, all precip is snow. Otherwise it is rain.

2.  PXTEMP1 = **USED IF RvS=1**: Lower Limit Temperature dividing tranistion from snow, deg C - if temp is less than or equal to PXTEMP1, all precip is snow. Otherwise it is mixed linearly.

3.  PXTEMP2 = **USED IF RvS=1**: Upper Limit Temperature dividing rain from transition, deg C - if temp is greater than or equal to PXTEMP2, all precip is rain. Otherwise it is mixed linearly. Tt = **USED IF RvS=3**: threshold temperature that defines 50/50 rain snow Tr = **USED IF RvS=3**: temperature range that defines the width of the S-shaped transition from rain to snow rhon0 = **USED IF mDEC_RHO_NEW=2**: constant new snow density (g/cm3)

-   **Density and Depth Params** (mDEC_RHO_METHOD=0, ANDERSON method)

1.  C1 = fractional increase in density (cm\^-1 hr\^-1)
2.  C2 = constant estimated by Kojima (cm\^3 / g);
3.  C3 = fractional settling rate at 0 C for rho_x \< THRESD (hr\^-1)
4.  C4 = constant THRESD = threshold density above which destructive metamorphism decreases
5.  C5 = increase in fractional settling rate when liquid water exists
6.  CX = destructive metamorphism decay factor when rho_x \> THRESD

-    **Density and Depth Params** (mDEC_RHO_METHOD=1, DENSITY-TIME CURVE)

1.  rmlt = maximum density for melting snow (g/cm3)

2.  rcld = maximum density for cold snow (g/cm3)

3.  trho = compaction time scale (hr)

-   **Density and Depth Params** (mDEC_RHO_METHOD=3, FIXED DENSITY)

1.  rho0 = constant bulk snow density (g/cm3)

## **SNOWout**  

1.   TIME = Lx7 time matrix (see time_builder.R format)

2.  SWE = Lx1 array of modeled SWE (mm)

3.   Hs = Lx1 array of modeled snow depth (cm)

4.   RHO = Lx1 array of modeled bulk snowpack density (kg m\^-3)

5.   SNOW = Lx1 array of new snowfall, water equivalent (mm)

6.   RAIN = Lx1 array of new rainfall, water equivalent (mm)

7.   Hn = Lx1 array of thickness of new snow layer (cm)

8.   RHOn = Lx1 array of new snowfall density (kg m\^-3)

9.   SWEq = Lx1 array of liquid water in snowpack (mm)

10.  SWEi = Lx1 array of solid water (ice) in snowpack (mm)

11.  M_nr = Lx1 array of melt caused from non-rain period (mm)

12.  M_ros = Lx1 array of melt caused by rain-on-snow (mm)

13.  MF = Lx1 array of melt factor (mm/deg C 6 hr)

14.  SRFRZ = Lx1 array of refrozen melt/rain water (mm)

15.  ATI = Lx1 array of antecedent temperature index (C)

16.  T_pack = Lx1 array of average snowpack temperature (C)

17.  T_snow = Lx1 array of snowfall precipitation temperature (C)

18.  T_rain = Lx1 array of rainfall precipitation temperature (C)

19. T_prcp = Lx1 array of precipitation temperature, all forms (C)

20.  Deficit = Lx1 array of heat deficit, aka NEGHS, Negative Heat Storage

21.  outflow = Lx1 array of outflow from the snowpack (snowmelt, or rain percolating through the snowpack). Note: Outflow is zero during times when it is raining on bare ground (no snowpack exists).
