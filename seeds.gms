$title "Small Energy Deployment System (SeEDS)"

* PURPOSE - To demonstrate how we approach using GAMS in its use 
*           of ReEDS with a simplified capacity expansion model
*           
*           Things we could add, mostly to avoid corner solutions:
*               1. operating reserve
*               2. curtailment
*               3. planning reserve / capacity credit
*               4. fuel and resource supply curves
*               5. real data
*               6. transmission expansion
* I would recommend keeping the modeling framework as simple as possible, and
* allowing users to make it as complex as needed for their use-case via their
* inputs.


* Three globals available:
* endyear: define end year for solve loop
* Sw_Tran: enable or disable transmission
* outname: outputs will be written to "seeds_%outname%.gdx"


$setglobal ds \

*Change the default slash if in UNIX
$ifthen.unix %system.filesys% == UNIX
$setglobal ds /
$endif.unix



* ----------- b_inputs ----------- *


*===============
* -- Indices --
*===============

$if not set Sw_Tran $SetGlobal Sw_Tran 1

scalar Sw_Tran "switch to enable or disable transmission" /%Sw_Tran%/ ;
* This switch, for example, is something I would handle during the data input
* processing step. If a user only defines one region, transmission is disabled.
* One less thing to handle inside the optimization. That is, unless this would
* break the transmission equations. Still, this could be avoided by creating a
* dummy node and a line with 0 capacity.
* I think switches are a slippery slope towards inflexibility and should be
* kept to a minimum.

set i "technology" /coal, gas-cc, wind/, 
    r "region"     /p1, p2/,
    h "timeslice"  /h1*h8760/,
    d "days"       /1*365/,
    t "years"      /2020*2030/,
    f "fuels"      /COL, GAS/,
    i_f(i,f) "tech -> fuel" /coal.col, gas-cc.gas/,
* new vintage for each year    
    v "vintage"    /init-1, init-2, #t /,
* begin subsets    
    hmod(h), dmod(d), hdmod(h,d),
    tmodel(t)    "modeled year",
    tfix(t)      "fixed (previously-solved) years",
    initv(v)     "vintages that existed as of start year" /init-1, init-2/,
    newv(v)      "new vintages" / #t /,
    val(i,v,r,t) "valid i/v/r/t capacity and generation combinations"
    val_inv(i,v,r,t) "valid investment combinations"
;


* create an alias of r as rr
* will demonstrate why this is necessary and under what circumstances
alias(r,rr) ;
alias(t,tt) ;

* need to declare tprev after tt has been created
set     tprev(t,tt)  "tt is the previously-solved year to t", 
        tpast(t,tt)  "tt is in the past to t" ;

* cannot leave unassigned parameters in the model
tmodel(t) = no ;
tfix(t) = no ;

* populate t/tt for all previously-linked sets
tprev(t,tt)$[t.val = tt.val + 1] = yes ;
tpast(t,tt)$[tt.val <= t.val] = yes ;


* -- end sets -- 

* -- begin data --

* few different types of input options but mainly we use
* scalars, tables, and parameter throughout ReEDS
* ReEDS or SeEDS?

scalar pvf_inv  "present value of investment" /1/, 
       pvf_onm  "20-year inverse capital recovery factor"
;

pvf_onm = 1 / 0.06936 ;

table hour_char(h,r,d,*)
$ondelim
$include inputs/hour_char.csv
$offdelim
;

* only want to model hours/days with load
hmod(h)$sum{(r,d),hour_char(h,r,d,"load")} = yes ;
dmod(d)$sum{(r,h),hour_char(h,r,d,"load")} = yes ;
hdmod(h,d)$sum{(r),hour_char(h,r,d,"load")} = yes ;
* Why is this necessary? Why not just require hourly time-series inputs
* along with a time-series defining the desired time-slices?

parameter hours(h) ;
hours(h) = sum(d,hour_char(h,"p1",d,"hours") );


* --  Starting capacities --

* for now, not allowing expansion of transmission
* will demonstrate use of global here
parameter trancap(r,rr)
/
$ondelim
$include inputs/trancap.csv
$offdelim
/ ;

* insure symmetry
trancap(rr,r) = trancap(r,rr) ;

scalar       tranloss "--%-- amount of losses in transmission"               /0.05/ ;


table cap_exog(i,v,r,t,*) "--MW-- starting capacity"
$ondelim
$include inputs%ds%cap_start.csv
$offdelim
;
* Starting capacity or exogenous capacity?

parameter m_cap_exog(i,v,r,t) ;
m_cap_exog(i,v,r,t) = sum(tt$(tt.val <= t.val),cap_exog(i,v,r,tt,"capacity")) ;
* Does this imply we are ignoring retirements?

parameter cf_in(i,r,h)
/
$ondelim
$include inputs/cf.csv
$offdelim
/;
* Again, I recommend consistency over minimalism. Hourly time-series inputs.
* Always. Then those are grouped together by the time-slices defined by the
* user. Users can then use whatever complicated clustering algorithms they
* want to define their timeslices but that's outside the scope os SIMPLE.

* -- Load -- 

parameter load(r,h,t) "-- MW per timeslice -- exogenous load" 
          load_growth(r) "CAGR for load relative to 2020" /p1 0.05, p2 0.015/
;
* Same thing here regarding inputs.

load(r,h,t) = sum(d,hour_char(h,r,d,"load")) * (1+load_growth(r)) ** (t.val - 2020) ;
* This "day" thing is new and confuses me. Is it needed?

* enable val for existing capacities
val(i,v,r,t)$[sum(tt$tpast(t,tt),cap_exog(i,v,r,tt,"capacity"))] = yes ;

* cannot build until 2021
val(i,newv,r,t)$[t.val >= 2020] = yes ;

* cannot build until your vintage becomes available
val(i,newv,r,t)$[t.val < newv.val] = no ;

val_inv(i,newv,r,t)$[t.val = newv.val] = yes ;

* -- parameterization of new capacity --

table new_char(i,*) "characteristics for new capacity"
$ondelim
$include inputs/new_char.csv
$offdelim
;

parameter cost_cap(i,t)       "cost of capacity investment"
          cost_fom(i,v,r,t)   "-- $ / MW -- annual fom costs"
          cost_vom(i,v,r,t)   "-- $ / MWh -- variable onm costs" 
          heat_rate(i,v,r,t)  "-- mmBTU / MWh -- fuel intensity" 
;


cost_fom(i,initv,r,t)  = sum(tt,cap_exog(i,initv,r,tt,"cost_fom") );
cost_vom(i,initv,r,t)  = sum(tt,cap_exog(i,initv,r,tt,"cost_vom") );
heat_rate(i,initv,r,t) = sum(tt,cap_exog(i,initv,r,tt,"heat_rate") );

cost_fom(i,newv,r,t)  = new_char(i,"cost_fom") - new_char(i,"cost_fom") * ((1-new_char(i,"fom_decline")) / (2030-2020)) * (newv.val - 2020) ;
cost_vom(i,newv,r,t)  = new_char(i,"cost_vom") - new_char(i,"cost_vom") * ((1-new_char(i,"vom_decline")) / (2030-2020)) * (newv.val - 2020) ;
heat_rate(i,newv,r,t) = new_char(i,"heat_rate") - new_char(i,"heat_rate") * ((1-new_char(i,"heat_rate_decline")) / (2030-2020)) * (newv.val - 2020) ;

cost_cap(i,t) = new_char(i,"cost_cap") - new_char(i,"cost_cap") * ((1-new_char(i,"fom_decline")) / (2030-2020)) * (t.val - 2020)


parameter avail(i,v,r,h,t) ;
avail(i,initv,r,h,t)$[sum(tt,cap_exog(i,initv,r,tt,"cf_tech"))] = cf_in(i,r,h) * sum(tt,cap_exog(i,initv,r,tt,"avail")) ;
avail(i,initv,r,h,t)$[not sum(tt,cap_exog(i,initv,r,tt,"cf_tech"))] =  sum(tt,cap_exog(i,initv,r,tt,"avail"));
avail(i,newv,r,h,t)$[new_char(i,"cf_tech")] = cf_in(i,r,h) * new_char(i,"avail") ;
avail(i,newv,r,h,t)$[not new_char(i,"cf_tech")] = new_char(i,"avail")

* note only declaring two observations here and will grow by growth rates
parameter fuel_price_f(f,t) "-- $ / mmBTU -- fuel prices"
/
gas.2020 4,
col.2020 2
/;

parameter fuel_growth(f) "-- $ / mmBTU per year -- growth of fuel prices from 2020"
/
gas 0.2
col 0.3
/;
* What if a user doesn't want to define linear growth rates for fuel?
* This is a dataset formulation thing, and it belongs outside the model.

* assuming linear growth of fuel prices
fuel_price_f(f,t)$[t.val > 2020] = fuel_price_f(f,"2020") + (t.val - 2020) * fuel_growth(f) ;

parameter fuel_price(i,t) "--$ / mmbtu-- fuel price used in the model" ;
fuel_price(i,t) = sum(f$i_f(i,f), fuel_price_f(f,t)) ;

* ----- Constraints ----- 

positive variables   CAP(i,v,r,t)    "-- MW -- capacity available for generation"
                     GEN(i,v,r,h,t)  "-- MW -- generation from available units"
                     INV(i,v,r,t)    "-- MW -- investment in new capacity"
                     FLOW(r,rr,h,t)  "-- MW -- transmission of electricity"
;

equation eq_cap_init(i,v,r,t)      "-- MW -- existing capacity cannot excced exogenously specified amounts"
         eq_cap_new(i,v,r,t)       "-- MW -- new capacity is the sum of previous years investments"
         eq_cap_limit(i,v,r,h,t)   "-- MW -- generation cannot exceed capacity"
         eq_tran_limit(r,rr,h,t)   "-- MW -- transmission cannot exceed trancap"
         eq_loadcon(r,h,t)         "-- MW -- generation plus net trade must meet demand"
;

eq_cap_init(i,v,r,t)$[initv(v)$val(i,v,r,t)$tmodel(t)].. 
* exogenously-specified capacity if in the first year
        m_cap_exog(i,v,r,t)$[t.val = 2020]

* previous-years solved value if past the first year
        + sum{tt$tprev(t,tt), CAP(i,v,r,tt) }$[t.val > 2020]

        =g=
* endogenous capacity
        CAP(i,v,r,t)
;

eq_cap_new(i,v,r,t)$[newv(v)$val(i,v,r,t)$tmodel(t)].. 
* sum of all previous-years investments
        sum{tt$[tpast(t,tt)$val_inv(i,v,r,tt)], INV(i,v,r,tt) }

        =g= 

* endogenous capacity for new vintages
        CAP(i,v,r,t)
;

eq_cap_limit(i,v,r,h,t)$[val(i,v,r,t)$tmodel(t)$hmod(h)]..
* available amount of capacity
        avail(i,v,r,h,t) * CAP(i,v,r,t)

        =g=

* generation         
        GEN(i,v,r,h,t)
;

eq_tran_limit(r,rr,h,t)$[tmodel(t)$(not sameas(r,rr))$hmod(h)]..
* scalar amount of transmission capacity
        trancap(r,rr)

        =g=

* bi-directional flow of electricity
        FLOW(r,rr,h,t) + FLOW(rr,r,h,t)
;

eq_loadcon(r,h,t)$[hmod(h)$tmodel(t)]..
* total generation
        sum{(i,v)$val(i,v,r,t), GEN(i,v,r,h,t) }
* net flow of electricity
        + sum{rr$[not sameas(r,rr)], 
* imports from rr to r face loss factor        
             (1 - tranloss) * FLOW(rr,r,h,t) 
* exports do not face loss factor
             - FLOW(r,rr,h,t) }

        =g=

        load(r,h,t)
;


* -----------------------------
* ----- c_supplyobjective ----- 
* -----------------------------

variable Z_inv(t) "--$-- investment portion of the objective function"
         Z_op(t)  "--$-- operations portion of the objective function"
         Z        "--$-- objective function, target of optimization"
;

equation eq_z_inv(t) "--$-- investment portion of the objective function"
         eq_z_op(t)  "--$-- operations portion of the objective function"
         eq_z        "--$-- objective function, target of optimization"
;

eq_z_inv(t)$tmodel(t)..

    Z_inv(t)

    =e= 

    sum{(i,v,r)$[val(i,v,r,t)$newv(v)$val_inv(i,v,r,t)], 
            cost_cap(i,t) * INV(i,v,r,t) }
;

eq_z_op(t)$tmodel(t)..

    Z_op(t)

    =e=

* vom costs
    sum{(i,v,r,h)$[val(i,v,r,t)$hmod(h)], 
        GEN(i,v,r,h,t) * cost_vom(i,v,r,t) * hours(h) }

* fom costs
    + sum{(i,v,r)$[val(i,v,r,t)], 
        CAP(i,v,r,t) * cost_fom(i,v,r,t) }

* fuel costs
    + sum{(i,v,r,h)$[val(i,v,r,t)$hmod(h)], 
        GEN(i,v,r,h,t) * heat_rate(i,v,r,t) * fuel_price(i,t) * hours(h) }

;

eq_z..  Z =e= sum{t$tmodel(t), pvf_onm * Z_op(t) + pvf_inv * Z_inv(t) } ;


* model declaration
model seeds /all/ ;

* -----------------------
* ----- d_solveprep ----- 
* -----------------------

* some rounding, some switch setup, ...
* creation of reporting parameters
parameter rep_z;


* --------------------------
* ----- d_solveoneyear ----- 
* --------------------------

$if not set endyear $SetGlobal endyear 2020

set tloop(t) /2020*%endyear%/;

loop(tloop,
* reset tmodel
    tmodel(t) = no ;
    tmodel(tloop) = yes;

* solve the model
    solve seeds using lp minimizing Z ;
* record objective function values
    rep_z(t,"op")$tmodel(t) = Z_op.l(t) ;
    rep_z(t,"inv")$tmodel(t) = Z_inv.l(t) ;
    rep_z(t,"total")$tmodel(t) = Z.l ;

* add tmodel to tfix
    tfix(tmodel) = yes ;

* -- d2_varfix -- 
    CAP.fx(i,v,r,tfix) = CAP.l(i,v,r,tfix) ;
    GEN.fx(i,v,r,h,tfix) = GEN.l(i,v,r,h,tfix) ;
    INV.fx(i,v,r,tfix) = INV.l(i,v,r,tfix) ;
    FLOW.fx(r,rr,h,tfix) = FLOW.l(r,rr,h,tfix) ;

* end loop over tloop
) ; 

execute_unload "alldata.gdx" ;

$exit


* ----------------
* --- e_report ---
* ----------------

* create some reporting parameters
* note not declaring indices.. half lazy half demo
parameter rep_gen, rep_cap, rep_price;

rep_gen(i,t) = sum{(v,r,h)$val(i,v,r,t),hours(h) * GEN.l(i,v,r,h,t) } ;
rep_cap(i,t) = sum{(v,r)$val(i,v,r,t),CAP.l(i,v,r,t) } ;
rep_price(r,h,t) = eq_loadcon.m(r,h,t) / pvf_onm ;

* dump the data
$if not set outname $setglobal outname BAU
execute_unload 'seeds_%outname%.gdx' ;
