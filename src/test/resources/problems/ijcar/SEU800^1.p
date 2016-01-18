%------------------------------------------------------------------------------
% File     : SEU800^1 : TPTP v6.2.0. Released v3.7.0.
% Domain   : Set Theory
% Problem  : More about Functions - Injective Functions
% Version  : Especial.
% English  : (! x:i.! y:i.! f:i.in f (injFuncSet x y) -> injective x y f)

% Refs     : [Bro08] Brown (2008), Email to G. Sutcliffe
% Source   : [Bro08]
% Names    : ZFC302g [Bro08]

% Status   : Theorem
% Rating   : 0.43 v6.1.0, 0.57 v5.5.0, 0.67 v5.4.0, 0.60 v5.2.0, 1.00 v3.7.0
% Syntax   : Number of formulae    :  687 ( 318 unit; 367 type; 319 defn)
%            Number of atoms       : 5957 ( 459 equality;2725 variable)
%            Maximal formula depth :  324 (   7 average)
%            Number of connectives : 4326 (  73   ~;  14   |;  53   &;3151   @)
%                                         (  17 <=>;1018  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&;   0  !!;   0  ??)
%            Number of type conns  :  170 ( 170   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :  372 ( 367   :)
%            Number of variables   : 1130 (   1 sgn;1009   !;  43   ?;  78   ^)
%                                         (1130   :;   0  !>;   0  ?*)
%                                         (   0  @-;   0  @+)
% SPC      : TH0_THM_EQU

% Comments : http://mathgate.info/detsetitem.php?id=425
%          : 
%------------------------------------------------------------------------------
thf(in_type,type,(
    in: $i > $i > $o )).

thf(exu_type,type,(
    exu: ( $i > $o ) > $o )).

thf(exu,definition,
    ( exu
    = ( ^ [Xphi: $i > $o] :
        ? [Xx: $i] :
          ( ( Xphi @ Xx )
          & ! [Xy: $i] :
              ( ( Xphi @ Xy )
             => ( Xx = Xy ) ) ) ) )).

thf(setextAx_type,type,(
    setextAx: $o )).

thf(setextAx,definition,
    ( setextAx
    = ( ! [A: $i,B: $i] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ A )
            <=> ( in @ Xx @ B ) )
         => ( A = B ) ) ) )).

thf(emptyset_type,type,(
    emptyset: $i )).

thf(emptysetAx_type,type,(
    emptysetAx: $o )).

thf(emptysetAx,definition,
    ( emptysetAx
    = ( ! [Xx: $i] :
          ~ ( in @ Xx @ emptyset ) ) )).

thf(setadjoin_type,type,(
    setadjoin: $i > $i > $i )).

thf(setadjoinAx_type,type,(
    setadjoinAx: $o )).

thf(setadjoinAx,definition,
    ( setadjoinAx
    = ( ! [Xx: $i,A: $i,Xy: $i] :
          ( ( in @ Xy @ ( setadjoin @ Xx @ A ) )
        <=> ( ( Xy = Xx )
            | ( in @ Xy @ A ) ) ) ) )).

thf(powerset_type,type,(
    powerset: $i > $i )).

thf(powersetAx_type,type,(
    powersetAx: $o )).

thf(powersetAx,definition,
    ( powersetAx
    = ( ! [A: $i,B: $i] :
          ( ( in @ B @ ( powerset @ A ) )
        <=> ! [Xx: $i] :
              ( ( in @ Xx @ B )
             => ( in @ Xx @ A ) ) ) ) )).

thf(setunion_type,type,(
    setunion: $i > $i )).

thf(setunionAx_type,type,(
    setunionAx: $o )).

thf(setunionAx,definition,
    ( setunionAx
    = ( ! [A: $i,Xx: $i] :
          ( ( in @ Xx @ ( setunion @ A ) )
        <=> ? [B: $i] :
              ( ( in @ Xx @ B )
              & ( in @ B @ A ) ) ) ) )).

thf(omega_type,type,(
    omega: $i )).

thf(omega0Ax_type,type,(
    omega0Ax: $o )).

thf(omega0Ax,definition,
    ( omega0Ax
    = ( in @ emptyset @ omega ) )).

thf(omegaSAx_type,type,(
    omegaSAx: $o )).

thf(omegaSAx,definition,
    ( omegaSAx
    = ( ! [Xx: $i] :
          ( ( in @ Xx @ omega )
         => ( in @ ( setadjoin @ Xx @ Xx ) @ omega ) ) ) )).

thf(omegaIndAx_type,type,(
    omegaIndAx: $o )).

thf(omegaIndAx,definition,
    ( omegaIndAx
    = ( ! [A: $i] :
          ( ( ( in @ emptyset @ A )
            & ! [Xx: $i] :
                ( ( ( in @ Xx @ omega )
                  & ( in @ Xx @ A ) )
               => ( in @ ( setadjoin @ Xx @ Xx ) @ A ) ) )
         => ! [Xx: $i] :
              ( ( in @ Xx @ omega )
             => ( in @ Xx @ A ) ) ) ) )).

thf(replAx_type,type,(
    replAx: $o )).

thf(replAx,definition,
    ( replAx
    = ( ! [Xphi: $i > $i > $o,A: $i] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( exu
                @ ^ [Xy: $i] :
                    ( Xphi @ Xx @ Xy ) ) )
         => ? [B: $i] :
            ! [Xx: $i] :
              ( ( in @ Xx @ B )
            <=> ? [Xy: $i] :
                  ( ( in @ Xy @ A )
                  & ( Xphi @ Xy @ Xx ) ) ) ) ) )).

thf(foundationAx_type,type,(
    foundationAx: $o )).

thf(foundationAx,definition,
    ( foundationAx
    = ( ! [A: $i] :
          ( ? [Xx: $i] :
              ( in @ Xx @ A )
         => ? [B: $i] :
              ( ( in @ B @ A )
              & ~ ( ? [Xx: $i] :
                      ( ( in @ Xx @ B )
                      & ( in @ Xx @ A ) ) ) ) ) ) )).

thf(wellorderingAx_type,type,(
    wellorderingAx: $o )).

thf(wellorderingAx,definition,
    ( wellorderingAx
    = ( ! [A: $i] :
        ? [B: $i] :
          ( ! [C: $i] :
              ( ( in @ C @ B )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ C )
                 => ( in @ Xx @ A ) ) )
          & ! [Xx: $i,Xy: $i] :
              ( ( ( in @ Xx @ A )
                & ( in @ Xy @ A ) )
             => ( ! [C: $i] :
                    ( ( in @ C @ B )
                   => ( ( in @ Xx @ C )
                    <=> ( in @ Xy @ C ) ) )
               => ( Xx = Xy ) ) )
          & ! [C: $i,D: $i] :
              ( ( ( in @ C @ B )
                & ( in @ D @ B ) )
             => ( ! [Xx: $i] :
                    ( ( in @ Xx @ C )
                   => ( in @ Xx @ D ) )
                | ! [Xx: $i] :
                    ( ( in @ Xx @ D )
                   => ( in @ Xx @ C ) ) ) )
          & ! [C: $i] :
              ( ( ! [Xx: $i] :
                    ( ( in @ Xx @ C )
                   => ( in @ Xx @ A ) )
                & ? [Xx: $i] :
                    ( in @ Xx @ C ) )
             => ? [D: $i,Xx: $i] :
                  ( ( in @ D @ B )
                  & ( in @ Xx @ C )
                  & ~ ( ? [Xy: $i] :
                          ( ( in @ Xy @ D )
                          & ( in @ Xy @ C ) ) )
                  & ! [E: $i] :
                      ( ( in @ E @ B )
                     => ( ! [Xy: $i] :
                            ( ( in @ Xy @ E )
                           => ( in @ Xy @ D ) )
                        | ( in @ Xx @ E ) ) ) ) ) ) ) )).

thf(descr_type,type,(
    descr: ( $i > $o ) > $i )).

thf(descrp_type,type,(
    descrp: $o )).

thf(descrp,definition,
    ( descrp
    = ( ! [Xphi: $i > $o] :
          ( ( exu
            @ ^ [Xx: $i] :
                ( Xphi @ Xx ) )
         => ( Xphi
            @ ( descr
              @ ^ [Xx: $i] :
                  ( Xphi @ Xx ) ) ) ) ) )).

thf(dsetconstr_type,type,(
    dsetconstr: $i > ( $i > $o ) > $i )).

thf(dsetconstrI_type,type,(
    dsetconstrI: $o )).

thf(dsetconstrI,definition,
    ( dsetconstrI
    = ( ! [A: $i,Xphi: $i > $o,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( ( Xphi @ Xx )
           => ( in @ Xx
              @ ( dsetconstr @ A
                @ ^ [Xy: $i] :
                    ( Xphi @ Xy ) ) ) ) ) ) )).

thf(dsetconstrEL_type,type,(
    dsetconstrEL: $o )).

thf(dsetconstrEL,definition,
    ( dsetconstrEL
    = ( ! [A: $i,Xphi: $i > $o,Xx: $i] :
          ( ( in @ Xx
            @ ( dsetconstr @ A
              @ ^ [Xy: $i] :
                  ( Xphi @ Xy ) ) )
         => ( in @ Xx @ A ) ) ) )).

thf(dsetconstrER_type,type,(
    dsetconstrER: $o )).

thf(dsetconstrER,definition,
    ( dsetconstrER
    = ( ! [A: $i,Xphi: $i > $o,Xx: $i] :
          ( ( in @ Xx
            @ ( dsetconstr @ A
              @ ^ [Xy: $i] :
                  ( Xphi @ Xy ) ) )
         => ( Xphi @ Xx ) ) ) )).

thf(exuE1_type,type,(
    exuE1: $o )).

thf(exuE1,definition,
    ( exuE1
    = ( ! [Xphi: $i > $o] :
          ( ( exu
            @ ^ [Xx: $i] :
                ( Xphi @ Xx ) )
         => ? [Xx: $i] :
              ( ( Xphi @ Xx )
              & ! [Xy: $i] :
                  ( ( Xphi @ Xy )
                 => ( Xx = Xy ) ) ) ) ) )).

thf(prop2set_type,type,(
    prop2set: $o > $i )).

thf(prop2setE_type,type,(
    prop2setE: $o )).

thf(prop2setE,definition,
    ( prop2setE
    = ( ! [Xphi: $o,Xx: $i] :
          ( ( in @ Xx @ ( prop2set @ Xphi ) )
         => Xphi ) ) )).

thf(emptysetE_type,type,(
    emptysetE: $o )).

thf(emptysetE,definition,
    ( emptysetE
    = ( ! [Xx: $i] :
          ( ( in @ Xx @ emptyset )
         => ! [Xphi: $o] : Xphi ) ) )).

thf(emptysetimpfalse_type,type,(
    emptysetimpfalse: $o )).

thf(emptysetimpfalse,definition,
    ( emptysetimpfalse
    = ( ! [Xx: $i] :
          ( ( in @ Xx @ emptyset )
         => $false ) ) )).

thf(notinemptyset_type,type,(
    notinemptyset: $o )).

thf(notinemptyset,definition,
    ( notinemptyset
    = ( ! [Xx: $i] :
          ~ ( in @ Xx @ emptyset ) ) )).

thf(exuE3e_type,type,(
    exuE3e: $o )).

thf(exuE3e,definition,
    ( exuE3e
    = ( ! [Xphi: $i > $o] :
          ( ( exu
            @ ^ [Xx: $i] :
                ( Xphi @ Xx ) )
         => ? [Xx: $i] :
              ( Xphi @ Xx ) ) ) )).

thf(setext_type,type,(
    setext: $o )).

thf(setext,definition,
    ( setext
    = ( ! [A: $i,B: $i] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( in @ Xx @ B ) )
         => ( ! [Xx: $i] :
                ( ( in @ Xx @ B )
               => ( in @ Xx @ A ) )
           => ( A = B ) ) ) ) )).

thf(emptyI_type,type,(
    emptyI: $o )).

thf(emptyI,definition,
    ( emptyI
    = ( ! [A: $i] :
          ( ! [Xx: $i] :
              ~ ( in @ Xx @ A )
         => ( A = emptyset ) ) ) )).

thf(noeltsimpempty_type,type,(
    noeltsimpempty: $o )).

thf(noeltsimpempty,definition,
    ( noeltsimpempty
    = ( ! [A: $i] :
          ( ! [Xx: $i] :
              ~ ( in @ Xx @ A )
         => ( A = emptyset ) ) ) )).

thf(setbeta_type,type,(
    setbeta: $o )).

thf(setbeta,definition,
    ( setbeta
    = ( ! [A: $i,Xphi: $i > $o,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( ( in @ Xx
              @ ( dsetconstr @ A
                @ ^ [Xy: $i] :
                    ( Xphi @ Xy ) ) )
          <=> ( Xphi @ Xx ) ) ) ) )).

thf(nonempty_type,type,(
    nonempty: $i > $o )).

thf(nonempty,definition,
    ( nonempty
    = ( ^ [Xx: $i] : ( Xx != emptyset ) ) )).

thf(nonemptyE1_type,type,(
    nonemptyE1: $o )).

thf(nonemptyE1,definition,
    ( nonemptyE1
    = ( ! [A: $i] :
          ( ( nonempty @ A )
         => ? [Xx: $i] :
              ( in @ Xx @ A ) ) ) )).

thf(nonemptyI_type,type,(
    nonemptyI: $o )).

thf(nonemptyI,definition,
    ( nonemptyI
    = ( ! [A: $i,Xphi: $i > $o,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( ( Xphi @ Xx )
           => ( nonempty
              @ ( dsetconstr @ A
                @ ^ [Xy: $i] :
                    ( Xphi @ Xy ) ) ) ) ) ) )).

thf(nonemptyI1_type,type,(
    nonemptyI1: $o )).

thf(nonemptyI1,definition,
    ( nonemptyI1
    = ( ! [A: $i] :
          ( ? [Xx: $i] :
              ( in @ Xx @ A )
         => ( nonempty @ A ) ) ) )).

thf(setadjoinIL_type,type,(
    setadjoinIL: $o )).

thf(setadjoinIL,definition,
    ( setadjoinIL
    = ( ! [Xx: $i,Xy: $i] :
          ( in @ Xx @ ( setadjoin @ Xx @ Xy ) ) ) )).

thf(emptyinunitempty_type,type,(
    emptyinunitempty: $o )).

thf(emptyinunitempty,definition,
    ( emptyinunitempty
    = ( in @ emptyset @ ( setadjoin @ emptyset @ emptyset ) ) )).

thf(setadjoinIR_type,type,(
    setadjoinIR: $o )).

thf(setadjoinIR,definition,
    ( setadjoinIR
    = ( ! [Xx: $i,A: $i,Xy: $i] :
          ( ( in @ Xy @ A )
         => ( in @ Xy @ ( setadjoin @ Xx @ A ) ) ) ) )).

thf(setadjoinE_type,type,(
    setadjoinE: $o )).

thf(setadjoinE,definition,
    ( setadjoinE
    = ( ! [Xx: $i,A: $i,Xy: $i] :
          ( ( in @ Xy @ ( setadjoin @ Xx @ A ) )
         => ! [Xphi: $o] :
              ( ( ( Xy = Xx )
               => Xphi )
             => ( ( ( in @ Xy @ A )
                 => Xphi )
               => Xphi ) ) ) ) )).

thf(setadjoinOr_type,type,(
    setadjoinOr: $o )).

thf(setadjoinOr,definition,
    ( setadjoinOr
    = ( ! [Xx: $i,A: $i,Xy: $i] :
          ( ( in @ Xy @ ( setadjoin @ Xx @ A ) )
         => ( ( Xy = Xx )
            | ( in @ Xy @ A ) ) ) ) )).

thf(setoftrueEq_type,type,(
    setoftrueEq: $o )).

thf(setoftrueEq,definition,
    ( setoftrueEq
    = ( ! [A: $i] :
          ( ( dsetconstr @ A
            @ ^ [Xx: $i] : $true )
          = A ) ) )).

thf(powersetI_type,type,(
    powersetI: $o )).

thf(powersetI,definition,
    ( powersetI
    = ( ! [A: $i,B: $i] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ B )
             => ( in @ Xx @ A ) )
         => ( in @ B @ ( powerset @ A ) ) ) ) )).

thf(emptyinPowerset_type,type,(
    emptyinPowerset: $o )).

thf(emptyinPowerset,definition,
    ( emptyinPowerset
    = ( ! [A: $i] :
          ( in @ emptyset @ ( powerset @ A ) ) ) )).

thf(emptyInPowerset_type,type,(
    emptyInPowerset: $o )).

thf(emptyInPowerset,definition,
    ( emptyInPowerset
    = ( ! [A: $i] :
          ( in @ emptyset @ ( powerset @ A ) ) ) )).

thf(powersetE_type,type,(
    powersetE: $o )).

thf(powersetE,definition,
    ( powersetE
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ B @ ( powerset @ A ) )
         => ( ( in @ Xx @ B )
           => ( in @ Xx @ A ) ) ) ) )).

thf(setunionI_type,type,(
    setunionI: $o )).

thf(setunionI,definition,
    ( setunionI
    = ( ! [A: $i,Xx: $i,B: $i] :
          ( ( in @ Xx @ B )
         => ( ( in @ B @ A )
           => ( in @ Xx @ ( setunion @ A ) ) ) ) ) )).

thf(setunionE_type,type,(
    setunionE: $o )).

thf(setunionE,definition,
    ( setunionE
    = ( ! [A: $i,Xx: $i] :
          ( ( in @ Xx @ ( setunion @ A ) )
         => ! [Xphi: $o] :
              ( ! [B: $i] :
                  ( ( in @ Xx @ B )
                 => ( ( in @ B @ A )
                   => Xphi ) )
             => Xphi ) ) ) )).

thf(subPowSU_type,type,(
    subPowSU: $o )).

thf(subPowSU,definition,
    ( subPowSU
    = ( ! [A: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( in @ Xx @ ( powerset @ ( setunion @ A ) ) ) ) ) )).

thf(exuE2_type,type,(
    exuE2: $o )).

thf(exuE2,definition,
    ( exuE2
    = ( ! [Xphi: $i > $o] :
          ( ( exu
            @ ^ [Xx: $i] :
                ( Xphi @ Xx ) )
         => ? [Xx: $i] :
            ! [Xy: $i] :
              ( ( Xphi @ Xy )
            <=> ( Xy = Xx ) ) ) ) )).

thf(nonemptyImpWitness_type,type,(
    nonemptyImpWitness: $o )).

thf(nonemptyImpWitness,definition,
    ( nonemptyImpWitness
    = ( ! [A: $i] :
          ( ( nonempty @ A )
         => ? [Xx: $i] :
              ( ( in @ Xx @ A )
              & $true ) ) ) )).

thf(uniqinunit_type,type,(
    uniqinunit: $o )).

thf(uniqinunit,definition,
    ( uniqinunit
    = ( ! [Xx: $i,Xy: $i] :
          ( ( in @ Xx @ ( setadjoin @ Xy @ emptyset ) )
         => ( Xx = Xy ) ) ) )).

thf(notinsingleton_type,type,(
    notinsingleton: $o )).

thf(notinsingleton,definition,
    ( notinsingleton
    = ( ! [Xx: $i,Xy: $i] :
          ( ( Xx != Xy )
         => ~ ( in @ Xy @ ( setadjoin @ Xx @ emptyset ) ) ) ) )).

thf(eqinunit_type,type,(
    eqinunit: $o )).

thf(eqinunit,definition,
    ( eqinunit
    = ( ! [Xx: $i,Xy: $i] :
          ( ( Xx = Xy )
         => ( in @ Xx @ ( setadjoin @ Xy @ emptyset ) ) ) ) )).

thf(singletonsswitch_type,type,(
    singletonsswitch: $o )).

thf(singletonsswitch,definition,
    ( singletonsswitch
    = ( ! [Xx: $i,Xy: $i] :
          ( ( in @ Xx @ ( setadjoin @ Xy @ emptyset ) )
         => ( in @ Xy @ ( setadjoin @ Xx @ emptyset ) ) ) ) )).

thf(upairsetE_type,type,(
    upairsetE: $o )).

thf(upairsetE,definition,
    ( upairsetE
    = ( ! [Xx: $i,Xy: $i,Xz: $i] :
          ( ( in @ Xz @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) )
         => ( ( Xz = Xx )
            | ( Xz = Xy ) ) ) ) )).

thf(upairsetIL_type,type,(
    upairsetIL: $o )).

thf(upairsetIL,definition,
    ( upairsetIL
    = ( ! [Xx: $i,Xy: $i] :
          ( in @ Xx @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) ) ) )).

thf(upairsetIR_type,type,(
    upairsetIR: $o )).

thf(upairsetIR,definition,
    ( upairsetIR
    = ( ! [Xx: $i,Xy: $i] :
          ( in @ Xy @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) ) ) )).

thf(emptyE1_type,type,(
    emptyE1: $o )).

thf(emptyE1,definition,
    ( emptyE1
    = ( ! [A: $i,Xphi: $i > $o] :
          ( ? [Xx: $i] :
              ( ( in @ Xx @ A )
              & ( Xphi @ Xx ) )
         => ( ( ( dsetconstr @ A
                @ ^ [Xx: $i] :
                    ( Xphi @ Xx ) )
              = emptyset )
           => $false ) ) ) )).

thf(vacuousDall_type,type,(
    vacuousDall: $o )).

thf(vacuousDall,definition,
    ( vacuousDall
    = ( ! [Xphi: $i > $o,Xx: $i] :
          ( ( in @ Xx @ emptyset )
         => ( Xphi @ Xx ) ) ) )).

thf(quantDeMorgan1_type,type,(
    quantDeMorgan1: $o )).

thf(quantDeMorgan1,definition,
    ( quantDeMorgan1
    = ( ! [A: $i,Xphi: $i > $o] :
          ( ~ ( ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ( Xphi @ Xx ) ) )
         => ? [Xx: $i] :
              ( ( in @ Xx @ A )
              & ~ ( Xphi @ Xx ) ) ) ) )).

thf(quantDeMorgan2_type,type,(
    quantDeMorgan2: $o )).

thf(quantDeMorgan2,definition,
    ( quantDeMorgan2
    = ( ! [A: $i,Xphi: $i > $o] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ~ ( Xphi @ Xx ) )
         => ~ ( ? [Xx: $i] :
                  ( ( in @ Xx @ A )
                  & ( Xphi @ Xx ) ) ) ) ) )).

thf(quantDeMorgan3_type,type,(
    quantDeMorgan3: $o )).

thf(quantDeMorgan3,definition,
    ( quantDeMorgan3
    = ( ! [A: $i,Xphi: $i > $o] :
          ( ~ ( ? [Xx: $i] :
                  ( ( in @ Xx @ A )
                  & ( Xphi @ Xx ) ) )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ~ ( Xphi @ Xx ) ) ) ) )).

thf(quantDeMorgan4_type,type,(
    quantDeMorgan4: $o )).

thf(quantDeMorgan4,definition,
    ( quantDeMorgan4
    = ( ! [A: $i,Xphi: $i > $o] :
          ( ? [Xx: $i] :
              ( ( in @ Xx @ A )
              & ~ ( Xphi @ Xx ) )
         => ~ ( ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ( Xphi @ Xx ) ) ) ) ) )).

thf(prop2setI_type,type,(
    prop2setI: $o )).

thf(prop2setI,definition,
    ( prop2setI
    = ( ! [Xphi: $o] :
          ( Xphi
         => ( in @ emptyset @ ( prop2set @ Xphi ) ) ) ) )).

thf(set2prop_type,type,(
    set2prop: $i > $o )).

thf(prop2set2propI_type,type,(
    prop2set2propI: $o )).

thf(prop2set2propI,definition,
    ( prop2set2propI
    = ( ! [Xphi: $o] :
          ( Xphi
         => ( set2prop @ ( prop2set @ Xphi ) ) ) ) )).

thf(notdexE_type,type,(
    notdexE: $o )).

thf(notdexE,definition,
    ( notdexE
    = ( ! [A: $i,Xphi: $i > $o] :
          ( ~ ( ? [Xx: $i] :
                  ( ( in @ Xx @ A )
                  & ( Xphi @ Xx ) ) )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ~ ( Xphi @ Xx ) ) ) ) )).

thf(notdallE_type,type,(
    notdallE: $o )).

thf(notdallE,definition,
    ( notdallE
    = ( ! [A: $i,Xphi: $i > $o] :
          ( ~ ( ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ( Xphi @ Xx ) ) )
         => ? [Xx: $i] :
              ( ( in @ Xx @ A )
              & ~ ( Xphi @ Xx ) ) ) ) )).

thf(exuI1_type,type,(
    exuI1: $o )).

thf(exuI1,definition,
    ( exuI1
    = ( ! [Xphi: $i > $o] :
          ( ? [Xx: $i] :
              ( ( Xphi @ Xx )
              & ! [Xy: $i] :
                  ( ( Xphi @ Xy )
                 => ( Xx = Xy ) ) )
         => ( exu
            @ ^ [Xx: $i] :
                ( Xphi @ Xx ) ) ) ) )).

thf(exuI3_type,type,(
    exuI3: $o )).

thf(exuI3,definition,
    ( exuI3
    = ( ! [Xphi: $i > $o] :
          ( ? [Xx: $i] :
              ( Xphi @ Xx )
         => ( ! [Xx: $i,Xy: $i] :
                ( ( Xphi @ Xx )
               => ( ( Xphi @ Xy )
                 => ( Xx = Xy ) ) )
           => ( exu
              @ ^ [Xx: $i] :
                  ( Xphi @ Xx ) ) ) ) ) )).

thf(exuI2_type,type,(
    exuI2: $o )).

thf(exuI2,definition,
    ( exuI2
    = ( ! [Xphi: $i > $o] :
          ( ? [Xx: $i] :
            ! [Xy: $i] :
              ( ( Xphi @ Xy )
            <=> ( Xy = Xx ) )
         => ( exu
            @ ^ [Xx: $i] :
                ( Xphi @ Xx ) ) ) ) )).

thf(inCongP_type,type,(
    inCongP: $o )).

thf(inCongP,definition,
    ( inCongP
    = ( ! [A: $i,B: $i] :
          ( ( A = B )
         => ! [Xx: $i,Xy: $i] :
              ( ( Xx = Xy )
             => ( ( in @ Xx @ A )
               => ( in @ Xy @ B ) ) ) ) ) )).

thf(in__Cong_type,type,(
    in__Cong: $o )).

thf(in__Cong,definition,
    ( in__Cong
    = ( ! [A: $i,B: $i] :
          ( ( A = B )
         => ! [Xx: $i,Xy: $i] :
              ( ( Xx = Xy )
             => ( ( in @ Xx @ A )
              <=> ( in @ Xy @ B ) ) ) ) ) )).

thf(exuE3u_type,type,(
    exuE3u: $o )).

thf(exuE3u,definition,
    ( exuE3u
    = ( ! [Xphi: $i > $o] :
          ( ( exu
            @ ^ [Xx: $i] :
                ( Xphi @ Xx ) )
         => ! [Xx: $i,Xy: $i] :
              ( ( Xphi @ Xx )
             => ( ( Xphi @ Xy )
               => ( Xx = Xy ) ) ) ) ) )).

thf(exu__Cong_type,type,(
    exu__Cong: $o )).

thf(exu__Cong,definition,
    ( exu__Cong
    = ( ! [Xphi: $i > $o,Xpsi: $i > $o] :
          ( ! [Xx: $i,Xy: $i] :
              ( ( Xx = Xy )
             => ( ( Xphi @ Xx )
              <=> ( Xpsi @ Xy ) ) )
         => ( ( exu
              @ ^ [Xx: $i] :
                  ( Xphi @ Xx ) )
          <=> ( exu
              @ ^ [Xx: $i] :
                  ( Xpsi @ Xx ) ) ) ) ) )).

thf(emptyset__Cong_type,type,(
    emptyset__Cong: $o )).

thf(emptyset__Cong,definition,
    ( emptyset__Cong
    = ( emptyset = emptyset ) )).

thf(setadjoin__Cong_type,type,(
    setadjoin__Cong: $o )).

thf(setadjoin__Cong,definition,
    ( setadjoin__Cong
    = ( ! [Xx: $i,Xy: $i] :
          ( ( Xx = Xy )
         => ! [Xz: $i,Xu: $i] :
              ( ( Xz = Xu )
             => ( ( setadjoin @ Xx @ Xz )
                = ( setadjoin @ Xy @ Xu ) ) ) ) ) )).

thf(powerset__Cong_type,type,(
    powerset__Cong: $o )).

thf(powerset__Cong,definition,
    ( powerset__Cong
    = ( ! [A: $i,B: $i] :
          ( ( A = B )
         => ( ( powerset @ A )
            = ( powerset @ B ) ) ) ) )).

thf(setunion__Cong_type,type,(
    setunion__Cong: $o )).

thf(setunion__Cong,definition,
    ( setunion__Cong
    = ( ! [A: $i,B: $i] :
          ( ( A = B )
         => ( ( setunion @ A )
            = ( setunion @ B ) ) ) ) )).

thf(omega__Cong_type,type,(
    omega__Cong: $o )).

thf(omega__Cong,definition,
    ( omega__Cong
    = ( omega = omega ) )).

thf(exuEu_type,type,(
    exuEu: $o )).

thf(exuEu,definition,
    ( exuEu
    = ( ! [Xphi: $i > $o] :
          ( ( exu
            @ ^ [Xx: $i] :
                ( Xphi @ Xx ) )
         => ! [Xx: $i,Xy: $i] :
              ( ( Xphi @ Xx )
             => ( ( Xphi @ Xy )
               => ( Xx = Xy ) ) ) ) ) )).

thf(descr__Cong_type,type,(
    descr__Cong: $o )).

thf(descr__Cong,definition,
    ( descr__Cong
    = ( ! [Xphi: $i > $o,Xpsi: $i > $o] :
          ( ! [Xx: $i,Xy: $i] :
              ( ( Xx = Xy )
             => ( ( Xphi @ Xx )
              <=> ( Xpsi @ Xy ) ) )
         => ( ( exu
              @ ^ [Xx: $i] :
                  ( Xphi @ Xx ) )
           => ( ( exu
                @ ^ [Xx: $i] :
                    ( Xpsi @ Xx ) )
             => ( ( descr
                  @ ^ [Xx: $i] :
                      ( Xphi @ Xx ) )
                = ( descr
                  @ ^ [Xx: $i] :
                      ( Xpsi @ Xx ) ) ) ) ) ) ) )).

thf(dsetconstr__Cong_type,type,(
    dsetconstr__Cong: $o )).

thf(dsetconstr__Cong,definition,
    ( dsetconstr__Cong
    = ( ! [A: $i,B: $i] :
          ( ( A = B )
         => ! [Xphi: $i > $o,Xpsi: $i > $o] :
              ( ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ! [Xy: $i] :
                      ( ( in @ Xy @ B )
                     => ( ( Xx = Xy )
                       => ( ( Xphi @ Xx )
                        <=> ( Xpsi @ Xy ) ) ) ) )
             => ( ( dsetconstr @ A
                  @ ^ [Xx: $i] :
                      ( Xphi @ Xx ) )
                = ( dsetconstr @ B
                  @ ^ [Xx: $i] :
                      ( Xpsi @ Xx ) ) ) ) ) ) )).

thf(subset_type,type,(
    subset: $i > $i > $o )).

thf(disjoint_type,type,(
    disjoint: $i > $i > $o )).

thf(setsmeet_type,type,(
    setsmeet: $i > $i > $o )).

thf(subsetI1_type,type,(
    subsetI1: $o )).

thf(subsetI1,definition,
    ( subsetI1
    = ( ! [A: $i,B: $i] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( in @ Xx @ B ) )
         => ( subset @ A @ B ) ) ) )).

thf(eqimpsubset2_type,type,(
    eqimpsubset2: $o )).

thf(eqimpsubset2,definition,
    ( eqimpsubset2
    = ( ! [A: $i,B: $i] :
          ( ( A = B )
         => ( subset @ B @ A ) ) ) )).

thf(eqimpsubset1_type,type,(
    eqimpsubset1: $o )).

thf(eqimpsubset1,definition,
    ( eqimpsubset1
    = ( ! [A: $i,B: $i] :
          ( ( A = B )
         => ( subset @ A @ B ) ) ) )).

thf(subsetI2_type,type,(
    subsetI2: $o )).

thf(subsetI2,definition,
    ( subsetI2
    = ( ! [A: $i,B: $i] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( in @ Xx @ B ) )
         => ( subset @ A @ B ) ) ) )).

thf(emptysetsubset_type,type,(
    emptysetsubset: $o )).

thf(emptysetsubset,definition,
    ( emptysetsubset
    = ( ! [A: $i] :
          ( subset @ emptyset @ A ) ) )).

thf(subsetE_type,type,(
    subsetE: $o )).

thf(subsetE,definition,
    ( subsetE
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( subset @ A @ B )
         => ( ( in @ Xx @ A )
           => ( in @ Xx @ B ) ) ) ) )).

thf(subsetE2_type,type,(
    subsetE2: $o )).

thf(subsetE2,definition,
    ( subsetE2
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( subset @ A @ B )
         => ( ~ ( in @ Xx @ B )
           => ~ ( in @ Xx @ A ) ) ) ) )).

thf(notsubsetI_type,type,(
    notsubsetI: $o )).

thf(notsubsetI,definition,
    ( notsubsetI
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( ~ ( in @ Xx @ B )
           => ~ ( subset @ A @ B ) ) ) ) )).

thf(notequalI1_type,type,(
    notequalI1: $o )).

thf(notequalI1,definition,
    ( notequalI1
    = ( ! [A: $i,B: $i] :
          ( ~ ( subset @ A @ B )
         => ( A != B ) ) ) )).

thf(notequalI2_type,type,(
    notequalI2: $o )).

thf(notequalI2,definition,
    ( notequalI2
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( ~ ( in @ Xx @ B )
           => ( A != B ) ) ) ) )).

thf(subsetRefl_type,type,(
    subsetRefl: $o )).

thf(subsetRefl,definition,
    ( subsetRefl
    = ( ! [A: $i] :
          ( subset @ A @ A ) ) )).

thf(subsetTrans_type,type,(
    subsetTrans: $o )).

thf(subsetTrans,definition,
    ( subsetTrans
    = ( ! [A: $i,B: $i,C: $i] :
          ( ( subset @ A @ B )
         => ( ( subset @ B @ C )
           => ( subset @ A @ C ) ) ) ) )).

thf(setadjoinSub_type,type,(
    setadjoinSub: $o )).

thf(setadjoinSub,definition,
    ( setadjoinSub
    = ( ! [Xx: $i,A: $i] :
          ( subset @ A @ ( setadjoin @ Xx @ A ) ) ) )).

thf(setadjoinSub2_type,type,(
    setadjoinSub2: $o )).

thf(setadjoinSub2,definition,
    ( setadjoinSub2
    = ( ! [A: $i,Xx: $i,B: $i] :
          ( ( subset @ A @ B )
         => ( subset @ A @ ( setadjoin @ Xx @ B ) ) ) ) )).

thf(subset2powerset_type,type,(
    subset2powerset: $o )).

thf(subset2powerset,definition,
    ( subset2powerset
    = ( ! [A: $i,B: $i] :
          ( ( subset @ A @ B )
         => ( in @ A @ ( powerset @ B ) ) ) ) )).

thf(setextsub_type,type,(
    setextsub: $o )).

thf(setextsub,definition,
    ( setextsub
    = ( ! [A: $i,B: $i] :
          ( ( subset @ A @ B )
         => ( ( subset @ B @ A )
           => ( A = B ) ) ) ) )).

thf(subsetemptysetimpeq_type,type,(
    subsetemptysetimpeq: $o )).

thf(subsetemptysetimpeq,definition,
    ( subsetemptysetimpeq
    = ( ! [A: $i] :
          ( ( subset @ A @ emptyset )
         => ( A = emptyset ) ) ) )).

thf(powersetI1_type,type,(
    powersetI1: $o )).

thf(powersetI1,definition,
    ( powersetI1
    = ( ! [A: $i,B: $i] :
          ( ( subset @ B @ A )
         => ( in @ B @ ( powerset @ A ) ) ) ) )).

thf(powersetE1_type,type,(
    powersetE1: $o )).

thf(powersetE1,definition,
    ( powersetE1
    = ( ! [A: $i,B: $i] :
          ( ( in @ B @ ( powerset @ A ) )
         => ( subset @ B @ A ) ) ) )).

thf(inPowerset_type,type,(
    inPowerset: $o )).

thf(inPowerset,definition,
    ( inPowerset
    = ( ! [A: $i] :
          ( in @ A @ ( powerset @ A ) ) ) )).

thf(powersetsubset_type,type,(
    powersetsubset: $o )).

thf(powersetsubset,definition,
    ( powersetsubset
    = ( ! [A: $i,B: $i] :
          ( ( subset @ A @ B )
         => ( subset @ ( powerset @ A ) @ ( powerset @ B ) ) ) ) )).

thf(sepInPowerset_type,type,(
    sepInPowerset: $o )).

thf(sepInPowerset,definition,
    ( sepInPowerset
    = ( ! [A: $i,Xphi: $i > $o] :
          ( in
          @ ( dsetconstr @ A
            @ ^ [Xx: $i] :
                ( Xphi @ Xx ) )
          @ ( powerset @ A ) ) ) )).

thf(sepSubset_type,type,(
    sepSubset: $o )).

thf(sepSubset,definition,
    ( sepSubset
    = ( ! [A: $i,Xphi: $i > $o] :
          ( subset
          @ ( dsetconstr @ A
            @ ^ [Xx: $i] :
                ( Xphi @ Xx ) )
          @ A ) ) )).

thf(binunion_type,type,(
    binunion: $i > $i > $i )).

thf(binunionIL_type,type,(
    binunionIL: $o )).

thf(binunionIL,definition,
    ( binunionIL
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( in @ Xx @ ( binunion @ A @ B ) ) ) ) )).

thf(upairset2IR_type,type,(
    upairset2IR: $o )).

thf(upairset2IR,definition,
    ( upairset2IR
    = ( ! [Xx: $i,Xy: $i] :
          ( in @ Xy @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) ) ) )).

thf(binunionIR_type,type,(
    binunionIR: $o )).

thf(binunionIR,definition,
    ( binunionIR
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ B )
         => ( in @ Xx @ ( binunion @ A @ B ) ) ) ) )).

thf(binunionEcases_type,type,(
    binunionEcases: $o )).

thf(binunionEcases,definition,
    ( binunionEcases
    = ( ! [A: $i,B: $i,Xx: $i,Xphi: $o] :
          ( ( in @ Xx @ ( binunion @ A @ B ) )
         => ( ( ( in @ Xx @ A )
             => Xphi )
           => ( ( ( in @ Xx @ B )
               => Xphi )
             => Xphi ) ) ) ) )).

thf(binunionE_type,type,(
    binunionE: $o )).

thf(binunionE,definition,
    ( binunionE
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ ( binunion @ A @ B ) )
         => ( ( in @ Xx @ A )
            | ( in @ Xx @ B ) ) ) ) )).

thf(binunionLsub_type,type,(
    binunionLsub: $o )).

thf(binunionLsub,definition,
    ( binunionLsub
    = ( ! [A: $i,B: $i] :
          ( subset @ A @ ( binunion @ A @ B ) ) ) )).

thf(binunionRsub_type,type,(
    binunionRsub: $o )).

thf(binunionRsub,definition,
    ( binunionRsub
    = ( ! [A: $i,B: $i] :
          ( subset @ B @ ( binunion @ A @ B ) ) ) )).

thf(binintersect_type,type,(
    binintersect: $i > $i > $i )).

thf(binintersectI_type,type,(
    binintersectI: $o )).

thf(binintersectI,definition,
    ( binintersectI
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( ( in @ Xx @ B )
           => ( in @ Xx @ ( binintersect @ A @ B ) ) ) ) ) )).

thf(binintersectSubset5_type,type,(
    binintersectSubset5: $o )).

thf(binintersectSubset5,definition,
    ( binintersectSubset5
    = ( ! [A: $i,B: $i,C: $i] :
          ( ( subset @ C @ A )
         => ( ( subset @ C @ B )
           => ( subset @ C @ ( binintersect @ A @ B ) ) ) ) ) )).

thf(binintersectEL_type,type,(
    binintersectEL: $o )).

thf(binintersectEL,definition,
    ( binintersectEL
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ ( binintersect @ A @ B ) )
         => ( in @ Xx @ A ) ) ) )).

thf(binintersectLsub_type,type,(
    binintersectLsub: $o )).

thf(binintersectLsub,definition,
    ( binintersectLsub
    = ( ! [A: $i,B: $i] :
          ( subset @ ( binintersect @ A @ B ) @ A ) ) )).

thf(binintersectSubset2_type,type,(
    binintersectSubset2: $o )).

thf(binintersectSubset2,definition,
    ( binintersectSubset2
    = ( ! [A: $i,B: $i] :
          ( ( subset @ A @ B )
         => ( ( binintersect @ A @ B )
            = A ) ) ) )).

thf(binintersectSubset3_type,type,(
    binintersectSubset3: $o )).

thf(binintersectSubset3,definition,
    ( binintersectSubset3
    = ( ! [A: $i,B: $i] :
          ( ( ( binintersect @ A @ B )
            = B )
         => ( subset @ B @ A ) ) ) )).

thf(binintersectER_type,type,(
    binintersectER: $o )).

thf(binintersectER,definition,
    ( binintersectER
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ ( binintersect @ A @ B ) )
         => ( in @ Xx @ B ) ) ) )).

thf(disjointsetsI1_type,type,(
    disjointsetsI1: $o )).

thf(disjointsetsI1,definition,
    ( disjointsetsI1
    = ( ! [A: $i,B: $i] :
          ( ~ ( ? [Xx: $i] :
                  ( ( in @ Xx @ A )
                  & ( in @ Xx @ B ) ) )
         => ( ( binintersect @ A @ B )
            = emptyset ) ) ) )).

thf(binintersectRsub_type,type,(
    binintersectRsub: $o )).

thf(binintersectRsub,definition,
    ( binintersectRsub
    = ( ! [A: $i,B: $i] :
          ( subset @ ( binintersect @ A @ B ) @ B ) ) )).

thf(binintersectSubset4_type,type,(
    binintersectSubset4: $o )).

thf(binintersectSubset4,definition,
    ( binintersectSubset4
    = ( ! [A: $i,B: $i] :
          ( ( subset @ B @ A )
         => ( ( binintersect @ A @ B )
            = B ) ) ) )).

thf(binintersectSubset1_type,type,(
    binintersectSubset1: $o )).

thf(binintersectSubset1,definition,
    ( binintersectSubset1
    = ( ! [A: $i,B: $i] :
          ( ( ( binintersect @ A @ B )
            = A )
         => ( subset @ A @ B ) ) ) )).

thf(bs114d_type,type,(
    bs114d: $o )).

thf(bs114d,definition,
    ( bs114d
    = ( ! [A: $i,B: $i,C: $i] :
          ( ( binintersect @ A @ ( binunion @ B @ C ) )
          = ( binunion @ ( binintersect @ A @ B ) @ ( binintersect @ A @ C ) ) ) ) )).

thf(regular_type,type,(
    regular: $i > $o )).

thf(setminus_type,type,(
    setminus: $i > $i > $i )).

thf(setminusI_type,type,(
    setminusI: $o )).

thf(setminusI,definition,
    ( setminusI
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( ~ ( in @ Xx @ B )
           => ( in @ Xx @ ( setminus @ A @ B ) ) ) ) ) )).

thf(setminusEL_type,type,(
    setminusEL: $o )).

thf(setminusEL,definition,
    ( setminusEL
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ ( setminus @ A @ B ) )
         => ( in @ Xx @ A ) ) ) )).

thf(setminusER_type,type,(
    setminusER: $o )).

thf(setminusER,definition,
    ( setminusER
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ ( setminus @ A @ B ) )
         => ~ ( in @ Xx @ B ) ) ) )).

thf(setminusSubset2_type,type,(
    setminusSubset2: $o )).

thf(setminusSubset2,definition,
    ( setminusSubset2
    = ( ! [A: $i,B: $i] :
          ( ( subset @ A @ B )
         => ( ( setminus @ A @ B )
            = emptyset ) ) ) )).

thf(setminusERneg_type,type,(
    setminusERneg: $o )).

thf(setminusERneg,definition,
    ( setminusERneg
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ~ ( in @ Xx @ ( setminus @ A @ B ) )
         => ( ( in @ Xx @ A )
           => ( in @ Xx @ B ) ) ) ) )).

thf(setminusELneg_type,type,(
    setminusELneg: $o )).

thf(setminusELneg,definition,
    ( setminusELneg
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ~ ( in @ Xx @ ( setminus @ A @ B ) )
         => ( ~ ( in @ Xx @ B )
           => ~ ( in @ Xx @ A ) ) ) ) )).

thf(setminusILneg_type,type,(
    setminusILneg: $o )).

thf(setminusILneg,definition,
    ( setminusILneg
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ~ ( in @ Xx @ A )
         => ~ ( in @ Xx @ ( setminus @ A @ B ) ) ) ) )).

thf(setminusIRneg_type,type,(
    setminusIRneg: $o )).

thf(setminusIRneg,definition,
    ( setminusIRneg
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ B )
         => ~ ( in @ Xx @ ( setminus @ A @ B ) ) ) ) )).

thf(setminusLsub_type,type,(
    setminusLsub: $o )).

thf(setminusLsub,definition,
    ( setminusLsub
    = ( ! [A: $i,B: $i] :
          ( subset @ ( setminus @ A @ B ) @ A ) ) )).

thf(setminusSubset1_type,type,(
    setminusSubset1: $o )).

thf(setminusSubset1,definition,
    ( setminusSubset1
    = ( ! [A: $i,B: $i] :
          ( ( ( setminus @ A @ B )
            = emptyset )
         => ( subset @ A @ B ) ) ) )).

thf(symdiff_type,type,(
    symdiff: $i > $i > $i )).

thf(symdiffE_type,type,(
    symdiffE: $o )).

thf(symdiffE,definition,
    ( symdiffE
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ ( symdiff @ A @ B ) )
         => ! [Xphi: $o] :
              ( ( ( in @ Xx @ A )
               => ( ~ ( in @ Xx @ B )
                 => Xphi ) )
             => ( ( ~ ( in @ Xx @ A )
                 => ( ( in @ Xx @ B )
                   => Xphi ) )
               => Xphi ) ) ) ) )).

thf(symdiffI1_type,type,(
    symdiffI1: $o )).

thf(symdiffI1,definition,
    ( symdiffI1
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( ~ ( in @ Xx @ B )
           => ( in @ Xx @ ( symdiff @ A @ B ) ) ) ) ) )).

thf(symdiffI2_type,type,(
    symdiffI2: $o )).

thf(symdiffI2,definition,
    ( symdiffI2
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ~ ( in @ Xx @ A )
         => ( ( in @ Xx @ B )
           => ( in @ Xx @ ( symdiff @ A @ B ) ) ) ) ) )).

thf(symdiffIneg1_type,type,(
    symdiffIneg1: $o )).

thf(symdiffIneg1,definition,
    ( symdiffIneg1
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( ( in @ Xx @ B )
           => ~ ( in @ Xx @ ( symdiff @ A @ B ) ) ) ) ) )).

thf(symdiffIneg2_type,type,(
    symdiffIneg2: $o )).

thf(symdiffIneg2,definition,
    ( symdiffIneg2
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ~ ( in @ Xx @ A )
         => ( ~ ( in @ Xx @ B )
           => ~ ( in @ Xx @ ( symdiff @ A @ B ) ) ) ) ) )).

thf(iskpair_type,type,(
    iskpair: $i > $o )).

thf(secondinupair_type,type,(
    secondinupair: $o )).

thf(secondinupair,definition,
    ( secondinupair
    = ( ! [Xx: $i,Xy: $i] :
          ( in @ Xy @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) ) ) )).

thf(setukpairIL_type,type,(
    setukpairIL: $o )).

thf(setukpairIL,definition,
    ( setukpairIL
    = ( ! [Xx: $i,Xy: $i] :
          ( in @ Xx @ ( setunion @ ( setadjoin @ ( setadjoin @ Xx @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) @ emptyset ) ) ) ) ) )).

thf(setukpairIR_type,type,(
    setukpairIR: $o )).

thf(setukpairIR,definition,
    ( setukpairIR
    = ( ! [Xx: $i,Xy: $i] :
          ( in @ Xy @ ( setunion @ ( setadjoin @ ( setadjoin @ Xx @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) @ emptyset ) ) ) ) ) )).

thf(kpairiskpair_type,type,(
    kpairiskpair: $o )).

thf(kpairiskpair,definition,
    ( kpairiskpair
    = ( ! [Xx: $i,Xy: $i] :
          ( iskpair @ ( setadjoin @ ( setadjoin @ Xx @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) @ emptyset ) ) ) ) )).

thf(kpair_type,type,(
    kpair: $i > $i > $i )).

thf(kpairp_type,type,(
    kpairp: $o )).

thf(kpairp,definition,
    ( kpairp
    = ( ! [Xx: $i,Xy: $i] :
          ( iskpair @ ( kpair @ Xx @ Xy ) ) ) )).

thf(cartprod_type,type,(
    cartprod: $i > $i > $i )).

thf(singletonsubset_type,type,(
    singletonsubset: $o )).

thf(singletonsubset,definition,
    ( singletonsubset
    = ( ! [A: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( subset @ ( setadjoin @ Xx @ emptyset ) @ A ) ) ) )).

thf(singletoninpowerset_type,type,(
    singletoninpowerset: $o )).

thf(singletoninpowerset,definition,
    ( singletoninpowerset
    = ( ! [A: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( in @ ( setadjoin @ Xx @ emptyset ) @ ( powerset @ A ) ) ) ) )).

thf(singletoninpowunion_type,type,(
    singletoninpowunion: $o )).

thf(singletoninpowunion,definition,
    ( singletoninpowunion
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( in @ ( setadjoin @ Xx @ emptyset ) @ ( powerset @ ( binunion @ A @ B ) ) ) ) ) )).

thf(upairset2E_type,type,(
    upairset2E: $o )).

thf(upairset2E,definition,
    ( upairset2E
    = ( ! [Xx: $i,Xy: $i,Xz: $i] :
          ( ( in @ Xz @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) )
         => ( ( Xz = Xx )
            | ( Xz = Xy ) ) ) ) )).

thf(upairsubunion_type,type,(
    upairsubunion: $o )).

thf(upairsubunion,definition,
    ( upairsubunion
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ B )
             => ( subset @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) @ ( binunion @ A @ B ) ) ) ) ) )).

thf(upairinpowunion_type,type,(
    upairinpowunion: $o )).

thf(upairinpowunion,definition,
    ( upairinpowunion
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ B )
             => ( in @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) @ ( powerset @ ( binunion @ A @ B ) ) ) ) ) ) )).

thf(ubforcartprodlem1_type,type,(
    ubforcartprodlem1: $o )).

thf(ubforcartprodlem1,definition,
    ( ubforcartprodlem1
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ B )
             => ( subset @ ( setadjoin @ ( setadjoin @ Xx @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) @ emptyset ) ) @ ( powerset @ ( binunion @ A @ B ) ) ) ) ) ) )).

thf(ubforcartprodlem2_type,type,(
    ubforcartprodlem2: $o )).

thf(ubforcartprodlem2,definition,
    ( ubforcartprodlem2
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ B )
             => ( in @ ( setadjoin @ ( setadjoin @ Xx @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) @ emptyset ) ) @ ( powerset @ ( powerset @ ( binunion @ A @ B ) ) ) ) ) ) ) )).

thf(ubforcartprodlem3_type,type,(
    ubforcartprodlem3: $o )).

thf(ubforcartprodlem3,definition,
    ( ubforcartprodlem3
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ B )
             => ( in @ ( kpair @ Xx @ Xy ) @ ( powerset @ ( powerset @ ( binunion @ A @ B ) ) ) ) ) ) ) )).

thf(cartprodpairin_type,type,(
    cartprodpairin: $o )).

thf(cartprodpairin,definition,
    ( cartprodpairin
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ B )
             => ( in @ ( kpair @ Xx @ Xy ) @ ( cartprod @ A @ B ) ) ) ) ) )).

thf(cartprodmempair1_type,type,(
    cartprodmempair1: $o )).

thf(cartprodmempair1,definition,
    ( cartprodmempair1
    = ( ! [A: $i,B: $i,Xu: $i] :
          ( ( in @ Xu @ ( cartprod @ A @ B ) )
         => ? [Xx: $i] :
              ( ( in @ Xx @ A )
              & ? [Xy: $i] :
                  ( ( in @ Xy @ B )
                  & ( Xu
                    = ( kpair @ Xx @ Xy ) ) ) ) ) ) )).

thf(cartprodmempair_type,type,(
    cartprodmempair: $o )).

thf(cartprodmempair,definition,
    ( cartprodmempair
    = ( ! [A: $i,B: $i,Xu: $i] :
          ( ( in @ Xu @ ( cartprod @ A @ B ) )
         => ( iskpair @ Xu ) ) ) )).

thf(setunionE2_type,type,(
    setunionE2: $o )).

thf(setunionE2,definition,
    ( setunionE2
    = ( ! [A: $i,Xx: $i] :
          ( ( in @ Xx @ ( setunion @ A ) )
         => ? [X: $i] :
              ( ( in @ X @ A )
              & ( in @ Xx @ X ) ) ) ) )).

thf(setunionsingleton1_type,type,(
    setunionsingleton1: $o )).

thf(setunionsingleton1,definition,
    ( setunionsingleton1
    = ( ! [A: $i] :
          ( subset @ ( setunion @ ( setadjoin @ A @ emptyset ) ) @ A ) ) )).

thf(setunionsingleton2_type,type,(
    setunionsingleton2: $o )).

thf(setunionsingleton2,definition,
    ( setunionsingleton2
    = ( ! [A: $i] :
          ( subset @ A @ ( setunion @ ( setadjoin @ A @ emptyset ) ) ) ) )).

thf(setunionsingleton_type,type,(
    setunionsingleton: $o )).

thf(setunionsingleton,definition,
    ( setunionsingleton
    = ( ! [Xx: $i] :
          ( ( setunion @ ( setadjoin @ Xx @ emptyset ) )
          = Xx ) ) )).

thf(singleton_type,type,(
    singleton: $i > $o )).

thf(singletonprop_type,type,(
    singletonprop: $o )).

thf(singletonprop,definition,
    ( singletonprop
    = ( ! [A: $i,Xphi: $i > $o] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ! [Xy: $i] :
                  ( ( in @ Xy @ A )
                 => ( ( Xphi @ Xx )
                   => ( ( Xphi @ Xy )
                     => ( Xx = Xy ) ) ) ) )
         => ( ? [Xx: $i] :
                ( ( in @ Xx @ A )
                & ( Xphi @ Xx ) )
           => ( singleton
              @ ( dsetconstr @ A
                @ ^ [Xx: $i] :
                    ( Xphi @ Xx ) ) ) ) ) ) )).

thf(ex1_type,type,(
    ex1: $i > ( $i > $o ) > $o )).

thf(ex1E1_type,type,(
    ex1E1: $o )).

thf(ex1E1,definition,
    ( ex1E1
    = ( ! [A: $i,Xphi: $i > $o] :
          ( ( ex1 @ A
            @ ^ [Xx: $i] :
                ( Xphi @ Xx ) )
         => ? [Xx: $i] :
              ( ( in @ Xx @ A )
              & ( Xphi @ Xx ) ) ) ) )).

thf(ex1I_type,type,(
    ex1I: $o )).

thf(ex1I,definition,
    ( ex1I
    = ( ! [A: $i,Xphi: $i > $o,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( ( Xphi @ Xx )
           => ( ! [Xy: $i] :
                  ( ( in @ Xy @ A )
                 => ( ( Xphi @ Xy )
                   => ( Xy = Xx ) ) )
             => ( ex1 @ A
                @ ^ [Xy: $i] :
                    ( Xphi @ Xy ) ) ) ) ) ) )).

thf(ex1I2_type,type,(
    ex1I2: $o )).

thf(ex1I2,definition,
    ( ex1I2
    = ( ! [A: $i,Xphi: $i > $o] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ! [Xy: $i] :
                  ( ( in @ Xy @ A )
                 => ( ( Xphi @ Xx )
                   => ( ( Xphi @ Xy )
                     => ( Xx = Xy ) ) ) ) )
         => ( ? [Xx: $i] :
                ( ( in @ Xx @ A )
                & ( Xphi @ Xx ) )
           => ( ex1 @ A
              @ ^ [Xx: $i] :
                  ( Xphi @ Xx ) ) ) ) ) )).

thf(singletonsuniq_type,type,(
    singletonsuniq: $o )).

thf(singletonsuniq,definition,
    ( singletonsuniq
    = ( ! [Xx: $i,Xy: $i] :
          ( ( ( setadjoin @ Xx @ emptyset )
            = ( setadjoin @ Xy @ emptyset ) )
         => ( Xx = Xy ) ) ) )).

thf(atmost1p_type,type,(
    atmost1p: $i > $o )).

thf(atleast2p_type,type,(
    atleast2p: $i > $o )).

thf(atmost2p_type,type,(
    atmost2p: $i > $o )).

thf(upairsetp_type,type,(
    upairsetp: $i > $o )).

thf(setukpairinjL1_type,type,(
    setukpairinjL1: $o )).

thf(setukpairinjL1,definition,
    ( setukpairinjL1
    = ( ! [Xx: $i,Xy: $i,Xz: $i] :
          ( ( in @ ( setadjoin @ Xz @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xx @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) @ emptyset ) ) )
         => ( Xx = Xz ) ) ) )).

thf(kfstsingleton_type,type,(
    kfstsingleton: $o )).

thf(kfstsingleton,definition,
    ( kfstsingleton
    = ( ! [Xu: $i] :
          ( ( iskpair @ Xu )
         => ( singleton
            @ ( dsetconstr @ ( setunion @ Xu )
              @ ^ [Xx: $i] :
                  ( in @ ( setadjoin @ Xx @ emptyset ) @ Xu ) ) ) ) ) )).

thf(theprop_type,type,(
    theprop: $o )).

thf(theprop,definition,
    ( theprop
    = ( ! [X: $i] :
          ( ( singleton @ X )
         => ( in @ ( setunion @ X ) @ X ) ) ) )).

thf(kfst_type,type,(
    kfst: $i > $i )).

thf(kfstpairEq_type,type,(
    kfstpairEq: $o )).

thf(kfstpairEq,definition,
    ( kfstpairEq
    = ( ! [Xx: $i,Xy: $i] :
          ( ( kfst @ ( kpair @ Xx @ Xy ) )
          = Xx ) ) )).

thf(cartprodfstin_type,type,(
    cartprodfstin: $o )).

thf(cartprodfstin,definition,
    ( cartprodfstin
    = ( ! [A: $i,B: $i,Xu: $i] :
          ( ( in @ Xu @ ( cartprod @ A @ B ) )
         => ( in @ ( kfst @ Xu ) @ A ) ) ) )).

thf(setukpairinjL2_type,type,(
    setukpairinjL2: $o )).

thf(setukpairinjL2,definition,
    ( setukpairinjL2
    = ( ! [Xx: $i,Xy: $i,Xz: $i,Xu: $i] :
          ( ( ( setadjoin @ ( setadjoin @ Xx @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) @ emptyset ) )
            = ( setadjoin @ ( setadjoin @ Xz @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xz @ ( setadjoin @ Xu @ emptyset ) ) @ emptyset ) ) )
         => ( Xx = Xz ) ) ) )).

thf(setukpairinjL_type,type,(
    setukpairinjL: $o )).

thf(setukpairinjL,definition,
    ( setukpairinjL
    = ( ! [Xx: $i,Xy: $i,Xz: $i,Xu: $i] :
          ( ( ( kpair @ Xx @ Xy )
            = ( kpair @ Xz @ Xu ) )
         => ( Xx = Xz ) ) ) )).

thf(setukpairinjR11_type,type,(
    setukpairinjR11: $o )).

thf(setukpairinjR11,definition,
    ( setukpairinjR11
    = ( ! [Xx: $i,Xy: $i] :
          ( ( Xx = Xy )
         => ( ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) )
            = ( setadjoin @ Xx @ emptyset ) ) ) ) )).

thf(setukpairinjR12_type,type,(
    setukpairinjR12: $o )).

thf(setukpairinjR12,definition,
    ( setukpairinjR12
    = ( ! [Xx: $i,Xy: $i] :
          ( ( Xx = Xy )
         => ( ( setadjoin @ ( setadjoin @ Xx @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) @ emptyset ) )
            = ( setadjoin @ ( setadjoin @ Xx @ emptyset ) @ emptyset ) ) ) ) )).

thf(setukpairinjR1_type,type,(
    setukpairinjR1: $o )).

thf(setukpairinjR1,definition,
    ( setukpairinjR1
    = ( ! [Xx: $i,Xy: $i,Xz: $i,Xu: $i] :
          ( ( ( setadjoin @ ( setadjoin @ Xx @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) @ emptyset ) )
            = ( setadjoin @ ( setadjoin @ Xz @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xz @ ( setadjoin @ Xu @ emptyset ) ) @ emptyset ) ) )
         => ( ( Xz = Xu )
           => ( Xy = Xu ) ) ) ) )).

thf(upairequniteq_type,type,(
    upairequniteq: $o )).

thf(upairequniteq,definition,
    ( upairequniteq
    = ( ! [Xx: $i,Xy: $i,Xz: $i] :
          ( ( ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) )
            = ( setadjoin @ Xz @ emptyset ) )
         => ( Xx = Xy ) ) ) )).

thf(setukpairinjR2_type,type,(
    setukpairinjR2: $o )).

thf(setukpairinjR2,definition,
    ( setukpairinjR2
    = ( ! [Xx: $i,Xy: $i,Xz: $i,Xu: $i] :
          ( ( ( setadjoin @ ( setadjoin @ Xx @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) @ emptyset ) )
            = ( setadjoin @ ( setadjoin @ Xz @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xz @ ( setadjoin @ Xu @ emptyset ) ) @ emptyset ) ) )
         => ( Xy = Xu ) ) ) )).

thf(setukpairinjR_type,type,(
    setukpairinjR: $o )).

thf(setukpairinjR,definition,
    ( setukpairinjR
    = ( ! [Xx: $i,Xy: $i,Xz: $i,Xu: $i] :
          ( ( ( kpair @ Xx @ Xy )
            = ( kpair @ Xz @ Xu ) )
         => ( Xy = Xu ) ) ) )).

thf(ksndsingleton_type,type,(
    ksndsingleton: $o )).

thf(ksndsingleton,definition,
    ( ksndsingleton
    = ( ! [Xu: $i] :
          ( ( iskpair @ Xu )
         => ( singleton
            @ ( dsetconstr @ ( setunion @ Xu )
              @ ^ [Xx: $i] :
                  ( Xu
                  = ( kpair @ ( kfst @ Xu ) @ Xx ) ) ) ) ) ) )).

thf(ksnd_type,type,(
    ksnd: $i > $i )).

thf(ksndpairEq_type,type,(
    ksndpairEq: $o )).

thf(ksndpairEq,definition,
    ( ksndpairEq
    = ( ! [Xx: $i,Xy: $i] :
          ( ( ksnd @ ( kpair @ Xx @ Xy ) )
          = Xy ) ) )).

thf(kpairsurjEq_type,type,(
    kpairsurjEq: $o )).

thf(kpairsurjEq,definition,
    ( kpairsurjEq
    = ( ! [Xu: $i] :
          ( ( iskpair @ Xu )
         => ( ( kpair @ ( kfst @ Xu ) @ ( ksnd @ Xu ) )
            = Xu ) ) ) )).

thf(cartprodsndin_type,type,(
    cartprodsndin: $o )).

thf(cartprodsndin,definition,
    ( cartprodsndin
    = ( ! [A: $i,B: $i,Xu: $i] :
          ( ( in @ Xu @ ( cartprod @ A @ B ) )
         => ( in @ ( ksnd @ Xu ) @ B ) ) ) )).

thf(cartprodpairmemEL_type,type,(
    cartprodpairmemEL: $o )).

thf(cartprodpairmemEL,definition,
    ( cartprodpairmemEL
    = ( ! [A: $i,B: $i,Xx: $i,Xy: $i] :
          ( ( in @ ( kpair @ Xx @ Xy ) @ ( cartprod @ A @ B ) )
         => ( in @ Xx @ A ) ) ) )).

thf(cartprodpairmemER_type,type,(
    cartprodpairmemER: $o )).

thf(cartprodpairmemER,definition,
    ( cartprodpairmemER
    = ( ! [A: $i,B: $i,Xx: $i,Xy: $i] :
          ( ( in @ ( kpair @ Xx @ Xy ) @ ( cartprod @ A @ B ) )
         => ( in @ Xy @ B ) ) ) )).

thf(cartprodmempaircEq_type,type,(
    cartprodmempaircEq: $o )).

thf(cartprodmempaircEq,definition,
    ( cartprodmempaircEq
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ B )
             => ( ( kpair @ Xx @ Xy )
                = ( kpair @ Xx @ Xy ) ) ) ) ) )).

thf(cartprodfstpairEq_type,type,(
    cartprodfstpairEq: $o )).

thf(cartprodfstpairEq,definition,
    ( cartprodfstpairEq
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ B )
             => ( ( kfst @ ( kpair @ Xx @ Xy ) )
                = Xx ) ) ) ) )).

thf(cartprodsndpairEq_type,type,(
    cartprodsndpairEq: $o )).

thf(cartprodsndpairEq,definition,
    ( cartprodsndpairEq
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ B )
             => ( ( ksnd @ ( kpair @ Xx @ Xy ) )
                = Xy ) ) ) ) )).

thf(cartprodpairsurjEq_type,type,(
    cartprodpairsurjEq: $o )).

thf(cartprodpairsurjEq,definition,
    ( cartprodpairsurjEq
    = ( ! [A: $i,B: $i,Xu: $i] :
          ( ( in @ Xu @ ( cartprod @ A @ B ) )
         => ( ( kpair @ ( kfst @ Xu ) @ ( ksnd @ Xu ) )
            = Xu ) ) ) )).

thf(breln_type,type,(
    breln: $i > $i > $i > $o )).

thf(dpsetconstr_type,type,(
    dpsetconstr: $i > $i > ( $i > $i > $o ) > $i )).

thf(dpsetconstrI_type,type,(
    dpsetconstrI: $o )).

thf(dpsetconstrI,definition,
    ( dpsetconstrI
    = ( ! [A: $i,B: $i,Xphi: $i > $i > $o,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ B )
             => ( ( Xphi @ Xx @ Xy )
               => ( in @ ( kpair @ Xx @ Xy )
                  @ ( dpsetconstr @ A @ B
                    @ ^ [Xz: $i,Xu: $i] :
                        ( Xphi @ Xz @ Xu ) ) ) ) ) ) ) )).

thf(dpsetconstrSub_type,type,(
    dpsetconstrSub: $o )).

thf(dpsetconstrSub,definition,
    ( dpsetconstrSub
    = ( ! [A: $i,B: $i,Xphi: $i > $i > $o] :
          ( subset
          @ ( dpsetconstr @ A @ B
            @ ^ [Xx: $i,Xy: $i] :
                ( Xphi @ Xx @ Xy ) )
          @ ( cartprod @ A @ B ) ) ) )).

thf(setOfPairsIsBReln_type,type,(
    setOfPairsIsBReln: $o )).

thf(setOfPairsIsBReln,definition,
    ( setOfPairsIsBReln
    = ( ! [A: $i,B: $i,Xphi: $i > $i > $o] :
          ( breln @ A @ B
          @ ( dpsetconstr @ A @ B
            @ ^ [Xx: $i,Xy: $i] :
                ( Xphi @ Xx @ Xy ) ) ) ) )).

thf(dpsetconstrERa_type,type,(
    dpsetconstrERa: $o )).

thf(dpsetconstrERa,definition,
    ( dpsetconstrERa
    = ( ! [A: $i,B: $i,Xphi: $i > $i > $o,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ B )
             => ( ( in @ ( kpair @ Xx @ Xy )
                  @ ( dpsetconstr @ A @ B
                    @ ^ [Xz: $i,Xu: $i] :
                        ( Xphi @ Xz @ Xu ) ) )
               => ( Xphi @ Xx @ Xy ) ) ) ) ) )).

thf(dpsetconstrEL1_type,type,(
    dpsetconstrEL1: $o )).

thf(dpsetconstrEL1,definition,
    ( dpsetconstrEL1
    = ( ! [A: $i,B: $i,Xphi: $i > $i > $o,Xx: $i,Xy: $i] :
          ( ( in @ ( kpair @ Xx @ Xy )
            @ ( dpsetconstr @ A @ B
              @ ^ [Xz: $i,Xu: $i] :
                  ( Xphi @ Xz @ Xu ) ) )
         => ( in @ Xx @ A ) ) ) )).

thf(dpsetconstrEL2_type,type,(
    dpsetconstrEL2: $o )).

thf(dpsetconstrEL2,definition,
    ( dpsetconstrEL2
    = ( ! [A: $i,B: $i,Xphi: $i > $i > $o,Xx: $i,Xy: $i] :
          ( ( in @ ( kpair @ Xx @ Xy )
            @ ( dpsetconstr @ A @ B
              @ ^ [Xz: $i,Xu: $i] :
                  ( Xphi @ Xz @ Xu ) ) )
         => ( in @ Xy @ B ) ) ) )).

thf(dpsetconstrER_type,type,(
    dpsetconstrER: $o )).

thf(dpsetconstrER,definition,
    ( dpsetconstrER
    = ( ! [A: $i,B: $i,Xphi: $i > $i > $o,Xx: $i,Xy: $i] :
          ( ( in @ ( kpair @ Xx @ Xy )
            @ ( dpsetconstr @ A @ B
              @ ^ [Xz: $i,Xu: $i] :
                  ( Xphi @ Xz @ Xu ) ) )
         => ( Xphi @ Xx @ Xy ) ) ) )).

thf(func_type,type,(
    func: $i > $i > $i > $o )).

thf(funcSet_type,type,(
    funcSet: $i > $i > $i )).

thf(funcImageSingleton_type,type,(
    funcImageSingleton: $o )).

thf(funcImageSingleton,definition,
    ( funcImageSingleton
    = ( ! [A: $i,B: $i,Xf: $i] :
          ( ( func @ A @ B @ Xf )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( singleton
                @ ( dsetconstr @ B
                  @ ^ [Xy: $i] :
                      ( in @ ( kpair @ Xx @ Xy ) @ Xf ) ) ) ) ) ) )).

thf(apProp_type,type,(
    apProp: $o )).

thf(apProp,definition,
    ( apProp
    = ( ! [A: $i,B: $i,Xf: $i] :
          ( ( func @ A @ B @ Xf )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( in
                @ ( setunion
                  @ ( dsetconstr @ B
                    @ ^ [Xy: $i] :
                        ( in @ ( kpair @ Xx @ Xy ) @ Xf ) ) )
                @ B ) ) ) ) )).

thf(ap_type,type,(
    ap: $i > $i > $i > $i > $i )).

thf(app_type,type,(
    app: $o )).

thf(app,definition,
    ( app
    = ( ! [A: $i,B: $i,Xf: $i] :
          ( ( func @ A @ B @ Xf )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( in @ ( ap @ A @ B @ Xf @ Xx ) @ B ) ) ) ) )).

thf(infuncsetfunc_type,type,(
    infuncsetfunc: $o )).

thf(infuncsetfunc,definition,
    ( infuncsetfunc
    = ( ! [A: $i,B: $i,Xf: $i] :
          ( ( in @ Xf @ ( funcSet @ A @ B ) )
         => ( func @ A @ B @ Xf ) ) ) )).

thf(ap2p_type,type,(
    ap2p: $o )).

thf(ap2p,definition,
    ( ap2p
    = ( ! [A: $i,B: $i,Xf: $i] :
          ( ( in @ Xf @ ( funcSet @ A @ B ) )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( in @ ( ap @ A @ B @ Xf @ Xx ) @ B ) ) ) ) )).

thf(funcinfuncset_type,type,(
    funcinfuncset: $o )).

thf(funcinfuncset,definition,
    ( funcinfuncset
    = ( ! [A: $i,B: $i,Xf: $i] :
          ( ( func @ A @ B @ Xf )
         => ( in @ Xf @ ( funcSet @ A @ B ) ) ) ) )).

thf(lamProp_type,type,(
    lamProp: $o )).

thf(lamProp,definition,
    ( lamProp
    = ( ! [A: $i,B: $i,Xf: $i > $i] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( in @ ( Xf @ Xx ) @ B ) )
         => ( func @ A @ B
            @ ( dpsetconstr @ A @ B
              @ ^ [Xx: $i,Xy: $i] :
                  ( ( Xf @ Xx )
                  = Xy ) ) ) ) ) )).

thf(lam_type,type,(
    lam: $i > $i > ( $i > $i ) > $i )).

thf(lamp_type,type,(
    lamp: $o )).

thf(lamp,definition,
    ( lamp
    = ( ! [A: $i,B: $i,Xf: $i > $i] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( in @ ( Xf @ Xx ) @ B ) )
         => ( func @ A @ B
            @ ( lam @ A @ B
              @ ^ [Xx: $i] :
                  ( Xf @ Xx ) ) ) ) ) )).

thf(lam2p_type,type,(
    lam2p: $o )).

thf(lam2p,definition,
    ( lam2p
    = ( ! [A: $i,B: $i,Xf: $i > $i] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( in @ ( Xf @ Xx ) @ B ) )
         => ( in
            @ ( lam @ A @ B
              @ ^ [Xx: $i] :
                  ( Xf @ Xx ) )
            @ ( funcSet @ A @ B ) ) ) ) )).

thf(brelnall1_type,type,(
    brelnall1: $o )).

thf(brelnall1,definition,
    ( brelnall1
    = ( ! [A: $i,B: $i,R: $i] :
          ( ( breln @ A @ B @ R )
         => ! [Xphi: $i > $o] :
              ( ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ! [Xy: $i] :
                      ( ( in @ Xy @ B )
                     => ( ( in @ ( kpair @ Xx @ Xy ) @ R )
                       => ( Xphi @ ( kpair @ Xx @ Xy ) ) ) ) )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ R )
                 => ( Xphi @ Xx ) ) ) ) ) )).

thf(brelnall2_type,type,(
    brelnall2: $o )).

thf(brelnall2,definition,
    ( brelnall2
    = ( ! [A: $i,B: $i,R: $i] :
          ( ( breln @ A @ B @ R )
         => ! [Xphi: $i > $o] :
              ( ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ! [Xy: $i] :
                      ( ( in @ Xy @ B )
                     => ( ( in @ ( kpair @ Xx @ Xy ) @ R )
                       => ( Xphi @ ( kpair @ Xx @ Xy ) ) ) ) )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ R )
                 => ( Xphi @ Xx ) ) ) ) ) )).

thf(ex1E2_type,type,(
    ex1E2: $o )).

thf(ex1E2,definition,
    ( ex1E2
    = ( ! [A: $i,Xphi: $i > $o] :
          ( ( ex1 @ A
            @ ^ [Xx: $i] :
                ( Xphi @ Xx ) )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ! [Xy: $i] :
                  ( ( in @ Xy @ A )
                 => ( ( Xphi @ Xx )
                   => ( ( Xphi @ Xy )
                     => ( Xx = Xy ) ) ) ) ) ) ) )).

thf(funcGraphProp1_type,type,(
    funcGraphProp1: $o )).

thf(funcGraphProp1,definition,
    ( funcGraphProp1
    = ( ! [A: $i,B: $i,Xf: $i] :
          ( ( func @ A @ B @ Xf )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( in @ ( kpair @ Xx @ ( ap @ A @ B @ Xf @ Xx ) ) @ Xf ) ) ) ) )).

thf(funcGraphProp3_type,type,(
    funcGraphProp3: $o )).

thf(funcGraphProp3,definition,
    ( funcGraphProp3
    = ( ! [A: $i,B: $i,Xf: $i] :
          ( ( in @ Xf @ ( funcSet @ A @ B ) )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( in @ ( kpair @ Xx @ ( ap @ A @ B @ Xf @ Xx ) ) @ Xf ) ) ) ) )).

thf(funcGraphProp2_type,type,(
    funcGraphProp2: $o )).

thf(funcGraphProp2,definition,
    ( funcGraphProp2
    = ( ! [A: $i,B: $i,Xf: $i] :
          ( ( func @ A @ B @ Xf )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ! [Xy: $i] :
                  ( ( in @ Xy @ B )
                 => ( ( in @ ( kpair @ Xx @ Xy ) @ Xf )
                   => ( ( ap @ A @ B @ Xf @ Xx )
                      = Xy ) ) ) ) ) ) )).

thf(funcextLem_type,type,(
    funcextLem: $o )).

thf(funcextLem,definition,
    ( funcextLem
    = ( ! [A: $i,B: $i,Xf: $i] :
          ( ( func @ A @ B @ Xf )
         => ! [Xg: $i] :
              ( ( func @ A @ B @ Xg )
             => ( ! [Xx: $i] :
                    ( ( in @ Xx @ A )
                   => ( ( ap @ A @ B @ Xf @ Xx )
                      = ( ap @ A @ B @ Xg @ Xx ) ) )
               => ! [Xx: $i] :
                    ( ( in @ Xx @ A )
                   => ! [Xy: $i] :
                        ( ( in @ Xy @ B )
                       => ( ( in @ ( kpair @ Xx @ Xy ) @ Xg )
                         => ( in @ ( kpair @ Xx @ Xy ) @ Xf ) ) ) ) ) ) ) ) )).

thf(funcGraphProp4_type,type,(
    funcGraphProp4: $o )).

thf(funcGraphProp4,definition,
    ( funcGraphProp4
    = ( ! [A: $i,B: $i,Xf: $i] :
          ( ( in @ Xf @ ( funcSet @ A @ B ) )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ! [Xy: $i] :
                  ( ( in @ Xy @ B )
                 => ( ( in @ ( kpair @ Xx @ Xy ) @ Xf )
                   => ( ( ap @ A @ B @ Xf @ Xx )
                      = Xy ) ) ) ) ) ) )).

thf(subbreln_type,type,(
    subbreln: $o )).

thf(subbreln,definition,
    ( subbreln
    = ( ! [A: $i,B: $i,R: $i] :
          ( ( breln @ A @ B @ R )
         => ! [S: $i] :
              ( ( breln @ A @ B @ S )
             => ( ! [Xx: $i] :
                    ( ( in @ Xx @ A )
                   => ! [Xy: $i] :
                        ( ( in @ Xy @ B )
                       => ( ( in @ ( kpair @ Xx @ Xy ) @ R )
                         => ( in @ ( kpair @ Xx @ Xy ) @ S ) ) ) )
               => ( subset @ R @ S ) ) ) ) ) )).

thf(eqbreln_type,type,(
    eqbreln: $o )).

thf(eqbreln,definition,
    ( eqbreln
    = ( ! [A: $i,B: $i,R: $i] :
          ( ( breln @ A @ B @ R )
         => ! [S: $i] :
              ( ( breln @ A @ B @ S )
             => ( ! [Xx: $i] :
                    ( ( in @ Xx @ A )
                   => ! [Xy: $i] :
                        ( ( in @ Xy @ B )
                       => ( ( in @ ( kpair @ Xx @ Xy ) @ R )
                         => ( in @ ( kpair @ Xx @ Xy ) @ S ) ) ) )
               => ( ! [Xx: $i] :
                      ( ( in @ Xx @ A )
                     => ! [Xy: $i] :
                          ( ( in @ Xy @ B )
                         => ( ( in @ ( kpair @ Xx @ Xy ) @ S )
                           => ( in @ ( kpair @ Xx @ Xy ) @ R ) ) ) )
                 => ( R = S ) ) ) ) ) ) )).

thf(funcext_type,type,(
    funcext: $o )).

thf(funcext,definition,
    ( funcext
    = ( ! [A: $i,B: $i,Xf: $i] :
          ( ( func @ A @ B @ Xf )
         => ! [Xg: $i] :
              ( ( func @ A @ B @ Xg )
             => ( ! [Xx: $i] :
                    ( ( in @ Xx @ A )
                   => ( ( ap @ A @ B @ Xf @ Xx )
                      = ( ap @ A @ B @ Xg @ Xx ) ) )
               => ( Xf = Xg ) ) ) ) ) )).

thf(funcext2_type,type,(
    funcext2: $o )).

thf(funcext2,definition,
    ( funcext2
    = ( ! [A: $i,B: $i,Xf: $i] :
          ( ( in @ Xf @ ( funcSet @ A @ B ) )
         => ! [Xg: $i] :
              ( ( in @ Xg @ ( funcSet @ A @ B ) )
             => ( ! [Xx: $i] :
                    ( ( in @ Xx @ A )
                   => ( ( ap @ A @ B @ Xf @ Xx )
                      = ( ap @ A @ B @ Xg @ Xx ) ) )
               => ( Xf = Xg ) ) ) ) ) )).

thf(ap2apEq1_type,type,(
    ap2apEq1: $o )).

thf(ap2apEq1,definition,
    ( ap2apEq1
    = ( ! [A: $i,B: $i,Xf: $i] :
          ( ( in @ Xf @ ( funcSet @ A @ B ) )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( ( ap @ A @ B @ Xf @ Xx )
                = ( ap @ A @ B @ Xf @ Xx ) ) ) ) ) )).

thf(ap2apEq2_type,type,(
    ap2apEq2: $o )).

thf(ap2apEq2,definition,
    ( ap2apEq2
    = ( ! [A: $i,B: $i,Xf: $i] :
          ( ( func @ A @ B @ Xf )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( ( ap @ A @ B @ Xf @ Xx )
                = ( ap @ A @ B @ Xf @ Xx ) ) ) ) ) )).

thf(beta1_type,type,(
    beta1: $o )).

thf(beta1,definition,
    ( beta1
    = ( ! [A: $i,B: $i,Xf: $i > $i] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( in @ ( Xf @ Xx ) @ B ) )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( ( ap @ A @ B
                  @ ( lam @ A @ B
                    @ ^ [Xy: $i] :
                        ( Xf @ Xy ) )
                  @ Xx )
                = ( Xf @ Xx ) ) ) ) ) )).

thf(eta1_type,type,(
    eta1: $o )).

thf(eta1,definition,
    ( eta1
    = ( ! [A: $i,B: $i,Xf: $i] :
          ( ( func @ A @ B @ Xf )
         => ( ( lam @ A @ B
              @ ^ [Xx: $i] :
                  ( ap @ A @ B @ Xf @ Xx ) )
            = Xf ) ) ) )).

thf(lam2lamEq_type,type,(
    lam2lamEq: $o )).

thf(lam2lamEq,definition,
    ( lam2lamEq
    = ( ! [A: $i,B: $i,Xf: $i > $i] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( in @ ( Xf @ Xx ) @ B ) )
         => ( ( lam @ A @ B
              @ ^ [Xx: $i] :
                  ( Xf @ Xx ) )
            = ( lam @ A @ B
              @ ^ [Xx: $i] :
                  ( Xf @ Xx ) ) ) ) ) )).

thf(beta2_type,type,(
    beta2: $o )).

thf(beta2,definition,
    ( beta2
    = ( ! [A: $i,B: $i,Xf: $i > $i] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( in @ ( Xf @ Xx ) @ B ) )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( ( ap @ A @ B
                  @ ( lam @ A @ B
                    @ ^ [Xy: $i] :
                        ( Xf @ Xy ) )
                  @ Xx )
                = ( Xf @ Xx ) ) ) ) ) )).

thf(eta2_type,type,(
    eta2: $o )).

thf(eta2,definition,
    ( eta2
    = ( ! [A: $i,B: $i,Xf: $i] :
          ( ( in @ Xf @ ( funcSet @ A @ B ) )
         => ( ( lam @ A @ B
              @ ^ [Xx: $i] :
                  ( ap @ A @ B @ Xf @ Xx ) )
            = Xf ) ) ) )).

thf(iffalseProp1_type,type,(
    iffalseProp1: $o )).

thf(iffalseProp1,definition,
    ( iffalseProp1
    = ( ! [A: $i,Xphi: $o,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ A )
             => ( ~ ( Xphi )
               => ( in @ Xy
                  @ ( dsetconstr @ A
                    @ ^ [Xz: $i] :
                        ( ( Xphi
                          & ( Xz = Xx ) )
                        | ( ~ ( Xphi )
                          & ( Xz = Xy ) ) ) ) ) ) ) ) ) )).

thf(iffalseProp2_type,type,(
    iffalseProp2: $o )).

thf(iffalseProp2,definition,
    ( iffalseProp2
    = ( ! [A: $i,Xphi: $o,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ A )
             => ( ~ ( Xphi )
               => ( ( dsetconstr @ A
                    @ ^ [Xz: $i] :
                        ( ( Xphi
                          & ( Xz = Xx ) )
                        | ( ~ ( Xphi )
                          & ( Xz = Xy ) ) ) )
                  = ( setadjoin @ Xy @ emptyset ) ) ) ) ) ) )).

thf(iftrueProp1_type,type,(
    iftrueProp1: $o )).

thf(iftrueProp1,definition,
    ( iftrueProp1
    = ( ! [A: $i,Xphi: $o,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ A )
             => ( Xphi
               => ( in @ Xx
                  @ ( dsetconstr @ A
                    @ ^ [Xz: $i] :
                        ( ( Xphi
                          & ( Xz = Xx ) )
                        | ( ~ ( Xphi )
                          & ( Xz = Xy ) ) ) ) ) ) ) ) ) )).

thf(iftrueProp2_type,type,(
    iftrueProp2: $o )).

thf(iftrueProp2,definition,
    ( iftrueProp2
    = ( ! [A: $i,Xphi: $o,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ A )
             => ( Xphi
               => ( ( dsetconstr @ A
                    @ ^ [Xz: $i] :
                        ( ( Xphi
                          & ( Xz = Xx ) )
                        | ( ~ ( Xphi )
                          & ( Xz = Xy ) ) ) )
                  = ( setadjoin @ Xx @ emptyset ) ) ) ) ) ) )).

thf(ifSingleton_type,type,(
    ifSingleton: $o )).

thf(ifSingleton,definition,
    ( ifSingleton
    = ( ! [A: $i,Xphi: $o,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ A )
             => ( singleton
                @ ( dsetconstr @ A
                  @ ^ [Xz: $i] :
                      ( ( Xphi
                        & ( Xz = Xx ) )
                      | ( ~ ( Xphi )
                        & ( Xz = Xy ) ) ) ) ) ) ) ) )).

thf(if_type,type,(
    if: $i > $o > $i > $i > $i )).

thf(ifp_type,type,(
    ifp: $o )).

thf(ifp,definition,
    ( ifp
    = ( ! [A: $i,Xphi: $o,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ A )
             => ( in @ ( if @ A @ Xphi @ Xx @ Xy ) @ A ) ) ) ) )).

thf(theeq_type,type,(
    theeq: $o )).

thf(theeq,definition,
    ( theeq
    = ( ! [X: $i] :
          ( ( singleton @ X )
         => ! [Xx: $i] :
              ( ( in @ Xx @ X )
             => ( ( setunion @ X )
                = Xx ) ) ) ) )).

thf(iftrue_type,type,(
    iftrue: $o )).

thf(iftrue,definition,
    ( iftrue
    = ( ! [A: $i,Xphi: $o,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ A )
             => ( Xphi
               => ( ( if @ A @ Xphi @ Xx @ Xy )
                  = Xx ) ) ) ) ) )).

thf(iffalse_type,type,(
    iffalse: $o )).

thf(iffalse,definition,
    ( iffalse
    = ( ! [A: $i,Xphi: $o,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ A )
             => ( ~ ( Xphi )
               => ( ( if @ A @ Xphi @ Xx @ Xy )
                  = Xy ) ) ) ) ) )).

thf(iftrueorfalse_type,type,(
    iftrueorfalse: $o )).

thf(iftrueorfalse,definition,
    ( iftrueorfalse
    = ( ! [A: $i,Xphi: $o,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ A )
             => ( in @ ( if @ A @ Xphi @ Xx @ Xy ) @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) ) ) ) ) )).

thf(binintersectT_lem_type,type,(
    binintersectT_lem: $o )).

thf(binintersectT_lem,definition,
    ( binintersectT_lem
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ( in @ ( binintersect @ X @ Y ) @ ( powerset @ A ) ) ) ) ) )).

thf(binunionT_lem_type,type,(
    binunionT_lem: $o )).

thf(binunionT_lem,definition,
    ( binunionT_lem
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ( in @ ( binunion @ X @ Y ) @ ( powerset @ A ) ) ) ) ) )).

thf(powersetT_lem_type,type,(
    powersetT_lem: $o )).

thf(powersetT_lem,definition,
    ( powersetT_lem
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ( in @ ( powerset @ X ) @ ( powerset @ ( powerset @ A ) ) ) ) ) )).

thf(setminusT_lem_type,type,(
    setminusT_lem: $o )).

thf(setminusT_lem,definition,
    ( setminusT_lem
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ( in @ ( setminus @ X @ Y ) @ ( powerset @ A ) ) ) ) ) )).

thf(complementT_lem_type,type,(
    complementT_lem: $o )).

thf(complementT_lem,definition,
    ( complementT_lem
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ( in @ ( setminus @ A @ X ) @ ( powerset @ A ) ) ) ) )).

thf(setextT_type,type,(
    setextT: $o )).

thf(setextT,definition,
    ( setextT
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ( ! [Xx: $i] :
                    ( ( in @ Xx @ A )
                   => ( ( in @ Xx @ X )
                     => ( in @ Xx @ Y ) ) )
               => ( ! [Xx: $i] :
                      ( ( in @ Xx @ A )
                     => ( ( in @ Xx @ Y )
                       => ( in @ Xx @ X ) ) )
                 => ( X = Y ) ) ) ) ) ) )).

thf(subsetTI_type,type,(
    subsetTI: $o )).

thf(subsetTI,definition,
    ( subsetTI
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ( ! [Xx: $i] :
                    ( ( in @ Xx @ A )
                   => ( ( in @ Xx @ X )
                     => ( in @ Xx @ Y ) ) )
               => ( subset @ X @ Y ) ) ) ) ) )).

thf(powersetTI1_type,type,(
    powersetTI1: $o )).

thf(powersetTI1,definition,
    ( powersetTI1
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ( ! [Xx: $i] :
                    ( ( in @ Xx @ A )
                   => ( ( in @ Xx @ X )
                     => ( in @ Xx @ Y ) ) )
               => ( in @ X @ ( powerset @ Y ) ) ) ) ) ) )).

thf(powersetTE1_type,type,(
    powersetTE1: $o )).

thf(powersetTE1,definition,
    ( powersetTE1
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ( ( in @ X @ ( powerset @ Y ) )
                   => ( ( in @ Xx @ X )
                     => ( in @ Xx @ Y ) ) ) ) ) ) ) )).

thf(complementTI1_type,type,(
    complementTI1: $o )).

thf(complementTI1,definition,
    ( complementTI1
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( ( in @ Xx @ X )
               => ~ ( in @ Xx @ ( setminus @ A @ X ) ) ) ) ) ) )).

thf(complementTE1_type,type,(
    complementTE1: $o )).

thf(complementTE1,definition,
    ( complementTE1
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( ~ ( in @ Xx @ ( setminus @ A @ X ) )
               => ( in @ Xx @ X ) ) ) ) ) )).

thf(binintersectTELcontra_type,type,(
    binintersectTELcontra: $o )).

thf(binintersectTELcontra,definition,
    ( binintersectTELcontra
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ( ~ ( in @ Xx @ X )
                   => ~ ( in @ Xx @ ( binintersect @ X @ Y ) ) ) ) ) ) ) )).

thf(binintersectTERcontra_type,type,(
    binintersectTERcontra: $o )).

thf(binintersectTERcontra,definition,
    ( binintersectTERcontra
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ( ~ ( in @ Xx @ Y )
                   => ~ ( in @ Xx @ ( binintersect @ X @ Y ) ) ) ) ) ) ) )).

thf(contrasubsetT_type,type,(
    contrasubsetT: $o )).

thf(contrasubsetT,definition,
    ( contrasubsetT
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ( ( subset @ X @ ( setminus @ A @ Y ) )
                   => ( ( in @ Xx @ Y )
                     => ~ ( in @ Xx @ X ) ) ) ) ) ) ) )).

thf(contrasubsetT1_type,type,(
    contrasubsetT1: $o )).

thf(contrasubsetT1,definition,
    ( contrasubsetT1
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ( ( subset @ X @ Y )
                   => ( ~ ( in @ Xx @ Y )
                     => ~ ( in @ Xx @ X ) ) ) ) ) ) ) )).

thf(contrasubsetT2_type,type,(
    contrasubsetT2: $o )).

thf(contrasubsetT2,definition,
    ( contrasubsetT2
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ( ( subset @ X @ Y )
               => ( subset @ ( setminus @ A @ Y ) @ ( setminus @ A @ X ) ) ) ) ) ) )).

thf(contrasubsetT3_type,type,(
    contrasubsetT3: $o )).

thf(contrasubsetT3,definition,
    ( contrasubsetT3
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ( ( subset @ ( setminus @ A @ Y ) @ ( setminus @ A @ X ) )
               => ( subset @ X @ Y ) ) ) ) ) )).

thf(doubleComplementI1_type,type,(
    doubleComplementI1: $o )).

thf(doubleComplementI1,definition,
    ( doubleComplementI1
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( ( in @ Xx @ X )
               => ( in @ Xx @ ( setminus @ A @ ( setminus @ A @ X ) ) ) ) ) ) ) )).

thf(doubleComplementE1_type,type,(
    doubleComplementE1: $o )).

thf(doubleComplementE1,definition,
    ( doubleComplementE1
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( ( in @ Xx @ ( setminus @ A @ ( setminus @ A @ X ) ) )
               => ( in @ Xx @ X ) ) ) ) ) )).

thf(doubleComplementSub1_type,type,(
    doubleComplementSub1: $o )).

thf(doubleComplementSub1,definition,
    ( doubleComplementSub1
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ( subset @ X @ ( setminus @ A @ ( setminus @ A @ X ) ) ) ) ) )).

thf(doubleComplementSub2_type,type,(
    doubleComplementSub2: $o )).

thf(doubleComplementSub2,definition,
    ( doubleComplementSub2
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ( subset @ ( setminus @ A @ ( setminus @ A @ X ) ) @ X ) ) ) )).

thf(doubleComplementEq_type,type,(
    doubleComplementEq: $o )).

thf(doubleComplementEq,definition,
    ( doubleComplementEq
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ( X
            = ( setminus @ A @ ( setminus @ A @ X ) ) ) ) ) )).

thf(complementTnotintersectT_type,type,(
    complementTnotintersectT: $o )).

thf(complementTnotintersectT,definition,
    ( complementTnotintersectT
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ( ( in @ Xx @ ( setminus @ A @ X ) )
                   => ~ ( in @ Xx @ ( binintersect @ X @ Y ) ) ) ) ) ) ) )).

thf(complementImpComplementIntersect_type,type,(
    complementImpComplementIntersect: $o )).

thf(complementImpComplementIntersect,definition,
    ( complementImpComplementIntersect
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ( ( in @ Xx @ ( setminus @ A @ X ) )
                   => ( in @ Xx @ ( setminus @ A @ ( binintersect @ X @ Y ) ) ) ) ) ) ) ) )).

thf(complementSubsetComplementIntersect_type,type,(
    complementSubsetComplementIntersect: $o )).

thf(complementSubsetComplementIntersect,definition,
    ( complementSubsetComplementIntersect
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ( subset @ ( setminus @ A @ X ) @ ( setminus @ A @ ( binintersect @ X @ Y ) ) ) ) ) ) )).

thf(complementInPowersetComplementIntersect_type,type,(
    complementInPowersetComplementIntersect: $o )).

thf(complementInPowersetComplementIntersect,definition,
    ( complementInPowersetComplementIntersect
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ( in @ ( setminus @ A @ X ) @ ( powerset @ ( setminus @ A @ ( binintersect @ X @ Y ) ) ) ) ) ) ) )).

thf(contraSubsetComplement_type,type,(
    contraSubsetComplement: $o )).

thf(contraSubsetComplement,definition,
    ( contraSubsetComplement
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ( ( subset @ X @ ( setminus @ A @ Y ) )
               => ! [Xx: $i] :
                    ( ( in @ Xx @ A )
                   => ( ( in @ Xx @ Y )
                     => ( in @ Xx @ ( setminus @ A @ X ) ) ) ) ) ) ) ) )).

thf(complementTcontraSubset_type,type,(
    complementTcontraSubset: $o )).

thf(complementTcontraSubset,definition,
    ( complementTcontraSubset
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ( ( subset @ X @ ( setminus @ A @ Y ) )
               => ( subset @ Y @ ( setminus @ A @ X ) ) ) ) ) ) )).

thf(binunionTILcontra_type,type,(
    binunionTILcontra: $o )).

thf(binunionTILcontra,definition,
    ( binunionTILcontra
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ( ~ ( in @ Xx @ ( binunion @ X @ Y ) )
                   => ~ ( in @ Xx @ X ) ) ) ) ) ) )).

thf(binunionTIRcontra_type,type,(
    binunionTIRcontra: $o )).

thf(binunionTIRcontra,definition,
    ( binunionTIRcontra
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ( ~ ( in @ Xx @ ( binunion @ X @ Y ) )
                   => ~ ( in @ Xx @ Y ) ) ) ) ) ) )).

thf(inIntersectImpInUnion_type,type,(
    inIntersectImpInUnion: $o )).

thf(inIntersectImpInUnion,definition,
    ( inIntersectImpInUnion
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Z: $i] :
                  ( ( in @ Z @ ( powerset @ A ) )
                 => ! [Xx: $i] :
                      ( ( in @ Xx @ A )
                     => ( ( in @ Xx @ ( binintersect @ X @ Y ) )
                       => ( in @ Xx @ ( binunion @ X @ Z ) ) ) ) ) ) ) ) )).

thf(inIntersectImpInUnion2_type,type,(
    inIntersectImpInUnion2: $o )).

thf(inIntersectImpInUnion2,definition,
    ( inIntersectImpInUnion2
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Z: $i] :
                  ( ( in @ Z @ ( powerset @ A ) )
                 => ! [Xx: $i] :
                      ( ( in @ Xx @ A )
                     => ( ( in @ Xx @ ( binintersect @ X @ Y ) )
                       => ( in @ Xx @ ( binunion @ Y @ Z ) ) ) ) ) ) ) ) )).

thf(inIntersectImpInIntersectUnions_type,type,(
    inIntersectImpInIntersectUnions: $o )).

thf(inIntersectImpInIntersectUnions,definition,
    ( inIntersectImpInIntersectUnions
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Z: $i] :
                  ( ( in @ Z @ ( powerset @ A ) )
                 => ! [Xx: $i] :
                      ( ( in @ Xx @ A )
                     => ( ( in @ Xx @ ( binintersect @ X @ Y ) )
                       => ( in @ Xx @ ( binintersect @ ( binunion @ X @ Z ) @ ( binunion @ Y @ Z ) ) ) ) ) ) ) ) ) )).

thf(intersectInPowersetIntersectUnions_type,type,(
    intersectInPowersetIntersectUnions: $o )).

thf(intersectInPowersetIntersectUnions,definition,
    ( intersectInPowersetIntersectUnions
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Z: $i] :
                  ( ( in @ Z @ ( powerset @ A ) )
                 => ( in @ ( binintersect @ X @ Y ) @ ( powerset @ ( binintersect @ ( binunion @ X @ Z ) @ ( binunion @ Y @ Z ) ) ) ) ) ) ) ) )).

thf(inComplementUnionImpNotIn1_type,type,(
    inComplementUnionImpNotIn1: $o )).

thf(inComplementUnionImpNotIn1,definition,
    ( inComplementUnionImpNotIn1
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ( ( in @ Xx @ ( setminus @ A @ ( binunion @ X @ Y ) ) )
                   => ~ ( in @ Xx @ X ) ) ) ) ) ) )).

thf(inComplementUnionImpInComplement1_type,type,(
    inComplementUnionImpInComplement1: $o )).

thf(inComplementUnionImpInComplement1,definition,
    ( inComplementUnionImpInComplement1
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ( ( in @ Xx @ ( setminus @ A @ ( binunion @ X @ Y ) ) )
                   => ( in @ Xx @ ( setminus @ A @ X ) ) ) ) ) ) ) )).

thf(binunionTE_type,type,(
    binunionTE: $o )).

thf(binunionTE,definition,
    ( binunionTE
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Xphi: $o,Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ( ( in @ Xx @ ( binunion @ X @ Y ) )
                   => ( ( ( in @ Xx @ X )
                       => Xphi )
                     => ( ( ( in @ Xx @ Y )
                         => Xphi )
                       => Xphi ) ) ) ) ) ) ) )).

thf(binunionTEcontra_type,type,(
    binunionTEcontra: $o )).

thf(binunionTEcontra,definition,
    ( binunionTEcontra
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ( ~ ( in @ Xx @ X )
                   => ( ~ ( in @ Xx @ Y )
                     => ~ ( in @ Xx @ ( binunion @ X @ Y ) ) ) ) ) ) ) ) )).

thf(demorgan2a1_type,type,(
    demorgan2a1: $o )).

thf(demorgan2a1,definition,
    ( demorgan2a1
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ( ( in @ Xx @ ( setminus @ A @ ( binunion @ X @ Y ) ) )
                   => ( in @ Xx @ ( setminus @ A @ X ) ) ) ) ) ) ) )).

thf(complementUnionInPowersetComplement_type,type,(
    complementUnionInPowersetComplement: $o )).

thf(complementUnionInPowersetComplement,definition,
    ( complementUnionInPowersetComplement
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ( in @ ( setminus @ A @ ( binunion @ X @ Y ) ) @ ( powerset @ ( setminus @ A @ X ) ) ) ) ) ) )).

thf(demorgan2a2_type,type,(
    demorgan2a2: $o )).

thf(demorgan2a2,definition,
    ( demorgan2a2
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ( ( in @ Xx @ ( setminus @ A @ ( binunion @ X @ Y ) ) )
                   => ( in @ Xx @ ( setminus @ A @ Y ) ) ) ) ) ) ) )).

thf(demorgan1a_type,type,(
    demorgan1a: $o )).

thf(demorgan1a,definition,
    ( demorgan1a
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ( ( in @ Xx @ ( setminus @ A @ ( binintersect @ X @ Y ) ) )
                   => ( in @ Xx @ ( binunion @ ( setminus @ A @ X ) @ ( setminus @ A @ Y ) ) ) ) ) ) ) ) )).

thf(demorgan1b_type,type,(
    demorgan1b: $o )).

thf(demorgan1b,definition,
    ( demorgan1b
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ( ( in @ Xx @ ( binunion @ ( setminus @ A @ X ) @ ( setminus @ A @ Y ) ) )
                   => ( in @ Xx @ ( setminus @ A @ ( binintersect @ X @ Y ) ) ) ) ) ) ) ) )).

thf(demorgan1_type,type,(
    demorgan1: $o )).

thf(demorgan1,definition,
    ( demorgan1
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ( ( setminus @ A @ ( binintersect @ X @ Y ) )
                = ( binunion @ ( setminus @ A @ X ) @ ( setminus @ A @ Y ) ) ) ) ) ) )).

thf(demorgan2a_type,type,(
    demorgan2a: $o )).

thf(demorgan2a,definition,
    ( demorgan2a
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ( ( in @ Xx @ ( setminus @ A @ ( binunion @ X @ Y ) ) )
                   => ( in @ Xx @ ( binintersect @ ( setminus @ A @ X ) @ ( setminus @ A @ Y ) ) ) ) ) ) ) ) )).

thf(demorgan2b2_type,type,(
    demorgan2b2: $o )).

thf(demorgan2b2,definition,
    ( demorgan2b2
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ( ( in @ Xx @ ( setminus @ A @ X ) )
                   => ( ( in @ Xx @ ( setminus @ A @ Y ) )
                     => ( in @ Xx @ ( setminus @ A @ ( binunion @ X @ Y ) ) ) ) ) ) ) ) ) )).

thf(demorgan2b_type,type,(
    demorgan2b: $o )).

thf(demorgan2b,definition,
    ( demorgan2b
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ( ( in @ Xx @ ( binintersect @ ( setminus @ A @ X ) @ ( setminus @ A @ Y ) ) )
                   => ( in @ Xx @ ( setminus @ A @ ( binunion @ X @ Y ) ) ) ) ) ) ) ) )).

thf(demorgan2_type,type,(
    demorgan2: $o )).

thf(demorgan2,definition,
    ( demorgan2
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ( ( setminus @ A @ ( binunion @ X @ Y ) )
                = ( binintersect @ ( setminus @ A @ X ) @ ( setminus @ A @ Y ) ) ) ) ) ) )).

thf(woz13rule0_type,type,(
    woz13rule0: $o )).

thf(woz13rule0,definition,
    ( woz13rule0
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ ( binintersect @ X @ Y ) )
                 => ( in @ Xx @ A ) ) ) ) ) )).

thf(woz13rule1_type,type,(
    woz13rule1: $o )).

thf(woz13rule1,definition,
    ( woz13rule1
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Z: $i] :
                  ( ( in @ Z @ ( powerset @ A ) )
                 => ( ( subset @ X @ Z )
                   => ( subset @ ( binintersect @ X @ Y ) @ Z ) ) ) ) ) ) )).

thf(woz13rule2_type,type,(
    woz13rule2: $o )).

thf(woz13rule2,definition,
    ( woz13rule2
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Z: $i] :
                  ( ( in @ Z @ ( powerset @ A ) )
                 => ( ( subset @ Y @ Z )
                   => ( subset @ ( binintersect @ X @ Y ) @ Z ) ) ) ) ) ) )).

thf(woz13rule3_type,type,(
    woz13rule3: $o )).

thf(woz13rule3,definition,
    ( woz13rule3
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Z: $i] :
                  ( ( in @ Z @ ( powerset @ A ) )
                 => ( ( subset @ X @ Y )
                   => ( ( subset @ X @ Z )
                     => ( subset @ X @ ( binintersect @ Y @ Z ) ) ) ) ) ) ) ) )).

thf(woz13rule4_type,type,(
    woz13rule4: $o )).

thf(woz13rule4,definition,
    ( woz13rule4
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Z: $i] :
                  ( ( in @ Z @ ( powerset @ A ) )
                 => ! [W: $i] :
                      ( ( in @ W @ ( powerset @ A ) )
                     => ( ( subset @ X @ Z )
                       => ( ( subset @ Y @ W )
                         => ( subset @ ( binintersect @ X @ Y ) @ ( binintersect @ Z @ W ) ) ) ) ) ) ) ) ) )).

thf(woz1_1_type,type,(
    woz1_1: $o )).

thf(woz1_1,definition,
    ( woz1_1
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ( in @ ( setminus @ A @ X ) @ ( powerset @ ( setminus @ A @ ( binintersect @ X @ Y ) ) ) ) ) ) ) )).

thf(woz1_2_type,type,(
    woz1_2: $o )).

thf(woz1_2,definition,
    ( woz1_2
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Z: $i] :
                  ( ( in @ Z @ ( powerset @ A ) )
                 => ! [W: $i] :
                      ( ( in @ W @ ( powerset @ A ) )
                     => ( ( setminus @ A @ ( binintersect @ ( binunion @ X @ Y ) @ ( binunion @ Z @ W ) ) )
                        = ( binunion @ ( binintersect @ ( setminus @ A @ X ) @ ( setminus @ A @ Y ) ) @ ( binintersect @ ( setminus @ A @ Z ) @ ( setminus @ A @ W ) ) ) ) ) ) ) ) ) )).

thf(woz1_3_type,type,(
    woz1_3: $o )).

thf(woz1_3,definition,
    ( woz1_3
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ! [Z: $i] :
                  ( ( in @ Z @ ( powerset @ A ) )
                 => ( in @ ( binintersect @ X @ Y ) @ ( powerset @ ( binintersect @ ( binunion @ X @ Z ) @ ( binunion @ Y @ Z ) ) ) ) ) ) ) ) )).

thf(woz1_4_type,type,(
    woz1_4: $o )).

thf(woz1_4,definition,
    ( woz1_4
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ( ( subset @ X @ ( setminus @ A @ Y ) )
               => ( subset @ Y @ ( setminus @ A @ X ) ) ) ) ) ) )).

thf(woz1_5_type,type,(
    woz1_5: $o )).

thf(woz1_5,definition,
    ( woz1_5
    = ( ! [A: $i,X: $i] :
          ( ( in @ X @ ( powerset @ A ) )
         => ! [Y: $i] :
              ( ( in @ Y @ ( powerset @ A ) )
             => ( in @ ( setminus @ A @ ( binunion @ X @ Y ) ) @ ( powerset @ ( setminus @ A @ X ) ) ) ) ) ) )).

thf(breln1_type,type,(
    breln1: $i > $i > $o )).

thf(breln1all2_type,type,(
    breln1all2: $o )).

thf(breln1all2,definition,
    ( breln1all2
    = ( ! [A: $i,R: $i] :
          ( ( breln1 @ A @ R )
         => ! [Xphi: $i > $o] :
              ( ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ! [Xy: $i] :
                      ( ( in @ Xy @ A )
                     => ( ( in @ ( kpair @ Xx @ Xy ) @ R )
                       => ( Xphi @ ( kpair @ Xx @ Xy ) ) ) ) )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ R )
                 => ( Xphi @ Xx ) ) ) ) ) )).

thf(breln1Set_type,type,(
    breln1Set: $i > $i )).

thf(breln1SetBreln1_type,type,(
    breln1SetBreln1: $o )).

thf(breln1SetBreln1,definition,
    ( breln1SetBreln1
    = ( ! [A: $i,R: $i] :
          ( ( in @ R @ ( breln1Set @ A ) )
         => ( breln1 @ A @ R ) ) ) )).

thf(transitive_type,type,(
    transitive: $i > $i > $o )).

thf(antisymmetric_type,type,(
    antisymmetric: $i > $i > $o )).

thf(reflexive_type,type,(
    reflexive: $i > $i > $o )).

thf(refltransitive_type,type,(
    refltransitive: $i > $i > $o )).

thf(refllinearorder_type,type,(
    refllinearorder: $i > $i > $o )).

thf(reflwellordering_type,type,(
    reflwellordering: $i > $i > $o )).

thf(choice2fnsingleton_type,type,(
    choice2fnsingleton: $o )).

thf(choice2fnsingleton,definition,
    ( choice2fnsingleton
    = ( ! [A: $i,B: $i,Xphi: $i > $i > $o] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ? [Xy: $i] :
                  ( ( in @ Xy @ B )
                  & ( Xphi @ Xx @ Xy ) ) )
         => ! [R: $i] :
              ( ( in @ R @ ( breln1Set @ B ) )
             => ( ( reflwellordering @ B @ R )
               => ! [Xx: $i] :
                    ( ( in @ Xx @ A )
                   => ( singleton
                      @ ( dsetconstr @ B
                        @ ^ [Xy: $i] :
                            ( ( Xphi @ Xx @ Xy )
                            & ! [Xz: $i] :
                                ( ( in @ Xz @ B )
                               => ( ( Xphi @ Xx @ Xz )
                                 => ( in @ ( kpair @ Xy @ Xz ) @ R ) ) ) ) ) ) ) ) ) ) ) )).

thf(setOfPairsIsBReln1_type,type,(
    setOfPairsIsBReln1: $o )).

thf(setOfPairsIsBReln1,definition,
    ( setOfPairsIsBReln1
    = ( ! [A: $i,Xphi: $i > $i > $o] :
          ( breln1 @ A
          @ ( dpsetconstr @ A @ A
            @ ^ [Xx: $i,Xy: $i] :
                ( Xphi @ Xx @ Xy ) ) ) ) )).

thf(breln1all1_type,type,(
    breln1all1: $o )).

thf(breln1all1,definition,
    ( breln1all1
    = ( ! [A: $i,R: $i] :
          ( ( breln1 @ A @ R )
         => ! [Xphi: $i > $o] :
              ( ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ! [Xy: $i] :
                      ( ( in @ Xy @ A )
                     => ( ( in @ ( kpair @ Xx @ Xy ) @ R )
                       => ( Xphi @ ( kpair @ Xx @ Xy ) ) ) ) )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ R )
                 => ( Xphi @ Xx ) ) ) ) ) )).

thf(subbreln1_type,type,(
    subbreln1: $o )).

thf(subbreln1,definition,
    ( subbreln1
    = ( ! [A: $i,R: $i] :
          ( ( breln1 @ A @ R )
         => ! [S: $i] :
              ( ( breln1 @ A @ S )
             => ( ! [Xx: $i] :
                    ( ( in @ Xx @ A )
                   => ! [Xy: $i] :
                        ( ( in @ Xy @ A )
                       => ( ( in @ ( kpair @ Xx @ Xy ) @ R )
                         => ( in @ ( kpair @ Xx @ Xy ) @ S ) ) ) )
               => ( subset @ R @ S ) ) ) ) ) )).

thf(eqbreln1_type,type,(
    eqbreln1: $o )).

thf(eqbreln1,definition,
    ( eqbreln1
    = ( ! [A: $i,R: $i] :
          ( ( breln1 @ A @ R )
         => ! [S: $i] :
              ( ( breln1 @ A @ S )
             => ( ! [Xx: $i] :
                    ( ( in @ Xx @ A )
                   => ! [Xy: $i] :
                        ( ( in @ Xy @ A )
                       => ( ( in @ ( kpair @ Xx @ Xy ) @ R )
                         => ( in @ ( kpair @ Xx @ Xy ) @ S ) ) ) )
               => ( ! [Xx: $i] :
                      ( ( in @ Xx @ A )
                     => ! [Xy: $i] :
                          ( ( in @ Xy @ A )
                         => ( ( in @ ( kpair @ Xx @ Xy ) @ S )
                           => ( in @ ( kpair @ Xx @ Xy ) @ R ) ) ) )
                 => ( R = S ) ) ) ) ) ) )).

thf(breln1invset_type,type,(
    breln1invset: $i > $i > $i )).

thf(breln1invprop_type,type,(
    breln1invprop: $o )).

thf(breln1invprop,definition,
    ( breln1invprop
    = ( ! [A: $i,R: $i] :
          ( ( breln1 @ A @ R )
         => ( breln1 @ A @ ( breln1invset @ A @ R ) ) ) ) )).

thf(breln1invI_type,type,(
    breln1invI: $o )).

thf(breln1invI,definition,
    ( breln1invI
    = ( ! [A: $i,R: $i] :
          ( ( breln1 @ A @ R )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ! [Xy: $i] :
                  ( ( in @ Xy @ A )
                 => ( ( in @ ( kpair @ Xx @ Xy ) @ R )
                   => ( in @ ( kpair @ Xy @ Xx ) @ ( breln1invset @ A @ R ) ) ) ) ) ) ) )).

thf(breln1invE_type,type,(
    breln1invE: $o )).

thf(breln1invE,definition,
    ( breln1invE
    = ( ! [A: $i,R: $i] :
          ( ( breln1 @ A @ R )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ! [Xy: $i] :
                  ( ( in @ Xy @ A )
                 => ( ( in @ ( kpair @ Xy @ Xx ) @ ( breln1invset @ A @ R ) )
                   => ( in @ ( kpair @ Xx @ Xy ) @ R ) ) ) ) ) ) )).

thf(breln1compset_type,type,(
    breln1compset: $i > $i > $i > $i )).

thf(breln1compprop_type,type,(
    breln1compprop: $o )).

thf(breln1compprop,definition,
    ( breln1compprop
    = ( ! [A: $i,R: $i] :
          ( ( breln1 @ A @ R )
         => ! [S: $i] :
              ( ( breln1 @ A @ S )
             => ( breln1 @ A @ ( breln1compset @ A @ R @ S ) ) ) ) ) )).

thf(breln1compI_type,type,(
    breln1compI: $o )).

thf(breln1compI,definition,
    ( breln1compI
    = ( ! [A: $i,R: $i] :
          ( ( breln1 @ A @ R )
         => ! [S: $i] :
              ( ( breln1 @ A @ S )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ! [Xy: $i] :
                      ( ( in @ Xy @ A )
                     => ! [Xz: $i] :
                          ( ( in @ Xz @ A )
                         => ( ( in @ ( kpair @ Xx @ Xz ) @ R )
                           => ( ( in @ ( kpair @ Xz @ Xy ) @ S )
                             => ( in @ ( kpair @ Xx @ Xy ) @ ( breln1compset @ A @ R @ S ) ) ) ) ) ) ) ) ) ) )).

thf(breln1compE_type,type,(
    breln1compE: $o )).

thf(breln1compE,definition,
    ( breln1compE
    = ( ! [A: $i,R: $i] :
          ( ( breln1 @ A @ R )
         => ! [S: $i] :
              ( ( breln1 @ A @ S )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ! [Xy: $i] :
                      ( ( in @ Xy @ A )
                     => ( ( in @ ( kpair @ Xx @ Xy ) @ ( breln1compset @ A @ R @ S ) )
                       => ? [Xz: $i] :
                            ( ( in @ Xz @ A )
                            & ( in @ ( kpair @ Xx @ Xz ) @ R )
                            & ( in @ ( kpair @ Xz @ Xy ) @ S ) ) ) ) ) ) ) ) )).

thf(breln1compEex_type,type,(
    breln1compEex: $o )).

thf(breln1compEex,definition,
    ( breln1compEex
    = ( ! [A: $i,R: $i] :
          ( ( breln1 @ A @ R )
         => ! [S: $i] :
              ( ( breln1 @ A @ S )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ! [Xy: $i] :
                      ( ( in @ Xy @ A )
                     => ( ( in @ ( kpair @ Xx @ Xy ) @ ( breln1compset @ A @ R @ S ) )
                       => ! [Xphi: $o] :
                            ( ! [Xz: $i] :
                                ( ( in @ Xz @ A )
                               => ( ( in @ ( kpair @ Xx @ Xz ) @ R )
                                 => ( ( in @ ( kpair @ Xz @ Xy ) @ S )
                                   => Xphi ) ) )
                           => Xphi ) ) ) ) ) ) ) )).

thf(breln1unionprop_type,type,(
    breln1unionprop: $o )).

thf(breln1unionprop,definition,
    ( breln1unionprop
    = ( ! [A: $i,R: $i] :
          ( ( breln1 @ A @ R )
         => ! [S: $i] :
              ( ( breln1 @ A @ S )
             => ( breln1 @ A @ ( binunion @ R @ S ) ) ) ) ) )).

thf(breln1unionIL_type,type,(
    breln1unionIL: $o )).

thf(breln1unionIL,definition,
    ( breln1unionIL
    = ( ! [A: $i,R: $i] :
          ( ( breln1 @ A @ R )
         => ! [S: $i] :
              ( ( breln1 @ A @ S )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ! [Xy: $i] :
                      ( ( in @ Xy @ A )
                     => ( ( in @ ( kpair @ Xx @ Xy ) @ R )
                       => ( in @ ( kpair @ Xx @ Xy ) @ ( binunion @ R @ S ) ) ) ) ) ) ) ) )).

thf(breln1unionIR_type,type,(
    breln1unionIR: $o )).

thf(breln1unionIR,definition,
    ( breln1unionIR
    = ( ! [A: $i,R: $i] :
          ( ( breln1 @ A @ R )
         => ! [S: $i] :
              ( ( breln1 @ A @ S )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ! [Xy: $i] :
                      ( ( in @ Xy @ A )
                     => ( ( in @ ( kpair @ Xx @ Xy ) @ S )
                       => ( in @ ( kpair @ Xx @ Xy ) @ ( binunion @ R @ S ) ) ) ) ) ) ) ) )).

thf(breln1unionI_type,type,(
    breln1unionI: $o )).

thf(breln1unionI,definition,
    ( breln1unionI
    = ( ! [A: $i,R: $i] :
          ( ( breln1 @ A @ R )
         => ! [S: $i] :
              ( ( breln1 @ A @ S )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ! [Xy: $i] :
                      ( ( in @ Xy @ A )
                     => ( ( ( in @ ( kpair @ Xx @ Xy ) @ R )
                          | ( in @ ( kpair @ Xx @ Xy ) @ S ) )
                       => ( in @ ( kpair @ Xx @ Xy ) @ ( binunion @ R @ S ) ) ) ) ) ) ) ) )).

thf(breln1unionE_type,type,(
    breln1unionE: $o )).

thf(breln1unionE,definition,
    ( breln1unionE
    = ( ! [A: $i,R: $i] :
          ( ( breln1 @ A @ R )
         => ! [S: $i] :
              ( ( breln1 @ A @ S )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ! [Xy: $i] :
                      ( ( in @ Xy @ A )
                     => ( ( in @ ( kpair @ Xx @ Xy ) @ ( binunion @ R @ S ) )
                       => ( ( in @ ( kpair @ Xx @ Xy ) @ R )
                          | ( in @ ( kpair @ Xx @ Xy ) @ S ) ) ) ) ) ) ) ) )).

thf(breln1unionEcases_type,type,(
    breln1unionEcases: $o )).

thf(breln1unionEcases,definition,
    ( breln1unionEcases
    = ( ! [A: $i,R: $i] :
          ( ( breln1 @ A @ R )
         => ! [S: $i] :
              ( ( breln1 @ A @ S )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ! [Xy: $i] :
                      ( ( in @ Xy @ A )
                     => ( ( in @ ( kpair @ Xx @ Xy ) @ ( binunion @ R @ S ) )
                       => ! [Xphi: $o] :
                            ( ( ( in @ ( kpair @ Xx @ Xy ) @ R )
                             => Xphi )
                           => ( ( ( in @ ( kpair @ Xx @ Xy ) @ S )
                               => Xphi )
                             => Xphi ) ) ) ) ) ) ) ) )).

thf(breln1unionCommutes_type,type,(
    breln1unionCommutes: $o )).

thf(breln1unionCommutes,definition,
    ( breln1unionCommutes
    = ( ! [A: $i,R: $i] :
          ( ( breln1 @ A @ R )
         => ! [S: $i] :
              ( ( breln1 @ A @ S )
             => ( ( binunion @ R @ S )
                = ( binunion @ S @ R ) ) ) ) ) )).

thf(woz2Ex_type,type,(
    woz2Ex: $o )).

thf(woz2Ex,definition,
    ( woz2Ex
    = ( ! [A: $i,R: $i] :
          ( ( breln1 @ A @ R )
         => ( R
            = ( breln1invset @ A @ ( breln1invset @ A @ R ) ) ) ) ) )).

thf(woz2W_type,type,(
    woz2W: $o )).

thf(woz2W,definition,
    ( woz2W
    = ( ! [A: $i,R: $i] :
          ( ( breln1 @ A @ R )
         => ! [S: $i] :
              ( ( breln1 @ A @ S )
             => ( ( breln1invset @ A @ ( breln1compset @ A @ R @ S ) )
                = ( breln1compset @ A @ ( breln1invset @ A @ S ) @ ( breln1invset @ A @ R ) ) ) ) ) ) )).

thf(woz2A_type,type,(
    woz2A: $o )).

thf(woz2A,definition,
    ( woz2A
    = ( ! [A: $i,R: $i] :
          ( ( breln1 @ A @ R )
         => ! [S: $i] :
              ( ( breln1 @ A @ S )
             => ! [T: $i] :
                  ( ( breln1 @ A @ T )
                 => ( ( breln1compset @ A @ ( binunion @ R @ S ) @ T )
                    = ( binunion @ ( breln1compset @ A @ R @ T ) @ ( breln1compset @ A @ S @ T ) ) ) ) ) ) ) )).

thf(woz2B_type,type,(
    woz2B: $o )).

thf(woz2B,definition,
    ( woz2B
    = ( ! [A: $i,R: $i] :
          ( ( breln1 @ A @ R )
         => ! [S: $i] :
              ( ( breln1 @ A @ S )
             => ! [T: $i] :
                  ( ( breln1 @ A @ T )
                 => ( ( breln1compset @ A @ ( binunion @ R @ S ) @ T )
                    = ( binunion @ ( breln1invset @ A @ ( breln1compset @ A @ ( breln1invset @ A @ T ) @ ( breln1invset @ A @ S ) ) ) @ ( breln1invset @ A @ ( breln1compset @ A @ ( breln1invset @ A @ T ) @ ( breln1invset @ A @ R ) ) ) ) ) ) ) ) ) )).

thf(image1Ex_type,type,(
    image1Ex: $o )).

thf(image1Ex,definition,
    ( image1Ex
    = ( ! [A: $i,Xf: $i > $i] :
        ? [B: $i] :
        ! [Xx: $i] :
          ( ( in @ Xx @ B )
        <=> ? [Xy: $i] :
              ( ( in @ Xy @ A )
              & ( Xx
                = ( Xf @ Xy ) ) ) ) ) )).

thf(image1Ex1_type,type,(
    image1Ex1: $o )).

thf(image1Ex1,definition,
    ( image1Ex1
    = ( ! [A: $i,Xf: $i > $i] :
          ( exu
          @ ^ [B: $i] :
            ! [Xx: $i] :
              ( ( in @ Xx @ B )
            <=> ? [Xy: $i] :
                  ( ( in @ Xy @ A )
                  & ( Xx
                    = ( Xf @ Xy ) ) ) ) ) ) )).

thf(image1_type,type,(
    image1: $i > ( $i > $i ) > $i )).

thf(image1Equiv_type,type,(
    image1Equiv: $o )).

thf(image1Equiv,definition,
    ( image1Equiv
    = ( ! [A: $i,Xf: $i > $i,Xx: $i] :
          ( ( in @ Xx
            @ ( image1 @ A
              @ ^ [Xy: $i] :
                  ( Xf @ Xy ) ) )
        <=> ? [Xy: $i] :
              ( ( in @ Xy @ A )
              & ( Xx
                = ( Xf @ Xy ) ) ) ) ) )).

thf(image1E_type,type,(
    image1E: $o )).

thf(image1E,definition,
    ( image1E
    = ( ! [A: $i,Xf: $i > $i,Xx: $i] :
          ( ( in @ Xx
            @ ( image1 @ A
              @ ^ [Xy: $i] :
                  ( Xf @ Xy ) ) )
         => ? [Xy: $i] :
              ( ( in @ Xy @ A )
              & ( Xx
                = ( Xf @ Xy ) ) ) ) ) )).

thf(image1I_type,type,(
    image1I: $o )).

thf(image1I,definition,
    ( image1I
    = ( ! [A: $i,Xf: $i > $i,Xx: $i] :
          ( ? [Xy: $i] :
              ( ( in @ Xy @ A )
              & ( Xx
                = ( Xf @ Xy ) ) )
         => ( in @ Xx
            @ ( image1 @ A
              @ ^ [Xy: $i] :
                  ( Xf @ Xy ) ) ) ) ) )).

thf(injective_type,type,(
    injective: $i > $i > $i > $o )).

thf(injFuncSet_type,type,(
    injFuncSet: $i > $i > $i )).

thf(injFuncSet,definition,
    ( injFuncSet
    = ( ^ [A: $i,B: $i] :
          ( dsetconstr @ ( funcSet @ A @ B )
          @ ^ [Xf: $i] :
              ( injective @ A @ B @ Xf ) ) ) )).

thf(injFuncInInjFuncSet_type,type,(
    injFuncInInjFuncSet: $o )).

thf(injFuncInInjFuncSet,definition,
    ( injFuncInInjFuncSet
    = ( ! [A: $i,B: $i,Xf: $i] :
          ( ( in @ Xf @ ( funcSet @ A @ B ) )
         => ( ( injective @ A @ B @ Xf )
           => ( in @ Xf @ ( injFuncSet @ A @ B ) ) ) ) ) )).

thf(injFuncSetFuncIn_type,type,(
    injFuncSetFuncIn: $o )).

thf(injFuncSetFuncIn,definition,
    ( injFuncSetFuncIn
    = ( ! [A: $i,B: $i,Xf: $i] :
          ( ( in @ Xf @ ( injFuncSet @ A @ B ) )
         => ( in @ Xf @ ( funcSet @ A @ B ) ) ) ) )).

thf(injFuncSetFuncInj,conjecture,
    ( setextAx
   => ( emptysetAx
     => ( setadjoinAx
       => ( powersetAx
         => ( setunionAx
           => ( omega0Ax
             => ( omegaSAx
               => ( omegaIndAx
                 => ( replAx
                   => ( foundationAx
                     => ( wellorderingAx
                       => ( descrp
                         => ( dsetconstrI
                           => ( dsetconstrEL
                             => ( dsetconstrER
                               => ( exuE1
                                 => ( prop2setE
                                   => ( emptysetE
                                     => ( emptysetimpfalse
                                       => ( notinemptyset
                                         => ( exuE3e
                                           => ( setext
                                             => ( emptyI
                                               => ( noeltsimpempty
                                                 => ( setbeta
                                                   => ( nonemptyE1
                                                     => ( nonemptyI
                                                       => ( nonemptyI1
                                                         => ( setadjoinIL
                                                           => ( emptyinunitempty
                                                             => ( setadjoinIR
                                                               => ( setadjoinE
                                                                 => ( setadjoinOr
                                                                   => ( setoftrueEq
                                                                     => ( powersetI
                                                                       => ( emptyinPowerset
                                                                         => ( emptyInPowerset
                                                                           => ( powersetE
                                                                             => ( setunionI
                                                                               => ( setunionE
                                                                                 => ( subPowSU
                                                                                   => ( exuE2
                                                                                     => ( nonemptyImpWitness
                                                                                       => ( uniqinunit
                                                                                         => ( notinsingleton
                                                                                           => ( eqinunit
                                                                                             => ( singletonsswitch
                                                                                               => ( upairsetE
                                                                                                 => ( upairsetIL
                                                                                                   => ( upairsetIR
                                                                                                     => ( emptyE1
                                                                                                       => ( vacuousDall
                                                                                                         => ( quantDeMorgan1
                                                                                                           => ( quantDeMorgan2
                                                                                                             => ( quantDeMorgan3
                                                                                                               => ( quantDeMorgan4
                                                                                                                 => ( prop2setI
                                                                                                                   => ( prop2set2propI
                                                                                                                     => ( notdexE
                                                                                                                       => ( notdallE
                                                                                                                         => ( exuI1
                                                                                                                           => ( exuI3
                                                                                                                             => ( exuI2
                                                                                                                               => ( inCongP
                                                                                                                                 => ( in__Cong
                                                                                                                                   => ( exuE3u
                                                                                                                                     => ( exu__Cong
                                                                                                                                       => ( emptyset__Cong
                                                                                                                                         => ( setadjoin__Cong
                                                                                                                                           => ( powerset__Cong
                                                                                                                                             => ( setunion__Cong
                                                                                                                                               => ( omega__Cong
                                                                                                                                                 => ( exuEu
                                                                                                                                                   => ( descr__Cong
                                                                                                                                                     => ( dsetconstr__Cong
                                                                                                                                                       => ( subsetI1
                                                                                                                                                         => ( eqimpsubset2
                                                                                                                                                           => ( eqimpsubset1
                                                                                                                                                             => ( subsetI2
                                                                                                                                                               => ( emptysetsubset
                                                                                                                                                                 => ( subsetE
                                                                                                                                                                   => ( subsetE2
                                                                                                                                                                     => ( notsubsetI
                                                                                                                                                                       => ( notequalI1
                                                                                                                                                                         => ( notequalI2
                                                                                                                                                                           => ( subsetRefl
                                                                                                                                                                             => ( subsetTrans
                                                                                                                                                                               => ( setadjoinSub
                                                                                                                                                                                 => ( setadjoinSub2
                                                                                                                                                                                   => ( subset2powerset
                                                                                                                                                                                     => ( setextsub
                                                                                                                                                                                       => ( subsetemptysetimpeq
                                                                                                                                                                                         => ( powersetI1
                                                                                                                                                                                           => ( powersetE1
                                                                                                                                                                                             => ( inPowerset
                                                                                                                                                                                               => ( powersetsubset
                                                                                                                                                                                                 => ( sepInPowerset
                                                                                                                                                                                                   => ( sepSubset
                                                                                                                                                                                                     => ( binunionIL
                                                                                                                                                                                                       => ( upairset2IR
                                                                                                                                                                                                         => ( binunionIR
                                                                                                                                                                                                           => ( binunionEcases
                                                                                                                                                                                                             => ( binunionE
                                                                                                                                                                                                               => ( binunionLsub
                                                                                                                                                                                                                 => ( binunionRsub
                                                                                                                                                                                                                   => ( binintersectI
                                                                                                                                                                                                                     => ( binintersectSubset5
                                                                                                                                                                                                                       => ( binintersectEL
                                                                                                                                                                                                                         => ( binintersectLsub
                                                                                                                                                                                                                           => ( binintersectSubset2
                                                                                                                                                                                                                             => ( binintersectSubset3
                                                                                                                                                                                                                               => ( binintersectER
                                                                                                                                                                                                                                 => ( disjointsetsI1
                                                                                                                                                                                                                                   => ( binintersectRsub
                                                                                                                                                                                                                                     => ( binintersectSubset4
                                                                                                                                                                                                                                       => ( binintersectSubset1
                                                                                                                                                                                                                                         => ( bs114d
                                                                                                                                                                                                                                           => ( setminusI
                                                                                                                                                                                                                                             => ( setminusEL
                                                                                                                                                                                                                                               => ( setminusER
                                                                                                                                                                                                                                                 => ( setminusSubset2
                                                                                                                                                                                                                                                   => ( setminusERneg
                                                                                                                                                                                                                                                     => ( setminusELneg
                                                                                                                                                                                                                                                       => ( setminusILneg
                                                                                                                                                                                                                                                         => ( setminusIRneg
                                                                                                                                                                                                                                                           => ( setminusLsub
                                                                                                                                                                                                                                                             => ( setminusSubset1
                                                                                                                                                                                                                                                               => ( symdiffE
                                                                                                                                                                                                                                                                 => ( symdiffI1
                                                                                                                                                                                                                                                                   => ( symdiffI2
                                                                                                                                                                                                                                                                     => ( symdiffIneg1
                                                                                                                                                                                                                                                                       => ( symdiffIneg2
                                                                                                                                                                                                                                                                         => ( secondinupair
                                                                                                                                                                                                                                                                           => ( setukpairIL
                                                                                                                                                                                                                                                                             => ( setukpairIR
                                                                                                                                                                                                                                                                               => ( kpairiskpair
                                                                                                                                                                                                                                                                                 => ( kpairp
                                                                                                                                                                                                                                                                                   => ( singletonsubset
                                                                                                                                                                                                                                                                                     => ( singletoninpowerset
                                                                                                                                                                                                                                                                                       => ( singletoninpowunion
                                                                                                                                                                                                                                                                                         => ( upairset2E
                                                                                                                                                                                                                                                                                           => ( upairsubunion
                                                                                                                                                                                                                                                                                             => ( upairinpowunion
                                                                                                                                                                                                                                                                                               => ( ubforcartprodlem1
                                                                                                                                                                                                                                                                                                 => ( ubforcartprodlem2
                                                                                                                                                                                                                                                                                                   => ( ubforcartprodlem3
                                                                                                                                                                                                                                                                                                     => ( cartprodpairin
                                                                                                                                                                                                                                                                                                       => ( cartprodmempair1
                                                                                                                                                                                                                                                                                                         => ( cartprodmempair
                                                                                                                                                                                                                                                                                                           => ( setunionE2
                                                                                                                                                                                                                                                                                                             => ( setunionsingleton1
                                                                                                                                                                                                                                                                                                               => ( setunionsingleton2
                                                                                                                                                                                                                                                                                                                 => ( setunionsingleton
                                                                                                                                                                                                                                                                                                                   => ( singletonprop
                                                                                                                                                                                                                                                                                                                     => ( ex1E1
                                                                                                                                                                                                                                                                                                                       => ( ex1I
                                                                                                                                                                                                                                                                                                                         => ( ex1I2
                                                                                                                                                                                                                                                                                                                           => ( singletonsuniq
                                                                                                                                                                                                                                                                                                                             => ( setukpairinjL1
                                                                                                                                                                                                                                                                                                                               => ( kfstsingleton
                                                                                                                                                                                                                                                                                                                                 => ( theprop
                                                                                                                                                                                                                                                                                                                                   => ( kfstpairEq
                                                                                                                                                                                                                                                                                                                                     => ( cartprodfstin
                                                                                                                                                                                                                                                                                                                                       => ( setukpairinjL2
                                                                                                                                                                                                                                                                                                                                         => ( setukpairinjL
                                                                                                                                                                                                                                                                                                                                           => ( setukpairinjR11
                                                                                                                                                                                                                                                                                                                                             => ( setukpairinjR12
                                                                                                                                                                                                                                                                                                                                               => ( setukpairinjR1
                                                                                                                                                                                                                                                                                                                                                 => ( upairequniteq
                                                                                                                                                                                                                                                                                                                                                   => ( setukpairinjR2
                                                                                                                                                                                                                                                                                                                                                     => ( setukpairinjR
                                                                                                                                                                                                                                                                                                                                                       => ( ksndsingleton
                                                                                                                                                                                                                                                                                                                                                         => ( ksndpairEq
                                                                                                                                                                                                                                                                                                                                                           => ( kpairsurjEq
                                                                                                                                                                                                                                                                                                                                                             => ( cartprodsndin
                                                                                                                                                                                                                                                                                                                                                               => ( cartprodpairmemEL
                                                                                                                                                                                                                                                                                                                                                                 => ( cartprodpairmemER
                                                                                                                                                                                                                                                                                                                                                                   => ( cartprodmempaircEq
                                                                                                                                                                                                                                                                                                                                                                     => ( cartprodfstpairEq
                                                                                                                                                                                                                                                                                                                                                                       => ( cartprodsndpairEq
                                                                                                                                                                                                                                                                                                                                                                         => ( cartprodpairsurjEq
                                                                                                                                                                                                                                                                                                                                                                           => ( dpsetconstrI
                                                                                                                                                                                                                                                                                                                                                                             => ( dpsetconstrSub
                                                                                                                                                                                                                                                                                                                                                                               => ( setOfPairsIsBReln
                                                                                                                                                                                                                                                                                                                                                                                 => ( dpsetconstrERa
                                                                                                                                                                                                                                                                                                                                                                                   => ( dpsetconstrEL1
                                                                                                                                                                                                                                                                                                                                                                                     => ( dpsetconstrEL2
                                                                                                                                                                                                                                                                                                                                                                                       => ( dpsetconstrER
                                                                                                                                                                                                                                                                                                                                                                                         => ( funcImageSingleton
                                                                                                                                                                                                                                                                                                                                                                                           => ( apProp
                                                                                                                                                                                                                                                                                                                                                                                             => ( app
                                                                                                                                                                                                                                                                                                                                                                                               => ( infuncsetfunc
                                                                                                                                                                                                                                                                                                                                                                                                 => ( ap2p
                                                                                                                                                                                                                                                                                                                                                                                                   => ( funcinfuncset
                                                                                                                                                                                                                                                                                                                                                                                                     => ( lamProp
                                                                                                                                                                                                                                                                                                                                                                                                       => ( lamp
                                                                                                                                                                                                                                                                                                                                                                                                         => ( lam2p
                                                                                                                                                                                                                                                                                                                                                                                                           => ( brelnall1
                                                                                                                                                                                                                                                                                                                                                                                                             => ( brelnall2
                                                                                                                                                                                                                                                                                                                                                                                                               => ( ex1E2
                                                                                                                                                                                                                                                                                                                                                                                                                 => ( funcGraphProp1
                                                                                                                                                                                                                                                                                                                                                                                                                   => ( funcGraphProp3
                                                                                                                                                                                                                                                                                                                                                                                                                     => ( funcGraphProp2
                                                                                                                                                                                                                                                                                                                                                                                                                       => ( funcextLem
                                                                                                                                                                                                                                                                                                                                                                                                                         => ( funcGraphProp4
                                                                                                                                                                                                                                                                                                                                                                                                                           => ( subbreln
                                                                                                                                                                                                                                                                                                                                                                                                                             => ( eqbreln
                                                                                                                                                                                                                                                                                                                                                                                                                               => ( funcext
                                                                                                                                                                                                                                                                                                                                                                                                                                 => ( funcext2
                                                                                                                                                                                                                                                                                                                                                                                                                                   => ( ap2apEq1
                                                                                                                                                                                                                                                                                                                                                                                                                                     => ( ap2apEq2
                                                                                                                                                                                                                                                                                                                                                                                                                                       => ( beta1
                                                                                                                                                                                                                                                                                                                                                                                                                                         => ( eta1
                                                                                                                                                                                                                                                                                                                                                                                                                                           => ( lam2lamEq
                                                                                                                                                                                                                                                                                                                                                                                                                                             => ( beta2
                                                                                                                                                                                                                                                                                                                                                                                                                                               => ( eta2
                                                                                                                                                                                                                                                                                                                                                                                                                                                 => ( iffalseProp1
                                                                                                                                                                                                                                                                                                                                                                                                                                                   => ( iffalseProp2
                                                                                                                                                                                                                                                                                                                                                                                                                                                     => ( iftrueProp1
                                                                                                                                                                                                                                                                                                                                                                                                                                                       => ( iftrueProp2
                                                                                                                                                                                                                                                                                                                                                                                                                                                         => ( ifSingleton
                                                                                                                                                                                                                                                                                                                                                                                                                                                           => ( ifp
                                                                                                                                                                                                                                                                                                                                                                                                                                                             => ( theeq
                                                                                                                                                                                                                                                                                                                                                                                                                                                               => ( iftrue
                                                                                                                                                                                                                                                                                                                                                                                                                                                                 => ( iffalse
                                                                                                                                                                                                                                                                                                                                                                                                                                                                   => ( iftrueorfalse
                                                                                                                                                                                                                                                                                                                                                                                                                                                                     => ( binintersectT_lem
                                                                                                                                                                                                                                                                                                                                                                                                                                                                       => ( binunionT_lem
                                                                                                                                                                                                                                                                                                                                                                                                                                                                         => ( powersetT_lem
                                                                                                                                                                                                                                                                                                                                                                                                                                                                           => ( setminusT_lem
                                                                                                                                                                                                                                                                                                                                                                                                                                                                             => ( complementT_lem
                                                                                                                                                                                                                                                                                                                                                                                                                                                                               => ( setextT
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 => ( subsetTI
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   => ( powersetTI1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     => ( powersetTE1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       => ( complementTI1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         => ( complementTE1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           => ( binintersectTELcontra
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             => ( binintersectTERcontra
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               => ( contrasubsetT
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 => ( contrasubsetT1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   => ( contrasubsetT2
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     => ( contrasubsetT3
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       => ( doubleComplementI1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         => ( doubleComplementE1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           => ( doubleComplementSub1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             => ( doubleComplementSub2
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               => ( doubleComplementEq
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 => ( complementTnotintersectT
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   => ( complementImpComplementIntersect
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     => ( complementSubsetComplementIntersect
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       => ( complementInPowersetComplementIntersect
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         => ( contraSubsetComplement
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           => ( complementTcontraSubset
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             => ( binunionTILcontra
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               => ( binunionTIRcontra
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 => ( inIntersectImpInUnion
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   => ( inIntersectImpInUnion2
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     => ( inIntersectImpInIntersectUnions
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       => ( intersectInPowersetIntersectUnions
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         => ( inComplementUnionImpNotIn1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           => ( inComplementUnionImpInComplement1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             => ( binunionTE
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               => ( binunionTEcontra
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 => ( demorgan2a1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   => ( complementUnionInPowersetComplement
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     => ( demorgan2a2
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       => ( demorgan1a
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         => ( demorgan1b
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           => ( demorgan1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             => ( demorgan2a
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               => ( demorgan2b2
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 => ( demorgan2b
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   => ( demorgan2
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     => ( woz13rule0
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       => ( woz13rule1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         => ( woz13rule2
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           => ( woz13rule3
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             => ( woz13rule4
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               => ( woz1_1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 => ( woz1_2
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   => ( woz1_3
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     => ( woz1_4
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       => ( woz1_5
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         => ( breln1all2
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           => ( breln1SetBreln1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             => ( choice2fnsingleton
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               => ( setOfPairsIsBReln1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 => ( breln1all1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   => ( subbreln1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     => ( eqbreln1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       => ( breln1invprop
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         => ( breln1invI
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           => ( breln1invE
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             => ( breln1compprop
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               => ( breln1compI
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 => ( breln1compE
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   => ( breln1compEex
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     => ( breln1unionprop
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       => ( breln1unionIL
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         => ( breln1unionIR
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           => ( breln1unionI
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             => ( breln1unionE
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               => ( breln1unionEcases
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 => ( breln1unionCommutes
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   => ( woz2Ex
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     => ( woz2W
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       => ( woz2A
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         => ( woz2B
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           => ( image1Ex
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             => ( image1Ex1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               => ( image1Equiv
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 => ( image1E
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   => ( image1I
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     => ( injFuncInInjFuncSet
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       => ( injFuncSetFuncIn
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         => ! [Xx: $i,Xy: $i,Xf: $i] :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ( ( in @ Xf @ ( injFuncSet @ Xx @ Xy ) )
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             => ( injective @ Xx @ Xy @ Xf ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )).

%------------------------------------------------------------------------------