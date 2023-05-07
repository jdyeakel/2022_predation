(* Created with the Wolfram Language : www.wolfram.com *)
{{P -> 0, C1 -> 0, C2 -> 0, R -> 0}, 
 {P -> -((\[Mu]1[Mp] + \[Sigma]1[Mp])/(w*bmort1[Mp])), 
  C1 -> \[Mu]Pred[Mp]/(w*bPred1[Mp]), C2 -> 0, R -> 0}, 
 {P -> (\[Mu]2[Mp] + \[Sigma]2[Mp])/((-1 + w)*bmort2[Mp]), C1 -> 0, 
  C2 -> \[Mu]Pred[Mp]/(bPred2[Mp] - w*bPred2[Mp]), R -> 0}, 
 {P -> 0, C1 -> 0, C2 -> 0, R -> kRes}, 
 {P -> -1/8*(3*kRes^2*w*\[Alpha]*bPred1[Mp]*Y1[Mp] + 
      kRes^(3/2)*Sqrt[w]*Sqrt[\[Alpha]]*Sqrt[bPred1[Mp]]*Sqrt[Y1[Mp]]*
       Sqrt[9*kRes*w*\[Alpha]*bPred1[Mp]*Y1[Mp] - 16*\[Lambda]1[Mp]*
          \[Mu]Pred[Mp]] + (2*Sqrt[kRes]*\[Mu]Pred[Mp]*
        Sqrt[9*kRes*w*\[Alpha]*bPred1[Mp]*Y1[Mp] - 16*\[Lambda]1[Mp]*
           \[Mu]Pred[Mp]]*\[Sigma]1[Mp])/(Sqrt[w]*Sqrt[\[Alpha]]*
        Sqrt[bPred1[Mp]]*Sqrt[Y1[Mp]]) + 2*kRes*\[Mu]Pred[Mp]*
       (-4*\[Lambda]1[Mp] + 4*\[Mu]1[Mp] + 3*\[Sigma]1[Mp]))/
     (kRes*w*bmort1[Mp]*\[Mu]Pred[Mp]), C1 -> \[Mu]Pred[Mp]/(w*bPred1[Mp]), 
  C2 -> 0, 
  R -> kRes/4 - (Sqrt[kRes]*Sqrt[9*kRes*w*\[Alpha]*bPred1[Mp]*Y1[Mp] - 
        16*\[Lambda]1[Mp]*\[Mu]Pred[Mp]])/(4*Sqrt[w]*Sqrt[\[Alpha]]*
      Sqrt[bPred1[Mp]]*Sqrt[Y1[Mp]])}, 
 {P -> (-3*kRes^2*w*\[Alpha]*bPred1[Mp]*Y1[Mp] + kRes^(3/2)*Sqrt[w]*
      Sqrt[\[Alpha]]*Sqrt[bPred1[Mp]]*Sqrt[Y1[Mp]]*
      Sqrt[9*kRes*w*\[Alpha]*bPred1[Mp]*Y1[Mp] - 16*\[Lambda]1[Mp]*
         \[Mu]Pred[Mp]] + 2*kRes*\[Mu]Pred[Mp]*(4*\[Lambda]1[Mp] - 
       4*\[Mu]1[Mp] - 3*\[Sigma]1[Mp]) + 
     (2*Sqrt[kRes]*\[Mu]Pred[Mp]*Sqrt[9*kRes*w*\[Alpha]*bPred1[Mp]*Y1[Mp] - 
         16*\[Lambda]1[Mp]*\[Mu]Pred[Mp]]*\[Sigma]1[Mp])/
      (Sqrt[w]*Sqrt[\[Alpha]]*Sqrt[bPred1[Mp]]*Sqrt[Y1[Mp]]))/
    (8*kRes*w*bmort1[Mp]*\[Mu]Pred[Mp]), C1 -> \[Mu]Pred[Mp]/(w*bPred1[Mp]), 
  C2 -> 0, 
  R -> kRes/4 + (Sqrt[kRes]*Sqrt[9*kRes*w*\[Alpha]*bPred1[Mp]*Y1[Mp] - 
        16*\[Lambda]1[Mp]*\[Mu]Pred[Mp]])/(4*Sqrt[w]*Sqrt[\[Alpha]]*
      Sqrt[bPred1[Mp]]*Sqrt[Y1[Mp]])}, 
 {P -> -1/8*((8*\[Lambda]2[Mp])/(-1 + w) - (8*\[Mu]2[Mp])/(-1 + w) + 
      ((3*Sqrt[kRes]*Sqrt[-1 + w]*Sqrt[\[Alpha]]*Sqrt[bPred2[Mp]]*
          Sqrt[Y2[Mp]] + Sqrt[9*kRes*(-1 + w)*\[Alpha]*bPred2[Mp]*Y2[Mp] + 
           16*\[Lambda]2[Mp]*\[Mu]Pred[Mp]])*(kRes*(-1 + w)*\[Alpha]*
          bPred2[Mp]*Y2[Mp] - 2*\[Mu]Pred[Mp]*\[Sigma]2[Mp]))/
       (Sqrt[kRes]*(-1 + w)^(3/2)*Sqrt[\[Alpha]]*Sqrt[bPred2[Mp]]*
        Sqrt[Y2[Mp]]*\[Mu]Pred[Mp]))/bmort2[Mp], C1 -> 0, 
  C2 -> \[Mu]Pred[Mp]/(bPred2[Mp] - w*bPred2[Mp]), 
  R -> kRes/4 - (Sqrt[kRes]*Sqrt[9*kRes*(-1 + w)*\[Alpha]*bPred2[Mp]*Y2[Mp] + 
        16*\[Lambda]2[Mp]*\[Mu]Pred[Mp]])/(4*Sqrt[-1 + w]*Sqrt[\[Alpha]]*
      Sqrt[bPred2[Mp]]*Sqrt[Y2[Mp]])}, 
 {P -> ((-8*\[Lambda]2[Mp])/(-1 + w) + (8*\[Mu]2[Mp])/(-1 + w) + 
     ((-3*Sqrt[kRes]*Sqrt[-1 + w]*Sqrt[\[Alpha]]*Sqrt[bPred2[Mp]]*
         Sqrt[Y2[Mp]] + Sqrt[9*kRes*(-1 + w)*\[Alpha]*bPred2[Mp]*Y2[Mp] + 
          16*\[Lambda]2[Mp]*\[Mu]Pred[Mp]])*
       (kRes*(-1 + w)*\[Alpha]*bPred2[Mp]*Y2[Mp] - 2*\[Mu]Pred[Mp]*
         \[Sigma]2[Mp]))/(Sqrt[kRes]*(-1 + w)^(3/2)*Sqrt[\[Alpha]]*
       Sqrt[bPred2[Mp]]*Sqrt[Y2[Mp]]*\[Mu]Pred[Mp]))/(8*bmort2[Mp]), C1 -> 0, 
  C2 -> \[Mu]Pred[Mp]/(bPred2[Mp] - w*bPred2[Mp]), 
  R -> kRes/4 + (Sqrt[kRes]*Sqrt[9*kRes*(-1 + w)*\[Alpha]*bPred2[Mp]*Y2[Mp] + 
        16*\[Lambda]2[Mp]*\[Mu]Pred[Mp]])/(4*Sqrt[-1 + w]*Sqrt[\[Alpha]]*
      Sqrt[bPred2[Mp]]*Sqrt[Y2[Mp]])}, 
 {P -> 0, C1 -> (\[Alpha]*Y1[Mp]*(-2*kRes*\[Lambda]1[Mp]^2 + 
      \[Lambda]1[Mp]*(4*kRes*\[Mu]1[Mp] + kRes*\[Sigma]1[Mp] - 
        Sqrt[kRes^2*(4*\[Lambda]1[Mp]^2 - 4*\[Lambda]1[Mp]*
            (2*\[Mu]1[Mp] + \[Sigma]1[Mp]) + (2*\[Mu]1[Mp] + 3*\[Sigma]1[Mp])^
            2)]) + \[Mu]1[Mp]*(-2*kRes*\[Mu]1[Mp] - 3*kRes*\[Sigma]1[Mp] + 
        Sqrt[kRes^2*(4*\[Lambda]1[Mp]^2 - 4*\[Lambda]1[Mp]*
            (2*\[Mu]1[Mp] + \[Sigma]1[Mp]) + (2*\[Mu]1[Mp] + 3*\[Sigma]1[Mp])^
            2)])))/(4*\[Lambda]1[Mp]*\[Sigma]1[Mp]^2), C2 -> 0, 
  R -> (kRes*(-2*\[Lambda]1[Mp] + 2*\[Mu]1[Mp] + \[Sigma]1[Mp]) - 
     Sqrt[kRes^2*(4*\[Lambda]1[Mp]^2 - 4*\[Lambda]1[Mp]*
         (2*\[Mu]1[Mp] + \[Sigma]1[Mp]) + (2*\[Mu]1[Mp] + 3*\[Sigma]1[Mp])^
         2)])/(4*\[Sigma]1[Mp])}, 
 {P -> 0, C1 -> (\[Alpha]*Y1[Mp]*(-2*kRes*\[Lambda]1[Mp]^2 + 
      \[Lambda]1[Mp]*(4*kRes*\[Mu]1[Mp] + kRes*\[Sigma]1[Mp] + 
        Sqrt[kRes^2*(4*\[Lambda]1[Mp]^2 - 4*\[Lambda]1[Mp]*
            (2*\[Mu]1[Mp] + \[Sigma]1[Mp]) + (2*\[Mu]1[Mp] + 3*\[Sigma]1[Mp])^
            2)]) - \[Mu]1[Mp]*(2*kRes*\[Mu]1[Mp] + 3*kRes*\[Sigma]1[Mp] + 
        Sqrt[kRes^2*(4*\[Lambda]1[Mp]^2 - 4*\[Lambda]1[Mp]*
            (2*\[Mu]1[Mp] + \[Sigma]1[Mp]) + (2*\[Mu]1[Mp] + 3*\[Sigma]1[Mp])^
            2)])))/(4*\[Lambda]1[Mp]*\[Sigma]1[Mp]^2), C2 -> 0, 
  R -> (kRes*(-2*\[Lambda]1[Mp] + 2*\[Mu]1[Mp] + \[Sigma]1[Mp]) + 
     Sqrt[kRes^2*(4*\[Lambda]1[Mp]^2 - 4*\[Lambda]1[Mp]*
         (2*\[Mu]1[Mp] + \[Sigma]1[Mp]) + (2*\[Mu]1[Mp] + 3*\[Sigma]1[Mp])^
         2)])/(4*\[Sigma]1[Mp])}, {P -> 0, C1 -> 0, 
  C2 -> (\[Alpha]*Y2[Mp]*(-2*kRes*\[Lambda]2[Mp]^2 + 
      \[Lambda]2[Mp]*(4*kRes*\[Mu]2[Mp] + kRes*\[Sigma]2[Mp] - 
        Sqrt[kRes^2*(4*\[Lambda]2[Mp]^2 - 4*\[Lambda]2[Mp]*
            (2*\[Mu]2[Mp] + \[Sigma]2[Mp]) + (2*\[Mu]2[Mp] + 3*\[Sigma]2[Mp])^
            2)]) + \[Mu]2[Mp]*(-2*kRes*\[Mu]2[Mp] - 3*kRes*\[Sigma]2[Mp] + 
        Sqrt[kRes^2*(4*\[Lambda]2[Mp]^2 - 4*\[Lambda]2[Mp]*
            (2*\[Mu]2[Mp] + \[Sigma]2[Mp]) + (2*\[Mu]2[Mp] + 3*\[Sigma]2[Mp])^
            2)])))/(4*\[Lambda]2[Mp]*\[Sigma]2[Mp]^2), 
  R -> (kRes*(-2*\[Lambda]2[Mp] + 2*\[Mu]2[Mp] + \[Sigma]2[Mp]) - 
     Sqrt[kRes^2*(4*\[Lambda]2[Mp]^2 - 4*\[Lambda]2[Mp]*
         (2*\[Mu]2[Mp] + \[Sigma]2[Mp]) + (2*\[Mu]2[Mp] + 3*\[Sigma]2[Mp])^
         2)])/(4*\[Sigma]2[Mp])}, {P -> 0, C1 -> 0, 
  C2 -> (\[Alpha]*Y2[Mp]*(-2*kRes*\[Lambda]2[Mp]^2 + 
      \[Lambda]2[Mp]*(4*kRes*\[Mu]2[Mp] + kRes*\[Sigma]2[Mp] + 
        Sqrt[kRes^2*(4*\[Lambda]2[Mp]^2 - 4*\[Lambda]2[Mp]*
            (2*\[Mu]2[Mp] + \[Sigma]2[Mp]) + (2*\[Mu]2[Mp] + 3*\[Sigma]2[Mp])^
            2)]) - \[Mu]2[Mp]*(2*kRes*\[Mu]2[Mp] + 3*kRes*\[Sigma]2[Mp] + 
        Sqrt[kRes^2*(4*\[Lambda]2[Mp]^2 - 4*\[Lambda]2[Mp]*
            (2*\[Mu]2[Mp] + \[Sigma]2[Mp]) + (2*\[Mu]2[Mp] + 3*\[Sigma]2[Mp])^
            2)])))/(4*\[Lambda]2[Mp]*\[Sigma]2[Mp]^2), 
  R -> (kRes*(-2*\[Lambda]2[Mp] + 2*\[Mu]2[Mp] + \[Sigma]2[Mp]) + 
     Sqrt[kRes^2*(4*\[Lambda]2[Mp]^2 - 4*\[Lambda]2[Mp]*
         (2*\[Mu]2[Mp] + \[Sigma]2[Mp]) + (2*\[Mu]2[Mp] + 3*\[Sigma]2[Mp])^
         2)])/(4*\[Sigma]2[Mp])}, 
 {P -> (kRes*(-1 + w)*bmort2[Mp]*
      (-(\[Sigma]1[Mp]*(2*\[Lambda]1[Mp]*(\[Lambda]2[Mp] - 2*\[Mu]2[Mp]) + 
          \[Lambda]2[Mp]*(2*\[Mu]1[Mp] + 3*\[Sigma]1[Mp]))) + 
       \[Lambda]1[Mp]*(2*\[Lambda]1[Mp] - 2*\[Mu]1[Mp] + 3*\[Sigma]1[Mp])*
        \[Sigma]2[Mp]) + kRes*w*bmort1[Mp]*
      (2*\[Lambda]2[Mp]*(-\[Lambda]2[Mp] + \[Mu]2[Mp])*\[Sigma]1[Mp] + 
       (2*\[Lambda]1[Mp]*(\[Lambda]2[Mp] + \[Mu]2[Mp]) - 
         \[Lambda]2[Mp]*(4*\[Mu]1[Mp] + 3*\[Sigma]1[Mp]))*\[Sigma]2[Mp] + 
       3*\[Lambda]1[Mp]*\[Sigma]2[Mp]^2) + \[Lambda]2[Mp]*\[Sigma]1[Mp]*
      Sqrt[kRes^2*(8*((-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + 
           w*bmort1[Mp]*\[Sigma]2[Mp])*((-1 + w)*bmort2[Mp]*
            (\[Mu]1[Mp] + \[Sigma]1[Mp]) + w*bmort1[Mp]*(\[Mu]2[Mp] + 
             \[Sigma]2[Mp])) + ((-1 + w)*bmort2[Mp]*(2*\[Lambda]1[Mp] - 
             2*\[Mu]1[Mp] - \[Sigma]1[Mp]) - w*bmort1[Mp]*
            (-2*\[Lambda]2[Mp] + 2*\[Mu]2[Mp] + \[Sigma]2[Mp]))^2)] - 
     \[Lambda]1[Mp]*\[Sigma]2[Mp]*
      Sqrt[kRes^2*(8*((-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + 
           w*bmort1[Mp]*\[Sigma]2[Mp])*((-1 + w)*bmort2[Mp]*
            (\[Mu]1[Mp] + \[Sigma]1[Mp]) + w*bmort1[Mp]*(\[Mu]2[Mp] + 
             \[Sigma]2[Mp])) + ((-1 + w)*bmort2[Mp]*(2*\[Lambda]1[Mp] - 
             2*\[Mu]1[Mp] - \[Sigma]1[Mp]) - w*bmort1[Mp]*
            (-2*\[Lambda]2[Mp] + 2*\[Mu]2[Mp] + \[Sigma]2[Mp]))^2)])/
    (4*kRes*((-1 + w)*bmort2[Mp]*\[Lambda]1[Mp] + 
      w*bmort1[Mp]*\[Lambda]2[Mp])*((-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + 
      w*bmort1[Mp]*\[Sigma]2[Mp])), 
  C1 -> (Y1[Mp]*((-1 + w)^2*bmort2[Mp]^2*(4*\[Lambda]2[Mp]*\[Mu]Pred[Mp]*
         \[Sigma]1[Mp]^2 + kRes*(-1 + w)*\[Alpha]*bPred2[Mp]*Y2[Mp]*
         (-2*(\[Lambda]1[Mp] - \[Mu]1[Mp])^2 + 
          (\[Lambda]1[Mp] - 3*\[Mu]1[Mp])*\[Sigma]1[Mp])) + 
      (-1 + w)*bmort2[Mp]*(w*bmort1[Mp]*(8*\[Lambda]2[Mp]*\[Mu]Pred[Mp]*
           \[Sigma]1[Mp]*\[Sigma]2[Mp] + kRes*(-1 + w)*\[Alpha]*bPred2[Mp]*
           Y2[Mp]*(-4*\[Mu]1[Mp]*\[Mu]2[Mp] - 3*\[Mu]2[Mp]*\[Sigma]1[Mp] + 
            \[Lambda]2[Mp]*(4*\[Mu]1[Mp] + \[Sigma]1[Mp]) - 
            3*\[Mu]1[Mp]*\[Sigma]2[Mp] + \[Lambda]1[Mp]*(-4*\[Lambda]2[Mp] + 
              4*\[Mu]2[Mp] + \[Sigma]2[Mp]))) + (-1 + w)*\[Alpha]*bPred2[Mp]*
         Y2[Mp]*(\[Lambda]1[Mp] - \[Mu]1[Mp])*
         Sqrt[kRes^2*((-1 + w)^2*bmort2[Mp]^2*(4*\[Lambda]1[Mp]^2 - 
              4*\[Lambda]1[Mp]*(2*\[Mu]1[Mp] + \[Sigma]1[Mp]) + 
              (2*\[Mu]1[Mp] + 3*\[Sigma]1[Mp])^2) + w^2*bmort1[Mp]^2*
             (4*\[Lambda]2[Mp]^2 - 4*\[Lambda]2[Mp]*(2*\[Mu]2[Mp] + 
                \[Sigma]2[Mp]) + (2*\[Mu]2[Mp] + 3*\[Sigma]2[Mp])^2) + 
            2*(-1 + w)*w*bmort1[Mp]*bmort2[Mp]*(-2*\[Lambda]2[Mp]*(
                2*\[Mu]1[Mp] + \[Sigma]1[Mp]) + (2*\[Mu]1[Mp] + 
                3*\[Sigma]1[Mp])*(2*\[Mu]2[Mp] + 3*\[Sigma]2[Mp]) + 
              \[Lambda]1[Mp]*(4*\[Lambda]2[Mp] - 2*(2*\[Mu]2[Mp] + 
                  \[Sigma]2[Mp]))))]) + w*bmort1[Mp]*
       (w*bmort1[Mp]*(4*\[Lambda]2[Mp]*\[Mu]Pred[Mp]*\[Sigma]2[Mp]^2 + 
          kRes*(-1 + w)*\[Alpha]*bPred2[Mp]*Y2[Mp]*
           (-2*(\[Lambda]2[Mp] - \[Mu]2[Mp])^2 + (\[Lambda]2[Mp] - 
              3*\[Mu]2[Mp])*\[Sigma]2[Mp])) + (-1 + w)*\[Alpha]*bPred2[Mp]*
         Y2[Mp]*(\[Lambda]2[Mp] - \[Mu]2[Mp])*
         Sqrt[kRes^2*((-1 + w)^2*bmort2[Mp]^2*(4*\[Lambda]1[Mp]^2 - 
              4*\[Lambda]1[Mp]*(2*\[Mu]1[Mp] + \[Sigma]1[Mp]) + 
              (2*\[Mu]1[Mp] + 3*\[Sigma]1[Mp])^2) + w^2*bmort1[Mp]^2*
             (4*\[Lambda]2[Mp]^2 - 4*\[Lambda]2[Mp]*(2*\[Mu]2[Mp] + 
                \[Sigma]2[Mp]) + (2*\[Mu]2[Mp] + 3*\[Sigma]2[Mp])^2) + 
            2*(-1 + w)*w*bmort1[Mp]*bmort2[Mp]*(-2*\[Lambda]2[Mp]*(
                2*\[Mu]1[Mp] + \[Sigma]1[Mp]) + (2*\[Mu]1[Mp] + 
                3*\[Sigma]1[Mp])*(2*\[Mu]2[Mp] + 3*\[Sigma]2[Mp]) + 
              \[Lambda]1[Mp]*(4*\[Lambda]2[Mp] - 2*(2*\[Mu]2[Mp] + 
                  \[Sigma]2[Mp]))))])))/
    (4*((-1 + w)*bPred2[Mp]*Y2[Mp]*\[Lambda]1[Mp] + w*bPred1[Mp]*Y1[Mp]*
       \[Lambda]2[Mp])*((-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + 
       w*bmort1[Mp]*\[Sigma]2[Mp])^2), 
  C2 -> (Y2[Mp]*(-4*\[Lambda]1[Mp]*\[Mu]Pred[Mp]*
       ((-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + w*bmort1[Mp]*\[Sigma]2[Mp])^2 - 
      w*\[Alpha]*bmort2[Mp]*bPred1[Mp]*Y1[Mp]*\[Lambda]1[Mp]*
       Sqrt[kRes^2*(8*((-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + 
            w*bmort1[Mp]*\[Sigma]2[Mp])*((-1 + w)*bmort2[Mp]*
             (\[Mu]1[Mp] + \[Sigma]1[Mp]) + w*bmort1[Mp]*(\[Mu]2[Mp] + 
              \[Sigma]2[Mp])) + ((-1 + w)*bmort2[Mp]*(2*\[Lambda]1[Mp] - 
              2*\[Mu]1[Mp] - \[Sigma]1[Mp]) - w*bmort1[Mp]*
             (-2*\[Lambda]2[Mp] + 2*\[Mu]2[Mp] + \[Sigma]2[Mp]))^2)] + 
      w^2*\[Alpha]*bmort2[Mp]*bPred1[Mp]*Y1[Mp]*\[Lambda]1[Mp]*
       Sqrt[kRes^2*(8*((-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + 
            w*bmort1[Mp]*\[Sigma]2[Mp])*((-1 + w)*bmort2[Mp]*
             (\[Mu]1[Mp] + \[Sigma]1[Mp]) + w*bmort1[Mp]*(\[Mu]2[Mp] + 
              \[Sigma]2[Mp])) + ((-1 + w)*bmort2[Mp]*(2*\[Lambda]1[Mp] - 
              2*\[Mu]1[Mp] - \[Sigma]1[Mp]) - w*bmort1[Mp]*
             (-2*\[Lambda]2[Mp] + 2*\[Mu]2[Mp] + \[Sigma]2[Mp]))^2)] + 
      w^2*\[Alpha]*bmort1[Mp]*bPred1[Mp]*Y1[Mp]*\[Lambda]2[Mp]*
       Sqrt[kRes^2*(8*((-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + 
            w*bmort1[Mp]*\[Sigma]2[Mp])*((-1 + w)*bmort2[Mp]*
             (\[Mu]1[Mp] + \[Sigma]1[Mp]) + w*bmort1[Mp]*(\[Mu]2[Mp] + 
              \[Sigma]2[Mp])) + ((-1 + w)*bmort2[Mp]*(2*\[Lambda]1[Mp] - 
              2*\[Mu]1[Mp] - \[Sigma]1[Mp]) - w*bmort1[Mp]*
             (-2*\[Lambda]2[Mp] + 2*\[Mu]2[Mp] + \[Sigma]2[Mp]))^2)] + 
      w*\[Alpha]*bmort2[Mp]*bPred1[Mp]*Y1[Mp]*\[Mu]1[Mp]*
       Sqrt[kRes^2*(8*((-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + 
            w*bmort1[Mp]*\[Sigma]2[Mp])*((-1 + w)*bmort2[Mp]*
             (\[Mu]1[Mp] + \[Sigma]1[Mp]) + w*bmort1[Mp]*(\[Mu]2[Mp] + 
              \[Sigma]2[Mp])) + ((-1 + w)*bmort2[Mp]*(2*\[Lambda]1[Mp] - 
              2*\[Mu]1[Mp] - \[Sigma]1[Mp]) - w*bmort1[Mp]*
             (-2*\[Lambda]2[Mp] + 2*\[Mu]2[Mp] + \[Sigma]2[Mp]))^2)] - 
      w^2*\[Alpha]*bmort2[Mp]*bPred1[Mp]*Y1[Mp]*\[Mu]1[Mp]*
       Sqrt[kRes^2*(8*((-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + 
            w*bmort1[Mp]*\[Sigma]2[Mp])*((-1 + w)*bmort2[Mp]*
             (\[Mu]1[Mp] + \[Sigma]1[Mp]) + w*bmort1[Mp]*(\[Mu]2[Mp] + 
              \[Sigma]2[Mp])) + ((-1 + w)*bmort2[Mp]*(2*\[Lambda]1[Mp] - 
              2*\[Mu]1[Mp] - \[Sigma]1[Mp]) - w*bmort1[Mp]*
             (-2*\[Lambda]2[Mp] + 2*\[Mu]2[Mp] + \[Sigma]2[Mp]))^2)] - 
      w^2*\[Alpha]*bmort1[Mp]*bPred1[Mp]*Y1[Mp]*\[Mu]2[Mp]*
       Sqrt[kRes^2*(8*((-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + 
            w*bmort1[Mp]*\[Sigma]2[Mp])*((-1 + w)*bmort2[Mp]*
             (\[Mu]1[Mp] + \[Sigma]1[Mp]) + w*bmort1[Mp]*(\[Mu]2[Mp] + 
              \[Sigma]2[Mp])) + ((-1 + w)*bmort2[Mp]*(2*\[Lambda]1[Mp] - 
              2*\[Mu]1[Mp] - \[Sigma]1[Mp]) - w*bmort1[Mp]*
             (-2*\[Lambda]2[Mp] + 2*\[Mu]2[Mp] + \[Sigma]2[Mp]))^2)] + 
      kRes*w*\[Alpha]*bPred1[Mp]*Y1[Mp]*((-1 + w)^2*bmort2[Mp]^2*
         (-2*(\[Lambda]1[Mp] - \[Mu]1[Mp])^2 + 
          (\[Lambda]1[Mp] - 3*\[Mu]1[Mp])*\[Sigma]1[Mp]) + 
        w^2*bmort1[Mp]^2*(-2*(\[Lambda]2[Mp] - \[Mu]2[Mp])^2 + 
          (\[Lambda]2[Mp] - 3*\[Mu]2[Mp])*\[Sigma]2[Mp]) + 
        (-1 + w)*w*bmort1[Mp]*bmort2[Mp]*(-4*\[Mu]1[Mp]*\[Mu]2[Mp] - 
          3*\[Mu]2[Mp]*\[Sigma]1[Mp] + \[Lambda]2[Mp]*(4*\[Mu]1[Mp] + 
            \[Sigma]1[Mp]) - 3*\[Mu]1[Mp]*\[Sigma]2[Mp] + 
          \[Lambda]1[Mp]*(-4*\[Lambda]2[Mp] + 4*\[Mu]2[Mp] + 
            \[Sigma]2[Mp])))))/
    (4*((-1 + w)*bPred2[Mp]*Y2[Mp]*\[Lambda]1[Mp] + w*bPred1[Mp]*Y1[Mp]*
       \[Lambda]2[Mp])*((-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + 
       w*bmort1[Mp]*\[Sigma]2[Mp])^2), 
  R -> (-(kRes*(-1 + w)*bmort2[Mp]*(2*\[Lambda]1[Mp] - 2*\[Mu]1[Mp] - 
        \[Sigma]1[Mp])) + kRes*w*bmort1[Mp]*(-2*\[Lambda]2[Mp] + 
       2*\[Mu]2[Mp] + \[Sigma]2[Mp]) + 
     Sqrt[kRes^2*(8*((-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + 
          w*bmort1[Mp]*\[Sigma]2[Mp])*((-1 + w)*bmort2[Mp]*
           (\[Mu]1[Mp] + \[Sigma]1[Mp]) + w*bmort1[Mp]*(\[Mu]2[Mp] + 
            \[Sigma]2[Mp])) + ((-1 + w)*bmort2[Mp]*(2*\[Lambda]1[Mp] - 
            2*\[Mu]1[Mp] - \[Sigma]1[Mp]) - w*bmort1[Mp]*(-2*\[Lambda]2[Mp] + 
            2*\[Mu]2[Mp] + \[Sigma]2[Mp]))^2)])/
    (4*(-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + 4*w*bmort1[Mp]*\[Sigma]2[Mp])}, 
 {P -> (kRes*(-1 + w)*bmort2[Mp]*
      (-(\[Sigma]1[Mp]*(2*\[Lambda]1[Mp]*(\[Lambda]2[Mp] - 2*\[Mu]2[Mp]) + 
          \[Lambda]2[Mp]*(2*\[Mu]1[Mp] + 3*\[Sigma]1[Mp]))) + 
       \[Lambda]1[Mp]*(2*\[Lambda]1[Mp] - 2*\[Mu]1[Mp] + 3*\[Sigma]1[Mp])*
        \[Sigma]2[Mp]) + kRes*w*bmort1[Mp]*
      (2*\[Lambda]2[Mp]*(-\[Lambda]2[Mp] + \[Mu]2[Mp])*\[Sigma]1[Mp] + 
       (2*\[Lambda]1[Mp]*(\[Lambda]2[Mp] + \[Mu]2[Mp]) - 
         \[Lambda]2[Mp]*(4*\[Mu]1[Mp] + 3*\[Sigma]1[Mp]))*\[Sigma]2[Mp] + 
       3*\[Lambda]1[Mp]*\[Sigma]2[Mp]^2) - \[Lambda]2[Mp]*\[Sigma]1[Mp]*
      Sqrt[kRes^2*(8*((-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + 
           w*bmort1[Mp]*\[Sigma]2[Mp])*((-1 + w)*bmort2[Mp]*
            (\[Mu]1[Mp] + \[Sigma]1[Mp]) + w*bmort1[Mp]*(\[Mu]2[Mp] + 
             \[Sigma]2[Mp])) + ((-1 + w)*bmort2[Mp]*(2*\[Lambda]1[Mp] - 
             2*\[Mu]1[Mp] - \[Sigma]1[Mp]) - w*bmort1[Mp]*
            (-2*\[Lambda]2[Mp] + 2*\[Mu]2[Mp] + \[Sigma]2[Mp]))^2)] + 
     \[Lambda]1[Mp]*\[Sigma]2[Mp]*
      Sqrt[kRes^2*(8*((-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + 
           w*bmort1[Mp]*\[Sigma]2[Mp])*((-1 + w)*bmort2[Mp]*
            (\[Mu]1[Mp] + \[Sigma]1[Mp]) + w*bmort1[Mp]*(\[Mu]2[Mp] + 
             \[Sigma]2[Mp])) + ((-1 + w)*bmort2[Mp]*(2*\[Lambda]1[Mp] - 
             2*\[Mu]1[Mp] - \[Sigma]1[Mp]) - w*bmort1[Mp]*
            (-2*\[Lambda]2[Mp] + 2*\[Mu]2[Mp] + \[Sigma]2[Mp]))^2)])/
    (4*kRes*((-1 + w)*bmort2[Mp]*\[Lambda]1[Mp] + 
      w*bmort1[Mp]*\[Lambda]2[Mp])*((-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + 
      w*bmort1[Mp]*\[Sigma]2[Mp])), 
  C1 -> -1/4*(Y1[Mp]*((-1 + w)^2*bmort2[Mp]^2*
        (-4*\[Lambda]2[Mp]*\[Mu]Pred[Mp]*\[Sigma]1[Mp]^2 + 
         kRes*(-1 + w)*\[Alpha]*bPred2[Mp]*Y2[Mp]*
          (2*(\[Lambda]1[Mp] - \[Mu]1[Mp])^2 - (\[Lambda]1[Mp] - 
             3*\[Mu]1[Mp])*\[Sigma]1[Mp])) + (-1 + w)*bmort2[Mp]*
        (w*bmort1[Mp]*(-8*\[Lambda]2[Mp]*\[Mu]Pred[Mp]*\[Sigma]1[Mp]*
            \[Sigma]2[Mp] + kRes*(-1 + w)*\[Alpha]*bPred2[Mp]*Y2[Mp]*
            (4*\[Mu]1[Mp]*\[Mu]2[Mp] + 3*\[Mu]2[Mp]*\[Sigma]1[Mp] - 
             \[Lambda]2[Mp]*(4*\[Mu]1[Mp] + \[Sigma]1[Mp]) + 
             \[Lambda]1[Mp]*(4*\[Lambda]2[Mp] - 4*\[Mu]2[Mp] - \[Sigma]2[
                Mp]) + 3*\[Mu]1[Mp]*\[Sigma]2[Mp])) + (-1 + w)*\[Alpha]*
          bPred2[Mp]*Y2[Mp]*(\[Lambda]1[Mp] - \[Mu]1[Mp])*
          Sqrt[kRes^2*((-1 + w)^2*bmort2[Mp]^2*(4*\[Lambda]1[Mp]^2 - 4*
                \[Lambda]1[Mp]*(2*\[Mu]1[Mp] + \[Sigma]1[Mp]) + 
               (2*\[Mu]1[Mp] + 3*\[Sigma]1[Mp])^2) + w^2*bmort1[Mp]^2*
              (4*\[Lambda]2[Mp]^2 - 4*\[Lambda]2[Mp]*(2*\[Mu]2[Mp] + 
                 \[Sigma]2[Mp]) + (2*\[Mu]2[Mp] + 3*\[Sigma]2[Mp])^2) + 
             2*(-1 + w)*w*bmort1[Mp]*bmort2[Mp]*(-2*\[Lambda]2[Mp]*
                (2*\[Mu]1[Mp] + \[Sigma]1[Mp]) + (2*\[Mu]1[Mp] + 
                 3*\[Sigma]1[Mp])*(2*\[Mu]2[Mp] + 3*\[Sigma]2[Mp]) + 
               \[Lambda]1[Mp]*(4*\[Lambda]2[Mp] - 2*(2*\[Mu]2[Mp] + 
                   \[Sigma]2[Mp]))))]) + w*bmort1[Mp]*
        (w*bmort1[Mp]*(-4*\[Lambda]2[Mp]*\[Mu]Pred[Mp]*\[Sigma]2[Mp]^2 + 
           kRes*(-1 + w)*\[Alpha]*bPred2[Mp]*Y2[Mp]*
            (2*(\[Lambda]2[Mp] - \[Mu]2[Mp])^2 - (\[Lambda]2[Mp] - 3*
                \[Mu]2[Mp])*\[Sigma]2[Mp])) + (-1 + w)*\[Alpha]*bPred2[Mp]*
          Y2[Mp]*(\[Lambda]2[Mp] - \[Mu]2[Mp])*
          Sqrt[kRes^2*((-1 + w)^2*bmort2[Mp]^2*(4*\[Lambda]1[Mp]^2 - 4*
                \[Lambda]1[Mp]*(2*\[Mu]1[Mp] + \[Sigma]1[Mp]) + 
               (2*\[Mu]1[Mp] + 3*\[Sigma]1[Mp])^2) + w^2*bmort1[Mp]^2*
              (4*\[Lambda]2[Mp]^2 - 4*\[Lambda]2[Mp]*(2*\[Mu]2[Mp] + 
                 \[Sigma]2[Mp]) + (2*\[Mu]2[Mp] + 3*\[Sigma]2[Mp])^2) + 
             2*(-1 + w)*w*bmort1[Mp]*bmort2[Mp]*(-2*\[Lambda]2[Mp]*
                (2*\[Mu]1[Mp] + \[Sigma]1[Mp]) + (2*\[Mu]1[Mp] + 
                 3*\[Sigma]1[Mp])*(2*\[Mu]2[Mp] + 3*\[Sigma]2[Mp]) + 
               \[Lambda]1[Mp]*(4*\[Lambda]2[Mp] - 2*(2*\[Mu]2[Mp] + 
                   \[Sigma]2[Mp]))))])))/
     (((-1 + w)*bPred2[Mp]*Y2[Mp]*\[Lambda]1[Mp] + w*bPred1[Mp]*Y1[Mp]*
        \[Lambda]2[Mp])*((-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + 
        w*bmort1[Mp]*\[Sigma]2[Mp])^2), 
  C2 -> -1/4*(Y2[Mp]*(4*\[Lambda]1[Mp]*\[Mu]Pred[Mp]*
        ((-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + w*bmort1[Mp]*\[Sigma]2[Mp])^2 + 
       kRes*w*\[Alpha]*bPred1[Mp]*Y1[Mp]*((-1 + w)^2*bmort2[Mp]^2*
          (2*(\[Lambda]1[Mp] - \[Mu]1[Mp])^2 - (\[Lambda]1[Mp] - 
             3*\[Mu]1[Mp])*\[Sigma]1[Mp]) + (-1 + w)*w*bmort1[Mp]*bmort2[Mp]*
          (4*\[Mu]1[Mp]*\[Mu]2[Mp] + 3*\[Mu]2[Mp]*\[Sigma]1[Mp] - 
           \[Lambda]2[Mp]*(4*\[Mu]1[Mp] + \[Sigma]1[Mp]) + 
           \[Lambda]1[Mp]*(4*\[Lambda]2[Mp] - 4*\[Mu]2[Mp] - \[Sigma]2[Mp]) + 
           3*\[Mu]1[Mp]*\[Sigma]2[Mp]) + w^2*bmort1[Mp]^2*
          (2*(\[Lambda]2[Mp] - \[Mu]2[Mp])^2 - (\[Lambda]2[Mp] - 
             3*\[Mu]2[Mp])*\[Sigma]2[Mp])) - w*\[Alpha]*bmort2[Mp]*bPred1[Mp]*
        Y1[Mp]*\[Lambda]1[Mp]*Sqrt[kRes^2*
          (8*((-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + w*bmort1[Mp]*\[Sigma]2[Mp])*
            ((-1 + w)*bmort2[Mp]*(\[Mu]1[Mp] + \[Sigma]1[Mp]) + 
             w*bmort1[Mp]*(\[Mu]2[Mp] + \[Sigma]2[Mp])) + 
           ((-1 + w)*bmort2[Mp]*(2*\[Lambda]1[Mp] - 2*\[Mu]1[Mp] - \[Sigma]1[
                Mp]) - w*bmort1[Mp]*(-2*\[Lambda]2[Mp] + 2*\[Mu]2[Mp] + 
               \[Sigma]2[Mp]))^2)] + w^2*\[Alpha]*bmort2[Mp]*bPred1[Mp]*
        Y1[Mp]*\[Lambda]1[Mp]*Sqrt[kRes^2*
          (8*((-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + w*bmort1[Mp]*\[Sigma]2[Mp])*
            ((-1 + w)*bmort2[Mp]*(\[Mu]1[Mp] + \[Sigma]1[Mp]) + 
             w*bmort1[Mp]*(\[Mu]2[Mp] + \[Sigma]2[Mp])) + 
           ((-1 + w)*bmort2[Mp]*(2*\[Lambda]1[Mp] - 2*\[Mu]1[Mp] - \[Sigma]1[
                Mp]) - w*bmort1[Mp]*(-2*\[Lambda]2[Mp] + 2*\[Mu]2[Mp] + 
               \[Sigma]2[Mp]))^2)] + w^2*\[Alpha]*bmort1[Mp]*bPred1[Mp]*
        Y1[Mp]*\[Lambda]2[Mp]*Sqrt[kRes^2*
          (8*((-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + w*bmort1[Mp]*\[Sigma]2[Mp])*
            ((-1 + w)*bmort2[Mp]*(\[Mu]1[Mp] + \[Sigma]1[Mp]) + 
             w*bmort1[Mp]*(\[Mu]2[Mp] + \[Sigma]2[Mp])) + 
           ((-1 + w)*bmort2[Mp]*(2*\[Lambda]1[Mp] - 2*\[Mu]1[Mp] - \[Sigma]1[
                Mp]) - w*bmort1[Mp]*(-2*\[Lambda]2[Mp] + 2*\[Mu]2[Mp] + 
               \[Sigma]2[Mp]))^2)] + w*\[Alpha]*bmort2[Mp]*bPred1[Mp]*Y1[Mp]*
        \[Mu]1[Mp]*Sqrt[kRes^2*(8*((-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + 
             w*bmort1[Mp]*\[Sigma]2[Mp])*((-1 + w)*bmort2[Mp]*
              (\[Mu]1[Mp] + \[Sigma]1[Mp]) + w*bmort1[Mp]*(\[Mu]2[Mp] + 
               \[Sigma]2[Mp])) + ((-1 + w)*bmort2[Mp]*(2*\[Lambda]1[Mp] - 2*
                \[Mu]1[Mp] - \[Sigma]1[Mp]) - w*bmort1[Mp]*
              (-2*\[Lambda]2[Mp] + 2*\[Mu]2[Mp] + \[Sigma]2[Mp]))^2)] - 
       w^2*\[Alpha]*bmort2[Mp]*bPred1[Mp]*Y1[Mp]*\[Mu]1[Mp]*
        Sqrt[kRes^2*(8*((-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + 
             w*bmort1[Mp]*\[Sigma]2[Mp])*((-1 + w)*bmort2[Mp]*
              (\[Mu]1[Mp] + \[Sigma]1[Mp]) + w*bmort1[Mp]*(\[Mu]2[Mp] + 
               \[Sigma]2[Mp])) + ((-1 + w)*bmort2[Mp]*(2*\[Lambda]1[Mp] - 2*
                \[Mu]1[Mp] - \[Sigma]1[Mp]) - w*bmort1[Mp]*
              (-2*\[Lambda]2[Mp] + 2*\[Mu]2[Mp] + \[Sigma]2[Mp]))^2)] - 
       w^2*\[Alpha]*bmort1[Mp]*bPred1[Mp]*Y1[Mp]*\[Mu]2[Mp]*
        Sqrt[kRes^2*(8*((-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + 
             w*bmort1[Mp]*\[Sigma]2[Mp])*((-1 + w)*bmort2[Mp]*
              (\[Mu]1[Mp] + \[Sigma]1[Mp]) + w*bmort1[Mp]*(\[Mu]2[Mp] + 
               \[Sigma]2[Mp])) + ((-1 + w)*bmort2[Mp]*(2*\[Lambda]1[Mp] - 2*
                \[Mu]1[Mp] - \[Sigma]1[Mp]) - w*bmort1[Mp]*
              (-2*\[Lambda]2[Mp] + 2*\[Mu]2[Mp] + \[Sigma]2[Mp]))^2)]))/
     (((-1 + w)*bPred2[Mp]*Y2[Mp]*\[Lambda]1[Mp] + w*bPred1[Mp]*Y1[Mp]*
        \[Lambda]2[Mp])*((-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + 
        w*bmort1[Mp]*\[Sigma]2[Mp])^2), 
  R -> -((kRes*(-1 + w)*bmort2[Mp]*(2*\[Lambda]1[Mp] - 2*\[Mu]1[Mp] - 
        \[Sigma]1[Mp]) - kRes*w*bmort1[Mp]*(-2*\[Lambda]2[Mp] + 
        2*\[Mu]2[Mp] + \[Sigma]2[Mp]) + 
      Sqrt[kRes^2*(8*((-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + 
           w*bmort1[Mp]*\[Sigma]2[Mp])*((-1 + w)*bmort2[Mp]*
            (\[Mu]1[Mp] + \[Sigma]1[Mp]) + w*bmort1[Mp]*(\[Mu]2[Mp] + 
             \[Sigma]2[Mp])) + ((-1 + w)*bmort2[Mp]*(2*\[Lambda]1[Mp] - 
             2*\[Mu]1[Mp] - \[Sigma]1[Mp]) - w*bmort1[Mp]*
            (-2*\[Lambda]2[Mp] + 2*\[Mu]2[Mp] + \[Sigma]2[Mp]))^2)])/
     (4*(-1 + w)*bmort2[Mp]*\[Sigma]1[Mp] + 4*w*bmort1[Mp]*\[Sigma]2[Mp]))}}
