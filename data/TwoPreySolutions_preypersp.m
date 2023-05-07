(* Created with the Wolfram Language : www.wolfram.com *)
{{P -> 0, C1 -> 0, C2 -> 0, R -> 0}, 
 {P -> -((\[Mu]1[M] + \[Sigma]1[M])/(w*bmort1[M])), 
  C1 -> \[Mu]Pred[M]/(w*bPred1[M]), C2 -> 0, R -> 0}, 
 {P -> (\[Mu]2[M] + \[Sigma]2[M])/((-1 + w)*bmort2[M]), C1 -> 0, 
  C2 -> \[Mu]Pred[M]/(bPred2[M] - w*bPred2[M]), R -> 0}, 
 {P -> 0, C1 -> 0, C2 -> 0, R -> kRes}, 
 {P -> -1/8*(3*kRes^2*w*\[Alpha]*bPred1[M]*Y1[M] + kRes^(3/2)*Sqrt[w]*
       Sqrt[\[Alpha]]*Sqrt[bPred1[M]]*Sqrt[Y1[M]]*
       Sqrt[9*kRes*w*\[Alpha]*bPred1[M]*Y1[M] - 16*\[Lambda]1[M]*
          \[Mu]Pred[M]] + (2*Sqrt[kRes]*\[Mu]Pred[M]*
        Sqrt[9*kRes*w*\[Alpha]*bPred1[M]*Y1[M] - 16*\[Lambda]1[M]*
           \[Mu]Pred[M]]*\[Sigma]1[M])/(Sqrt[w]*Sqrt[\[Alpha]]*
        Sqrt[bPred1[M]]*Sqrt[Y1[M]]) + 2*kRes*\[Mu]Pred[M]*
       (-4*\[Lambda]1[M] + 4*\[Mu]1[M] + 3*\[Sigma]1[M]))/
     (kRes*w*bmort1[M]*\[Mu]Pred[M]), C1 -> \[Mu]Pred[M]/(w*bPred1[M]), 
  C2 -> 0, R -> kRes/4 - (Sqrt[kRes]*Sqrt[9*kRes*w*\[Alpha]*bPred1[M]*Y1[M] - 
        16*\[Lambda]1[M]*\[Mu]Pred[M]])/(4*Sqrt[w]*Sqrt[\[Alpha]]*
      Sqrt[bPred1[M]]*Sqrt[Y1[M]])}, 
 {P -> (-3*kRes^2*w*\[Alpha]*bPred1[M]*Y1[M] + kRes^(3/2)*Sqrt[w]*
      Sqrt[\[Alpha]]*Sqrt[bPred1[M]]*Sqrt[Y1[M]]*
      Sqrt[9*kRes*w*\[Alpha]*bPred1[M]*Y1[M] - 16*\[Lambda]1[M]*
         \[Mu]Pred[M]] + 2*kRes*\[Mu]Pred[M]*(4*\[Lambda]1[M] - 4*\[Mu]1[M] - 
       3*\[Sigma]1[M]) + (2*Sqrt[kRes]*\[Mu]Pred[M]*
       Sqrt[9*kRes*w*\[Alpha]*bPred1[M]*Y1[M] - 16*\[Lambda]1[M]*
          \[Mu]Pred[M]]*\[Sigma]1[M])/(Sqrt[w]*Sqrt[\[Alpha]]*Sqrt[bPred1[M]]*
       Sqrt[Y1[M]]))/(8*kRes*w*bmort1[M]*\[Mu]Pred[M]), 
  C1 -> \[Mu]Pred[M]/(w*bPred1[M]), C2 -> 0, 
  R -> kRes/4 + (Sqrt[kRes]*Sqrt[9*kRes*w*\[Alpha]*bPred1[M]*Y1[M] - 
        16*\[Lambda]1[M]*\[Mu]Pred[M]])/(4*Sqrt[w]*Sqrt[\[Alpha]]*
      Sqrt[bPred1[M]]*Sqrt[Y1[M]])}, 
 {P -> -1/8*((8*\[Lambda]2[M])/(-1 + w) - (8*\[Mu]2[M])/(-1 + w) + 
      ((3*Sqrt[kRes]*Sqrt[-1 + w]*Sqrt[\[Alpha]]*Sqrt[bPred2[M]]*
          Sqrt[Y2[M]] + Sqrt[9*kRes*(-1 + w)*\[Alpha]*bPred2[M]*Y2[M] + 
           16*\[Lambda]2[M]*\[Mu]Pred[M]])*(kRes*(-1 + w)*\[Alpha]*bPred2[M]*
          Y2[M] - 2*\[Mu]Pred[M]*\[Sigma]2[M]))/(Sqrt[kRes]*(-1 + w)^(3/2)*
        Sqrt[\[Alpha]]*Sqrt[bPred2[M]]*Sqrt[Y2[M]]*\[Mu]Pred[M]))/bmort2[M], 
  C1 -> 0, C2 -> \[Mu]Pred[M]/(bPred2[M] - w*bPred2[M]), 
  R -> kRes/4 - (Sqrt[kRes]*Sqrt[9*kRes*(-1 + w)*\[Alpha]*bPred2[M]*Y2[M] + 
        16*\[Lambda]2[M]*\[Mu]Pred[M]])/(4*Sqrt[-1 + w]*Sqrt[\[Alpha]]*
      Sqrt[bPred2[M]]*Sqrt[Y2[M]])}, 
 {P -> ((-8*\[Lambda]2[M])/(-1 + w) + (8*\[Mu]2[M])/(-1 + w) + 
     ((-3*Sqrt[kRes]*Sqrt[-1 + w]*Sqrt[\[Alpha]]*Sqrt[bPred2[M]]*
         Sqrt[Y2[M]] + Sqrt[9*kRes*(-1 + w)*\[Alpha]*bPred2[M]*Y2[M] + 
          16*\[Lambda]2[M]*\[Mu]Pred[M]])*(kRes*(-1 + w)*\[Alpha]*bPred2[M]*
         Y2[M] - 2*\[Mu]Pred[M]*\[Sigma]2[M]))/(Sqrt[kRes]*(-1 + w)^(3/2)*
       Sqrt[\[Alpha]]*Sqrt[bPred2[M]]*Sqrt[Y2[M]]*\[Mu]Pred[M]))/
    (8*bmort2[M]), C1 -> 0, C2 -> \[Mu]Pred[M]/(bPred2[M] - w*bPred2[M]), 
  R -> kRes/4 + (Sqrt[kRes]*Sqrt[9*kRes*(-1 + w)*\[Alpha]*bPred2[M]*Y2[M] + 
        16*\[Lambda]2[M]*\[Mu]Pred[M]])/(4*Sqrt[-1 + w]*Sqrt[\[Alpha]]*
      Sqrt[bPred2[M]]*Sqrt[Y2[M]])}, 
 {P -> 0, C1 -> (\[Alpha]*Y1[M]*(-2*kRes*\[Lambda]1[M]^2 + 
      \[Lambda]1[M]*(4*kRes*\[Mu]1[M] + kRes*\[Sigma]1[M] - 
        Sqrt[kRes^2*(4*\[Lambda]1[M]^2 - 4*\[Lambda]1[M]*(2*\[Mu]1[M] + 
             \[Sigma]1[M]) + (2*\[Mu]1[M] + 3*\[Sigma]1[M])^2)]) + 
      \[Mu]1[M]*(-2*kRes*\[Mu]1[M] - 3*kRes*\[Sigma]1[M] + 
        Sqrt[kRes^2*(4*\[Lambda]1[M]^2 - 4*\[Lambda]1[M]*(2*\[Mu]1[M] + 
             \[Sigma]1[M]) + (2*\[Mu]1[M] + 3*\[Sigma]1[M])^2)])))/
    (4*\[Lambda]1[M]*\[Sigma]1[M]^2), C2 -> 0, 
  R -> (kRes*(-2*\[Lambda]1[M] + 2*\[Mu]1[M] + \[Sigma]1[M]) - 
     Sqrt[kRes^2*(4*\[Lambda]1[M]^2 - 4*\[Lambda]1[M]*(2*\[Mu]1[M] + 
          \[Sigma]1[M]) + (2*\[Mu]1[M] + 3*\[Sigma]1[M])^2)])/
    (4*\[Sigma]1[M])}, 
 {P -> 0, C1 -> (\[Alpha]*Y1[M]*(-2*kRes*\[Lambda]1[M]^2 + 
      \[Lambda]1[M]*(4*kRes*\[Mu]1[M] + kRes*\[Sigma]1[M] + 
        Sqrt[kRes^2*(4*\[Lambda]1[M]^2 - 4*\[Lambda]1[M]*(2*\[Mu]1[M] + 
             \[Sigma]1[M]) + (2*\[Mu]1[M] + 3*\[Sigma]1[M])^2)]) - 
      \[Mu]1[M]*(2*kRes*\[Mu]1[M] + 3*kRes*\[Sigma]1[M] + 
        Sqrt[kRes^2*(4*\[Lambda]1[M]^2 - 4*\[Lambda]1[M]*(2*\[Mu]1[M] + 
             \[Sigma]1[M]) + (2*\[Mu]1[M] + 3*\[Sigma]1[M])^2)])))/
    (4*\[Lambda]1[M]*\[Sigma]1[M]^2), C2 -> 0, 
  R -> (kRes*(-2*\[Lambda]1[M] + 2*\[Mu]1[M] + \[Sigma]1[M]) + 
     Sqrt[kRes^2*(4*\[Lambda]1[M]^2 - 4*\[Lambda]1[M]*(2*\[Mu]1[M] + 
          \[Sigma]1[M]) + (2*\[Mu]1[M] + 3*\[Sigma]1[M])^2)])/
    (4*\[Sigma]1[M])}, {P -> 0, C1 -> 0, 
  C2 -> (\[Alpha]*Y2[M]*(-2*kRes*\[Lambda]2[M]^2 + 
      \[Lambda]2[M]*(4*kRes*\[Mu]2[M] + kRes*\[Sigma]2[M] - 
        Sqrt[kRes^2*(4*\[Lambda]2[M]^2 - 4*\[Lambda]2[M]*(2*\[Mu]2[M] + 
             \[Sigma]2[M]) + (2*\[Mu]2[M] + 3*\[Sigma]2[M])^2)]) + 
      \[Mu]2[M]*(-2*kRes*\[Mu]2[M] - 3*kRes*\[Sigma]2[M] + 
        Sqrt[kRes^2*(4*\[Lambda]2[M]^2 - 4*\[Lambda]2[M]*(2*\[Mu]2[M] + 
             \[Sigma]2[M]) + (2*\[Mu]2[M] + 3*\[Sigma]2[M])^2)])))/
    (4*\[Lambda]2[M]*\[Sigma]2[M]^2), 
  R -> (kRes*(-2*\[Lambda]2[M] + 2*\[Mu]2[M] + \[Sigma]2[M]) - 
     Sqrt[kRes^2*(4*\[Lambda]2[M]^2 - 4*\[Lambda]2[M]*(2*\[Mu]2[M] + 
          \[Sigma]2[M]) + (2*\[Mu]2[M] + 3*\[Sigma]2[M])^2)])/
    (4*\[Sigma]2[M])}, {P -> 0, C1 -> 0, 
  C2 -> (\[Alpha]*Y2[M]*(-2*kRes*\[Lambda]2[M]^2 + 
      \[Lambda]2[M]*(4*kRes*\[Mu]2[M] + kRes*\[Sigma]2[M] + 
        Sqrt[kRes^2*(4*\[Lambda]2[M]^2 - 4*\[Lambda]2[M]*(2*\[Mu]2[M] + 
             \[Sigma]2[M]) + (2*\[Mu]2[M] + 3*\[Sigma]2[M])^2)]) - 
      \[Mu]2[M]*(2*kRes*\[Mu]2[M] + 3*kRes*\[Sigma]2[M] + 
        Sqrt[kRes^2*(4*\[Lambda]2[M]^2 - 4*\[Lambda]2[M]*(2*\[Mu]2[M] + 
             \[Sigma]2[M]) + (2*\[Mu]2[M] + 3*\[Sigma]2[M])^2)])))/
    (4*\[Lambda]2[M]*\[Sigma]2[M]^2), 
  R -> (kRes*(-2*\[Lambda]2[M] + 2*\[Mu]2[M] + \[Sigma]2[M]) + 
     Sqrt[kRes^2*(4*\[Lambda]2[M]^2 - 4*\[Lambda]2[M]*(2*\[Mu]2[M] + 
          \[Sigma]2[M]) + (2*\[Mu]2[M] + 3*\[Sigma]2[M])^2)])/
    (4*\[Sigma]2[M])}, 
 {P -> (kRes*(-1 + w)*bmort2[M]*
      (-(\[Sigma]1[M]*(2*\[Lambda]1[M]*(\[Lambda]2[M] - 2*\[Mu]2[M]) + 
          \[Lambda]2[M]*(2*\[Mu]1[M] + 3*\[Sigma]1[M]))) + 
       \[Lambda]1[M]*(2*\[Lambda]1[M] - 2*\[Mu]1[M] + 3*\[Sigma]1[M])*
        \[Sigma]2[M]) + kRes*w*bmort1[M]*
      (2*\[Lambda]2[M]*(-\[Lambda]2[M] + \[Mu]2[M])*\[Sigma]1[M] + 
       (2*\[Lambda]1[M]*(\[Lambda]2[M] + \[Mu]2[M]) - \[Lambda]2[M]*
          (4*\[Mu]1[M] + 3*\[Sigma]1[M]))*\[Sigma]2[M] + 
       3*\[Lambda]1[M]*\[Sigma]2[M]^2) + \[Lambda]2[M]*\[Sigma]1[M]*
      Sqrt[kRes^2*(8*((-1 + w)*bmort2[M]*\[Sigma]1[M] + 
           w*bmort1[M]*\[Sigma]2[M])*((-1 + w)*bmort2[M]*(\[Mu]1[M] + 
             \[Sigma]1[M]) + w*bmort1[M]*(\[Mu]2[M] + \[Sigma]2[M])) + 
         ((-1 + w)*bmort2[M]*(2*\[Lambda]1[M] - 2*\[Mu]1[M] - \[Sigma]1[M]) - 
           w*bmort1[M]*(-2*\[Lambda]2[M] + 2*\[Mu]2[M] + \[Sigma]2[M]))^2)] - 
     \[Lambda]1[M]*\[Sigma]2[M]*
      Sqrt[kRes^2*(8*((-1 + w)*bmort2[M]*\[Sigma]1[M] + 
           w*bmort1[M]*\[Sigma]2[M])*((-1 + w)*bmort2[M]*(\[Mu]1[M] + 
             \[Sigma]1[M]) + w*bmort1[M]*(\[Mu]2[M] + \[Sigma]2[M])) + 
         ((-1 + w)*bmort2[M]*(2*\[Lambda]1[M] - 2*\[Mu]1[M] - \[Sigma]1[M]) - 
           w*bmort1[M]*(-2*\[Lambda]2[M] + 2*\[Mu]2[M] + \[Sigma]2[M]))^2)])/
    (4*kRes*((-1 + w)*bmort2[M]*\[Lambda]1[M] + w*bmort1[M]*\[Lambda]2[M])*
     ((-1 + w)*bmort2[M]*\[Sigma]1[M] + w*bmort1[M]*\[Sigma]2[M])), 
  C1 -> (Y1[M]*((-1 + w)^2*bmort2[M]^2*(4*\[Lambda]2[M]*\[Mu]Pred[M]*
         \[Sigma]1[M]^2 + kRes*(-1 + w)*\[Alpha]*bPred2[M]*Y2[M]*
         (-2*(\[Lambda]1[M] - \[Mu]1[M])^2 + (\[Lambda]1[M] - 3*\[Mu]1[M])*
           \[Sigma]1[M])) + (-1 + w)*bmort2[M]*
       (w*bmort1[M]*(8*\[Lambda]2[M]*\[Mu]Pred[M]*\[Sigma]1[M]*\[Sigma]2[M] + 
          kRes*(-1 + w)*\[Alpha]*bPred2[M]*Y2[M]*(-4*\[Mu]1[M]*\[Mu]2[M] - 
            3*\[Mu]2[M]*\[Sigma]1[M] + \[Lambda]2[M]*(4*\[Mu]1[M] + 
              \[Sigma]1[M]) - 3*\[Mu]1[M]*\[Sigma]2[M] + \[Lambda]1[M]*
             (-4*\[Lambda]2[M] + 4*\[Mu]2[M] + \[Sigma]2[M]))) + 
        (-1 + w)*\[Alpha]*bPred2[M]*Y2[M]*(\[Lambda]1[M] - \[Mu]1[M])*
         Sqrt[kRes^2*((-1 + w)^2*bmort2[M]^2*(4*\[Lambda]1[M]^2 - 
              4*\[Lambda]1[M]*(2*\[Mu]1[M] + \[Sigma]1[M]) + (2*\[Mu]1[M] + 
                3*\[Sigma]1[M])^2) + w^2*bmort1[M]^2*(4*\[Lambda]2[M]^2 - 
              4*\[Lambda]2[M]*(2*\[Mu]2[M] + \[Sigma]2[M]) + (2*\[Mu]2[M] + 
                3*\[Sigma]2[M])^2) + 2*(-1 + w)*w*bmort1[M]*bmort2[M]*
             (-2*\[Lambda]2[M]*(2*\[Mu]1[M] + \[Sigma]1[M]) + 
              (2*\[Mu]1[M] + 3*\[Sigma]1[M])*(2*\[Mu]2[M] + 3*\[Sigma]2[M]) + 
              \[Lambda]1[M]*(4*\[Lambda]2[M] - 2*(2*\[Mu]2[M] + \[Sigma]2[
                   M]))))]) + w*bmort1[M]*
       (w*bmort1[M]*(4*\[Lambda]2[M]*\[Mu]Pred[M]*\[Sigma]2[M]^2 + 
          kRes*(-1 + w)*\[Alpha]*bPred2[M]*Y2[M]*
           (-2*(\[Lambda]2[M] - \[Mu]2[M])^2 + (\[Lambda]2[M] - 3*\[Mu]2[M])*
             \[Sigma]2[M])) + (-1 + w)*\[Alpha]*bPred2[M]*Y2[M]*
         (\[Lambda]2[M] - \[Mu]2[M])*Sqrt[kRes^2*((-1 + w)^2*bmort2[M]^2*
             (4*\[Lambda]1[M]^2 - 4*\[Lambda]1[M]*(2*\[Mu]1[M] + 
                \[Sigma]1[M]) + (2*\[Mu]1[M] + 3*\[Sigma]1[M])^2) + 
            w^2*bmort1[M]^2*(4*\[Lambda]2[M]^2 - 4*\[Lambda]2[M]*(
                2*\[Mu]2[M] + \[Sigma]2[M]) + (2*\[Mu]2[M] + 3*\[Sigma]2[M])^
               2) + 2*(-1 + w)*w*bmort1[M]*bmort2[M]*(-2*\[Lambda]2[M]*(
                2*\[Mu]1[M] + \[Sigma]1[M]) + (2*\[Mu]1[M] + 3*\[Sigma]1[M])*(
                2*\[Mu]2[M] + 3*\[Sigma]2[M]) + \[Lambda]1[M]*(
                4*\[Lambda]2[M] - 2*(2*\[Mu]2[M] + \[Sigma]2[M]))))])))/
    (4*((-1 + w)*bPred2[M]*Y2[M]*\[Lambda]1[M] + w*bPred1[M]*Y1[M]*
       \[Lambda]2[M])*((-1 + w)*bmort2[M]*\[Sigma]1[M] + 
       w*bmort1[M]*\[Sigma]2[M])^2), 
  C2 -> (Y2[M]*(-4*\[Lambda]1[M]*\[Mu]Pred[M]*
       ((-1 + w)*bmort2[M]*\[Sigma]1[M] + w*bmort1[M]*\[Sigma]2[M])^2 - 
      w*\[Alpha]*bmort2[M]*bPred1[M]*Y1[M]*\[Lambda]1[M]*
       Sqrt[kRes^2*(8*((-1 + w)*bmort2[M]*\[Sigma]1[M] + w*bmort1[M]*
             \[Sigma]2[M])*((-1 + w)*bmort2[M]*(\[Mu]1[M] + \[Sigma]1[M]) + 
            w*bmort1[M]*(\[Mu]2[M] + \[Sigma]2[M])) + 
          ((-1 + w)*bmort2[M]*(2*\[Lambda]1[M] - 2*\[Mu]1[M] - 
              \[Sigma]1[M]) - w*bmort1[M]*(-2*\[Lambda]2[M] + 2*\[Mu]2[M] + 
              \[Sigma]2[M]))^2)] + w^2*\[Alpha]*bmort2[M]*bPred1[M]*Y1[M]*
       \[Lambda]1[M]*Sqrt[kRes^2*(8*((-1 + w)*bmort2[M]*\[Sigma]1[M] + 
            w*bmort1[M]*\[Sigma]2[M])*((-1 + w)*bmort2[M]*(\[Mu]1[M] + 
              \[Sigma]1[M]) + w*bmort1[M]*(\[Mu]2[M] + \[Sigma]2[M])) + 
          ((-1 + w)*bmort2[M]*(2*\[Lambda]1[M] - 2*\[Mu]1[M] - 
              \[Sigma]1[M]) - w*bmort1[M]*(-2*\[Lambda]2[M] + 2*\[Mu]2[M] + 
              \[Sigma]2[M]))^2)] + w^2*\[Alpha]*bmort1[M]*bPred1[M]*Y1[M]*
       \[Lambda]2[M]*Sqrt[kRes^2*(8*((-1 + w)*bmort2[M]*\[Sigma]1[M] + 
            w*bmort1[M]*\[Sigma]2[M])*((-1 + w)*bmort2[M]*(\[Mu]1[M] + 
              \[Sigma]1[M]) + w*bmort1[M]*(\[Mu]2[M] + \[Sigma]2[M])) + 
          ((-1 + w)*bmort2[M]*(2*\[Lambda]1[M] - 2*\[Mu]1[M] - 
              \[Sigma]1[M]) - w*bmort1[M]*(-2*\[Lambda]2[M] + 2*\[Mu]2[M] + 
              \[Sigma]2[M]))^2)] + w*\[Alpha]*bmort2[M]*bPred1[M]*Y1[M]*
       \[Mu]1[M]*Sqrt[kRes^2*(8*((-1 + w)*bmort2[M]*\[Sigma]1[M] + 
            w*bmort1[M]*\[Sigma]2[M])*((-1 + w)*bmort2[M]*(\[Mu]1[M] + 
              \[Sigma]1[M]) + w*bmort1[M]*(\[Mu]2[M] + \[Sigma]2[M])) + 
          ((-1 + w)*bmort2[M]*(2*\[Lambda]1[M] - 2*\[Mu]1[M] - 
              \[Sigma]1[M]) - w*bmort1[M]*(-2*\[Lambda]2[M] + 2*\[Mu]2[M] + 
              \[Sigma]2[M]))^2)] - w^2*\[Alpha]*bmort2[M]*bPred1[M]*Y1[M]*
       \[Mu]1[M]*Sqrt[kRes^2*(8*((-1 + w)*bmort2[M]*\[Sigma]1[M] + 
            w*bmort1[M]*\[Sigma]2[M])*((-1 + w)*bmort2[M]*(\[Mu]1[M] + 
              \[Sigma]1[M]) + w*bmort1[M]*(\[Mu]2[M] + \[Sigma]2[M])) + 
          ((-1 + w)*bmort2[M]*(2*\[Lambda]1[M] - 2*\[Mu]1[M] - 
              \[Sigma]1[M]) - w*bmort1[M]*(-2*\[Lambda]2[M] + 2*\[Mu]2[M] + 
              \[Sigma]2[M]))^2)] - w^2*\[Alpha]*bmort1[M]*bPred1[M]*Y1[M]*
       \[Mu]2[M]*Sqrt[kRes^2*(8*((-1 + w)*bmort2[M]*\[Sigma]1[M] + 
            w*bmort1[M]*\[Sigma]2[M])*((-1 + w)*bmort2[M]*(\[Mu]1[M] + 
              \[Sigma]1[M]) + w*bmort1[M]*(\[Mu]2[M] + \[Sigma]2[M])) + 
          ((-1 + w)*bmort2[M]*(2*\[Lambda]1[M] - 2*\[Mu]1[M] - 
              \[Sigma]1[M]) - w*bmort1[M]*(-2*\[Lambda]2[M] + 2*\[Mu]2[M] + 
              \[Sigma]2[M]))^2)] + kRes*w*\[Alpha]*bPred1[M]*Y1[M]*
       ((-1 + w)^2*bmort2[M]^2*(-2*(\[Lambda]1[M] - \[Mu]1[M])^2 + 
          (\[Lambda]1[M] - 3*\[Mu]1[M])*\[Sigma]1[M]) + 
        w^2*bmort1[M]^2*(-2*(\[Lambda]2[M] - \[Mu]2[M])^2 + 
          (\[Lambda]2[M] - 3*\[Mu]2[M])*\[Sigma]2[M]) + 
        (-1 + w)*w*bmort1[M]*bmort2[M]*(-4*\[Mu]1[M]*\[Mu]2[M] - 
          3*\[Mu]2[M]*\[Sigma]1[M] + \[Lambda]2[M]*(4*\[Mu]1[M] + 
            \[Sigma]1[M]) - 3*\[Mu]1[M]*\[Sigma]2[M] + \[Lambda]1[M]*
           (-4*\[Lambda]2[M] + 4*\[Mu]2[M] + \[Sigma]2[M])))))/
    (4*((-1 + w)*bPred2[M]*Y2[M]*\[Lambda]1[M] + w*bPred1[M]*Y1[M]*
       \[Lambda]2[M])*((-1 + w)*bmort2[M]*\[Sigma]1[M] + 
       w*bmort1[M]*\[Sigma]2[M])^2), 
  R -> (-(kRes*(-1 + w)*bmort2[M]*(2*\[Lambda]1[M] - 2*\[Mu]1[M] - 
        \[Sigma]1[M])) + kRes*w*bmort1[M]*(-2*\[Lambda]2[M] + 2*\[Mu]2[M] + 
       \[Sigma]2[M]) + Sqrt[kRes^2*(8*((-1 + w)*bmort2[M]*\[Sigma]1[M] + 
          w*bmort1[M]*\[Sigma]2[M])*((-1 + w)*bmort2[M]*(\[Mu]1[M] + 
            \[Sigma]1[M]) + w*bmort1[M]*(\[Mu]2[M] + \[Sigma]2[M])) + 
        ((-1 + w)*bmort2[M]*(2*\[Lambda]1[M] - 2*\[Mu]1[M] - \[Sigma]1[M]) - 
          w*bmort1[M]*(-2*\[Lambda]2[M] + 2*\[Mu]2[M] + \[Sigma]2[M]))^2)])/
    (4*(-1 + w)*bmort2[M]*\[Sigma]1[M] + 4*w*bmort1[M]*\[Sigma]2[M])}, 
 {P -> (kRes*(-1 + w)*bmort2[M]*
      (-(\[Sigma]1[M]*(2*\[Lambda]1[M]*(\[Lambda]2[M] - 2*\[Mu]2[M]) + 
          \[Lambda]2[M]*(2*\[Mu]1[M] + 3*\[Sigma]1[M]))) + 
       \[Lambda]1[M]*(2*\[Lambda]1[M] - 2*\[Mu]1[M] + 3*\[Sigma]1[M])*
        \[Sigma]2[M]) + kRes*w*bmort1[M]*
      (2*\[Lambda]2[M]*(-\[Lambda]2[M] + \[Mu]2[M])*\[Sigma]1[M] + 
       (2*\[Lambda]1[M]*(\[Lambda]2[M] + \[Mu]2[M]) - \[Lambda]2[M]*
          (4*\[Mu]1[M] + 3*\[Sigma]1[M]))*\[Sigma]2[M] + 
       3*\[Lambda]1[M]*\[Sigma]2[M]^2) - \[Lambda]2[M]*\[Sigma]1[M]*
      Sqrt[kRes^2*(8*((-1 + w)*bmort2[M]*\[Sigma]1[M] + 
           w*bmort1[M]*\[Sigma]2[M])*((-1 + w)*bmort2[M]*(\[Mu]1[M] + 
             \[Sigma]1[M]) + w*bmort1[M]*(\[Mu]2[M] + \[Sigma]2[M])) + 
         ((-1 + w)*bmort2[M]*(2*\[Lambda]1[M] - 2*\[Mu]1[M] - \[Sigma]1[M]) - 
           w*bmort1[M]*(-2*\[Lambda]2[M] + 2*\[Mu]2[M] + \[Sigma]2[M]))^2)] + 
     \[Lambda]1[M]*\[Sigma]2[M]*
      Sqrt[kRes^2*(8*((-1 + w)*bmort2[M]*\[Sigma]1[M] + 
           w*bmort1[M]*\[Sigma]2[M])*((-1 + w)*bmort2[M]*(\[Mu]1[M] + 
             \[Sigma]1[M]) + w*bmort1[M]*(\[Mu]2[M] + \[Sigma]2[M])) + 
         ((-1 + w)*bmort2[M]*(2*\[Lambda]1[M] - 2*\[Mu]1[M] - \[Sigma]1[M]) - 
           w*bmort1[M]*(-2*\[Lambda]2[M] + 2*\[Mu]2[M] + \[Sigma]2[M]))^2)])/
    (4*kRes*((-1 + w)*bmort2[M]*\[Lambda]1[M] + w*bmort1[M]*\[Lambda]2[M])*
     ((-1 + w)*bmort2[M]*\[Sigma]1[M] + w*bmort1[M]*\[Sigma]2[M])), 
  C1 -> -1/4*(Y1[M]*((-1 + w)^2*bmort2[M]^2*(-4*\[Lambda]2[M]*\[Mu]Pred[M]*
          \[Sigma]1[M]^2 + kRes*(-1 + w)*\[Alpha]*bPred2[M]*Y2[M]*
          (2*(\[Lambda]1[M] - \[Mu]1[M])^2 - (\[Lambda]1[M] - 3*\[Mu]1[M])*
            \[Sigma]1[M])) + (-1 + w)*bmort2[M]*
        (w*bmort1[M]*(-8*\[Lambda]2[M]*\[Mu]Pred[M]*\[Sigma]1[M]*
            \[Sigma]2[M] + kRes*(-1 + w)*\[Alpha]*bPred2[M]*Y2[M]*
            (4*\[Mu]1[M]*\[Mu]2[M] + 3*\[Mu]2[M]*\[Sigma]1[M] - 
             \[Lambda]2[M]*(4*\[Mu]1[M] + \[Sigma]1[M]) + \[Lambda]1[M]*
              (4*\[Lambda]2[M] - 4*\[Mu]2[M] - \[Sigma]2[M]) + 
             3*\[Mu]1[M]*\[Sigma]2[M])) + (-1 + w)*\[Alpha]*bPred2[M]*Y2[M]*
          (\[Lambda]1[M] - \[Mu]1[M])*Sqrt[kRes^2*((-1 + w)^2*bmort2[M]^2*
              (4*\[Lambda]1[M]^2 - 4*\[Lambda]1[M]*(2*\[Mu]1[M] + 
                 \[Sigma]1[M]) + (2*\[Mu]1[M] + 3*\[Sigma]1[M])^2) + 
             w^2*bmort1[M]^2*(4*\[Lambda]2[M]^2 - 4*\[Lambda]2[M]*
                (2*\[Mu]2[M] + \[Sigma]2[M]) + (2*\[Mu]2[M] + 3*\[Sigma]2[M])^
                2) + 2*(-1 + w)*w*bmort1[M]*bmort2[M]*(-2*\[Lambda]2[M]*
                (2*\[Mu]1[M] + \[Sigma]1[M]) + (2*\[Mu]1[M] + 3*\[Sigma]1[M])*
                (2*\[Mu]2[M] + 3*\[Sigma]2[M]) + \[Lambda]1[M]*
                (4*\[Lambda]2[M] - 2*(2*\[Mu]2[M] + \[Sigma]2[M]))))]) + 
       w*bmort1[M]*(w*bmort1[M]*(-4*\[Lambda]2[M]*\[Mu]Pred[M]*
            \[Sigma]2[M]^2 + kRes*(-1 + w)*\[Alpha]*bPred2[M]*Y2[M]*
            (2*(\[Lambda]2[M] - \[Mu]2[M])^2 - (\[Lambda]2[M] - 3*\[Mu]2[M])*
              \[Sigma]2[M])) + (-1 + w)*\[Alpha]*bPred2[M]*Y2[M]*
          (\[Lambda]2[M] - \[Mu]2[M])*Sqrt[kRes^2*((-1 + w)^2*bmort2[M]^2*
              (4*\[Lambda]1[M]^2 - 4*\[Lambda]1[M]*(2*\[Mu]1[M] + 
                 \[Sigma]1[M]) + (2*\[Mu]1[M] + 3*\[Sigma]1[M])^2) + 
             w^2*bmort1[M]^2*(4*\[Lambda]2[M]^2 - 4*\[Lambda]2[M]*
                (2*\[Mu]2[M] + \[Sigma]2[M]) + (2*\[Mu]2[M] + 3*\[Sigma]2[M])^
                2) + 2*(-1 + w)*w*bmort1[M]*bmort2[M]*(-2*\[Lambda]2[M]*
                (2*\[Mu]1[M] + \[Sigma]1[M]) + (2*\[Mu]1[M] + 3*\[Sigma]1[M])*
                (2*\[Mu]2[M] + 3*\[Sigma]2[M]) + \[Lambda]1[M]*
                (4*\[Lambda]2[M] - 2*(2*\[Mu]2[M] + \[Sigma]2[M]))))])))/
     (((-1 + w)*bPred2[M]*Y2[M]*\[Lambda]1[M] + w*bPred1[M]*Y1[M]*
        \[Lambda]2[M])*((-1 + w)*bmort2[M]*\[Sigma]1[M] + 
        w*bmort1[M]*\[Sigma]2[M])^2), 
  C2 -> -1/4*(Y2[M]*(4*\[Lambda]1[M]*\[Mu]Pred[M]*
        ((-1 + w)*bmort2[M]*\[Sigma]1[M] + w*bmort1[M]*\[Sigma]2[M])^2 + 
       kRes*w*\[Alpha]*bPred1[M]*Y1[M]*((-1 + w)^2*bmort2[M]^2*
          (2*(\[Lambda]1[M] - \[Mu]1[M])^2 - (\[Lambda]1[M] - 3*\[Mu]1[M])*
            \[Sigma]1[M]) + (-1 + w)*w*bmort1[M]*bmort2[M]*
          (4*\[Mu]1[M]*\[Mu]2[M] + 3*\[Mu]2[M]*\[Sigma]1[M] - 
           \[Lambda]2[M]*(4*\[Mu]1[M] + \[Sigma]1[M]) + \[Lambda]1[M]*
            (4*\[Lambda]2[M] - 4*\[Mu]2[M] - \[Sigma]2[M]) + 
           3*\[Mu]1[M]*\[Sigma]2[M]) + w^2*bmort1[M]^2*
          (2*(\[Lambda]2[M] - \[Mu]2[M])^2 - (\[Lambda]2[M] - 3*\[Mu]2[M])*
            \[Sigma]2[M])) - w*\[Alpha]*bmort2[M]*bPred1[M]*Y1[M]*
        \[Lambda]1[M]*Sqrt[kRes^2*(8*((-1 + w)*bmort2[M]*\[Sigma]1[M] + 
             w*bmort1[M]*\[Sigma]2[M])*((-1 + w)*bmort2[M]*(\[Mu]1[M] + 
               \[Sigma]1[M]) + w*bmort1[M]*(\[Mu]2[M] + \[Sigma]2[M])) + 
           ((-1 + w)*bmort2[M]*(2*\[Lambda]1[M] - 2*\[Mu]1[M] - \[Sigma]1[
                M]) - w*bmort1[M]*(-2*\[Lambda]2[M] + 2*\[Mu]2[M] + \[Sigma]2[
                M]))^2)] + w^2*\[Alpha]*bmort2[M]*bPred1[M]*Y1[M]*
        \[Lambda]1[M]*Sqrt[kRes^2*(8*((-1 + w)*bmort2[M]*\[Sigma]1[M] + 
             w*bmort1[M]*\[Sigma]2[M])*((-1 + w)*bmort2[M]*(\[Mu]1[M] + 
               \[Sigma]1[M]) + w*bmort1[M]*(\[Mu]2[M] + \[Sigma]2[M])) + 
           ((-1 + w)*bmort2[M]*(2*\[Lambda]1[M] - 2*\[Mu]1[M] - \[Sigma]1[
                M]) - w*bmort1[M]*(-2*\[Lambda]2[M] + 2*\[Mu]2[M] + \[Sigma]2[
                M]))^2)] + w^2*\[Alpha]*bmort1[M]*bPred1[M]*Y1[M]*
        \[Lambda]2[M]*Sqrt[kRes^2*(8*((-1 + w)*bmort2[M]*\[Sigma]1[M] + 
             w*bmort1[M]*\[Sigma]2[M])*((-1 + w)*bmort2[M]*(\[Mu]1[M] + 
               \[Sigma]1[M]) + w*bmort1[M]*(\[Mu]2[M] + \[Sigma]2[M])) + 
           ((-1 + w)*bmort2[M]*(2*\[Lambda]1[M] - 2*\[Mu]1[M] - \[Sigma]1[
                M]) - w*bmort1[M]*(-2*\[Lambda]2[M] + 2*\[Mu]2[M] + \[Sigma]2[
                M]))^2)] + w*\[Alpha]*bmort2[M]*bPred1[M]*Y1[M]*\[Mu]1[M]*
        Sqrt[kRes^2*(8*((-1 + w)*bmort2[M]*\[Sigma]1[M] + w*bmort1[M]*
              \[Sigma]2[M])*((-1 + w)*bmort2[M]*(\[Mu]1[M] + \[Sigma]1[M]) + 
             w*bmort1[M]*(\[Mu]2[M] + \[Sigma]2[M])) + 
           ((-1 + w)*bmort2[M]*(2*\[Lambda]1[M] - 2*\[Mu]1[M] - \[Sigma]1[
                M]) - w*bmort1[M]*(-2*\[Lambda]2[M] + 2*\[Mu]2[M] + \[Sigma]2[
                M]))^2)] - w^2*\[Alpha]*bmort2[M]*bPred1[M]*Y1[M]*\[Mu]1[M]*
        Sqrt[kRes^2*(8*((-1 + w)*bmort2[M]*\[Sigma]1[M] + w*bmort1[M]*
              \[Sigma]2[M])*((-1 + w)*bmort2[M]*(\[Mu]1[M] + \[Sigma]1[M]) + 
             w*bmort1[M]*(\[Mu]2[M] + \[Sigma]2[M])) + 
           ((-1 + w)*bmort2[M]*(2*\[Lambda]1[M] - 2*\[Mu]1[M] - \[Sigma]1[
                M]) - w*bmort1[M]*(-2*\[Lambda]2[M] + 2*\[Mu]2[M] + \[Sigma]2[
                M]))^2)] - w^2*\[Alpha]*bmort1[M]*bPred1[M]*Y1[M]*\[Mu]2[M]*
        Sqrt[kRes^2*(8*((-1 + w)*bmort2[M]*\[Sigma]1[M] + w*bmort1[M]*
              \[Sigma]2[M])*((-1 + w)*bmort2[M]*(\[Mu]1[M] + \[Sigma]1[M]) + 
             w*bmort1[M]*(\[Mu]2[M] + \[Sigma]2[M])) + 
           ((-1 + w)*bmort2[M]*(2*\[Lambda]1[M] - 2*\[Mu]1[M] - \[Sigma]1[
                M]) - w*bmort1[M]*(-2*\[Lambda]2[M] + 2*\[Mu]2[M] + \[Sigma]2[
                M]))^2)]))/(((-1 + w)*bPred2[M]*Y2[M]*\[Lambda]1[M] + 
       w*bPred1[M]*Y1[M]*\[Lambda]2[M])*((-1 + w)*bmort2[M]*\[Sigma]1[M] + 
        w*bmort1[M]*\[Sigma]2[M])^2), 
  R -> -((kRes*(-1 + w)*bmort2[M]*(2*\[Lambda]1[M] - 2*\[Mu]1[M] - 
        \[Sigma]1[M]) - kRes*w*bmort1[M]*(-2*\[Lambda]2[M] + 2*\[Mu]2[M] + 
        \[Sigma]2[M]) + Sqrt[kRes^2*(8*((-1 + w)*bmort2[M]*\[Sigma]1[M] + 
           w*bmort1[M]*\[Sigma]2[M])*((-1 + w)*bmort2[M]*(\[Mu]1[M] + 
             \[Sigma]1[M]) + w*bmort1[M]*(\[Mu]2[M] + \[Sigma]2[M])) + 
         ((-1 + w)*bmort2[M]*(2*\[Lambda]1[M] - 2*\[Mu]1[M] - \[Sigma]1[M]) - 
           w*bmort1[M]*(-2*\[Lambda]2[M] + 2*\[Mu]2[M] + \[Sigma]2[M]))^2)])/
     (4*(-1 + w)*bmort2[M]*\[Sigma]1[M] + 4*w*bmort1[M]*\[Sigma]2[M]))}}
