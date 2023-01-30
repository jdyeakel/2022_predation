(* Created with the Wolfram Language : www.wolfram.com *)
{{P -> 0, C1 -> 0, C2 -> 0, R -> 0}, 
 {P -> -((\[Mu][M] + \[Sigma][M])/(w*b[M])), C1 -> \[Mu]Pred[M]/(w*bPred[M]), 
  C2 -> 0, R -> 0}, {P -> (\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]])/
    ((-1 + w)*b[M*\[Phi]]), C1 -> 0, 
  C2 -> -(\[Mu]Pred[M]/((-1 + w)*bPred[M*\[Phi]])), R -> 0}, 
 {P -> 0, C1 -> 0, C2 -> 0, R -> kRes}, 
 {P -> -1/8*(3*kRes^2*w*\[Alpha]*bPred[M]*Y[M] + kRes^(3/2)*Sqrt[w]*
       Sqrt[\[Alpha]]*Sqrt[bPred[M]]*Sqrt[Y[M]]*
       Sqrt[9*kRes*w*\[Alpha]*bPred[M]*Y[M] - 16*\[Lambda][M]*\[Mu]Pred[M]] + 
      (2*Sqrt[kRes]*\[Mu]Pred[M]*Sqrt[9*kRes*w*\[Alpha]*bPred[M]*Y[M] - 
          16*\[Lambda][M]*\[Mu]Pred[M]]*\[Sigma][M])/(Sqrt[w]*Sqrt[\[Alpha]]*
        Sqrt[bPred[M]]*Sqrt[Y[M]]) + 2*kRes*\[Mu]Pred[M]*
       (-4*\[Lambda][M] + 4*\[Mu][M] + 3*\[Sigma][M]))/
     (kRes*w*b[M]*\[Mu]Pred[M]), C1 -> \[Mu]Pred[M]/(w*bPred[M]), C2 -> 0, 
  R -> kRes/4 - (Sqrt[kRes]*Sqrt[9*kRes*w*\[Alpha]*bPred[M]*Y[M] - 
        16*\[Lambda][M]*\[Mu]Pred[M]])/(4*Sqrt[w]*Sqrt[\[Alpha]]*
      Sqrt[bPred[M]]*Sqrt[Y[M]])}, 
 {P -> (-3*kRes^2*w*\[Alpha]*bPred[M]*Y[M] + kRes^(3/2)*Sqrt[w]*
      Sqrt[\[Alpha]]*Sqrt[bPred[M]]*Sqrt[Y[M]]*
      Sqrt[9*kRes*w*\[Alpha]*bPred[M]*Y[M] - 16*\[Lambda][M]*\[Mu]Pred[M]] + 
     2*kRes*\[Mu]Pred[M]*(4*\[Lambda][M] - 4*\[Mu][M] - 3*\[Sigma][M]) + 
     (2*Sqrt[kRes]*\[Mu]Pred[M]*Sqrt[9*kRes*w*\[Alpha]*bPred[M]*Y[M] - 
         16*\[Lambda][M]*\[Mu]Pred[M]]*\[Sigma][M])/(Sqrt[w]*Sqrt[\[Alpha]]*
       Sqrt[bPred[M]]*Sqrt[Y[M]]))/(8*kRes*w*b[M]*\[Mu]Pred[M]), 
  C1 -> \[Mu]Pred[M]/(w*bPred[M]), C2 -> 0, 
  R -> kRes/4 + (Sqrt[kRes]*Sqrt[9*kRes*w*\[Alpha]*bPred[M]*Y[M] - 
        16*\[Lambda][M]*\[Mu]Pred[M]])/(4*Sqrt[w]*Sqrt[\[Alpha]]*
      Sqrt[bPred[M]]*Sqrt[Y[M]])}, 
 {P -> -1/8*((8*\[Lambda][M*\[Phi]])/(-1 + w) - (8*\[Mu][M*\[Phi]])/
       (-1 + w) + ((3*Sqrt[kRes]*Sqrt[-1 + w]*Sqrt[\[Alpha]]*
          Sqrt[bPred[M*\[Phi]]]*Sqrt[Y[M*\[Phi]]] + 
         Sqrt[9*kRes*(-1 + w)*\[Alpha]*bPred[M*\[Phi]]*Y[M*\[Phi]] + 
           16*\[Lambda][M*\[Phi]]*\[Mu]Pred[M]])*
        (kRes*(-1 + w)*\[Alpha]*bPred[M*\[Phi]]*Y[M*\[Phi]] - 
         2*\[Mu]Pred[M]*\[Sigma][M*\[Phi]]))/(Sqrt[kRes]*(-1 + w)^(3/2)*
        Sqrt[\[Alpha]]*Sqrt[bPred[M*\[Phi]]]*Sqrt[Y[M*\[Phi]]]*\[Mu]Pred[M]))/
     b[M*\[Phi]], C1 -> 0, C2 -> -(\[Mu]Pred[M]/((-1 + w)*bPred[M*\[Phi]])), 
  R -> kRes/4 - (Sqrt[kRes]*Sqrt[9*kRes*(-1 + w)*\[Alpha]*bPred[M*\[Phi]]*
         Y[M*\[Phi]] + 16*\[Lambda][M*\[Phi]]*\[Mu]Pred[M]])/
     (4*Sqrt[-1 + w]*Sqrt[\[Alpha]]*Sqrt[bPred[M*\[Phi]]]*
      Sqrt[Y[M*\[Phi]]])}, 
 {P -> ((-8*\[Lambda][M*\[Phi]])/(-1 + w) + (8*\[Mu][M*\[Phi]])/(-1 + w) + 
     ((-3*Sqrt[kRes]*Sqrt[-1 + w]*Sqrt[\[Alpha]]*Sqrt[bPred[M*\[Phi]]]*
         Sqrt[Y[M*\[Phi]]] + Sqrt[9*kRes*(-1 + w)*\[Alpha]*bPred[M*\[Phi]]*
           Y[M*\[Phi]] + 16*\[Lambda][M*\[Phi]]*\[Mu]Pred[M]])*
       (kRes*(-1 + w)*\[Alpha]*bPred[M*\[Phi]]*Y[M*\[Phi]] - 
        2*\[Mu]Pred[M]*\[Sigma][M*\[Phi]]))/(Sqrt[kRes]*(-1 + w)^(3/2)*
       Sqrt[\[Alpha]]*Sqrt[bPred[M*\[Phi]]]*Sqrt[Y[M*\[Phi]]]*\[Mu]Pred[M]))/
    (8*b[M*\[Phi]]), C1 -> 0, 
  C2 -> -(\[Mu]Pred[M]/((-1 + w)*bPred[M*\[Phi]])), 
  R -> kRes/4 + (Sqrt[kRes]*Sqrt[9*kRes*(-1 + w)*\[Alpha]*bPred[M*\[Phi]]*
         Y[M*\[Phi]] + 16*\[Lambda][M*\[Phi]]*\[Mu]Pred[M]])/
     (4*Sqrt[-1 + w]*Sqrt[\[Alpha]]*Sqrt[bPred[M*\[Phi]]]*
      Sqrt[Y[M*\[Phi]]])}, 
 {P -> 0, C1 -> (\[Alpha]*Y[M]*(-2*kRes*\[Lambda][M]^2 + 
      \[Lambda][M]*(4*kRes*\[Mu][M] + kRes*\[Sigma][M] - 
        Sqrt[kRes^2*(4*\[Lambda][M]^2 - 4*\[Lambda][M]*(2*\[Mu][M] + 
             \[Sigma][M]) + (2*\[Mu][M] + 3*\[Sigma][M])^2)]) + 
      \[Mu][M]*(-2*kRes*\[Mu][M] - 3*kRes*\[Sigma][M] + 
        Sqrt[kRes^2*(4*\[Lambda][M]^2 - 4*\[Lambda][M]*(2*\[Mu][M] + 
             \[Sigma][M]) + (2*\[Mu][M] + 3*\[Sigma][M])^2)])))/
    (4*\[Lambda][M]*\[Sigma][M]^2), C2 -> 0, 
  R -> (kRes*(-2*\[Lambda][M] + 2*\[Mu][M] + \[Sigma][M]) - 
     Sqrt[kRes^2*(4*\[Lambda][M]^2 - 4*\[Lambda][M]*(2*\[Mu][M] + 
          \[Sigma][M]) + (2*\[Mu][M] + 3*\[Sigma][M])^2)])/(4*\[Sigma][M])}, 
 {P -> 0, C1 -> (\[Alpha]*Y[M]*(-2*kRes*\[Lambda][M]^2 + 
      \[Lambda][M]*(4*kRes*\[Mu][M] + kRes*\[Sigma][M] + 
        Sqrt[kRes^2*(4*\[Lambda][M]^2 - 4*\[Lambda][M]*(2*\[Mu][M] + 
             \[Sigma][M]) + (2*\[Mu][M] + 3*\[Sigma][M])^2)]) - 
      \[Mu][M]*(2*kRes*\[Mu][M] + 3*kRes*\[Sigma][M] + 
        Sqrt[kRes^2*(4*\[Lambda][M]^2 - 4*\[Lambda][M]*(2*\[Mu][M] + 
             \[Sigma][M]) + (2*\[Mu][M] + 3*\[Sigma][M])^2)])))/
    (4*\[Lambda][M]*\[Sigma][M]^2), C2 -> 0, 
  R -> (kRes*(-2*\[Lambda][M] + 2*\[Mu][M] + \[Sigma][M]) + 
     Sqrt[kRes^2*(4*\[Lambda][M]^2 - 4*\[Lambda][M]*(2*\[Mu][M] + 
          \[Sigma][M]) + (2*\[Mu][M] + 3*\[Sigma][M])^2)])/(4*\[Sigma][M])}, 
 {P -> 0, C1 -> 0, C2 -> (\[Alpha]*Y[M*\[Phi]]*
     (-2*kRes*\[Lambda][M*\[Phi]]^2 + \[Lambda][M*\[Phi]]*
       (4*kRes*\[Mu][M*\[Phi]] + kRes*\[Sigma][M*\[Phi]] - 
        Sqrt[kRes^2*(4*\[Lambda][M*\[Phi]]^2 - 4*\[Lambda][M*\[Phi]]*
            (2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]) + 
           (2*\[Mu][M*\[Phi]] + 3*\[Sigma][M*\[Phi]])^2)]) + 
      \[Mu][M*\[Phi]]*(-2*kRes*\[Mu][M*\[Phi]] - 3*kRes*\[Sigma][M*\[Phi]] + 
        Sqrt[kRes^2*(4*\[Lambda][M*\[Phi]]^2 - 4*\[Lambda][M*\[Phi]]*
            (2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]) + 
           (2*\[Mu][M*\[Phi]] + 3*\[Sigma][M*\[Phi]])^2)])))/
    (4*\[Lambda][M*\[Phi]]*\[Sigma][M*\[Phi]]^2), 
  R -> (kRes*(-2*\[Lambda][M*\[Phi]] + 2*\[Mu][M*\[Phi]] + 
       \[Sigma][M*\[Phi]]) - Sqrt[kRes^2*(4*\[Lambda][M*\[Phi]]^2 - 
        4*\[Lambda][M*\[Phi]]*(2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]) + 
        (2*\[Mu][M*\[Phi]] + 3*\[Sigma][M*\[Phi]])^2)])/
    (4*\[Sigma][M*\[Phi]])}, {P -> 0, C1 -> 0, 
  C2 -> (\[Alpha]*Y[M*\[Phi]]*(-2*kRes*\[Lambda][M*\[Phi]]^2 + 
      \[Lambda][M*\[Phi]]*(4*kRes*\[Mu][M*\[Phi]] + kRes*\[Sigma][M*\[Phi]] + 
        Sqrt[kRes^2*(4*\[Lambda][M*\[Phi]]^2 - 4*\[Lambda][M*\[Phi]]*
            (2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]) + 
           (2*\[Mu][M*\[Phi]] + 3*\[Sigma][M*\[Phi]])^2)]) - 
      \[Mu][M*\[Phi]]*(2*kRes*\[Mu][M*\[Phi]] + 3*kRes*\[Sigma][M*\[Phi]] + 
        Sqrt[kRes^2*(4*\[Lambda][M*\[Phi]]^2 - 4*\[Lambda][M*\[Phi]]*
            (2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]) + 
           (2*\[Mu][M*\[Phi]] + 3*\[Sigma][M*\[Phi]])^2)])))/
    (4*\[Lambda][M*\[Phi]]*\[Sigma][M*\[Phi]]^2), 
  R -> (kRes*(-2*\[Lambda][M*\[Phi]] + 2*\[Mu][M*\[Phi]] + 
       \[Sigma][M*\[Phi]]) + Sqrt[kRes^2*(4*\[Lambda][M*\[Phi]]^2 - 
        4*\[Lambda][M*\[Phi]]*(2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]) + 
        (2*\[Mu][M*\[Phi]] + 3*\[Sigma][M*\[Phi]])^2)])/
    (4*\[Sigma][M*\[Phi]])}, 
 {P -> (kRes*(-1 + w)*b[M*\[Phi]]*
      (-(\[Sigma][M]*(2*\[Lambda][M]*(\[Lambda][M*\[Phi]] - 
            2*\[Mu][M*\[Phi]]) + \[Lambda][M*\[Phi]]*(2*\[Mu][M] + 
            3*\[Sigma][M]))) + \[Lambda][M]*(2*\[Lambda][M] - 2*\[Mu][M] + 
         3*\[Sigma][M])*\[Sigma][M*\[Phi]]) + 
     kRes*w*b[M]*(-2*\[Lambda][M*\[Phi]]^2*\[Sigma][M] + 
       \[Lambda][M]*\[Sigma][M*\[Phi]]*(2*\[Mu][M*\[Phi]] + 
         3*\[Sigma][M*\[Phi]]) + \[Lambda][M*\[Phi]]*
        (2*\[Mu][M*\[Phi]]*\[Sigma][M] + (2*\[Lambda][M] - 4*\[Mu][M] - 
           3*\[Sigma][M])*\[Sigma][M*\[Phi]])) + \[Lambda][M*\[Phi]]*
      \[Sigma][M]*Sqrt[kRes^2*(8*((-1 + w)*b[M*\[Phi]]*\[Sigma][M] + 
           w*b[M]*\[Sigma][M*\[Phi]])*((-1 + w)*b[M*\[Phi]]*
            (\[Mu][M] + \[Sigma][M]) + w*b[M]*(\[Mu][M*\[Phi]] + 
             \[Sigma][M*\[Phi]])) + ((-1 + w)*b[M*\[Phi]]*(2*\[Lambda][M] - 
             2*\[Mu][M] - \[Sigma][M]) - w*b[M]*(-2*\[Lambda][M*\[Phi]] + 
             2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]))^2)] - 
     \[Lambda][M]*\[Sigma][M*\[Phi]]*
      Sqrt[kRes^2*(8*((-1 + w)*b[M*\[Phi]]*\[Sigma][M] + 
           w*b[M]*\[Sigma][M*\[Phi]])*((-1 + w)*b[M*\[Phi]]*
            (\[Mu][M] + \[Sigma][M]) + w*b[M]*(\[Mu][M*\[Phi]] + 
             \[Sigma][M*\[Phi]])) + ((-1 + w)*b[M*\[Phi]]*(2*\[Lambda][M] - 
             2*\[Mu][M] - \[Sigma][M]) - w*b[M]*(-2*\[Lambda][M*\[Phi]] + 
             2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]))^2)])/
    (4*kRes*((-1 + w)*b[M*\[Phi]]*\[Lambda][M] + w*b[M]*\[Lambda][M*\[Phi]])*
     ((-1 + w)*b[M*\[Phi]]*\[Sigma][M] + w*b[M]*\[Sigma][M*\[Phi]])), 
  C1 -> (Y[M]*((-1 + w)^2*b[M*\[Phi]]^2*(4*\[Lambda][M*\[Phi]]*\[Mu]Pred[M]*
         \[Sigma][M]^2 - kRes*(-1 + w)*\[Alpha]*bPred[M*\[Phi]]*Y[M*\[Phi]]*
         (2*(\[Lambda][M] - \[Mu][M])^2 - (\[Lambda][M] - 3*\[Mu][M])*
           \[Sigma][M])) + (-1 + w)*b[M*\[Phi]]*
       (w*b[M]*(8*\[Lambda][M*\[Phi]]*\[Mu]Pred[M]*\[Sigma][M]*
           \[Sigma][M*\[Phi]] + kRes*(-1 + w)*\[Alpha]*bPred[M*\[Phi]]*
           Y[M*\[Phi]]*(\[Lambda][M*\[Phi]]*(4*\[Mu][M] + \[Sigma][M]) - 
            \[Mu][M*\[Phi]]*(4*\[Mu][M] + 3*\[Sigma][M]) - 
            3*\[Mu][M]*\[Sigma][M*\[Phi]] + \[Lambda][M]*
             (-4*\[Lambda][M*\[Phi]] + 4*\[Mu][M*\[Phi]] + \[Sigma][M*
                \[Phi]]))) + (-1 + w)*\[Alpha]*bPred[M*\[Phi]]*Y[M*\[Phi]]*
         (\[Lambda][M] - \[Mu][M])*Sqrt[kRes^2*((-1 + w)^2*b[M*\[Phi]]^2*
             (4*\[Lambda][M]^2 - 4*\[Lambda][M]*(2*\[Mu][M] + \[Sigma][M]) + 
              (2*\[Mu][M] + 3*\[Sigma][M])^2) + 2*(-1 + w)*w*b[M]*b[M*\[Phi]]*
             (4*(\[Lambda][M] - \[Mu][M])*(\[Lambda][M*\[Phi]] - 
                \[Mu][M*\[Phi]]) - 2*(\[Lambda][M*\[Phi]] - 
                3*\[Mu][M*\[Phi]])*\[Sigma][M] + (-2*\[Lambda][M] + 
                6*\[Mu][M] + 9*\[Sigma][M])*\[Sigma][M*\[Phi]]) + 
            w^2*b[M]^2*(4*\[Lambda][M*\[Phi]]^2 - 4*\[Lambda][M*\[Phi]]*(
                2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]) + 
              (2*\[Mu][M*\[Phi]] + 3*\[Sigma][M*\[Phi]])^2))]) + 
      w*b[M]*(w*b[M]*(4*\[Lambda][M*\[Phi]]*\[Mu]Pred[M]*\[Sigma][M*\[Phi]]^
            2 + kRes*(-1 + w)*\[Alpha]*bPred[M*\[Phi]]*Y[M*\[Phi]]*
           (-2*(\[Lambda][M*\[Phi]] - \[Mu][M*\[Phi]])^2 + 
            (\[Lambda][M*\[Phi]] - 3*\[Mu][M*\[Phi]])*\[Sigma][M*\[Phi]])) + 
        (-1 + w)*\[Alpha]*bPred[M*\[Phi]]*Y[M*\[Phi]]*(\[Lambda][M*\[Phi]] - 
          \[Mu][M*\[Phi]])*Sqrt[kRes^2*((-1 + w)^2*b[M*\[Phi]]^2*
             (4*\[Lambda][M]^2 - 4*\[Lambda][M]*(2*\[Mu][M] + \[Sigma][M]) + 
              (2*\[Mu][M] + 3*\[Sigma][M])^2) + 2*(-1 + w)*w*b[M]*b[M*\[Phi]]*
             (4*(\[Lambda][M] - \[Mu][M])*(\[Lambda][M*\[Phi]] - 
                \[Mu][M*\[Phi]]) - 2*(\[Lambda][M*\[Phi]] - 
                3*\[Mu][M*\[Phi]])*\[Sigma][M] + (-2*\[Lambda][M] + 
                6*\[Mu][M] + 9*\[Sigma][M])*\[Sigma][M*\[Phi]]) + 
            w^2*b[M]^2*(4*\[Lambda][M*\[Phi]]^2 - 4*\[Lambda][M*\[Phi]]*(
                2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]) + 
              (2*\[Mu][M*\[Phi]] + 3*\[Sigma][M*\[Phi]])^2))])))/
    (4*((-1 + w)*bPred[M*\[Phi]]*Y[M*\[Phi]]*\[Lambda][M] + 
      w*bPred[M]*Y[M]*\[Lambda][M*\[Phi]])*
     ((-1 + w)*b[M*\[Phi]]*\[Sigma][M] + w*b[M]*\[Sigma][M*\[Phi]])^2), 
  C2 -> (Y[M*\[Phi]]*(-4*\[Lambda][M]*\[Mu]Pred[M]*
       ((-1 + w)*b[M*\[Phi]]*\[Sigma][M] + w*b[M]*\[Sigma][M*\[Phi]])^2 - 
      w*\[Alpha]*b[M*\[Phi]]*bPred[M]*Y[M]*\[Lambda][M]*
       Sqrt[kRes^2*(8*((-1 + w)*b[M*\[Phi]]*\[Sigma][M] + 
            w*b[M]*\[Sigma][M*\[Phi]])*((-1 + w)*b[M*\[Phi]]*
             (\[Mu][M] + \[Sigma][M]) + w*b[M]*(\[Mu][M*\[Phi]] + 
              \[Sigma][M*\[Phi]])) + ((-1 + w)*b[M*\[Phi]]*(2*\[Lambda][M] - 
              2*\[Mu][M] - \[Sigma][M]) - w*b[M]*(-2*\[Lambda][M*\[Phi]] + 
              2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]))^2)] + 
      w^2*\[Alpha]*b[M*\[Phi]]*bPred[M]*Y[M]*\[Lambda][M]*
       Sqrt[kRes^2*(8*((-1 + w)*b[M*\[Phi]]*\[Sigma][M] + 
            w*b[M]*\[Sigma][M*\[Phi]])*((-1 + w)*b[M*\[Phi]]*
             (\[Mu][M] + \[Sigma][M]) + w*b[M]*(\[Mu][M*\[Phi]] + 
              \[Sigma][M*\[Phi]])) + ((-1 + w)*b[M*\[Phi]]*(2*\[Lambda][M] - 
              2*\[Mu][M] - \[Sigma][M]) - w*b[M]*(-2*\[Lambda][M*\[Phi]] + 
              2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]))^2)] + 
      w^2*\[Alpha]*b[M]*bPred[M]*Y[M]*\[Lambda][M*\[Phi]]*
       Sqrt[kRes^2*(8*((-1 + w)*b[M*\[Phi]]*\[Sigma][M] + 
            w*b[M]*\[Sigma][M*\[Phi]])*((-1 + w)*b[M*\[Phi]]*
             (\[Mu][M] + \[Sigma][M]) + w*b[M]*(\[Mu][M*\[Phi]] + 
              \[Sigma][M*\[Phi]])) + ((-1 + w)*b[M*\[Phi]]*(2*\[Lambda][M] - 
              2*\[Mu][M] - \[Sigma][M]) - w*b[M]*(-2*\[Lambda][M*\[Phi]] + 
              2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]))^2)] + 
      w*\[Alpha]*b[M*\[Phi]]*bPred[M]*Y[M]*\[Mu][M]*
       Sqrt[kRes^2*(8*((-1 + w)*b[M*\[Phi]]*\[Sigma][M] + 
            w*b[M]*\[Sigma][M*\[Phi]])*((-1 + w)*b[M*\[Phi]]*
             (\[Mu][M] + \[Sigma][M]) + w*b[M]*(\[Mu][M*\[Phi]] + 
              \[Sigma][M*\[Phi]])) + ((-1 + w)*b[M*\[Phi]]*(2*\[Lambda][M] - 
              2*\[Mu][M] - \[Sigma][M]) - w*b[M]*(-2*\[Lambda][M*\[Phi]] + 
              2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]))^2)] - 
      w^2*\[Alpha]*b[M*\[Phi]]*bPred[M]*Y[M]*\[Mu][M]*
       Sqrt[kRes^2*(8*((-1 + w)*b[M*\[Phi]]*\[Sigma][M] + 
            w*b[M]*\[Sigma][M*\[Phi]])*((-1 + w)*b[M*\[Phi]]*
             (\[Mu][M] + \[Sigma][M]) + w*b[M]*(\[Mu][M*\[Phi]] + 
              \[Sigma][M*\[Phi]])) + ((-1 + w)*b[M*\[Phi]]*(2*\[Lambda][M] - 
              2*\[Mu][M] - \[Sigma][M]) - w*b[M]*(-2*\[Lambda][M*\[Phi]] + 
              2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]))^2)] - 
      w^2*\[Alpha]*b[M]*bPred[M]*Y[M]*\[Mu][M*\[Phi]]*
       Sqrt[kRes^2*(8*((-1 + w)*b[M*\[Phi]]*\[Sigma][M] + 
            w*b[M]*\[Sigma][M*\[Phi]])*((-1 + w)*b[M*\[Phi]]*
             (\[Mu][M] + \[Sigma][M]) + w*b[M]*(\[Mu][M*\[Phi]] + 
              \[Sigma][M*\[Phi]])) + ((-1 + w)*b[M*\[Phi]]*(2*\[Lambda][M] - 
              2*\[Mu][M] - \[Sigma][M]) - w*b[M]*(-2*\[Lambda][M*\[Phi]] + 
              2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]))^2)] + 
      kRes*w*\[Alpha]*bPred[M]*Y[M]*(-((-1 + w)^2*b[M*\[Phi]]^2*
          (2*(\[Lambda][M] - \[Mu][M])^2 - (\[Lambda][M] - 3*\[Mu][M])*
            \[Sigma][M])) + w^2*b[M]^2*
         (-2*(\[Lambda][M*\[Phi]] - \[Mu][M*\[Phi]])^2 + 
          (\[Lambda][M*\[Phi]] - 3*\[Mu][M*\[Phi]])*\[Sigma][M*\[Phi]]) + 
        (-1 + w)*w*b[M]*b[M*\[Phi]]*(\[Lambda][M*\[Phi]]*(4*\[Mu][M] + 
            \[Sigma][M]) - \[Mu][M*\[Phi]]*(4*\[Mu][M] + 3*\[Sigma][M]) - 
          3*\[Mu][M]*\[Sigma][M*\[Phi]] + \[Lambda][M]*
           (-4*\[Lambda][M*\[Phi]] + 4*\[Mu][M*\[Phi]] + 
            \[Sigma][M*\[Phi]])))))/
    (4*((-1 + w)*bPred[M*\[Phi]]*Y[M*\[Phi]]*\[Lambda][M] + 
      w*bPred[M]*Y[M]*\[Lambda][M*\[Phi]])*
     ((-1 + w)*b[M*\[Phi]]*\[Sigma][M] + w*b[M]*\[Sigma][M*\[Phi]])^2), 
  R -> (-(kRes*(-1 + w)*b[M*\[Phi]]*(2*\[Lambda][M] - 2*\[Mu][M] - 
        \[Sigma][M])) + kRes*w*b[M]*(-2*\[Lambda][M*\[Phi]] + 
       2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]) + 
     Sqrt[kRes^2*(8*((-1 + w)*b[M*\[Phi]]*\[Sigma][M] + 
          w*b[M]*\[Sigma][M*\[Phi]])*((-1 + w)*b[M*\[Phi]]*
           (\[Mu][M] + \[Sigma][M]) + w*b[M]*(\[Mu][M*\[Phi]] + 
            \[Sigma][M*\[Phi]])) + ((-1 + w)*b[M*\[Phi]]*(2*\[Lambda][M] - 
            2*\[Mu][M] - \[Sigma][M]) - w*b[M]*(-2*\[Lambda][M*\[Phi]] + 
            2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]))^2)])/
    (4*(-1 + w)*b[M*\[Phi]]*\[Sigma][M] + 4*w*b[M]*\[Sigma][M*\[Phi]])}, 
 {P -> (kRes*(-1 + w)*b[M*\[Phi]]*
      (-(\[Sigma][M]*(2*\[Lambda][M]*(\[Lambda][M*\[Phi]] - 
            2*\[Mu][M*\[Phi]]) + \[Lambda][M*\[Phi]]*(2*\[Mu][M] + 
            3*\[Sigma][M]))) + \[Lambda][M]*(2*\[Lambda][M] - 2*\[Mu][M] + 
         3*\[Sigma][M])*\[Sigma][M*\[Phi]]) + 
     kRes*w*b[M]*(-2*\[Lambda][M*\[Phi]]^2*\[Sigma][M] + 
       \[Lambda][M]*\[Sigma][M*\[Phi]]*(2*\[Mu][M*\[Phi]] + 
         3*\[Sigma][M*\[Phi]]) + \[Lambda][M*\[Phi]]*
        (2*\[Mu][M*\[Phi]]*\[Sigma][M] + (2*\[Lambda][M] - 4*\[Mu][M] - 
           3*\[Sigma][M])*\[Sigma][M*\[Phi]])) - \[Lambda][M*\[Phi]]*
      \[Sigma][M]*Sqrt[kRes^2*(8*((-1 + w)*b[M*\[Phi]]*\[Sigma][M] + 
           w*b[M]*\[Sigma][M*\[Phi]])*((-1 + w)*b[M*\[Phi]]*
            (\[Mu][M] + \[Sigma][M]) + w*b[M]*(\[Mu][M*\[Phi]] + 
             \[Sigma][M*\[Phi]])) + ((-1 + w)*b[M*\[Phi]]*(2*\[Lambda][M] - 
             2*\[Mu][M] - \[Sigma][M]) - w*b[M]*(-2*\[Lambda][M*\[Phi]] + 
             2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]))^2)] + 
     \[Lambda][M]*\[Sigma][M*\[Phi]]*
      Sqrt[kRes^2*(8*((-1 + w)*b[M*\[Phi]]*\[Sigma][M] + 
           w*b[M]*\[Sigma][M*\[Phi]])*((-1 + w)*b[M*\[Phi]]*
            (\[Mu][M] + \[Sigma][M]) + w*b[M]*(\[Mu][M*\[Phi]] + 
             \[Sigma][M*\[Phi]])) + ((-1 + w)*b[M*\[Phi]]*(2*\[Lambda][M] - 
             2*\[Mu][M] - \[Sigma][M]) - w*b[M]*(-2*\[Lambda][M*\[Phi]] + 
             2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]))^2)])/
    (4*kRes*((-1 + w)*b[M*\[Phi]]*\[Lambda][M] + w*b[M]*\[Lambda][M*\[Phi]])*
     ((-1 + w)*b[M*\[Phi]]*\[Sigma][M] + w*b[M]*\[Sigma][M*\[Phi]])), 
  C1 -> -1/4*(Y[M]*((-1 + w)^2*b[M*\[Phi]]^2*(-4*\[Lambda][M*\[Phi]]*
          \[Mu]Pred[M]*\[Sigma][M]^2 + kRes*(-1 + w)*\[Alpha]*bPred[M*\[Phi]]*
          Y[M*\[Phi]]*(2*(\[Lambda][M] - \[Mu][M])^2 - 
           (\[Lambda][M] - 3*\[Mu][M])*\[Sigma][M])) + 
       (-1 + w)*b[M*\[Phi]]*(w*b[M]*(-8*\[Lambda][M*\[Phi]]*\[Mu]Pred[M]*
            \[Sigma][M]*\[Sigma][M*\[Phi]] + kRes*(-1 + w)*\[Alpha]*
            bPred[M*\[Phi]]*Y[M*\[Phi]]*(-(\[Lambda][M*\[Phi]]*(4*\[Mu][M] + 
                \[Sigma][M])) + \[Mu][M*\[Phi]]*(4*\[Mu][M] + 3*
                \[Sigma][M]) + \[Lambda][M]*(4*\[Lambda][M*\[Phi]] - 4*
                \[Mu][M*\[Phi]] - \[Sigma][M*\[Phi]]) + 3*\[Mu][M]*
              \[Sigma][M*\[Phi]])) + (-1 + w)*\[Alpha]*bPred[M*\[Phi]]*
          Y[M*\[Phi]]*(\[Lambda][M] - \[Mu][M])*
          Sqrt[kRes^2*((-1 + w)^2*b[M*\[Phi]]^2*(4*\[Lambda][M]^2 - 4*
                \[Lambda][M]*(2*\[Mu][M] + \[Sigma][M]) + (2*\[Mu][M] + 
                 3*\[Sigma][M])^2) + 2*(-1 + w)*w*b[M]*b[M*\[Phi]]*
              (4*(\[Lambda][M] - \[Mu][M])*(\[Lambda][M*\[Phi]] - 
                 \[Mu][M*\[Phi]]) - 2*(\[Lambda][M*\[Phi]] - 
                 3*\[Mu][M*\[Phi]])*\[Sigma][M] + (-2*\[Lambda][M] + 
                 6*\[Mu][M] + 9*\[Sigma][M])*\[Sigma][M*\[Phi]]) + 
             w^2*b[M]^2*(4*\[Lambda][M*\[Phi]]^2 - 4*\[Lambda][M*\[Phi]]*
                (2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]) + 
               (2*\[Mu][M*\[Phi]] + 3*\[Sigma][M*\[Phi]])^2))]) + 
       w*b[M]*(w*b[M]*(-4*\[Lambda][M*\[Phi]]*\[Mu]Pred[M]*\[Sigma][M*\[Phi]]^
             2 + kRes*(-1 + w)*\[Alpha]*bPred[M*\[Phi]]*Y[M*\[Phi]]*
            (2*(\[Lambda][M*\[Phi]] - \[Mu][M*\[Phi]])^2 - 
             (\[Lambda][M*\[Phi]] - 3*\[Mu][M*\[Phi]])*\[Sigma][M*\[Phi]])) + 
         (-1 + w)*\[Alpha]*bPred[M*\[Phi]]*Y[M*\[Phi]]*(\[Lambda][M*\[Phi]] - 
           \[Mu][M*\[Phi]])*Sqrt[kRes^2*((-1 + w)^2*b[M*\[Phi]]^2*
              (4*\[Lambda][M]^2 - 4*\[Lambda][M]*(2*\[Mu][M] + \[Sigma][M]) + 
               (2*\[Mu][M] + 3*\[Sigma][M])^2) + 2*(-1 + w)*w*b[M]*
              b[M*\[Phi]]*(4*(\[Lambda][M] - \[Mu][M])*(\[Lambda][M*\[Phi]] - 
                 \[Mu][M*\[Phi]]) - 2*(\[Lambda][M*\[Phi]] - 
                 3*\[Mu][M*\[Phi]])*\[Sigma][M] + (-2*\[Lambda][M] + 
                 6*\[Mu][M] + 9*\[Sigma][M])*\[Sigma][M*\[Phi]]) + 
             w^2*b[M]^2*(4*\[Lambda][M*\[Phi]]^2 - 4*\[Lambda][M*\[Phi]]*
                (2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]) + 
               (2*\[Mu][M*\[Phi]] + 3*\[Sigma][M*\[Phi]])^2))])))/
     (((-1 + w)*bPred[M*\[Phi]]*Y[M*\[Phi]]*\[Lambda][M] + 
       w*bPred[M]*Y[M]*\[Lambda][M*\[Phi]])*
      ((-1 + w)*b[M*\[Phi]]*\[Sigma][M] + w*b[M]*\[Sigma][M*\[Phi]])^2), 
  C2 -> -1/4*(Y[M*\[Phi]]*(4*\[Lambda][M]*\[Mu]Pred[M]*
        ((-1 + w)*b[M*\[Phi]]*\[Sigma][M] + w*b[M]*\[Sigma][M*\[Phi]])^2 + 
       kRes*w*\[Alpha]*bPred[M]*Y[M]*((-1 + w)^2*b[M*\[Phi]]^2*
          (2*(\[Lambda][M] - \[Mu][M])^2 - (\[Lambda][M] - 3*\[Mu][M])*
            \[Sigma][M]) + (-1 + w)*w*b[M]*b[M*\[Phi]]*
          (-(\[Lambda][M*\[Phi]]*(4*\[Mu][M] + \[Sigma][M])) + 
           \[Mu][M*\[Phi]]*(4*\[Mu][M] + 3*\[Sigma][M]) + 
           \[Lambda][M]*(4*\[Lambda][M*\[Phi]] - 4*\[Mu][M*\[Phi]] - 
             \[Sigma][M*\[Phi]]) + 3*\[Mu][M]*\[Sigma][M*\[Phi]]) + 
         w^2*b[M]^2*(2*(\[Lambda][M*\[Phi]] - \[Mu][M*\[Phi]])^2 - 
           (\[Lambda][M*\[Phi]] - 3*\[Mu][M*\[Phi]])*\[Sigma][M*\[Phi]])) - 
       w*\[Alpha]*b[M*\[Phi]]*bPred[M]*Y[M]*\[Lambda][M]*
        Sqrt[kRes^2*(8*((-1 + w)*b[M*\[Phi]]*\[Sigma][M] + 
             w*b[M]*\[Sigma][M*\[Phi]])*((-1 + w)*b[M*\[Phi]]*
              (\[Mu][M] + \[Sigma][M]) + w*b[M]*(\[Mu][M*\[Phi]] + \[Sigma][
                M*\[Phi]])) + ((-1 + w)*b[M*\[Phi]]*(2*\[Lambda][M] - 2*
                \[Mu][M] - \[Sigma][M]) - w*b[M]*(-2*\[Lambda][M*\[Phi]] + 2*
                \[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]))^2)] + 
       w^2*\[Alpha]*b[M*\[Phi]]*bPred[M]*Y[M]*\[Lambda][M]*
        Sqrt[kRes^2*(8*((-1 + w)*b[M*\[Phi]]*\[Sigma][M] + 
             w*b[M]*\[Sigma][M*\[Phi]])*((-1 + w)*b[M*\[Phi]]*
              (\[Mu][M] + \[Sigma][M]) + w*b[M]*(\[Mu][M*\[Phi]] + \[Sigma][
                M*\[Phi]])) + ((-1 + w)*b[M*\[Phi]]*(2*\[Lambda][M] - 2*
                \[Mu][M] - \[Sigma][M]) - w*b[M]*(-2*\[Lambda][M*\[Phi]] + 2*
                \[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]))^2)] + 
       w^2*\[Alpha]*b[M]*bPred[M]*Y[M]*\[Lambda][M*\[Phi]]*
        Sqrt[kRes^2*(8*((-1 + w)*b[M*\[Phi]]*\[Sigma][M] + 
             w*b[M]*\[Sigma][M*\[Phi]])*((-1 + w)*b[M*\[Phi]]*
              (\[Mu][M] + \[Sigma][M]) + w*b[M]*(\[Mu][M*\[Phi]] + \[Sigma][
                M*\[Phi]])) + ((-1 + w)*b[M*\[Phi]]*(2*\[Lambda][M] - 2*
                \[Mu][M] - \[Sigma][M]) - w*b[M]*(-2*\[Lambda][M*\[Phi]] + 2*
                \[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]))^2)] + 
       w*\[Alpha]*b[M*\[Phi]]*bPred[M]*Y[M]*\[Mu][M]*
        Sqrt[kRes^2*(8*((-1 + w)*b[M*\[Phi]]*\[Sigma][M] + 
             w*b[M]*\[Sigma][M*\[Phi]])*((-1 + w)*b[M*\[Phi]]*
              (\[Mu][M] + \[Sigma][M]) + w*b[M]*(\[Mu][M*\[Phi]] + \[Sigma][
                M*\[Phi]])) + ((-1 + w)*b[M*\[Phi]]*(2*\[Lambda][M] - 2*
                \[Mu][M] - \[Sigma][M]) - w*b[M]*(-2*\[Lambda][M*\[Phi]] + 2*
                \[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]))^2)] - 
       w^2*\[Alpha]*b[M*\[Phi]]*bPred[M]*Y[M]*\[Mu][M]*
        Sqrt[kRes^2*(8*((-1 + w)*b[M*\[Phi]]*\[Sigma][M] + 
             w*b[M]*\[Sigma][M*\[Phi]])*((-1 + w)*b[M*\[Phi]]*
              (\[Mu][M] + \[Sigma][M]) + w*b[M]*(\[Mu][M*\[Phi]] + \[Sigma][
                M*\[Phi]])) + ((-1 + w)*b[M*\[Phi]]*(2*\[Lambda][M] - 2*
                \[Mu][M] - \[Sigma][M]) - w*b[M]*(-2*\[Lambda][M*\[Phi]] + 2*
                \[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]))^2)] - 
       w^2*\[Alpha]*b[M]*bPred[M]*Y[M]*\[Mu][M*\[Phi]]*
        Sqrt[kRes^2*(8*((-1 + w)*b[M*\[Phi]]*\[Sigma][M] + 
             w*b[M]*\[Sigma][M*\[Phi]])*((-1 + w)*b[M*\[Phi]]*
              (\[Mu][M] + \[Sigma][M]) + w*b[M]*(\[Mu][M*\[Phi]] + \[Sigma][
                M*\[Phi]])) + ((-1 + w)*b[M*\[Phi]]*(2*\[Lambda][M] - 2*
                \[Mu][M] - \[Sigma][M]) - w*b[M]*(-2*\[Lambda][M*\[Phi]] + 2*
                \[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]))^2)]))/
     (((-1 + w)*bPred[M*\[Phi]]*Y[M*\[Phi]]*\[Lambda][M] + 
       w*bPred[M]*Y[M]*\[Lambda][M*\[Phi]])*
      ((-1 + w)*b[M*\[Phi]]*\[Sigma][M] + w*b[M]*\[Sigma][M*\[Phi]])^2), 
  R -> -((kRes*(-1 + w)*b[M*\[Phi]]*(2*\[Lambda][M] - 2*\[Mu][M] - 
        \[Sigma][M]) - kRes*w*b[M]*(-2*\[Lambda][M*\[Phi]] + 
        2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]) + 
      Sqrt[kRes^2*(8*((-1 + w)*b[M*\[Phi]]*\[Sigma][M] + 
           w*b[M]*\[Sigma][M*\[Phi]])*((-1 + w)*b[M*\[Phi]]*
            (\[Mu][M] + \[Sigma][M]) + w*b[M]*(\[Mu][M*\[Phi]] + 
             \[Sigma][M*\[Phi]])) + ((-1 + w)*b[M*\[Phi]]*(2*\[Lambda][M] - 
             2*\[Mu][M] - \[Sigma][M]) - w*b[M]*(-2*\[Lambda][M*\[Phi]] + 
             2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]))^2)])/
     (4*(-1 + w)*b[M*\[Phi]]*\[Sigma][M] + 4*w*b[M]*\[Sigma][M*\[Phi]]))}}
