(* Created with the Wolfram Language : www.wolfram.com *)
{{P -> 0, C1 -> 0, C2 -> 0, R -> 0}, {P -> -((\[Mu][M] + \[Sigma][M])/b[M]), 
  C1 -> \[Mu]Pred[M]/bPred[M], C2 -> 0, R -> 0}, 
 {P -> -((\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]])/b[M*\[Phi]]), C1 -> 0, 
  C2 -> \[Mu]Pred[M]/bPred[M*\[Phi]], R -> 0}, 
 {P -> 0, C1 -> 0, C2 -> 0, R -> kRes}, 
 {P -> -1/8*(-8*\[Lambda][M] + 8*\[Mu][M] + 
      ((3*Sqrt[kRes]*Sqrt[\[Alpha]]*Sqrt[bPred[M]]*Sqrt[Y[M]] + 
         Sqrt[9*kRes*\[Alpha]*bPred[M]*Y[M] - 16*\[Lambda][M]*\[Mu]Pred[M]])*
        (kRes*\[Alpha]*bPred[M]*Y[M] + 2*\[Mu]Pred[M]*\[Sigma][M]))/
       (Sqrt[kRes]*Sqrt[\[Alpha]]*Sqrt[bPred[M]]*Sqrt[Y[M]]*\[Mu]Pred[M]))/
     b[M], C1 -> \[Mu]Pred[M]/bPred[M], C2 -> 0, 
  R -> kRes/4 - (Sqrt[kRes]*Sqrt[9*kRes*\[Alpha]*bPred[M]*Y[M] - 
        16*\[Lambda][M]*\[Mu]Pred[M]])/(4*Sqrt[\[Alpha]]*Sqrt[bPred[M]]*
      Sqrt[Y[M]])}, 
 {P -> (8*\[Lambda][M] - 8*\[Mu][M] + 
     ((-3*Sqrt[kRes]*Sqrt[\[Alpha]]*Sqrt[bPred[M]]*Sqrt[Y[M]] + 
        Sqrt[9*kRes*\[Alpha]*bPred[M]*Y[M] - 16*\[Lambda][M]*\[Mu]Pred[M]])*
       (kRes*\[Alpha]*bPred[M]*Y[M] + 2*\[Mu]Pred[M]*\[Sigma][M]))/
      (Sqrt[kRes]*Sqrt[\[Alpha]]*Sqrt[bPred[M]]*Sqrt[Y[M]]*\[Mu]Pred[M]))/
    (8*b[M]), C1 -> \[Mu]Pred[M]/bPred[M], C2 -> 0, 
  R -> kRes/4 + (Sqrt[kRes]*Sqrt[9*kRes*\[Alpha]*bPred[M]*Y[M] - 
        16*\[Lambda][M]*\[Mu]Pred[M]])/(4*Sqrt[\[Alpha]]*Sqrt[bPred[M]]*
      Sqrt[Y[M]])}, 
 {P -> -1/8*(-8*\[Lambda][M*\[Phi]] + 8*\[Mu][M*\[Phi]] + 
      ((3*Sqrt[kRes]*Sqrt[\[Alpha]]*Sqrt[bPred[M*\[Phi]]]*Sqrt[Y[M*\[Phi]]] + 
         Sqrt[9*kRes*\[Alpha]*bPred[M*\[Phi]]*Y[M*\[Phi]] - 
           16*\[Lambda][M*\[Phi]]*\[Mu]Pred[M]])*
        (kRes*\[Alpha]*bPred[M*\[Phi]]*Y[M*\[Phi]] + 2*\[Mu]Pred[M]*
          \[Sigma][M*\[Phi]]))/(Sqrt[kRes]*Sqrt[\[Alpha]]*
        Sqrt[bPred[M*\[Phi]]]*Sqrt[Y[M*\[Phi]]]*\[Mu]Pred[M]))/b[M*\[Phi]], 
  C1 -> 0, C2 -> \[Mu]Pred[M]/bPred[M*\[Phi]], 
  R -> kRes/4 - (Sqrt[kRes]*Sqrt[9*kRes*\[Alpha]*bPred[M*\[Phi]]*
         Y[M*\[Phi]] - 16*\[Lambda][M*\[Phi]]*\[Mu]Pred[M]])/
     (4*Sqrt[\[Alpha]]*Sqrt[bPred[M*\[Phi]]]*Sqrt[Y[M*\[Phi]]])}, 
 {P -> (8*\[Lambda][M*\[Phi]] - 8*\[Mu][M*\[Phi]] + 
     ((-3*Sqrt[kRes]*Sqrt[\[Alpha]]*Sqrt[bPred[M*\[Phi]]]*Sqrt[Y[M*\[Phi]]] + 
        Sqrt[9*kRes*\[Alpha]*bPred[M*\[Phi]]*Y[M*\[Phi]] - 
          16*\[Lambda][M*\[Phi]]*\[Mu]Pred[M]])*
       (kRes*\[Alpha]*bPred[M*\[Phi]]*Y[M*\[Phi]] + 2*\[Mu]Pred[M]*
         \[Sigma][M*\[Phi]]))/(Sqrt[kRes]*Sqrt[\[Alpha]]*
       Sqrt[bPred[M*\[Phi]]]*Sqrt[Y[M*\[Phi]]]*\[Mu]Pred[M]))/
    (8*b[M*\[Phi]]), C1 -> 0, C2 -> \[Mu]Pred[M]/bPred[M*\[Phi]], 
  R -> kRes/4 + (Sqrt[kRes]*Sqrt[9*kRes*\[Alpha]*bPred[M*\[Phi]]*
         Y[M*\[Phi]] - 16*\[Lambda][M*\[Phi]]*\[Mu]Pred[M]])/
     (4*Sqrt[\[Alpha]]*Sqrt[bPred[M*\[Phi]]]*Sqrt[Y[M*\[Phi]]])}, 
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
 {
  P -> (kRes*b[M*\[Phi]]*(\[Sigma][M]*(2*\[Lambda][M]*(\[Lambda][M*\[Phi]] - 
           2*\[Mu][M*\[Phi]]) + \[Lambda][M*\[Phi]]*(2*\[Mu][M] + 
           3*\[Sigma][M])) + \[Lambda][M]*(-2*\[Lambda][M] + 2*\[Mu][M] - 
         3*\[Sigma][M])*\[Sigma][M*\[Phi]]) + 
     kRes*b[M]*(-2*\[Lambda][M*\[Phi]]^2*\[Sigma][M] + 
       \[Lambda][M]*\[Sigma][M*\[Phi]]*(2*\[Mu][M*\[Phi]] + 
         3*\[Sigma][M*\[Phi]]) + \[Lambda][M*\[Phi]]*
        (2*\[Mu][M*\[Phi]]*\[Sigma][M] + (2*\[Lambda][M] - 4*\[Mu][M] - 
           3*\[Sigma][M])*\[Sigma][M*\[Phi]])) + 
     (-(\[Lambda][M*\[Phi]]*\[Sigma][M]) + \[Lambda][M]*\[Sigma][M*\[Phi]])*
      Sqrt[kRes^2*(b[M*\[Phi]]^2*(4*\[Lambda][M]^2 - 4*\[Lambda][M]*
            (2*\[Mu][M] + \[Sigma][M]) + (2*\[Mu][M] + 3*\[Sigma][M])^2) - 
         2*b[M]*b[M*\[Phi]]*(4*(\[Lambda][M] - \[Mu][M])*
            (\[Lambda][M*\[Phi]] - \[Mu][M*\[Phi]]) - 
           2*(\[Lambda][M*\[Phi]] - 3*\[Mu][M*\[Phi]])*\[Sigma][M] + 
           (-2*\[Lambda][M] + 6*\[Mu][M] + 9*\[Sigma][M])*
            \[Sigma][M*\[Phi]]) + b[M]^2*(4*\[Lambda][M*\[Phi]]^2 - 
           4*\[Lambda][M*\[Phi]]*(2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]) + 
           (2*\[Mu][M*\[Phi]] + 3*\[Sigma][M*\[Phi]])^2))])/
    (4*kRes*(b[M*\[Phi]]*\[Lambda][M] - b[M]*\[Lambda][M*\[Phi]])*
     (b[M*\[Phi]]*\[Sigma][M] - b[M]*\[Sigma][M*\[Phi]])), 
  C1 -> -1/4*(Y[M]*(b[M*\[Phi]]^2*(4*\[Lambda][M*\[Phi]]*\[Mu]Pred[M]*
          \[Sigma][M]^2 + kRes*\[Alpha]*bPred[M*\[Phi]]*Y[M*\[Phi]]*
          (2*(\[Lambda][M] - \[Mu][M])^2 - (\[Lambda][M] - 3*\[Mu][M])*
            \[Sigma][M])) + b[M*\[Phi]]*
        (b[M]*(kRes*\[Alpha]*bPred[M*\[Phi]]*Y[M*\[Phi]]*
            (-4*(\[Lambda][M] - \[Mu][M])*(\[Lambda][M*\[Phi]] - \[Mu][
                M*\[Phi]]) + (\[Lambda][M*\[Phi]] - 3*\[Mu][M*\[Phi]])*
              \[Sigma][M]) + (kRes*\[Alpha]*bPred[M*\[Phi]]*Y[M*\[Phi]]*
              (\[Lambda][M] - 3*\[Mu][M]) - 8*\[Lambda][M*\[Phi]]*
              \[Mu]Pred[M]*\[Sigma][M])*\[Sigma][M*\[Phi]]) + 
         \[Alpha]*bPred[M*\[Phi]]*Y[M*\[Phi]]*(-\[Lambda][M] + \[Mu][M])*
          Sqrt[kRes^2*(b[M*\[Phi]]^2*(4*\[Lambda][M]^2 - 4*\[Lambda][M]*
                (2*\[Mu][M] + \[Sigma][M]) + (2*\[Mu][M] + 3*\[Sigma][M])^
                2) - 2*b[M]*b[M*\[Phi]]*(4*(\[Lambda][M] - \[Mu][M])*
                (\[Lambda][M*\[Phi]] - \[Mu][M*\[Phi]]) - 2*
                (\[Lambda][M*\[Phi]] - 3*\[Mu][M*\[Phi]])*\[Sigma][M] + 
               (-2*\[Lambda][M] + 6*\[Mu][M] + 9*\[Sigma][M])*\[Sigma][
                 M*\[Phi]]) + b[M]^2*(4*\[Lambda][M*\[Phi]]^2 - 4*
                \[Lambda][M*\[Phi]]*(2*\[Mu][M*\[Phi]] + \[Sigma][
                  M*\[Phi]]) + (2*\[Mu][M*\[Phi]] + 3*\[Sigma][M*\[Phi]])^
                2))]) + b[M]*(b[M]*(4*\[Lambda][M*\[Phi]]*\[Mu]Pred[M]*
            \[Sigma][M*\[Phi]]^2 + kRes*\[Alpha]*bPred[M*\[Phi]]*Y[M*\[Phi]]*
            (2*(\[Lambda][M*\[Phi]] - \[Mu][M*\[Phi]])^2 - 
             (\[Lambda][M*\[Phi]] - 3*\[Mu][M*\[Phi]])*\[Sigma][M*\[Phi]])) + 
         \[Alpha]*bPred[M*\[Phi]]*Y[M*\[Phi]]*(\[Lambda][M*\[Phi]] - 
           \[Mu][M*\[Phi]])*Sqrt[kRes^2*(b[M*\[Phi]]^2*(4*\[Lambda][M]^2 - 4*
                \[Lambda][M]*(2*\[Mu][M] + \[Sigma][M]) + (2*\[Mu][M] + 
                 3*\[Sigma][M])^2) - 2*b[M]*b[M*\[Phi]]*(4*(\[Lambda][M] - 
                 \[Mu][M])*(\[Lambda][M*\[Phi]] - \[Mu][M*\[Phi]]) - 2*
                (\[Lambda][M*\[Phi]] - 3*\[Mu][M*\[Phi]])*\[Sigma][M] + 
               (-2*\[Lambda][M] + 6*\[Mu][M] + 9*\[Sigma][M])*\[Sigma][
                 M*\[Phi]]) + b[M]^2*(4*\[Lambda][M*\[Phi]]^2 - 4*
                \[Lambda][M*\[Phi]]*(2*\[Mu][M*\[Phi]] + \[Sigma][
                  M*\[Phi]]) + (2*\[Mu][M*\[Phi]] + 3*\[Sigma][M*\[Phi]])^
                2))])))/((bPred[M*\[Phi]]*Y[M*\[Phi]]*\[Lambda][M] - 
       bPred[M]*Y[M]*\[Lambda][M*\[Phi]])*(b[M*\[Phi]]*\[Sigma][M] - 
        b[M]*\[Sigma][M*\[Phi]])^2), 
  C2 -> (Y[M*\[Phi]]*(b[M*\[Phi]]^2*(4*\[Lambda][M]*\[Mu]Pred[M]*
         \[Sigma][M]^2 + kRes*\[Alpha]*bPred[M]*Y[M]*
         (2*(\[Lambda][M] - \[Mu][M])^2 - (\[Lambda][M] - 3*\[Mu][M])*
           \[Sigma][M])) + b[M*\[Phi]]*
       (b[M]*(kRes*\[Alpha]*bPred[M]*Y[M]*(-4*(\[Lambda][M] - \[Mu][M])*
             (\[Lambda][M*\[Phi]] - \[Mu][M*\[Phi]]) + 
            (\[Lambda][M*\[Phi]] - 3*\[Mu][M*\[Phi]])*\[Sigma][M]) + 
          (kRes*\[Alpha]*bPred[M]*Y[M]*(\[Lambda][M] - 3*\[Mu][M]) - 
            8*\[Lambda][M]*\[Mu]Pred[M]*\[Sigma][M])*\[Sigma][M*\[Phi]]) + 
        \[Alpha]*bPred[M]*Y[M]*(-\[Lambda][M] + \[Mu][M])*
         Sqrt[kRes^2*(b[M*\[Phi]]^2*(4*\[Lambda][M]^2 - 4*\[Lambda][M]*(
                2*\[Mu][M] + \[Sigma][M]) + (2*\[Mu][M] + 3*\[Sigma][M])^2) - 
            2*b[M]*b[M*\[Phi]]*(4*(\[Lambda][M] - \[Mu][M])*(\[Lambda][
                 M*\[Phi]] - \[Mu][M*\[Phi]]) - 2*(\[Lambda][M*\[Phi]] - 
                3*\[Mu][M*\[Phi]])*\[Sigma][M] + (-2*\[Lambda][M] + 
                6*\[Mu][M] + 9*\[Sigma][M])*\[Sigma][M*\[Phi]]) + 
            b[M]^2*(4*\[Lambda][M*\[Phi]]^2 - 4*\[Lambda][M*\[Phi]]*(
                2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]) + 
              (2*\[Mu][M*\[Phi]] + 3*\[Sigma][M*\[Phi]])^2))]) + 
      b[M]*(b[M]*(4*\[Lambda][M]*\[Mu]Pred[M]*\[Sigma][M*\[Phi]]^2 + 
          kRes*\[Alpha]*bPred[M]*Y[M]*(2*(\[Lambda][M*\[Phi]] - \[Mu][
                M*\[Phi]])^2 - (\[Lambda][M*\[Phi]] - 3*\[Mu][M*\[Phi]])*
             \[Sigma][M*\[Phi]])) + \[Alpha]*bPred[M]*Y[M]*
         (\[Lambda][M*\[Phi]] - \[Mu][M*\[Phi]])*
         Sqrt[kRes^2*(b[M*\[Phi]]^2*(4*\[Lambda][M]^2 - 4*\[Lambda][M]*(
                2*\[Mu][M] + \[Sigma][M]) + (2*\[Mu][M] + 3*\[Sigma][M])^2) - 
            2*b[M]*b[M*\[Phi]]*(4*(\[Lambda][M] - \[Mu][M])*(\[Lambda][
                 M*\[Phi]] - \[Mu][M*\[Phi]]) - 2*(\[Lambda][M*\[Phi]] - 
                3*\[Mu][M*\[Phi]])*\[Sigma][M] + (-2*\[Lambda][M] + 
                6*\[Mu][M] + 9*\[Sigma][M])*\[Sigma][M*\[Phi]]) + 
            b[M]^2*(4*\[Lambda][M*\[Phi]]^2 - 4*\[Lambda][M*\[Phi]]*(
                2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]) + 
              (2*\[Mu][M*\[Phi]] + 3*\[Sigma][M*\[Phi]])^2))])))/
    (4*(bPred[M*\[Phi]]*Y[M*\[Phi]]*\[Lambda][M] - 
      bPred[M]*Y[M]*\[Lambda][M*\[Phi]])*
     (b[M*\[Phi]]*\[Sigma][M] - b[M]*\[Sigma][M*\[Phi]])^2), 
  R -> (kRes*b[M*\[Phi]]*(-2*\[Lambda][M] + 2*\[Mu][M] + \[Sigma][M]) + 
     kRes*b[M]*(2*\[Lambda][M*\[Phi]] - 2*\[Mu][M*\[Phi]] - 
       \[Sigma][M*\[Phi]]) + 
     Sqrt[kRes^2*(8*(b[M*\[Phi]]*\[Sigma][M] - b[M]*\[Sigma][M*\[Phi]])*
         (b[M*\[Phi]]*(\[Mu][M] + \[Sigma][M]) - b[M]*(\[Mu][M*\[Phi]] + 
            \[Sigma][M*\[Phi]])) + 
        (b[M*\[Phi]]*(2*\[Lambda][M] - 2*\[Mu][M] - \[Sigma][M]) + 
          b[M]*(-2*\[Lambda][M*\[Phi]] + 2*\[Mu][M*\[Phi]] + 
            \[Sigma][M*\[Phi]]))^2)])/(4*b[M*\[Phi]]*\[Sigma][M] - 
     4*b[M]*\[Sigma][M*\[Phi]])}, 
 {
  P -> (kRes*b[M*\[Phi]]*(\[Sigma][M]*(2*\[Lambda][M]*(\[Lambda][M*\[Phi]] - 
           2*\[Mu][M*\[Phi]]) + \[Lambda][M*\[Phi]]*(2*\[Mu][M] + 
           3*\[Sigma][M])) + \[Lambda][M]*(-2*\[Lambda][M] + 2*\[Mu][M] - 
         3*\[Sigma][M])*\[Sigma][M*\[Phi]]) + 
     kRes*b[M]*(-2*\[Lambda][M*\[Phi]]^2*\[Sigma][M] + 
       \[Lambda][M]*\[Sigma][M*\[Phi]]*(2*\[Mu][M*\[Phi]] + 
         3*\[Sigma][M*\[Phi]]) + \[Lambda][M*\[Phi]]*
        (2*\[Mu][M*\[Phi]]*\[Sigma][M] + (2*\[Lambda][M] - 4*\[Mu][M] - 
           3*\[Sigma][M])*\[Sigma][M*\[Phi]])) + 
     (\[Lambda][M*\[Phi]]*\[Sigma][M] - \[Lambda][M]*\[Sigma][M*\[Phi]])*
      Sqrt[kRes^2*(b[M*\[Phi]]^2*(4*\[Lambda][M]^2 - 4*\[Lambda][M]*
            (2*\[Mu][M] + \[Sigma][M]) + (2*\[Mu][M] + 3*\[Sigma][M])^2) - 
         2*b[M]*b[M*\[Phi]]*(4*(\[Lambda][M] - \[Mu][M])*
            (\[Lambda][M*\[Phi]] - \[Mu][M*\[Phi]]) - 
           2*(\[Lambda][M*\[Phi]] - 3*\[Mu][M*\[Phi]])*\[Sigma][M] + 
           (-2*\[Lambda][M] + 6*\[Mu][M] + 9*\[Sigma][M])*
            \[Sigma][M*\[Phi]]) + b[M]^2*(4*\[Lambda][M*\[Phi]]^2 - 
           4*\[Lambda][M*\[Phi]]*(2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]) + 
           (2*\[Mu][M*\[Phi]] + 3*\[Sigma][M*\[Phi]])^2))])/
    (4*kRes*(b[M*\[Phi]]*\[Lambda][M] - b[M]*\[Lambda][M*\[Phi]])*
     (b[M*\[Phi]]*\[Sigma][M] - b[M]*\[Sigma][M*\[Phi]])), 
  C1 -> -1/4*(Y[M]*(b[M*\[Phi]]^2*(4*\[Lambda][M*\[Phi]]*\[Mu]Pred[M]*
          \[Sigma][M]^2 + kRes*\[Alpha]*bPred[M*\[Phi]]*Y[M*\[Phi]]*
          (2*(\[Lambda][M] - \[Mu][M])^2 - (\[Lambda][M] - 3*\[Mu][M])*
            \[Sigma][M])) + b[M*\[Phi]]*
        (b[M]*(kRes*\[Alpha]*bPred[M*\[Phi]]*Y[M*\[Phi]]*
            (-4*(\[Lambda][M] - \[Mu][M])*(\[Lambda][M*\[Phi]] - \[Mu][
                M*\[Phi]]) + (\[Lambda][M*\[Phi]] - 3*\[Mu][M*\[Phi]])*
              \[Sigma][M]) + (kRes*\[Alpha]*bPred[M*\[Phi]]*Y[M*\[Phi]]*
              (\[Lambda][M] - 3*\[Mu][M]) - 8*\[Lambda][M*\[Phi]]*
              \[Mu]Pred[M]*\[Sigma][M])*\[Sigma][M*\[Phi]]) + 
         \[Alpha]*bPred[M*\[Phi]]*Y[M*\[Phi]]*(\[Lambda][M] - \[Mu][M])*
          Sqrt[kRes^2*(b[M*\[Phi]]^2*(4*\[Lambda][M]^2 - 4*\[Lambda][M]*
                (2*\[Mu][M] + \[Sigma][M]) + (2*\[Mu][M] + 3*\[Sigma][M])^
                2) - 2*b[M]*b[M*\[Phi]]*(4*(\[Lambda][M] - \[Mu][M])*
                (\[Lambda][M*\[Phi]] - \[Mu][M*\[Phi]]) - 2*
                (\[Lambda][M*\[Phi]] - 3*\[Mu][M*\[Phi]])*\[Sigma][M] + 
               (-2*\[Lambda][M] + 6*\[Mu][M] + 9*\[Sigma][M])*\[Sigma][
                 M*\[Phi]]) + b[M]^2*(4*\[Lambda][M*\[Phi]]^2 - 4*
                \[Lambda][M*\[Phi]]*(2*\[Mu][M*\[Phi]] + \[Sigma][
                  M*\[Phi]]) + (2*\[Mu][M*\[Phi]] + 3*\[Sigma][M*\[Phi]])^
                2))]) + b[M]*(b[M]*(4*\[Lambda][M*\[Phi]]*\[Mu]Pred[M]*
            \[Sigma][M*\[Phi]]^2 + kRes*\[Alpha]*bPred[M*\[Phi]]*Y[M*\[Phi]]*
            (2*(\[Lambda][M*\[Phi]] - \[Mu][M*\[Phi]])^2 - 
             (\[Lambda][M*\[Phi]] - 3*\[Mu][M*\[Phi]])*\[Sigma][M*\[Phi]])) + 
         \[Alpha]*bPred[M*\[Phi]]*Y[M*\[Phi]]*(-\[Lambda][M*\[Phi]] + 
           \[Mu][M*\[Phi]])*Sqrt[kRes^2*(b[M*\[Phi]]^2*(4*\[Lambda][M]^2 - 4*
                \[Lambda][M]*(2*\[Mu][M] + \[Sigma][M]) + (2*\[Mu][M] + 
                 3*\[Sigma][M])^2) - 2*b[M]*b[M*\[Phi]]*(4*(\[Lambda][M] - 
                 \[Mu][M])*(\[Lambda][M*\[Phi]] - \[Mu][M*\[Phi]]) - 2*
                (\[Lambda][M*\[Phi]] - 3*\[Mu][M*\[Phi]])*\[Sigma][M] + 
               (-2*\[Lambda][M] + 6*\[Mu][M] + 9*\[Sigma][M])*\[Sigma][
                 M*\[Phi]]) + b[M]^2*(4*\[Lambda][M*\[Phi]]^2 - 4*
                \[Lambda][M*\[Phi]]*(2*\[Mu][M*\[Phi]] + \[Sigma][
                  M*\[Phi]]) + (2*\[Mu][M*\[Phi]] + 3*\[Sigma][M*\[Phi]])^
                2))])))/((bPred[M*\[Phi]]*Y[M*\[Phi]]*\[Lambda][M] - 
       bPred[M]*Y[M]*\[Lambda][M*\[Phi]])*(b[M*\[Phi]]*\[Sigma][M] - 
        b[M]*\[Sigma][M*\[Phi]])^2), 
  C2 -> (Y[M*\[Phi]]*(b[M*\[Phi]]^2*(4*\[Lambda][M]*\[Mu]Pred[M]*
         \[Sigma][M]^2 + kRes*\[Alpha]*bPred[M]*Y[M]*
         (2*(\[Lambda][M] - \[Mu][M])^2 - (\[Lambda][M] - 3*\[Mu][M])*
           \[Sigma][M])) + b[M*\[Phi]]*
       (b[M]*(kRes*\[Alpha]*bPred[M]*Y[M]*(-4*(\[Lambda][M] - \[Mu][M])*
             (\[Lambda][M*\[Phi]] - \[Mu][M*\[Phi]]) + 
            (\[Lambda][M*\[Phi]] - 3*\[Mu][M*\[Phi]])*\[Sigma][M]) + 
          (kRes*\[Alpha]*bPred[M]*Y[M]*(\[Lambda][M] - 3*\[Mu][M]) - 
            8*\[Lambda][M]*\[Mu]Pred[M]*\[Sigma][M])*\[Sigma][M*\[Phi]]) + 
        \[Alpha]*bPred[M]*Y[M]*(\[Lambda][M] - \[Mu][M])*
         Sqrt[kRes^2*(b[M*\[Phi]]^2*(4*\[Lambda][M]^2 - 4*\[Lambda][M]*(
                2*\[Mu][M] + \[Sigma][M]) + (2*\[Mu][M] + 3*\[Sigma][M])^2) - 
            2*b[M]*b[M*\[Phi]]*(4*(\[Lambda][M] - \[Mu][M])*(\[Lambda][
                 M*\[Phi]] - \[Mu][M*\[Phi]]) - 2*(\[Lambda][M*\[Phi]] - 
                3*\[Mu][M*\[Phi]])*\[Sigma][M] + (-2*\[Lambda][M] + 
                6*\[Mu][M] + 9*\[Sigma][M])*\[Sigma][M*\[Phi]]) + 
            b[M]^2*(4*\[Lambda][M*\[Phi]]^2 - 4*\[Lambda][M*\[Phi]]*(
                2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]) + 
              (2*\[Mu][M*\[Phi]] + 3*\[Sigma][M*\[Phi]])^2))]) + 
      b[M]*(b[M]*(4*\[Lambda][M]*\[Mu]Pred[M]*\[Sigma][M*\[Phi]]^2 + 
          kRes*\[Alpha]*bPred[M]*Y[M]*(2*(\[Lambda][M*\[Phi]] - \[Mu][
                M*\[Phi]])^2 - (\[Lambda][M*\[Phi]] - 3*\[Mu][M*\[Phi]])*
             \[Sigma][M*\[Phi]])) + \[Alpha]*bPred[M]*Y[M]*
         (-\[Lambda][M*\[Phi]] + \[Mu][M*\[Phi]])*
         Sqrt[kRes^2*(b[M*\[Phi]]^2*(4*\[Lambda][M]^2 - 4*\[Lambda][M]*(
                2*\[Mu][M] + \[Sigma][M]) + (2*\[Mu][M] + 3*\[Sigma][M])^2) - 
            2*b[M]*b[M*\[Phi]]*(4*(\[Lambda][M] - \[Mu][M])*(\[Lambda][
                 M*\[Phi]] - \[Mu][M*\[Phi]]) - 2*(\[Lambda][M*\[Phi]] - 
                3*\[Mu][M*\[Phi]])*\[Sigma][M] + (-2*\[Lambda][M] + 
                6*\[Mu][M] + 9*\[Sigma][M])*\[Sigma][M*\[Phi]]) + 
            b[M]^2*(4*\[Lambda][M*\[Phi]]^2 - 4*\[Lambda][M*\[Phi]]*(
                2*\[Mu][M*\[Phi]] + \[Sigma][M*\[Phi]]) + 
              (2*\[Mu][M*\[Phi]] + 3*\[Sigma][M*\[Phi]])^2))])))/
    (4*(bPred[M*\[Phi]]*Y[M*\[Phi]]*\[Lambda][M] - 
      bPred[M]*Y[M]*\[Lambda][M*\[Phi]])*
     (b[M*\[Phi]]*\[Sigma][M] - b[M]*\[Sigma][M*\[Phi]])^2), 
  R -> (kRes*b[M*\[Phi]]*(-2*\[Lambda][M] + 2*\[Mu][M] + \[Sigma][M]) + 
     kRes*b[M]*(2*\[Lambda][M*\[Phi]] - 2*\[Mu][M*\[Phi]] - 
       \[Sigma][M*\[Phi]]) - 
     Sqrt[kRes^2*(8*(b[M*\[Phi]]*\[Sigma][M] - b[M]*\[Sigma][M*\[Phi]])*
         (b[M*\[Phi]]*(\[Mu][M] + \[Sigma][M]) - b[M]*(\[Mu][M*\[Phi]] + 
            \[Sigma][M*\[Phi]])) + 
        (b[M*\[Phi]]*(2*\[Lambda][M] - 2*\[Mu][M] - \[Sigma][M]) + 
          b[M]*(-2*\[Lambda][M*\[Phi]] + 2*\[Mu][M*\[Phi]] + 
            \[Sigma][M*\[Phi]]))^2)])/(4*b[M*\[Phi]]*\[Sigma][M] - 
     4*b[M]*\[Sigma][M*\[Phi]])}}
