labelWith : Stream labelType -> List a -> List (labelType, a)
labelWith xs [] = []
labelWith (lbl :: lbls) (val :: vals) = (lbl, val) :: labelWith lbls vals


label : List a -> List (Integer, a)
label = labelWith (iterate (+1) 0)
