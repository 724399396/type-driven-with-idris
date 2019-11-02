maybeAdd : Maybe Int -> Maybe Int -> Maybe Int
maybeAdd x y = case x of
                    Nothing => Nothing
                    Just x_val => case y of
                                       Nothing => Nothing
                                       Just y_val => Just (x_val + y_val)
