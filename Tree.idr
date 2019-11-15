data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case compare x val of
                                      LT => Node (insert x left) val right
                                      EQ => orig
                                      GT => Node left val (insert x right)


Functor Tree where
        map func Empty = Empty
        map func (Node left e right) = Node (map func left) (func e) (map func right)


Foldable Tree where
  foldr func acc Empty = acc
  foldr func acc (Node left e right) = let leftfold = foldr func acc left
                                           rightfold = foldr func leftfold right in
                                           func e rightfold

