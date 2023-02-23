-- https://www.cis.upenn.edu/~cis1940/spring13/hw/07-folds-monoids.pdf

module Ex7
    ( 
    ) where


-- Exercise 1 We first consider how to write some simple operations on these
-- JoinLists. Perhaps the most important operation we will consider is how to
-- append two JoinLists. Previously, we said that the point of JoinLists is to
-- represent append operations as data, but what about the annotations? Write an
-- append function for JoinLists that yields a new JoinList whose monoidal
-- annotation is derived from those of the two arguments.
--   (+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a

-- You may find it helpful to implement a helper function
--   tag :: Monoid m => JoinList m a -> m
-- which gets the annotation at the root of a JoinList.

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty 
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) j k = Append (tag j <> tag k) j k
