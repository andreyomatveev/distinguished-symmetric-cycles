-- Andrey O. Matveev
-- After several observations mentioned in the monograph A.O. Matveev, Symmetric Cycles: A 2D Perspective
-- on Higher Dimensional Discrete Hypercubes, the Power Sets of Finite Sets, and Set Families,
-- Leanpub, 2022, https://leanpub.com/SymmetricCycles .
--
-- We deal with vertices of the hypercube graph H(t,2). --- Recall that the vertex set of
-- the graph H(t,2) by convention is the set {1,-1}^t of t-dimensional {1,-1}-valued row vectors that are regarded
-- as elements of the t-dimensional real Euclidean space.
-- We call the set of positive integers {1,2, ...,t} the ground set.
-- Two vertices of the hypercube graph H(t,2) by convention are adjacent if and only if there is precisely one element (call it an index) 
-- in the ground set for which the corresponding components of the vertices differ (in other words, the Hamming distance 
-- between those vertices equals 1).
-- The set {1,-1}^t of cardinality 2^t is sometimes called a t-dimensional discrete hypercube. --- Another (and,
-- arguably, much more popular) discrete hypercube is the set {0,1}^t.
--
-- As said earlier, on the theory side, under the vertices of hypercube graphs, we mean row vectors of Euclidean spaces but, on the code side, 
-- in the present Haskell module we implement vertices as Data.Sequence-stuctures 
-- (https://hackage.haskell.org/package/containers/docs/Data-Sequence.html).

{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module DistinguishedSymmetricCyclesModule
  ( getVertexOfPMOneDistingSymmCycle
  , obtainDecompSequenceForPMOneVertexWRTDistingSymmCycle
  , obtainXVectorForPMOneVertexWRTDistingSymmCycle
  ) where

import           Data.List     (find)
import           Data.Maybe    (isNothing)

import qualified Data.Sequence as S

type PMOne = Int

type PMOneVertex = S.Seq PMOne

getVertexOfPMOneDistingSymmCycle :: Int -> Int -> S.Seq PMOne
-- The name means "To get a vertex of a distinguished symmetric cycle in a hypercube graph".
--
-- This function returns the "orderNumber"th vertex of the distinguished symmetric cycle in the hypercube graph H("dimension", 2).
-- The index set of the negative part of this vertex is one (maybe, empty) interval of the ground set {1, 2, ..., "dimension"}.
-- The index of one endpont of the nonempty negative interval is an endpoint (i.e., the minimal or maximal element) of the ground set.
-- The index set of the negative part of the "dimension"th vertex of the distinguished symmetric cycle is the entire ground set.
--
-- Call for instance
--    ghci> getVertexOfDistingSymmCycle 6 0
-- to get the result:
--    fromList [1,1,1,1,1,1]
-- Call
--    ghci> getVertexOfDistingSymmCycle 6 6
-- to get the result:
--    fromList [-1,-1,-1,-1,-1,-1]
-- Call
--    ghci> getVertexOfDistingSymmCycle 6 4
-- to get the result:
--    fromList [-1,-1,-1,-1,1,1]
-- Call
--    ghci> getVertexOfDistingSymmCycle 6 11
-- to get the result:
--    fromList [1,1,1,1,1,-1]
getVertexOfPMOneDistingSymmCycle dimension orderNumber
  | dimension <= 3 = S.empty -- "N/A: Dimension of the vertex under construction should be >= 3"
  | orderNumber < 0 || orderNumber >= 2 * dimension = S.empty -- "N/A: Order number of a vertex in the distinguished symmetric cycle should be between 0 (included) and (2*dimension - 1) (included)"
  | otherwise =
    if orderNumber < dimension
      then S.replicate orderNumber ((-1) :: PMOne) S.><
           S.replicate (dimension - orderNumber) (1 :: PMOne)
      else S.replicate (orderNumber - dimension) (1 :: PMOne) S.><
           S.replicate (2 * dimension - orderNumber) ((-1) :: PMOne)

obtainDecompSequenceForPMOneVertexWRTDistingSymmCycle :: S.Seq Int -> S.Seq Int
-- The name means "To obtain the (index sequence of) decomposition sequence for a vertex with respect to a distinguished symmetric cycle in a hypercube graph".
--
-- Given a vertex "intSeq" of the hypercube graph H((length intSeq),2), this function returns an ascending sequence
-- of nonnegative integer superscripts (i, j, ..., s) such that the set {R^i, R^j, ..., R^s} is the unique inclusion-minimal
-- subset of the vertex set of the distinguished symmetric cycle in the graph H((length intSeq),2) for which we have
--    "intSeq" = R^i + R^j + ... + R^s ,
-- that means the familiar componentwise summation of integer-valued vectors.
-- Enjoy two specific properties of the decomposition set {R^i, R^j, ..., R^s} for the input vertex "intSeq":
--    this decomposition set contains an odd number of vertices, and
--    the vertices of the decomposition set, regarded as elements of the (length intSeq)-dimensional real Euclidean space, are linearly independent.
-- Thus, our procedure of decomposing has strong linear algebraic roots, but in reality it is in a sense computation-free, since the procedure is merely
-- based on revealing the interval structure of the index set of the negative part of a vertex.
-- Call for instance (for diving into Proposition 4.9(i), Example 4.10(i) and Figure 4.3 of the monograph)
--    ghci>  obtainDecompSequenceForPMOneVertexWRTDistingSymmCycle (S.fromList [-1, -1, 1, -1, -1, 1])
-- to get the result:
--    fromList [2,5,9]
-- Call (cf. Proposition 4.9(ii), Example 4.10(ii) and Figure 4.4 of the monograph)
--    ghci> obtainDecompSequenceForPMOneVertexWRTDistingSymmCycle (S.fromList [-1, 1, -1, 1, 1, -1])
-- to get the result:
--    fromList [1,3,6,8,11]
-- Call (cf. Proposition 4.9(iii), Example 4.10(iii) and Figure 4.5 of the monograph)
--    ghci> obtainDecompSequenceForPMOneVertexWRTDistingSymmCycle (S.fromList [1, -1, -1, 1, -1, 1])
-- to get the result:
--    fromList [0,3,5,7,10]
-- Call (cf. Proposition 4.9(iv), Example 4.10(iv) and Figure 4.6 of the monograph)
--    ghci> obtainDecompSequenceForPMOneVertexWRTDistingSymmCycle (S.fromList [1, 1, -1, 1, -1, -1])
-- to get the result:
--    fromList [3,8,10]
obtainDecompSequenceForPMOneVertexWRTDistingSymmCycle intSeq =
  if not (isPMOneVertex intSeq)
    then S.empty -- "N/A: The input integer collection is not a plus-one-minus-one-valued vector of dimension >= 3"
    else do
      let indexedXVector =
            S.zip
              (S.fromList [0 .. S.length intSeq - 1])
              (getXVectorWRTDistingSymmCycle intSeq)
      let decompSubseqs =
            getDecompSequenceForPMOneVertexWRTDistingSymmCycle
              (S.length intSeq)
              indexedXVector
              (S.empty, S.empty)
      uncurry (S.><) decompSubseqs

obtainXVectorForPMOneVertexWRTDistingSymmCycle :: S.Seq Int -> S.Seq Int
-- The name means "To obtain the x-vector for a vertex with respect to a distinguished symmetric cycle in a hypercube graph".
-- The x-vector is defined in Section 1.1.4 of the monograph. The x-vector is just a concise way to describe
-- the decomposition set for a vertex with respect to a symmetric cycle. On the theory side, its components
-- are indexed starting with 1.
--
-- Call for instance (for diving once again into Proposition 4.9(i), Example 4.10(i) and Figure 4.3 of the monograph)
--    ghci>  obtainXVectorForPMOneVertexWRTDistingSymmCycle (S.fromList [-1, -1, 1, -1, -1, 1])
-- to get the result:
--    fromList [0,0,1,-1,0,1]
-- Call (cf. Proposition 4.9(ii), Example 4.10(ii) and Figure 4.4 of the monograph)
--    ghci> obtainXVectorForPMOneVertexWRTDistingSymmCycle (S.fromList [-1, 1, -1, 1, 1, -1])
-- to get the result:
--    fromList [-1,1,-1,1,0,-1]
-- Call (cf. Proposition 4.9(iii), Example 4.10(iii) and Figure 4.5 of the monograph)
--    ghci> obtainXVectorForPMOneVertexWRTDistingSymmCycle (S.fromList [1, -1, -1, 1, -1, 1])
-- to get the result:
--    fromList [1,-1,0,1,-1,1]
-- Call (cf. Proposition 4.9(iv), Example 4.10(iv) and Figure 4.6 of the monograph)
--    ghci> obtainXVectorForPMOneVertexWRTDistingSymmCycle (S.fromList [1, 1, -1, 1, -1, -1])
-- to get the result:
--    fromList [0,0,-1,1,-1,0]
obtainXVectorForPMOneVertexWRTDistingSymmCycle intSeq =
  if not (isPMOneVertex intSeq)
    then S.empty -- "N/A: The input integer collection is not a plus-one-minus-one-valued vector of dimension >= 3"
    else getXVectorWRTDistingSymmCycle intSeq

getDecompSequenceForPMOneVertexWRTDistingSymmCycle ::
     Int -> S.Seq (Int, Int) -> (S.Seq Int, S.Seq Int) -> (S.Seq Int, S.Seq Int)
getDecompSequenceForPMOneVertexWRTDistingSymmCycle dimension currIndexedXVector (currMinorComponents, currMajorComponents)
  | currIndexedXVector == S.empty = (currMinorComponents, currMajorComponents)
  | otherwise =
    if snd (currIndexedXVector `S.index` 0) == 1
      then getDecompSequenceForPMOneVertexWRTDistingSymmCycle
             dimension
             (S.drop 1 currIndexedXVector)
             ( currMinorComponents S.|> fst (currIndexedXVector `S.index` 0)
             , currMajorComponents)
      else if snd (currIndexedXVector `S.index` 0) == (-1)
             then getDecompSequenceForPMOneVertexWRTDistingSymmCycle
                    dimension
                    (S.drop 1 currIndexedXVector)
                    ( currMinorComponents
                    , currMajorComponents S.|>
                      (dimension + fst (currIndexedXVector `S.index` 0)))
             else getDecompSequenceForPMOneVertexWRTDistingSymmCycle -- i.e., here we have: snd (currIndexedXVector `S.index` 0) == 0
                    dimension
                    (S.drop 1 currIndexedXVector)
                    (currMinorComponents, currMajorComponents)

isPMOneVertex :: S.Seq Int -> Bool
isPMOneVertex intSeq =
  S.length intSeq >= 3 && isNothing (find (\c -> (c /= 1) && (c /= -1)) intSeq)

getXVectorWRTDistingSymmCycle :: PMOneVertex -> S.Seq Int
-- After Proposition 4.9
getXVectorWRTDistingSymmCycle vertex
-- Complaints heard here forced us to activate the above "-Wno-incomplete-patterns" language option
  | isPositivePMOneVertex vertex =
    S.update 0 1 (S.replicate (S.length vertex) (0 :: Int))
  | isNegativePMOneVertex vertex =
    S.update 0 (-1) (S.replicate (S.length vertex) (0 :: Int))
  | otherwise =
    if | vertex `S.index` 0 == -1 && vertex `S.index` (S.length vertex - 1) == 1 ->
         getXVectorWRTDistingSymmCycleLeft vertex
       | vertex `S.index` 0 == -1 &&
           vertex `S.index` (S.length vertex - 1) == -1 ->
         getXVectorWRTDistingSymmCycleLeftRight vertex
       | vertex `S.index` 0 == 1 && vertex `S.index` (S.length vertex - 1) == 1 ->
         getXVectorWRTDistingSymmCycleVoid vertex
       | vertex `S.index` 0 == 1 && vertex `S.index` (S.length vertex - 1) == -1 ->
         getXVectorWRTDistingSymmCycleRight vertex

isPositivePMOneVertex :: PMOneVertex -> Bool
isPositivePMOneVertex vertex =
  S.length vertex >= 3 && isNothing (find (/= 1) vertex)

isNegativePMOneVertex :: PMOneVertex -> Bool
isNegativePMOneVertex vertex =
  S.length vertex >= 3 && isNothing (find (/= (-1)) vertex)

getXVectorWRTDistingSymmCycleLeft :: PMOneVertex -> S.Seq Int
-- See Proposition 4.9 (i)
getXVectorWRTDistingSymmCycleLeft vertex = do
  let leng = S.length vertex
  let endPointsOfIntervalsLeft =
        getEndPointsOfIntervalsPlanA
          (S.zip (S.fromList [0 .. leng - 1]) vertex)
          S.empty
  let pre 
       = S.replicate leng (0 :: Int)
  let preXVector =
        S.update (snd (endPointsOfIntervalsLeft `S.index` 0) + 1) 1 pre
  let endPointsOfIntervalsLeftRemainder = S.drop 1 endPointsOfIntervalsLeft
  completeCreationOfXVector preXVector endPointsOfIntervalsLeftRemainder

getXVectorWRTDistingSymmCycleLeftRight :: PMOneVertex -> S.Seq Int
-- See Proposition 4.9 (ii)
getXVectorWRTDistingSymmCycleLeftRight vertex = do
  let leng = S.length vertex
  let endPointsOfIntervalsLeftRight =
        getEndPointsOfIntervalsPlanA
          (S.zip (S.fromList [0 .. leng - 1]) vertex)
          S.empty
  let pre 
       = S.update 0 (-1) (S.replicate leng (0 :: Int))
  let preXVector =
        S.update
          (fst (S.reverse endPointsOfIntervalsLeftRight `S.index` 0))
          (-1)
          (S.update (snd (endPointsOfIntervalsLeftRight `S.index` 0) + 1) 1 pre)
  let endPointsOfIntervalsLeftRightRemainder =
        S.take (leng - 2) (S.drop 1 endPointsOfIntervalsLeftRight)
  completeCreationOfXVector preXVector endPointsOfIntervalsLeftRightRemainder

getXVectorWRTDistingSymmCycleVoid :: PMOneVertex -> S.Seq Int
-- See Proposition 4.9 (iii)
getXVectorWRTDistingSymmCycleVoid vertex = do
  let leng = S.length vertex
  let endPointsOfIntervalsVoid =
        getEndPointsOfIntervalsPlanB
          (snd
             (S.spanl
                (\s -> snd s == 1)
                (snd
                   (S.spanl
                      (\s -> snd s == -1)
                      (S.zip (S.fromList [0 .. leng - 1]) vertex)))))
          S.empty
  let preXVector 
       = S.update 0 1 (S.replicate leng (0 :: Int))
  completeCreationOfXVector preXVector endPointsOfIntervalsVoid

getXVectorWRTDistingSymmCycleRight :: PMOneVertex -> S.Seq Int
-- See Proposition 4.9 (iv)
getXVectorWRTDistingSymmCycleRight vertex = do
  let leng = S.length vertex
  let endPointsOfIntervalsRight =
        getEndPointsOfIntervalsPlanB
          (snd
             (S.spanl
                (\s -> snd s == 1)
                (snd
                   (S.spanl
                      (\s -> snd s == -1)
                      (S.zip (S.fromList [0 .. leng - 1]) vertex)))))
          S.empty
  let pre 
       = S.replicate leng (0 :: Int)
  let preXVector =
        S.update
          (fst (S.reverse endPointsOfIntervalsRight `S.index` 0))
          (-1)
          pre
  let endPointsOfIntervalsRightRemainder =
        S.take
          (S.length endPointsOfIntervalsRight - 1)
          endPointsOfIntervalsRight
  completeCreationOfXVector preXVector endPointsOfIntervalsRightRemainder

getEndPointsOfIntervalsPlanA ::
     S.Seq (Int, PMOne) -> S.Seq (Int, Int) -> S.Seq (Int, Int)
getEndPointsOfIntervalsPlanA indexedPMOneSeq endPointIndices
  | indexedPMOneSeq == S.empty = endPointIndices
  | otherwise =
    let endPointsOfCurrentFirstInterval 
         =
          ( fst (indexedPMOneSeq `S.index` 0)
          , fst
              (S.reverse (fst (S.spanl (\s -> snd s == -1) indexedPMOneSeq)) `S.index`
               0))
     in getEndPointsOfIntervalsPlanA
          (snd
             (S.spanl
                (\s -> snd s == 1)
                (snd (S.spanl (\s -> snd s == -1) indexedPMOneSeq))))
          (endPointIndices S.|> endPointsOfCurrentFirstInterval)

getEndPointsOfIntervalsPlanB ::
     S.Seq (Int, PMOne) -> S.Seq (Int, Int) -> S.Seq (Int, Int)
getEndPointsOfIntervalsPlanB indexedPMOneSeqWithLongestPrefixOfPositiveElementsRemoved endPointIndices
  | indexedPMOneSeqWithLongestPrefixOfPositiveElementsRemoved == S.empty =
    endPointIndices
  | otherwise =
    let endPointsOfCurrentFirstInterval 
         =
          ( fst
              (indexedPMOneSeqWithLongestPrefixOfPositiveElementsRemoved `S.index`
               0)
          , fst
              (S.reverse
                 (fst
                    (S.spanl
                       (\s -> snd s == -1)
                       indexedPMOneSeqWithLongestPrefixOfPositiveElementsRemoved)) `S.index`
               0))
     in getEndPointsOfIntervalsPlanB
          (snd
             (S.spanl
                (\s -> snd s == 1)
                (snd
                   (S.spanl
                      (\s -> snd s == -1)
                      indexedPMOneSeqWithLongestPrefixOfPositiveElementsRemoved))))
          (endPointIndices S.|> endPointsOfCurrentFirstInterval)

completeCreationOfXVector :: S.Seq Int -> S.Seq (Int, Int) -> S.Seq Int
completeCreationOfXVector currentPreXVector remainingEndPointsOfIntervals
  | remainingEndPointsOfIntervals == S.empty = currentPreXVector
  | otherwise = do
    let onceUpdatedCurrentPreXVector 
         =
          S.update
            (snd (remainingEndPointsOfIntervals `S.index` 0) + 1)
            1
            currentPreXVector
    let twiceUpdatedCurrentPreXVector 
         =
          S.update
            (fst (remainingEndPointsOfIntervals `S.index` 0))
            (-1)
            onceUpdatedCurrentPreXVector
     in completeCreationOfXVector
          twiceUpdatedCurrentPreXVector
          (S.drop 1 remainingEndPointsOfIntervals)
