module Main where

import qualified Data.Sequence         as S
import           DistinguishedSymmetricCyclesModule (getVertexOfPMOneDistingSymmCycle,
                                                     getSizeOfDecompositionForPMOneVertexWRTDistingSymmCycle,   
                                                     obtainDecompSequenceForPMOneVertexWRTDistingSymmCycle,
                                                     obtainXVectorForPMOneVertexWRTDistingSymmCycle)

main :: IO ()
main = do
  putStrLn ("\n" ++ "First, let us generate the vertex sequence of the distinguished symmetric cycle in the hypercube graph H(6,2):")
  putStrLn ("\n" ++ "getVertexOfPMOneDistingSymmCycle 6 0    returns   "  ++ show (getVertexOfPMOneDistingSymmCycle 6 0))
  putStrLn ("\n" ++ "getVertexOfPMOneDistingSymmCycle 6 1    returns   "  ++ show (getVertexOfPMOneDistingSymmCycle 6 1))
  putStrLn ("\n" ++ "getVertexOfPMOneDistingSymmCycle 6 2    returns   "  ++ show (getVertexOfPMOneDistingSymmCycle 6 2))
  putStrLn ("\n" ++ "getVertexOfPMOneDistingSymmCycle 6 3    returns   "  ++ show (getVertexOfPMOneDistingSymmCycle 6 3))
  putStrLn ("\n" ++ "getVertexOfPMOneDistingSymmCycle 6 4    returns   "  ++ show (getVertexOfPMOneDistingSymmCycle 6 4))
  putStrLn ("\n" ++ "getVertexOfPMOneDistingSymmCycle 6 5    returns   "  ++ show (getVertexOfPMOneDistingSymmCycle 6 5))
  putStrLn ("\n" ++ "getVertexOfPMOneDistingSymmCycle 6 6    returns   "  ++ show (getVertexOfPMOneDistingSymmCycle 6 6))
  putStrLn ("\n" ++ "getVertexOfPMOneDistingSymmCycle 6 7    returns   "  ++ show (getVertexOfPMOneDistingSymmCycle 6 7))
  putStrLn ("\n" ++ "getVertexOfPMOneDistingSymmCycle 6 8    returns   "  ++ show (getVertexOfPMOneDistingSymmCycle 6 8))
  putStrLn ("\n" ++ "getVertexOfPMOneDistingSymmCycle 6 9    returns   "  ++ show (getVertexOfPMOneDistingSymmCycle 6 9))
  putStrLn ("\n" ++ "getVertexOfPMOneDistingSymmCycle 6 10   returns   "  ++ show (getVertexOfPMOneDistingSymmCycle 6 10))
  putStrLn ("\n" ++ "getVertexOfPMOneDistingSymmCycle 6 11   returns   "  ++ show (getVertexOfPMOneDistingSymmCycle 6 11) ++ "\n\n")

  putStrLn ("\n" ++ "Let us find the sizes (i.e., cardinalities) of the decomposition sets for four vertices of the hypercube graph H(6,2) with respect to its distinguished symmetric cycle:")
  putStrLn ("\n" ++ "getSizeOfDecompositionForPMOneVertexWRTDistingSymmCycle (S.fromList [-1, -1, 1, -1, -1, 1])   returns   "  
                 ++ show (getSizeOfDecompositionForPMOneVertexWRTDistingSymmCycle (S.fromList [-1, -1, 1, -1, -1, 1]))) 
  putStrLn ("\n" ++ "getSizeOfDecompositionForPMOneVertexWRTDistingSymmCycle (S.fromList [-1, 1, -1, 1, 1, -1])    returns   "  
                 ++ show (getSizeOfDecompositionForPMOneVertexWRTDistingSymmCycle (S.fromList [-1, 1, -1, 1, 1, -1]))) 
  putStrLn ("\n" ++ "getSizeOfDecompositionForPMOneVertexWRTDistingSymmCycle (S.fromList [1, -1, -1, 1, -1, 1])    returns   "  
                 ++ show (getSizeOfDecompositionForPMOneVertexWRTDistingSymmCycle (S.fromList [1, -1, -1, 1, -1, 1]))) 
  putStrLn ("\n" ++ "getSizeOfDecompositionForPMOneVertexWRTDistingSymmCycle (S.fromList [1, 1, -1, 1, -1, -1])    returns   "  
                 ++ show (getSizeOfDecompositionForPMOneVertexWRTDistingSymmCycle (S.fromList [1, 1, -1, 1, -1, -1])) ++ "\n\n")   

  putStrLn ("\n" ++ "Now, let us obtain the (index sequences of) decomposition sequences for four vertices of the hypercube graph H(6,2) with respect to its distinguished symmetric cycle:")
  putStrLn ("\n" ++ "obtainDecompSequenceForPMOneVertexWRTDistingSymmCycle (S.fromList [-1, -1, 1, -1, -1, 1])   returns   "  
                 ++ show (obtainDecompSequenceForPMOneVertexWRTDistingSymmCycle (S.fromList [-1, -1, 1, -1, -1, 1]))) 
  putStrLn ("\n" ++ "obtainDecompSequenceForPMOneVertexWRTDistingSymmCycle (S.fromList [-1, 1, -1, 1, 1, -1])    returns   "  
                 ++ show (obtainDecompSequenceForPMOneVertexWRTDistingSymmCycle (S.fromList [-1, 1, -1, 1, 1, -1]))) 
  putStrLn ("\n" ++ "obtainDecompSequenceForPMOneVertexWRTDistingSymmCycle (S.fromList [1, -1, -1, 1, -1, 1])    returns   "  
                 ++ show (obtainDecompSequenceForPMOneVertexWRTDistingSymmCycle (S.fromList [1, -1, -1, 1, -1, 1]))) 
  putStrLn ("\n" ++ "obtainDecompSequenceForPMOneVertexWRTDistingSymmCycle (S.fromList [1, 1, -1, 1, -1, -1])    returns   "  
                 ++ show (obtainDecompSequenceForPMOneVertexWRTDistingSymmCycle (S.fromList [1, 1, -1, 1, -1, -1])) ++ "\n\n") 

  putStrLn ("\n" ++ "Finally, note that the above descriptions of decompositions have in fact been derived from x-vectors:") 
  putStrLn ("\n" ++ "obtainXVectorForPMOneVertexWRTDistingSymmCycle (S.fromList [-1, -1, 1, -1, -1, 1])   returns   "  ++ show (obtainXVectorForPMOneVertexWRTDistingSymmCycle (S.fromList [-1, -1, 1, -1, -1, 1])))
  putStrLn ("\n" ++ "obtainXVectorForPMOneVertexWRTDistingSymmCycle (S.fromList [-1, 1, -1, 1, 1, -1])    returns   "  ++ show (obtainXVectorForPMOneVertexWRTDistingSymmCycle (S.fromList [-1, 1, -1, 1, 1, -1])))
  putStrLn ("\n" ++ "obtainXVectorForPMOneVertexWRTDistingSymmCycle (S.fromList [1, -1, -1, 1, -1, 1])    returns   "  ++ show (obtainXVectorForPMOneVertexWRTDistingSymmCycle (S.fromList [1, -1, -1, 1, -1, 1])))
  putStrLn ("\n" ++ "obtainXVectorForPMOneVertexWRTDistingSymmCycle (S.fromList [1, 1, -1, 1, -1, -1])    returns   "  ++ show (obtainXVectorForPMOneVertexWRTDistingSymmCycle (S.fromList [1, 1, -1, 1, -1, -1])))