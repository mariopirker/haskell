module Testing.SummarizeTest where

import ComputationGraph.Summarize
import ComputationGraph.StateGraph
import ComputationGraph.HeapGraph
import ComputationGraph.State
import ComputationGraph.Program
import ComputationGraph.Instructions
import ComputationGraph.HelperFunctions
import ComputationGraph.Morphism
import ComputationGraph.InstanceRelation
import Testing.TestProgram
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Char as Char

{- ###########################################################
 - ########### Test States ###################################
 - ########################################################### -}


heapstest1 = Heaps heaptest1'' 3
heaptest1 = Map.singleton (Addr 0) (ObjPair ("List",ftstest1'))
heaptest1' = Map.insert (Addr 1) (AbsVariable (ClassVar "List")) heaptest1
heaptest1'' = Map.insert (Addr 2) (AbsVariable (ClassVar "List")) heaptest1'
ftstest1 = Map.singleton ("List","next") (AddVal (Addr 1))
ftstest1' = Map.insert ("int","val") (AbsIntVal "AbsInt") ftstest1
stktest1 = []
loctest1 = [(AddVal (Addr 0)),(AddVal (Addr 2)),(AddVal(Addr 0))]
frmstest1 = [Frame stktest1 loctest1 cns mns 0]
statest1 = State heapstest1 frmstest1 ius 0

heapstest2 = Heaps heaptest2''' 4
heaptest2 = Map.singleton (Addr 0) (ObjPair ("List",ftstest2'))
heaptest2' = Map.insert (Addr 1) (ObjPair ("List",ftstest22')) heaptest2
heaptest2'' = Map.insert (Addr 2) (AbsVariable (ClassVar "List")) heaptest2'
heaptest2''' = Map.insert (Addr 4) (AbsVariable (ClassVar "List")) heaptest2''
ftstest2 = Map.singleton ("List","next") (AddVal (Addr 1))
ftstest2' = Map.insert ("int","val") (AbsIntVal "AbsInt") ftstest2
ftstest22 = Map.singleton ("List","next") (AddVal (Addr 4))
ftstest22'= Map.insert ("int","val") (AbsIntVal "AbsInt") ftstest22
stktest2 = []
loctest2 = [(AddVal (Addr 0)),(AddVal (Addr 2)),(AddVal(Addr 1))]
frmstest2 = [Frame stktest2 loctest2 cns mns 0]
statest2 = State heapstest2 frmstest2 ius 0



{- Test Data according to example at page 22 with two states -}
-- State S -- is Figure 12 from the Paper
heapsss = Heaps heapss 2
heapss' = Map.singleton (Addr 1) (ObjPair ("List",fts))
heapss = Map.insert (Addr 2) (AbsVariable (ClassVar "list")) heapss'
fts = Map.singleton ("List","next") (AddVal (Addr 2))
stks = []
locs = [(AddVal (Addr 1)), (AddVal (Addr 1))]
cns = "List"
mns = "Append"
frmss = [Frame stks locs cns mns 0]
ius = []
states = State heapsss frmss ius 0

-- State S' - is Figure 13 from the Paper
heapsss' = Heaps heapss'' 3
heapss'''' = Map.singleton (Addr 3) (ObjPair ("List",fts1))
heapss''' = Map.insert (Addr 4) (ObjPair ("List",fts2)) heapss''''
heapss'' = Map.insert (Addr 5) (AbsVariable (ClassVar "list")) heapss'''
fts1 = Map.singleton ("List","next") (AddVal (Addr 4))
fts2 = Map.singleton ("List","next") (AddVal (Addr 5))
stks' = []
locs' = [(AddVal (Addr 3)), (AddVal (Addr 4))]
frmss' = [Frame stks' locs' cns mns 0]
ius' = []
states' = State heapsss' frmss' ius' 0

-- State A
heapsa = Heaps heapsa' 1
heapsa' = Map.singleton (Addr 1) (AbsVariable (ClassVar "list"))
stksa = []
locsa = [AddVal(Addr 1)]
frmsa = [Frame stksa locsa cns mns 0]
statea =  State heapsa frmsa ius' 0

-- State A'
heapsaa = Heaps heapsaa'' 2
heapsaa' = Map.singleton (Addr 2) (ObjPair ("List",ftsa1))
heapsaa'' = Map.insert (Addr 3) (AbsVariable (ClassVar "list")) heapsaa'
ftsa1= Map.singleton ("List","next") (AddVal (Addr 3))
stska' = []
locsa' = [AddVal (Addr 2)]
frmsa' = [Frame stska' locsa' cns mns 0]
statea' = State heapsaa frmsa' ius' 0

-- State B
heapsb = Heaps heapsb'' 2
heapsb' = Map.singleton (Addr 1) (ObjPair ("List",ftsb1'))
heapsb'' = Map.insert (Addr 2) (AbsVariable (ClassVar "list")) heapsb'
ftsb1 = Map.singleton ("List","next") (AddVal (Addr 2))
ftsb1' = Map.insert ("int","val") Unit ftsb1
stkb = []
locb = [AddVal (Addr 1)]
frmb = [Frame stkb locb cns mns 0]
stateb = State heapsb frmb ius' 0

-- State B'
heapsbb = Heaps heapsbb''' 3
heapsbb' = Map.singleton (Addr 3) (ObjPair ("List",ftsbb1'))
heapsbb'' = Map.insert (Addr 4) (ObjPair ("List",ftsbb2')) heapsbb'
heapsbb'''= Map.insert (Addr 5) (AbsVariable (ClassVar "list")) heapsbb''
ftsbb1 = Map.singleton ("List","next") (AddVal (Addr 4))
ftsbb1' = Map.insert ("int","val") Null ftsbb1
ftsbb2 = Map.singleton ("List","next") (AddVal (Addr 5))
ftsbb2' = Map.insert ("int","val") Null ftsbb2
stkbb = []
locbb = [AddVal (Addr 3)]
frmbb = [Frame stkbb locbb cns mns 0]
stateb' = State heapsbb frmbb ius' 0

-- State C
heapsc = Heaps heapsc'' 2
heapsc' = Map.singleton (Addr 1) (ObjPair ("IntList",ftsc1''))
heapsc'' = Map.insert (Addr 2) (AbsVariable (ClassVar ("intList"))) heapsc'
ftsc1= Map.singleton ("List","next") Null
ftsc1' = Map.insert ("int","val") Unit ftsc1
ftsc1'' = Map.insert ("IntList","head") (AddVal (Addr 2)) ftsc1'
stkc= []
locc = [AddVal (Addr 1)]
frmc = [Frame stkc locc cns mns 0]
statec = State heapsc frmc ius' 0

-- State C'
heapscc = Heaps heapscc''' 3
heapscc' = Map.singleton (Addr 3) (ObjPair ("BinaryTreeList",ftscc1'''))
heapscc'' = Map.insert (Addr 4) (AbsVariable (ClassVar ("treeList"))) heapscc'
heapscc''' = Map.insert (Addr 5) (AbsVariable (ClassVar ("binaryTreeList"))) heapscc''
ftscc1= Map.singleton ("List","next") Unit
ftscc1' = Map.insert ("int","val") Null ftscc1
ftscc1'' = Map.insert ("TreeList","root") (AddVal (Addr 4)) ftscc1'
ftscc1''' = Map.insert ("BinaryTreeList","left") (AddVal (Addr 5)) ftscc1''
stkcc= []
loccc = [AddVal (Addr 3)]
frmcc = [Frame stkcc loccc cns mns 0]
statec' = State heapscc frmcc ius' 0

-- State D
heapsd = Heaps heapsd' 1
heapsd' = Map.singleton (Addr 1) (AbsVariable (ClassVar "intList"))
stksd = []
locsd = [AddVal(Addr 1)]
frmsd = [Frame stksd locsd cns mns 0]
stated =  State heapsd frmsd ius' 0

-- State D'
heapsdd = Heaps heapsdd' 1
heapsdd' = Map.singleton (Addr 2) (AbsVariable (ClassVar "binaryTreeList"))
stksdd = []
locsdd = [AddVal(Addr 2)]
frmsdd = [Frame stksdd locsdd cns mns 0]
stated' =  State heapsdd frmsdd ius' 0

-- State E
heapse= Heaps heapse' 1
heapse' = Map.singleton (Addr 1) (AbsVariable (ClassVar "intList"))
stkse= []
locse= [AddVal(Addr 1)]
frmse= [Frame stkse locse cns mns 0]
statee =  State heapse frmse ius' 0

-- State E'
heapsee = Heaps heapscc''' 3
heapsee' = Map.singleton (Addr 3) (ObjPair ("BinaryTreeList",ftsee1'''))
heapsee'' = Map.insert (Addr 4) (AbsVariable (ClassVar ("treeList"))) heapsee'
heapsee''' = Map.insert (Addr 5) (AbsVariable (ClassVar ("binaryTreeList"))) heapsee''
ftsee1= Map.singleton ("List","next") Unit
ftsee1' = Map.insert ("int","val") Null ftsee1
ftsee1'' = Map.insert ("TreeList","root") (AddVal (Addr 4)) ftsee1'
ftsee1''' = Map.insert ("BinaryTreeList","left") (AddVal (Addr 5)) ftsee1''
stkee= []
locee = [AddVal (Addr 3)]
frmee = [Frame stkee locee cns mns 0]
statee' = State heapsee frmee ius' 0

