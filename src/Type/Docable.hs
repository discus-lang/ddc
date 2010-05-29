
module Type.Docable where
import DDC.Type
import DDC.Util.Doc
import DDC.Main.Pretty
import Type.Pretty		()


instance Docable Kind Str where
 doc kk
  = case kk of
	KNil
	 -> DLeaf (ppr "KNil")
	
	KCon kiCon super
	 -> DNode "KCon"
	 $  DList
		[ DNode "kiCon" $ DLeaf $ ppr kiCon
		, DNode "super" $ DLeaf $ ppr super]
		
	KFun k1 k2
	 -> DNode "KFun"
	 $  DList
		[ DNode "k1" $ doc k1
		, DNode "k2" $ doc k2 ]
		
	KApp k1 t2
	 -> DNode "KApp"
	 $  DList
		[ DNode "k1" $ doc k1
		, DNode "t2" $ doc t2 ]
		
	KSum ks
	 -> DNode "KSum"
	  $ DList $ map doc ks
	
	
instance Docable TyCon Str where
 doc tt
  = case tt of
	TyConFun	
	 -> DLeaf (ppr "TyConFun")
	
	TyConData name kind	
	 -> DNode "TyConData"
	 $  DList
		[ DNode "name" $ DLeaf $ ppr name
		, DNode "kind" $ DLeaf $ ppr kind ]
		
	TyConWitness wit kind
	 -> DNode "TyConWitness"
	 $  DList
		[ DNode "witness" $ DLeaf $ ppr wit
		, DNode "kind"    $ DLeaf $ ppr kind ]
		
	
instance Docable Type Str where
 doc tt 
  = case tt of
	TVar k u
	 -> DNode "TVar"
	 $  DList
		[ DNode "kind" $ doc k
		, DNode "var"  $ doc u ]

	TCon tyCon
	 -> DNode "TCon" $ doc tyCon

	TApp t1 t2
	 -> DNode "TApp"
	 $  DList 
		[ doc t1
		, doc t2]
		

	_ -> DLeaf (ppr tt)

	
instance Docable Bound Str where
 doc uu
  = case uu of
	UVar v	-> DLeaf $ ppr v
	
	
