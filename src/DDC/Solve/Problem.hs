{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Solve.Problem
	( Problem	(..)
	, ProbDef	(..)
	, ProbSig	(..)
	, ProbProjDict	(..)
	, ProbClassInst	(..) )
where
import DDC.Var
import DDC.Type
import DDC.Type.Data
import DDC.Type.SigMode
import DDC.Solve.Location
import Constraint.Exp
import Data.Map			(Map)


-- | A problem for the type inferencer.
--   Contains type definitions from the environment along with constraints that need to be solved.
--
--   TODO: We want to convert some of these maps into lookup functions so we can
--         lazilly load information from the interface files instead of loading and
--         parsing every single one. At the moment most of the compile time goes
--         into just parsing interface files.
--
data Problem
	= Problem
	{ -- | Types of imported bindings.
	  problemDefs		:: Map Var ProbDef
	
	  -- | Data definitions.
	, problemDataDefs	:: Map Var DataDef
		
	  -- | Type signatures defined in the source file.
	  --   There can be multiple signatures for the same variable,
	  --   perhaps with different `SigMode`s
	, problemSigs		:: Map Var [ProbSig]
	
	  -- | Projection dictionaries.
	  --   For the type being projected, maps the name of the outer-most constructor
	  --   to list of dictionaries involving that constructor.
	, problemProjDicts	:: Map Var [ProbProjDict]
	
	  -- | Type class instances.
	, problemClassInt	:: Map Var [ProbClassInst] 
	
	  -- | Constraint tree from the module being compiled.
	, problemConstraints	:: CTree }
	

-- | Type of an imported binding.
data ProbDef
	= ProbDef 
	{ probDefVar		:: Var
	, probDefSrc		:: TypeSource
	, probDefType		:: Type }


-- | Type signature defined in the module being compiled.
data ProbSig
	= ProbSig
	{ probSigVar		:: Var
	, probSigSrc		:: TypeSource
	, probSigMode		:: SigMode
	, probSigType		:: Type }

	
-- | Projection dictionary.
data ProbProjDict
	= ProbProjDict
	{ probProjDictVar	:: Var
	, probProjDictSrc	:: TypeSource
	, probProjDictFuns	:: Map Var Var }
	

-- | Value type class instance.
data ProbClassInst
	= ProbClassInst
	{ probClassInstVar	:: Var
	, probClassInstSrc	:: TypeSource
	, probClassInstTypeArgs	:: [Type] }


