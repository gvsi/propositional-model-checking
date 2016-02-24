-- Inf2D Assignment 1 (Last updated: 25 Jan 2016)

-- Good Scholarly Practice
-- Please remember the University requirement as regards all assessed work for credit.
-- Details about this can be found at:
-- http://www.ed.ac.uk/academic-services/students/undergraduate/discipline/academic-misconduct
-- and at:
-- http://web.inf.ed.ac.uk/infweb/admin/policies/academic-misconduct
-- Furthermore, you are required to take reasonable measures to protect your assessed work from
-- unauthorised access.For example, if you put any such work on a public repository then you must
-- set access permissions appropriately (generally permitting access only to yourself, or your
-- group in the case of group practicals).

module Inf2d where
import Data.List
import Debug.Trace
import Data.Ord
import Data.Maybe
import Data.Time

-- Type synonyms for the data structures
-- Symbols are strings (a negative sign as the first character represents a negated symbol)
type Symbol = String
-- Clause = a disjuntion of symbols
type Clause = [Symbol]
-- Sentence = Statements. This is a list of a list of symbols
type Sentence = [[Symbol]]
-- Models are represented as a list of (Symbol,Boolean) tuples
type Model = [(Symbol, Bool)]
-- The knowledge base is represented as a list of statements
type KB = [Sentence]


-----------------------------------------------
-- STUDENT MATRICULATION NUMBER:
-----------------------------------------------
studentId::String
studentId = "s1448512"

--------------------------------------------------
-- ASSIGNMENT TASKS
-- Refer to assignment sheet for details of tasks
--------------------------------------------------

----------TASK 1: REPRESENTATION (2 marks)----------------------------------------------------------
wumpusFact::Sentence
wumpusFact = [["-B12","P11","P22","P31"],["-P11","B12"],["-P22","B12"],["-P31","B12"]]

----------TASK 2: GENERAL HELPER FUNCTIONS (10 marks)-----------------------------------------------

-- Finds the assigned literal to a symbol from a given model

-- Filters the model to see the matching symbols and returns the value of the head or Nothing
lookupAssignment :: Symbol -> Model -> Maybe Bool
lookupAssignment symbol model = if null res then Nothing else Just (snd $ head $ res)
  where res = filter (\x -> fst  x == symbol) model

-- Negate a symbol
negateSymbol :: Symbol -> Symbol
negateSymbol symbol = if head symbol == '-' then tail symbol else "-" ++ symbol

-- For a given symbol, this function checks if it is negated(i.e., has a negation sign).
isNegated :: Symbol -> Bool
isNegated symbol = head symbol == '-'

-- This function takes a symbol and returns an Symbol without any negation sign if the original
-- symbol had one.
getUnsignedSymbol :: Symbol -> Symbol
getUnsignedSymbol symbol = if head symbol == '-' then tail symbol else symbol

-- Gets a list of all symbols in for all given sentences

-- Concatenates all statements in a list of symbols, removes the sign of each, and filters out the duplicates
getSymbols :: [Sentence] -> [Symbol]
getSymbols stmts = nub $ map getUnsignedSymbol (concat $ concat stmts)
----------TASK 3: TRUTH TABLE ENUMERATION AND ENTAILMENT (40 marks)---------------------------------

-- My own helper function
-- Finds the value of the symbol (taking into account its sign) over a model
evaluateSymbol :: Symbol -> Model -> Bool
evaluateSymbol symbol model = if isNegated symbol then
                               not $ fromJust(lookupAssignment (getUnsignedSymbol symbol) model)
                              else
                               fromJust(lookupAssignment (getUnsignedSymbol symbol) model)

-- Function takes as input a list of symbols, and returns a list of models (all possible assignment
-- of True or False to the symbols.)

-- Recursively generate the possible models from each symbol being assigned True and False
-- by concatening the value for a symbol with the remaining ones
generateModels :: [Symbol] -> [Model]
generateModels [x] = [[(x, True)], [(x, False)]]
generateModels (x:xs) = concat $ map ( \t -> [(x, True) : t] ++ [(x, False) : t]) (generateModels xs)

-- This function evaluates the truth value of a propositional sentence using the symbols
-- assignments in the model.

-- For each clause and for each symbol of the cluase, evaluate the symbol over the model
-- if no symbol is found in the model, return an error, otherwise evaluate the symbol
-- then "or"s all symbols for each clause and "and"s all clauses in the statement
pLogicEvaluate :: Sentence -> Model -> Bool
pLogicEvaluate stmt model =  and [ or [ if lookupAssignment (getUnsignedSymbol symbol) model == Nothing then error "No symbol found in model!" else evaluateSymbol symbol model | symbol <- clause ] | clause <- stmt]


-- This function checks the truth value of list of a propositional sentence using the symbols
-- assignments in the model. It returns true only when all sentences in the list are true.
plTrue :: [Sentence]-> Model -> Bool
plTrue sentences model = and [pLogicEvaluate sentence model | sentence <- sentences]

-- This function takes as input a knowledgebase (i.e. a list of propositional sentences),
-- a query (i.e. a propositional sentence), and a list of symbols.
-- IT recursively enumerates the models of the domain using its symbols to check if there
-- is a model that satisfies the knowledge base and the query. It returns a list of all such models.

-- Checks between all possible models generated from the symbols, whether the kb and query are satisfiable
-- Could have implemented a recursive helper method, but here I am leveraging the
-- generateModels, which has already been implemented
ttCheckAll :: [Sentence] -> Sentence -> [Symbol] -> [Model]
ttCheckAll kb query symbols = [ model | model <- generateModels symbols, if plTrue kb model then pLogicEvaluate query model else True]

-- This function determines if a model satisfes both the knowledge base and the query, returning
-- true or false.
ttEntails :: [Sentence] -> Sentence -> Bool
ttEntails kb query = not $ null $ ttCheckAll kb query (getSymbols (query : kb))


-- This function determines if a model satisfes both the knowledge base and the query.
-- It returns a list of all models for which the knowledge base entails the query.

-- Get the symbols of BOTH the query and the knowledge base and passes them to the
-- truth table checker
ttEntailsModels :: [Sentence] -> Sentence -> [Model]
ttEntailsModels kb query = ttCheckAll kb query (getSymbols (query : kb))

----------TASK 4: DPLL (43 marks)-------------------------------------------------------------------

-- The early termination function checks if a sentence is true or false even with a
-- partially completed model.

-- For each symbol of each clause, evaluate it.
-- If symbol is not in model, give it the value False, so that "or"ing them will
-- yield the expected value of early termination
earlyTerminate :: Sentence -> Model -> Bool
earlyTerminate sentence model = and [ or [ eval symbol | symbol <- clause] | clause <- sentence]
  where eval sym = if lookupAssignment (getUnsignedSymbol sym) model == Nothing then False else evaluateSymbol sym model

-- This function finds pure symbol, i.e, a symbol that always appears with the same "sign" in all
-- clauses.
-- It takes a list of symbols, a list of clauses and a model as inputs.
-- It returns Just a tuple of a symbol and the truth value to assign to that
-- symbol. If no pure symbol is found, it should return Nothing

-- Builds a list of pureSymbols and then returns the head + sign if not empty
-- Only checks for purity in the clauses that cannot be ignored because of the model (i.e. clausesToCheck)
findPureSymbol :: [Symbol] -> [Clause] -> Model -> Maybe (Symbol, Bool)
findPureSymbol symbols clauses model = if null pureSymbols then Nothing else Just ((head pureSymbols), elem (head pureSymbols) allSyms)
  where
    pureSymbols = [ symbol | symbol <- symbols, isPure symbol]
    isPure sym = (elem sym allSyms && not (elem (negateSymbol sym) allSyms)) ||
      (elem (negateSymbol sym) allSyms && not (elem sym allSyms))
    allSyms = concat clausesToCheck
    clausesToCheck = filter (\clause -> not (evalClause clause)) clauses
    evalClause clause = or (map evalSymbol clause)
    evalSymbol sym =  if lookupAssignment (getUnsignedSymbol sym) model == Nothing then False else evaluateSymbol sym model


-- This function finds a unit clause from a given list of clauses and a model of assignments.
-- It returns Just a tuple of a symbol and the truth value to assign to that symbol. If no unit
-- clause is found, it should return Nothing.

-- Returns the head of the list of all unit clauses, if not empty
-- a clause is in uniClauses if it doesn't have True symbols (since the clause can be ignored)
-- and if it containes only one symbol that is not false
findUnitClause :: [Clause] -> Model -> Maybe (Symbol, Bool)
findUnitClause clauses model = if null unitClauses then Nothing else
  let unitSymbol = head (filter isNotFalse (head unitClauses)) in Just (getUnsignedSymbol unitSymbol, not (isNegated unitSymbol))
  where
    unitClauses = [clause | clause <- clauses, not (hasTrueSymbols clause) && isUnitClause clause]
    hasTrueSymbols clause = or (map evalSymbol clause)
    evalSymbol sym =  if lookupAssignment (getUnsignedSymbol sym) model == Nothing then False else evaluateSymbol sym model
    isUnitClause clause = length (filter (isNotFalse) clause) == 1
    isNotFalse sym = if lookupAssignment (getUnsignedSymbol sym) model == Nothing then True else evaluateSymbol sym model


-- This function check the satisfability of a sentence in propositional logic. It takes as input a
-- list of clauses in CNF, a list of symbols for the domain, and model.
-- It returns true if there is a model which satises the propositional sentence.
-- Otherwise it returns false.

-- Uses an auxiliary recursive function dpll_rec that iteratively builds the model
-- it checks whether it can earlyTerminate, then it checks whether there are still clauses
-- where all the symbols are not all false, then it looks for the first pureSymbol
-- and calls the function again recursively giving the value to the pureSymbol
-- similarly for finding unit clauses
-- otherwise it evaluates the clauses over two possible models, where a symbol
-- is assigned both True and False value
dpll :: [Clause] -> [Symbol] -> Bool
dpll clauses symbols = dpll_rec clauses symbols []

dpll_rec :: [Clause] -> [Symbol] -> Model -> Bool
dpll_rec clauses symbols model | earlyTerminate clauses model = True
                               | not ( null (filter (allSymbolsFalse) clauses) ) = False
                               | findPureSymbol symbols clauses model /= Nothing = let aPureSymbol = fromJust(findPureSymbol symbols clauses model)
                                                                                   in (dpll_rec clauses (delete (fst aPureSymbol) symbols) (model ++ [aPureSymbol]))
                               | findUnitClause clauses model /= Nothing = let aUnitClauseSymbol = fromJust(findUnitClause clauses model)
                                                                                   in (dpll_rec clauses (delete (fst aUnitClauseSymbol) symbols) (model ++ [aUnitClauseSymbol]))
                               | otherwise = dpll_rec clauses (tail symbols) (model ++ [(head symbols, True)]) || dpll_rec clauses (tail symbols) (model ++ [(head symbols, False)])

  where
    allSymbolsFalse clause = and (map isFalse clause)
    isFalse sym = if lookupAssignment (getUnsignedSymbol sym) model == Nothing then False else evaluateSymbol sym model == False

-- This function serves as the entry point to the dpll function. It takes a list clauses in CNF as
-- input and returns true or false.
-- It uses the dpll function above to determine the satisability of its input sentence.
dpllSatisfiable :: [Clause] -> Bool
dpllSatisfiable clauses = dpll clauses (getSymbols [clauses])

----------TASK 5: EVALUATION (5 marks)--------------------------------------------------------------
-- EVALUATION
-- a knowledge base (i.e. a sentence in propositional
-- logic), and a query sentence. Both items should have their clauses in CNF representation
-- and should be assigned to the following respectively:

-- a valid model would be B = False, A = True, D = True, C = True
evalKB :: [Sentence]
evalKB = [[["A","B"],["C","D"]],[["-B","D"],["A","B","D"]],[["E"],["F"],["G"],["H"],["I"],["J"]]]

evalQuery :: Sentence
evalQuery = [["A","B","C"],["-B"],["A","B"],["A","C"],["C"],["-C","D"]]


-- RUNTIMES
-- Enter the average runtimes of the ttEntails and dpllSatisable functions respectively
-- I get the values by displaying the running time in GHCi (type :set +s in the console)

-- Testing function: tCheckAll evalKB evalQuery (getSymbols (evalQuery : evalKB))
runtimeTtEntails :: Double
runtimeTtEntails = 360

-- Testing function: dpllSatisable (concat (evalKB) ++ evalQuery)
runtimeDpll :: Double
runtimeDpll = 10
