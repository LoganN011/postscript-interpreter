# PostScript Interpreter — Codebase Reference

This document describes every type definition and function across the `postscript-interpreter` project, explaining **what** each does and **how** it is implemented.

---

## Table of Contents

1. [Project Architecture](#project-architecture)
2. [Core Library (`src/`)](#core-library-src)
   - [PostScript.Types](#postscripttypes-srcpostscripttypeshs)
   - [PostScript.Lexer](#postscriptlexer-srcpostscriptlexerhs)
   - [PostScript.Environment](#postscriptenvironment-srcpostscriptenvironmenths)
   - [PostScript.Evaluator](#postscriptevaluator-srcpostscriptevaluatorhs)
   - [PostScript](#postscript-srcpostscripths)
3. [Primitives (`src/PostScript/Primitives/`)](#primitives-srcpostscriptprimitives)
   - [PostScript.Primitives.Math](#postscriptprimitivesmath-srcpostscriptprimitivesmathhs)
   - [PostScript.Primitives.Stack](#postscriptprimitivesstack-srcpostscriptprimitivesstackhs)
   - [PostScript.Primitives.Dictionary](#postscriptprimitivesdictionary-srcpostscriptprimitivesdictionaryhs)
   - [PostScript.Primitives.Control](#postscriptprimitivescontrol-srcpostscriptprimitivescontrolhs)
   - [PostScript.Primitives.Graphics.Transform](#postscriptprimitivesgraphicstransform-srcpostscriptprimitivesgraphicstransformhs)
   - [PostScript.Primitives.Graphics.Path](#postscriptprimitivesgraphicspath-srcpostscriptprimitivesgraphicspathhs)
   - [PostScript.Primitives.Graphics.State](#postscriptprimitivesgraphicsstate-srcpostscriptprimitivesgraphicsstatehs)
   - [PostScript.Primitives.Graphics.Text](#postscriptprimitivesgraphicstext-srcpostscriptprimitivesgraphicstexths)
   - [PostScript.Primitives.Graphics.Render](#postscriptprimitivesgraphicsrender-srcpostscriptprimitivesgraphicsrenderhs)
4. [CLI Executable (`app/`)](#cli-executable-app)
   - [Main](#main-appmainhs)
5. [Dual-View GUI Executable (`app-gui/`)](#dual-view-gui-executable-app-gui)
   - [Types](#types-app-guitypeshs)
   - [ImageLoader](#imageloader-app-guiimageloaderhs)
   - [TestBrowser](#testbrowser-app-guitestbrowserhs)
   - [GlossRender](#glossrender-app-guiglossrenderhs)
   - [Main](#main-app-guimainhs)
6. [Test Suite (`test/`)](#test-suite-test)
   - [Spec](#spec-testspecs)

---

## Project Architecture

The package is defined in `postscript-interpreter.cabal` (GHC2021, `-Wall -O2`, `LambdaCase` enabled) with four targets:
- **Library (`postscript-interpreter`)**: Lexer, AST/VM types, evaluator, and primitive operators (math, stack/containers, dictionaries, control flow, 2D transforms, paths, graphics state, text/fonts, and SVG rendering).
- **CLI (`postscript-interpreter`)**: Parses a `.ps` file, runs the interpreter, and outputs rendered SVG page(s) to stdout or `.svg` file(s).
- **GUI (`postscript-gui`)**: Interactive Gloss/OpenGL dual-view viewer that discovers `.ps` test files and reference images, evaluates PostScript asynchronously on a background thread, and renders pages side-by-side with pan/zoom and page navigation.
- **Test Suite (`postscript-interpreter-test`)**: QuickCheck property tests, `.ps` program stack-result integration tests, and differential tests comparing stdout against Ghostscript (`gs`).

---

## Core Library (`src/`)

### `PostScript.Types` (`src/PostScript/Types.hs`)

Defines the core data model, graphics state, VM state, and monad stack.

#### Types & Instances

- **`data Object`**: Represents any PostScript value on the operand stack, in dictionaries/arrays, or in parsed token streams.
  - Constructors: `PSInteger !Int`, `PSReal !Double`, `PSBoolean !Bool`, `PSString !String`, `PSName !String` (literal `/name`), `PSSymbol !String` (executable name), `PSOperator !String` (inlined/bound built-in operator), `PSBlock ![Object]` (deferred `{ ... }` procedure), `PSDict !Int` / `PSArray !Int` (heap references by integer ID), `PSNull`, `PSMark`, `PSSave !Int` (VM snapshot token). Derives `Show, Eq`.
- **`type Matrix`**: 6-tuple `(Double, Double, Double, Double, Double, Double)` representing a 2D affine transformation matrix `(a, b, c, d, tx, ty)`.
- **`data PathSegment`**: Vector path construction primitives stored in device coordinates.
  - Constructors: `MoveTo !Double !Double`, `LineTo !Double !Double`, `CurveTo !Double !Double !Double !Double !Double !Double`, `Arc !Double !Double !Double !Double !Double`, `ArcN !Double !Double !Double !Double !Double`, `ClosePath`. Derives `Show, Eq`.
- **`data GraphicsState`**: Record holding active visual and path state:
  - `ctm :: !Matrix` (Current Transformation Matrix), `currentPath :: ![PathSegment]` (stored in reverse construction order), `currentPoint :: !(Maybe (Double, Double))` (device coordinates), `rgbColor :: !(Double, Double, Double)`, `lineWidth :: !Double`, `lineCap :: !Int`, `lineJoin :: !Int`, `miterLimit :: !Double`, `dashArray :: ![Double]`, `dashOffset :: !Double`, `fontName :: !String`, `fontSize :: !Double`, `clipPath :: !(Maybe [PathSegment])`. Derives `Show, Eq`.
- **`data GraphicCommand`**: Intermediate representation of painting/page events emitted during execution for SVG or Gloss rendering.
  - Constructors: `DrawStroke GraphicsState [PathSegment]`, `DrawFill GraphicsState Bool [PathSegment]` (`Bool` = even-odd rule), `DrawText GraphicsState (Double, Double) String`, `DrawPage`. Derives `Show, Eq`.
- **`data PSState`**: Top-level global interpreter VM state:
  - `operandStack :: ![Object]`, `dictStack :: ![Int]` (stack of dictionary heap IDs), `dictHeap :: !(Map Int (Map String Object))`, `arrayHeap :: !(Map Int [Object])`, `nextId :: !Int`, `randSeed :: !Int`, `gState :: !GraphicsState`, `gStateStack :: ![GraphicsState]`, `saveSnapshots :: ![(Int, Map Int (Map String Object), Map Int [Object], GraphicsState)]`, `canvasElements :: ![String]` (accumulated SVG strings in reverse order), `graphicCommands :: ![GraphicCommand]` (accumulated draw commands in reverse order), `executionTerminated :: !Bool`, `loopExited :: !Bool`, `printAction :: String -> IO ()`.
- **`instance Show PSState`**: Formats `PSState` by printing only `operandStack` and `executionTerminated` (avoiding unshowable `printAction` and large heaps).
- **`type Interpreter a`**: `StateT PSState IO a` — the strict state transformer monad over `IO` used by all evaluator and primitive functions.

#### Functions & Values

- **`identityMatrix :: Matrix`**: Returns `(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)`.
- **`defaultSystemDict :: Map String Object`**: Builds the initial root dictionary map containing `"true" -> PSBoolean True` and `"false" -> PSBoolean False`.
- **`initGraphicsState :: GraphicsState`**: Constructs the default `GraphicsState` (`identityMatrix`, empty path, `Nothing` current point, black `(0,0,0)` color, `lineWidth = 1.0`, `lineCap = 0`, `lineJoin = 0`, `miterLimit = 10.0`, empty dash array, `"Helvetica"` size `10.0`, no clip path).
- **`initPSState :: PSState`**: Constructs the initial VM state with empty operand stack, `dictStack = [0, 0, 0]`, `dictHeap` seeded at key `0` with `defaultSystemDict`, empty `arrayHeap`, `nextId = 1`, `randSeed = 1`, `initGraphicsState`, empty stacks/snapshots/commands, `False` flags, and `printAction = putStr`.

---

### `PostScript.Lexer` (`src/PostScript/Lexer.hs`)

Megaparsec-based tokenizer and parser for PostScript source text.

#### Types

- **`type Parser`**: `Parsec Void String` — Megaparsec parser over `String` with no custom error component.

#### Functions

- **`sc :: Parser ()`**: Space consumer that skips any PostScript whitespace (`c <= ' '`, including spaces, tabs, newlines, null bytes) and `%` single-line comments via `L.space` and `L.skipLineComment`.
- **`lexeme :: Parser a -> Parser a`**: Wraps a parser with `L.lexeme sc` to consume trailing whitespace/comments.
- **`symbol :: String -> Parser String`**: Matches an exact string and consumes trailing whitespace via `L.symbol sc`.
- **`isDelimiter :: Char -> Bool`**: Returns `True` for characters `<= ' '` or in `"%()<>[]{}/"`, marking token boundaries.
- **`pNumber :: Parser Object`**: Parses integers, radix integers (`base#digits` up to base 36, validating each digit against `base`), floating-point reals (including leading-dot `.5` and trailing-dot `72.`), and scientific notation (`e`/`E` with optional sign), with optional leading `+`/`-`. Returns `PSInteger` or `PSReal`.
- **`pImmName :: Parser Object`**: Parses immediately evaluated names (`//name`). Checks `isBuiltin name`; returns `PSOperator name` if built-in, otherwise `PSSymbol name`.
- **`pName :: Parser Object`**: Parses literal names (`/name`) by consuming `/` followed by non-delimiter characters, returning `PSName name`.
- **`pString :: Parser Object`** & **`pStringContent :: Parser String`**: Parses `(...)` string literals. Supports balanced nested parentheses recursively via `try`, and backslash escape sequences (`\n`, `\r`, `\t`, `\b`, `\f`, `\\`, `\(`, `\)`, or literal escaped char).
- **`pBlock :: Parser Object`**: Parses `{ ... }` procedure blocks by matching `"{"`, parsing `many pObject`, and matching `"}"`, returning `PSBlock objs`.
- **`pDelimOp :: Parser Object`**: Parses `<<`, `>>`, `[`, and `]` into `PSSymbol` tokens (`try`ing 2-char delimiters before 1-char).
- **`pSymbol :: Parser Object`**: Parses unquoted identifiers; maps `"true"`/`"false"` to `PSBoolean`, `"null"` to `PSNull`, and all others to `PSSymbol ident`.
- **`pObject :: Parser Object`**: Top-level object choice combinator trying `pBlock`, `pDelimOp`, `pImmName`, `pName`, `pString`, `try pNumber`, and `pSymbol`.
- **`pProgram :: Parser [Object]`**: Consumes leading whitespace/comments, parses `many pObject`, and requires `eof`.
- **`parsePostScript :: String -> Either String [Object]`**: Runs `pProgram` on an input string, returning either a formatted Megaparsec `errorBundlePretty` string or the parsed `[Object]` list.
- **`parseOneToken :: String -> Maybe (Object, String)`**: Parses a single `pObject` (after `sc`) and returns `(obj, remainingInput)` using `getInput`, or `Nothing` on failure (used by the `token` operator).

---

### `PostScript.Environment` (`src/PostScript/Environment.hs`)

Provides low-level VM state accessors and mutators in the `Interpreter` monad.

#### Functions

- **`push :: Object -> Interpreter ()`**: Prepends an `Object` to `operandStack`.
- **`pop :: Interpreter Object`**: Pops and returns the top `Object` of `operandStack`; calls `error "stack empty"` if empty.
- **`peek :: Interpreter Object`**: Returns the top `Object` without removing it; errors if empty.
- **`popTwo :: Interpreter (Object, Object)`**: Pops the top two items as `(top, second)`; errors if fewer than two items exist.
- **`lookupDict :: String -> Interpreter (Maybe Object)`**: Searches `dictStack` from top to bottom, looking up each dictionary ID in `dictHeap` until a binding for `key` is found.
- **`defineSymbol :: String -> Object -> Interpreter ()`**: Inserts/overwrites `key -> val` in the topmost dictionary on `dictStack` via `Map.adjust`.
- **`pushDict :: Int -> Interpreter ()`**: Pushes a dictionary ID onto `dictStack`.
- **`popDict :: Interpreter Int`**: Pops the top dictionary ID off `dictStack` and returns it, refusing to pop the last (root) dictionary.
- **`newArray :: [Object] -> Interpreter Object`**: Allocates a new array in `arrayHeap` at `nextId`, increments `nextId`, and returns `PSArray aid`.
- **`newDict :: Map String Object -> Interpreter Object`**: Allocates a new dictionary in `dictHeap` at `nextId`, increments `nextId`, and returns `PSDict did`.
- **`getGState :: Interpreter GraphicsState`**: Returns the current `gState`.
- **`modifyGState :: (GraphicsState -> GraphicsState) -> Interpreter ()`**: Applies a pure transformation to `gState`.
- **`toCode :: Object -> [Object]`**: Unwraps `PSBlock objs` to `objs`, or wraps any other `Object` in a singleton list `[obj]`.
- **`runLoopStep :: Interpreter () -> Interpreter () -> Interpreter ()`**: Executes one loop body `action`, then inspects `PSState`: if `loopExited` is `True`, clears `loopExited` to `False` and stops; if `executionTerminated` is `True`, stops; otherwise runs `onContinue`.
- **`loopWhile :: [Interpreter ()] -> Interpreter ()`**: Sequentially runs a list of loop-step actions using `runLoopStep`, short-circuiting early if `executionTerminated` or `loopExited` becomes `True`.
- **`isBuiltin :: String -> Bool`**: Checks if a symbol name is in the whitespace-delimited list of all recognized built-in operator names.

---

### `PostScript.Evaluator` (`src/PostScript/Evaluator.hs`)

Drives execution of parsed PostScript object streams and dispatches built-in operators.

#### Functions

- **`evalProgram :: [Object] -> Interpreter ()`**: Recursively evaluates `[Object]` in order, checking `executionTerminated` and `loopExited` before each step to allow `quit` and `exit` to unwind immediately.
- **`evalObject :: Object -> Interpreter ()`**: Evaluates a single `Object` per PostScript semantics:
  - `PSOperator name`: calls `executeBuiltin name`.
  - `PSSymbol name`: looks up `name` in `dictStack`; if bound to `PSBlock code`, executes `evalProgram code`; if bound to `PSOperator op`, calls `executeBuiltin op`; if bound to `PSSymbol sym`, recursively evaluates it; if bound to any other literal value, pushes it; if unbound, falls back to `executeBuiltin name`.
  - Any other `Object` (numbers, strings, `PSName`, `PSBlock`, `PSDict`, `PSArray`, etc.): pushed onto `operandStack`.
- **`executeBuiltin :: String -> Interpreter ()`**: Pattern-matches operator names to their corresponding primitive functions (stack, container, output, math, dictionary, relational/boolean, control flow, matrix/transform, path, graphics state/painting, font/text, and `token`), or calls `error ("Undefined symbol: " ++ name)`.
- **`psToken :: Interpreter ()`**: Implements PostScript `token`: pops a `PSString`, calls `parseOneToken`, and pushes `PSString rest`, `obj`, and `PSBoolean True` on success, or `PSBoolean False` on failure.
- **`runInterpreter :: [Object] -> PSState -> IO (Either String PSState)`**: Runs `execStateT (evalProgram objs) s0` wrapped in `Control.Exception.try`, converting any `SomeException` into `Left (displayException ex)`.

---

### `PostScript` (`src/PostScript.hs`)

Top-level library facade re-exporting `PostScript.Types`, `PostScript.Lexer`, and `PostScript.Evaluator`.

#### Functions

- **`executeScript :: String -> IO (Either String PSState)`**: Convenience wrapper calling `executeScriptWith initPSState`.
- **`executeScriptWith :: PSState -> String -> IO (Either String PSState)`**: Parses raw PostScript source with `parsePostScript` and, on success, runs `runInterpreter objs s0`.

---

## Primitives (`src/PostScript/Primitives/`)

### `PostScript.Primitives.Math` (`src/PostScript/Primitives/Math.hs`)

Arithmetic, rounding, trigonometry, logarithms, type conversion, PRNG, and timing operators.

#### Functions

- **`toDouble :: Object -> Maybe Double`**: Extracts `Double` from `PSInteger` (via `fromIntegral`) or `PSReal`, returning `Nothing` otherwise.
- **`psAdd`**, **`psSub`**, **`psMul`**: Pop two numbers; if both are `PSInteger`, perform exact `Int` arithmetic and push `PSInteger`; otherwise coerce both via `toDouble` and push `PSReal`.
- **`psDiv`**: Pops two numbers as `Double`, errors on division by zero, and always pushes `PSReal (x / y)`.
- **`psIdiv`**, **`psMod`**: Pop two `PSInteger`s, error on zero divisor, and push `PSInteger` using Haskell `quot` and `rem` (truncating toward zero, matching PostScript).
- **`psNeg`**, **`psAbs`**: Pop one number, preserving `PSInteger` or `PSReal`, and push its negation or absolute value.
- **`psCeiling`**, **`psFloor`**, **`psRound`**, **`psTruncate`**: Pop a number as `Double` and push `PSReal` of the rounded integer (`round` uses `floor (r + 0.5)` so ties round toward $+\infty$).
- **`psSqrt`**: Pops a non-negative number as `Double` and pushes `PSReal (sqrt r)`.
- **`psAtan`**: Pops `den` and `num`, computes `atan2 y x` in degrees, normalizes negative angles by adding `360.0` so the result is in `[0, 360)`, and pushes `PSReal`.
- **`psCos`**, **`psSin`**: Pop angle in degrees, convert to radians, compute `cos`/`sin`, snap values with `abs res < 1e-12` to `0.0`, and push `PSReal`.
- **`psExp`**: Pops `exp` and `base` as `Double` and pushes `PSReal (b ** e)`.
- **`psLn`**, **`psLog`**: Pop a strictly positive `Double` and push `PSReal` of `log r` (base $e$) or `logBase 10.0 r`.
- **`psCvi`**: Pops `PSInteger`, `PSReal`, or numeric `PSString` (parsed as `Int` or `Double`), truncates to `Int`, and pushes `PSInteger`.
- **`psCvr`**: Pops `PSInteger`, `PSReal`, or numeric `PSString`, converts to `Double`, and pushes `PSReal`.
- **`psRand`**, **`psSrand`**, **`psRrand`**: Linear congruential PRNG (`(s * 1103515245 + 12345) mod 2^31`) operating on `randSeed` in `PSState`. `rand` advances and pushes; `srand` sets `randSeed mod 2^31`; `rrand` pushes current `randSeed`.
- **`psUsertime`**: Calls `System.CPUTime.getCPUTime` (picoseconds) via `liftIO`, divides by `10^9` to get milliseconds, and pushes `PSInteger`.

---

### `PostScript.Primitives.Stack` (`src/PostScript/Primitives/Stack.hs`)

Operand stack manipulation, array/string/dict container operations, mark delimiters, formatting/output, and string searching.

#### Functions

- **`psPop`**, **`psDup`**, **`psExch`**: Standard stack top operations (`pop`, `dup`, `exch`).
- **`psIndex`**: Pops non-negative `PSInteger n` and pushes a copy of `operandStack !! n`.
- **`psRoll`**: Pops `j` and `n`; if `n > 1`, splits the top `n` elements and rotates them by `k = (j mod n + n) mod n`.
- **`psClear`**, **`psCount`**, **`psQuit`**: `clear` empties `operandStack`; `count` pushes its length; `quit` sets `executionTerminated = True`.
- **`psCopy`**: Polymorphic `copy`:
  - `PSInteger n`: duplicates the top `n` stack items.
  - `PSArray dst` over `PSArray src` or `PSBlock xs`: overwrites the prefix of `dst` in `arrayHeap` and pushes `PSArray dst`.
  - `PSDict dst` over `PSDict src`: merges `src` into `dst` via `Map.union` and pushes `PSDict dst`.
- **`psArray`**: Pops non-negative `n` and allocates an array of `n` `PSNull`s via `newArray`.
- **`psLength`**: Pushes length of `PSArray`, `PSDict` (`Map.size`), `PSString`, or `PSBlock`.
- **`psGet`**: Indexes into `PSArray` or `PSBlock` by `Int`, `PSString` by `Int` (pushing character code via `fromEnum`), or `PSDict` by `toKey`.
- **`psPut`**: Mutates `PSArray` or `PSDict` in their respective heaps, or mutates a `PSString` byte at index `i` across the stack and dictionary heap via `mutateStringInState`.
- **`psForall`**: Pops procedure and container (`PSArray` or `PSDict`); iterates over elements (or `(PSName k, v)` pairs) using `loopWhile`, pushing values and invoking `eval`.
- **`psAload`**, **`psAstore`**: `aload` pushes all elements of an array followed by the array itself; `astore` pops `n` items from the stack (reversing them into index order) into the array and pushes the array back.
- **`psCvx`**: Converts `PSArray` to `PSBlock`, `PSName s` to `PSOperator s` (if built-in) or `PSSymbol s`, leaving other objects unchanged.
- **`psMark`**, **`psCountToMark`**, **`psClearToMark`**, **`psStartDict`**, **`psEndArray`**, **`psEndDict`**, **`popToMark`**:
  - `psMark` / `psStartDict` (`[` / `mark` / `<<`) push `PSMark`.
  - `popToMark` splits `operandStack` at the topmost `PSMark`, removes it, and returns the collected items in original push order (`reverse items`).
  - `psEndArray` (`]`) allocates a `PSArray` from `popToMark`.
  - `psEndDict` (`>>`) chunks `popToMark` into `(key, value)` pairs and allocates a `PSDict`.
- **`outputStr`**, **`psPstack`**, **`psStack`**, **`psPrintEq`**, **`psPrintDebug`**, **`psPrint`**:
  - `outputStr` invokes `printAction` in `IO`.
  - `psStack` / `psPrintEq` (`=`) format with `debug = False`; `psPstack` / `psPrintDebug` (`==`) format with `debug = True`; `psPrint` prints `PSString`s directly or formats other objects.
- **`formatObject :: Map Int [Object] -> Bool -> Object -> String`**: Renders an `Object` to `String`. When `debug = False`, prints bare strings/names/numbers or `"--nostringval--"`. When `debug = True`, prints PostScript syntax (`(escaped)` strings via `escapeString`, `/name`, `--op--`, `{...}` blocks, `[...]` arrays resolved from `arrayHeap`, `-dict-`, `-mark-`, `-savelevel-`).
- **`escapeString :: String -> String`**: Escapes `\0`, `(`, `)`, and `\` for `==` output.
- **`psType`**: Pushes the PostScript type name symbol (e.g., `"integertype"`, `"arraytype"`, `"nametype"`).
- **`psStringAlloc`**: Allocates a `PSString` of `n` null bytes (`'\0'`).
- **`psCvlit`**: Converts `PSSymbol` to `PSName` and `PSBlock` to a heap-allocated `PSArray`.
- **`psCvn`**: Converts `PSString`, `PSName`, or `PSSymbol` to `PSName`.
- **`psXcheck`**: Pushes `True` for `PSBlock`, `PSSymbol`, or `PSOperator`, and `False` otherwise.
- **`showInRadix :: Int -> Int -> String`** & **`psCvrs`**: Formats a number in base `2..36` into a pre-allocated `PSString` (mutating it via `mutateStringInState`) and pushes the formatted substring.
- **`psAnchorsearch`**: Tests whether `seek` is a prefix of `str`; pushes `post`, `match`, `True` on match, or `str`, `False` otherwise.
- **`psSearch`**: Searches `str` for the first occurrence of `seek` using `findSub`; pushes `post`, `match`, `pre`, `True` on match, or `str`, `False` otherwise.
- **`psCvs`**: Formats any object with `formatObject ... False`, writes it into the target `PSString` via `mutateStringInState`, and pushes the formatted substring.
- **`mutateStringInState :: Object -> String -> Interpreter ()`**: Replaces all occurrences of `oldObj` with `PSString newContent` on `operandStack` and inside `dictHeap`.

---

### `PostScript.Primitives.Dictionary` (`src/PostScript/Primitives/Dictionary.hs`)

Dictionary allocation, stack management, and symbol binding/lookup operators.

#### Functions

- **`toKey :: Object -> String`**: Extracts a `String` key from `PSName`, `PSSymbol`, `PSString`, or falls back to `show` for numbers/booleans/other objects.
- **`popKey :: Interpreter String`**: Pops an object and applies `toKey`.
- **`psDef`**: Pops `val` and `key`, then binds `key -> val` in the top dictionary via `defineSymbol`.
- **`psDict`**: Pops non-negative integer capacity and allocates an empty `PSDict` via `newDict Map.empty`.
- **`psBegin`**, **`psEnd`**: `begin` pops `PSDict did` and pushes `did` onto `dictStack`; `end` pops the top dictionary via `popDict`.
- **`psKnown`**: Pops `key` and `PSDict did`; pushes `True` if `key` is a member of that dictionary (or `did == 0 && isBuiltin key`).
- **`psLoad`**: Pops `key`, searches `dictStack` via `lookupDict`, falling back to `PSOperator key` if `isBuiltin key`, or errors if undefined.
- **`psWhere`** & **`findDictInStack`**: Searches `dictStack` top-to-bottom for the first dictionary containing `key`; pushes `(PSDict did, True)` (falling back to `(PSDict 0, True)` for built-ins) or `False`.
- **`psCleardictstack`**: Resets `dictStack` to `[0, 0, 0]`.
- **`psUserdict`**, **`psSystemdict`**: Push `PSDict 0`.
- **`psCurrentdict`**: Pushes `PSDict did` for the top of `dictStack` (or `PSDict 0` if empty).
- **`psCountdictstack`**: Pushes `PSInteger (length dictStack)`.
- **`psDictstack`**: Pops a `PSArray aid`, writes `dictStack` (bottom-to-top, `reverse`) as `PSDict` objects into `arrayHeap`, and pushes `PSArray aid`.
- **`psMaxlength`**: Pops `PSDict did` and pushes `max 10 (Map.size d)`.
- **`psUndef`**: Pops `key` and `PSDict did` and deletes `key` from that dictionary in `dictHeap`.
- **`psStore`**: Pops `val` and `key`; searches `dictStack` with `findDictInStack` to update the existing binding if found, otherwise defines `key` in the current dictionary via `defineSymbol`.

---

### `PostScript.Primitives.Control` (`src/PostScript/Primitives/Control.hs`)

Relational, boolean, bitwise, conditional, looping, and procedure-binding operators.

#### Functions

- **`toDouble :: Object -> Maybe Double`**: Converts `PSInteger`/`PSReal` to `Double`.
- **`psEq`**, **`psNe`**: Pop two objects; compare `PSInteger`/`PSReal` numerically (including mixed `Int`/`Real` via `toDouble`), or fall back to structural `Eq` on `Object`.
- **`compareNumbers :: (Double -> Double -> Bool) -> (Int -> Int -> Bool) -> String -> Interpreter ()`**: Helper for numeric order comparisons (`psGt`, `psGe`, `psLt`, `psLe`).
- **`psAnd`**, **`psOr`**, **`psNot`**, **`psXor`**: Polymorphic over `PSBoolean` (logical) and `PSInteger` (bitwise `.&.`, `.|.`, `complement`, `xor`).
- **`psBitshift`**: Pops `shiftAmt` and `x` (`PSInteger`s) and pushes `PSInteger (shift x shiftAmt)`.
- **`psIf`**: Pops `proc` and `PSBoolean`; executes `evalProg (toCode proc)` if `True`.
- **`psIfelse`**: Pops `proc2`, `proc1`, and `PSBoolean`; executes `proc1` if `True`, `proc2` if `False`.
- **`psRepeat`**: Pops `proc` and non-negative `PSInteger n`; loops `n` times using `runLoopStep` so `exit` or `quit` can break early.
- **`psFor`**: Pops `proc`, `limit`, `step`, and `init`; runs an integer loop if all three bounds are `PSInteger`, otherwise coerces to `Double`. Pushes the loop counter before each iteration and steps via `runLoopStep`.
- **`psLoop`**: Pops `proc` and loops infinitely via `runLoopStep` until `psExit` or `quit` is executed.
- **`psExit`**: Sets `loopExited = True` in `PSState` to break out of the innermost enclosing loop.
- **`psExec`**: Pops an object; executes `PSBlock`, `PSSymbol`, or `PSOperator` via `evalProg`, or pushes any other literal back onto the stack.
- **`psBind`** & **`bindObj`**: `psBind` pops a `PSBlock` and recursively walks its objects with `bindObj`, replacing any `PSSymbol name` that resolves to a `PSOperator` in `dictStack` (or satisfies `isBuiltin name`) with `PSOperator name`, leaving other symbols/literals intact.

---

### `PostScript.Primitives.Graphics.Transform` (`src/PostScript/Primitives/Graphics/Transform.hs`)

2D affine matrix math, CTM (Current Transformation Matrix) manipulation, and point/delta projection operators.

#### Functions

- **`toDouble`**, **`popNum`**: Numeric extraction helpers.
- **`matrixToObjects :: Matrix -> [Object]`** & **`objectsToMatrix :: [Object] -> Maybe Matrix`**: Convert between the 6-tuple `Matrix` and a 6-element `[PSReal ...]` list.
- **`popMatrix :: Interpreter (Int, Matrix)`**: Pops a `PSArray` and extracts its 6-element `Matrix` from `arrayHeap`.
- **`popDestArray :: Interpreter Int`**: Pops a `PSArray` and verifies it has length `>= 6`.
- **`writeMatrixArray :: Int -> Matrix -> Interpreter ()`**: Stores a `Matrix` into `arrayHeap` at `aid`.
- **`psMatrix`**: Allocates a new 6-element identity matrix array on `arrayHeap` and pushes it.
- **`psInitmatrix`**: Resets `ctm` in `gState` to `identityMatrix`.
- **`psDefaultmatrix`**, **`psIdentmatrix`**, **`psCurrentmatrix`**: Pop a destination array, write `identityMatrix` (for `defaultmatrix`/`identmatrix`) or the active `ctm` (for `currentmatrix`) into it, and push the array back.
- **`psSetmatrix`**: Pops a matrix array and sets `ctm` to it.
- **`psConcat`**: Pops a matrix `m` and premultiplies it onto `ctm` (`multiplyMatrix m (ctm gs)`).
- **`psConcatmatrix`**: Pops `m3`, `m2`, `m1`, writes `multiplyMatrix m1 m2` into `m3`, and pushes `m3`.
- **`psInvertmatrix`**: Pops `m2` and `m1`, computes `invertMatrix m1` (erroring if singular), writes the result into `m2`, and pushes `m2`.
- **`psTranslate`**, **`psScale`**, **`psRotate`**: Dual-mode operators (peeking the top of stack for `PSArray`):
  - If a destination matrix array is on top, writes the translation/scaling/rotation matrix into that array and pushes it.
  - Otherwise, premultiplies the transformation matrix onto `ctm`.
- **`popPointWithMatrix :: Interpreter ((Double, Double), Matrix)`**: Peeks for an optional matrix array argument; if absent, uses `ctm`.
- **`psTransform`**, **`psItransform`**: Project `(x, y)` forward through `m` (`transformPoint m pt`) or backward through `invertMatrix m`.
- **`psDtransform`**, **`psIdtransform`**: Project delta vector `(dx, dy)` using only the linear `(a, b, c, d)` part of `m` or `invertMatrix m`, ignoring translation `(tx, ty)`.
- **`multiplyMatrix :: Matrix -> Matrix -> Matrix`**: Standard 2D affine matrix multiplication.
- **`transformPoint :: Matrix -> (Double, Double) -> (Double, Double)`**: Computes `(a*x + c*y + tx, b*x + d*y + ty)`.
- **`invertMatrix :: Matrix -> Maybe Matrix`**: Computes the analytical inverse of a 2D affine matrix, returning `Nothing` if `abs (a*d - b*c) < 1e-12`.

---

### `PostScript.Primitives.Graphics.Path` (`src/PostScript/Primitives/Graphics/Path.hs`)

Vector path construction, arc approximation, path flattening/stroking, and bounding-box/iteration operators. All path segments in `GraphicsState.currentPath` are stored in device coordinates (transformed by `ctm`) in reverse order.

#### Functions

- **`popNum`**, **`popPoint`**, **`pushPt`**: Stack helpers for numbers and `(x, y)` pairs.
- **`getUserCurrentPoint :: String -> Interpreter (Double, Double, Matrix)`**: Retrieves `currentPoint` (in device coordinates), inverts `ctm`, and returns `(xu, yu, ctm)` in user coordinates; errors with `"nocurrentpoint"` or `"undefinedresult"` if missing/singular.
- **`addEndpoint :: (Double -> Double -> PathSegment) -> (Double, Double) -> Interpreter ()`**: Prepends a `PathSegment` to `currentPath` and updates `currentPoint`.
- **`psNewpath`**: Clears `currentPath` and sets `currentPoint = Nothing`.
- **`psCurrentpoint`**: Pushes `(xu, yu)` from `getUserCurrentPoint`.
- **`psMoveto`**, **`psRmoveto`**: Append `MoveTo` in device coordinates (absolute or relative to `getUserCurrentPoint`).
- **`psLineto`**, **`psRlineto`**: Append `LineTo` in device coordinates (requiring an active `currentPoint`).
- **`psCurveto`**, **`psRcurveto`**: Append `CurveTo` cubic Bézier segments in device coordinates.
- **`psClosepath`**: Finds the most recent `MoveTo` in `currentPath`, appends `ClosePath`, and sets `currentPoint` back to that subpath's start point.
- **`psArc`**, **`psArcn`**, **`arcAngleSpans`**, **`addArc`**:
  - `psArc` (`isClockwise = False`) and `psArcn` (`isClockwise = True`) delegate to `addArc`.
  - `arcAngleSpans` subdivides the angular sweep into spans of at most $90^\circ$.
  - `addArc` converts each span into a cubic Bézier `CurveTo` using $k = \frac{4}{3}\tan(\Delta\theta / 4)$ and transforms control/end points through `ctm` (so non-uniform scaling produces elliptical arcs), prepending a `MoveTo` or `LineTo` lead segment if needed.
- **`psStrokepath`** & **`extractSubpaths`**: Approximates `strokepath` by extracting polyline subpaths with `extractSubpaths`, expanding each segment into a rectangle quad of half-width `max 0.5 (lineWidth / 2)` plus 12-sided disks for round joins/caps, and replacing `currentPath`.
- **`psArcto`**: Computes the tangent points `t1`, `t2` for a fillet arc of radius `r` between `currentPoint -> (x1, y1)` and `(x1, y1) -> (x2, y2)`, appends `LineTo` segments, updates `currentPoint`, and pushes `t1` and `t2`.
- **`psPathbbox`**: Collects all segment points (sampling `Arc`/`ArcN` via `segmentPoints`), inverse-transforms them into user space via `invertMatrix ctm`, and pushes `llx lly urx ury`.
- **`psFlattenpath`**, **`flattenSegments`**, **`sampleBezierPts`**, **`sampleArcPts`**: Replace `CurveTo`, `Arc`, and `ArcN` segments in `currentPath` with 16-segment `LineTo` polylines.
- **`psPathforall`** & **`stepSegment`**: Iterates over `currentPath` in forward order (sampling arcs to `LineTo`), inverse-transforms coordinates to user space, pushes them onto the operand stack, and invokes the corresponding `move`, `line`, `curve`, or `close` procedure via `loopWhile`.
- **`psClippath`**: Sets `currentPath` to the active `clipPath` (if any), or to the default US Letter page rectangle `(0,0)-(612,792)`.

---

### `PostScript.Primitives.Graphics.State` (`src/PostScript/Primitives/Graphics/State.hs`)

Graphics state stack, VM snapshots (`save`/`restore`), color spaces, stroke styling, clipping, and painting/page operators.

#### Functions

- **`popNum`**, **`clamp01`**: Pops a number as `Double` and clamps values to `[0.0, 1.0]`.
- **`psGsave`**, **`psGrestore`**, **`psGrestoreall`**: Push `gState` onto `gStateStack`, pop and restore the top `GraphicsState`, or restore the bottom-most `GraphicsState` and clear `gStateStack`.
- **`psSave`**, **`psRestore`**: `save` snapshots `(sid, dictHeap, arrayHeap, gState)` in `saveSnapshots`, pushes `gState` onto `gStateStack`, and pushes `PSSave sid`. `restore` pops `PSSave sid` (or `PSInteger sid`) and restores the matching snapshot's heaps and `gState` (falling back to `gStateStack` if needed).
- **`psSetrgbcolor`**, **`psSetgray`**, **`psSetcmykcolor`**, **`psSethsbcolor`**, **`hsbToRgb`**: Set `rgbColor` in `gState`, converting grayscale, CMYK (`(1-c)*(1-k)`), or HSB (via 6-sector `hsbToRgb`) into clamped RGB.
- **`psCurrentrgbcolor`**, **`psCurrentgray`**, **`psCurrentlinewidth`**, **`psCurrentmiterlimit`**: Push the current RGB components, NTSC luminance gray (`0.3*r + 0.59*g + 0.11*b`), line width, or miter limit.
- **`psSetlinewidth`**, **`psSetlinecap`**, **`psSetlinejoin`**, **`psSetmiterlimit`**, **`psSetdash`**: Update stroke styling fields in `gState`.
- **`recordDrawAction`**: Helper for painting operators: reverses `currentPath` into forward order; if non-empty, builds `(svgElem, GraphicCommand)`, prepends them to `canvasElements` and `graphicCommands`, and clears `currentPath` and `currentPoint`.
- **`psStroke`**, **`psFill`**, **`psEofill`**: Call `recordDrawAction` with `renderStrokeToSVG`/`DrawStroke`, `renderFillToSVG ... False`/`DrawFill ... False` (nonzero winding), and `renderFillToSVG ... True`/`DrawFill ... True` (even-odd).
- **`psClip`**, **`psEoclip`**, **`psInitclip`**: `clip`/`eoclip` save `reverse currentPath` into `clipPath` while retaining `currentPath`; `initclip` resets `clipPath = Nothing`.
- **`psShowpage`**: Appends `DrawPage` to `graphicCommands` and clears `currentPath` and `currentPoint`.

---

### `PostScript.Primitives.Graphics.Text` (`src/PostScript/Primitives/Graphics/Text.hs`)

Font dictionary management, text rendering, spacing variants, and glyph outline paths.

#### Functions

- **`popStr`**, **`popNum`**: Stack helpers extracting strings/names or numbers.
- **`psFindfont`**: Pops font name, looks it up in `dictStack` (returning an existing `PSDict` if registered via `definefont`), or creates a new font dictionary with `"FontName"` and `"Scale" = 1.0`.
- **`psScalefont`**: Pops `scale` and font dict, creates a new font dict with `"Scale"` multiplied by `scale`, and pushes it.
- **`psSetfont`**: Pops a font dict and sets `fontName` and `fontSize` in `gState`.
- **`psCurrentfont`**: Constructs and pushes a font dict reflecting `fontName` and `fontSize` of `gState`.
- **`psShow`**: Pops `txt`, requires an active `currentPoint`, emits `renderTextToSVG` and `DrawText` to `PSState`, and advances `currentPoint` in user space by `length txt * fontSize * 0.55` before transforming back through `ctm`.
- **`psStringwidth`**: Pops `txt` and pushes `(length txt * fontSize * 0.55, 0.0)`.
- **`showWithExtraSpacing :: (Char -> (Double, Double)) -> String -> Interpreter ()`**: Renders a string character-by-character, emitting `DrawText` / SVG `<text>` for each character and advancing `currentPoint` by `fontSize * 0.55` plus the per-character `(extraX, extraY)` offset.
- **`psAshow`**, **`psWidthshow`**, **`psAwidthshow`**: Text spacing operators implemented via `showWithExtraSpacing` (`ashow` adds `(ax, ay)` to every char; `widthshow` adds `(cx, cy)` when char code matches `char`; `awidthshow` combines both).
- **`psKshow`**: Renders `txt` character-by-character and, between each adjacent pair `(c_i, c_{i+1})`, pushes their integer character codes and executes `proc`.
- **`glyphOutlines :: Char -> (Double, [[(Double, Double)]])`**: Minimal normalized polygonal glyph outline table (explicitly defined for `'t'`, `'e'`, `'x'`, `'X'`, `'T'`, `' '`, and a hollow box fallback for other characters).
- **`psCharpath`**: Pops `isStroke` boolean and `txt`, converts each character's `glyphOutlines` subpaths scaled by `fontSize` and transformed by `ctm` into `MoveTo`/`LineTo`/`ClosePath` segments appended to `currentPath`, and advances `currentPoint`.
- **`psMakefont`**: Pops a 6-element matrix array and a font dict, scales the font's `"Scale"` by `sqrt (abs (a*d - b*c))`, and pushes the new font dict.
- **`psDefinefont`**: Pops a font dict and name, binds `name -> fontObj` in the active dictionary via `defineSymbol`, and pushes `fontObj`.

---

### `PostScript.Primitives.Graphics.Render` (`src/PostScript/Primitives/Graphics/Render.hs`)

Translates `GraphicCommand`s and `GraphicsState` into SVG 1.1 XML strings.

#### Functions

- **`fmtD :: Double -> String`**: Formats numbers cleanly (integers without `.0`, reals rounded to 3 decimal places).
- **`renderPathToSVG :: [PathSegment] -> String`**: Converts `MoveTo`, `LineTo`, `CurveTo`, `Arc`, `ArcN`, and `ClosePath` into SVG `d="..."` commands (`M`, `L`, `C`, `A`, `Z`).
- **`fmtRGB :: (Double, Double, Double) -> String`**: Clamps `[0, 1]` channels and formats `"rgb(r,g,b)"` in `0..255`.
- **`joinComma :: [String] -> String`**: Joins strings with `","`.
- **`hashString :: String -> Int`**: Deterministic DJB-style hash (`h * 31 + ord c mod 1000000007`) for generating unique SVG `<clipPath id="clip_...">` IDs.
- **`wrapWithClip :: GraphicsState -> String -> String`**: Wraps an SVG element in `<g><clipPath id="..."><path d="..." /></clipPath><g clip-path="url(#...)">...</g></g>` when `clipPath gs` is non-empty.
- **`renderStrokeToSVG :: GraphicsState -> [PathSegment] -> String`**: Emits a `<path fill="none" ...>` with `stroke`, `stroke-width` (scaled by `sqrt (abs (det ctm))`), `stroke-linecap`, `stroke-linejoin`, optional `stroke-miterlimit`, `stroke-dasharray`, and `stroke-dashoffset`, wrapped in `wrapWithClip`.
- **`renderFillToSVG :: GraphicsState -> Bool -> [PathSegment] -> String`**: Emits `<path fill="..." stroke="none" />` with `fill-rule="evenodd"` when `isEvenOdd` is `True`, wrapped in `wrapWithClip`.
- **`escapeXML :: String -> String`**: Escapes `&`, `<`, `>`, `"`, `'` for XML text/attributes.
- **`renderTextToSVG :: GraphicsState -> (Double, Double) -> String -> String`**: Emits an SVG `<text>` element with a local `transform="matrix(...)"` that counters the root Y-flip and applies `ctm` + negative `fontSize` sign, plus `font-family`, `font-size`, `font-style="italic"`, `font-weight="bold"`, and `fill`.
- **`generateSVGDocument :: Double -> Double -> [String] -> String`**: Wraps SVG elements in an XML header, `<svg>` root with `pt` dimensions and `viewBox`, and a `<g transform="translate(0, h) scale(1, -1)">` group to map PostScript's bottom-left origin to SVG's top-left origin.
- **`renderCommandToSVG :: GraphicCommand -> Maybe String`**: Dispatches `DrawStroke`, `DrawFill`, `DrawText`, and returns `Nothing` for `DrawPage`.
- **`splitByPages :: [GraphicCommand] -> [[GraphicCommand]]`**: Splits a `GraphicCommand` list at `DrawPage` boundaries, dropping empty pages.
- **`renderAllPagesToSVG :: Double -> Double -> [GraphicCommand] -> [String]`**: Partitions commands by page and renders each non-empty page to a complete SVG document string.

---

## CLI Executable (`app/`)

### `Main` (`app/Main.hs`)

Command-line interface for executing `.ps` files and writing SVG output.

#### Functions

- **`main :: IO ()`**: Reads CLI arguments via `getArgs`, parses them with `parseArgs`, and runs `run` (or prints usage and exits with failure).
- **`parseArgs :: [String] -> Maybe (FilePath, Maybe FilePath, Maybe Int)`**: Parses `<file.ps>`, optional `-o <out_path_or_dir>` (or second positional arg), and optional `--page <n>`.
- **`run :: FilePath -> Maybe FilePath -> Maybe Int -> IO ()`**: Reads the `.ps` file, runs `executeScript`, reverses `graphicCommands`, and renders all pages at `612 x 792` via `renderAllPagesToSVG`. Outputs either a single selected `--page` (to file or stdout), the first page to stdout if `-o` is omitted, or writes all pages via `writePages`.
- **`writePages :: FilePath -> String -> [String] -> IO ()`**: Writes a single page to `<target>` (or `<target>/<baseName>.svg` if `target` is a directory or lacks `.svg`), or writes multi-page output as `<outBase>_1.svg`, `<outBase>_2.svg`, etc., creating parent directories as needed.

---

## Dual-View GUI Executable (`app-gui/`)

### `Types` (`app-gui/Types.hs`)

Defines state and configuration types for the Gloss GUI viewer.

#### Types

- **`data ViewMode`**: `ViewSideBySide | ViewInterpretedOnly | ViewReferenceOnly` (derives `Show, Eq`).
- **`data TestCase`**: Holds `testName :: String`, `psFilePath :: FilePath`, and `refImagePaths :: [FilePath]` (derives `Show, Eq`).
- **`data ViewerState`**: Holds the list of `testCases`, `activeIndex`, `viewMode`, `currentTestPages :: [Picture]`, `pageIndex`, `referencePages :: [Picture]`, `panOffset`, `zoomScale`, `isDragging`, `dragAnchor`, `showMenuList`, `menuScroll`, `statusMessage`, `currentJobId`, `activeWorkerTid :: Maybe ThreadId`, and `asyncMVar :: MVar (Int, Either String [Picture], String)` for thread-safe background evaluation results.

---

### `ImageLoader` (`app-gui/ImageLoader.hs`)

Loads reference raster images and downsamples high-DPI scans for OpenGL/Gloss display.

#### Functions

- **`loadReferencePicture :: FilePath -> (Float, Float) -> IO (Maybe Picture)`**: Reads `.jpg`/`.png`/`.bmp` via JuicyPixels `readImage`, converts to RGBA8, computes an integer downsampling factor `k`, downsamples with `downsampleKxRGBA8` if `k >= 2`, encodes to BMP and parses with `Codec.BMP.parseBMP` into `bitmapOfBMP`, and scales uniformly to fit `(targetW, targetH)`.
- **`downsampleKxRGBA8 :: Int -> Image PixelRGBA8 -> Image PixelRGBA8`**: Box/area-averaging downsampler over `k x k` pixel blocks using mutable storable vectors (`V.create`, `VM.new`, `VM.unsafeWrite`) so thin line art remains anti-aliased.
- **`missingImagePlaceholder :: String -> Picture`**: Returns a Gloss `Picture` displaying `"No Reference Image"` and the test name.

---

### `TestBrowser` (`app-gui/TestBrowser.hs`)

Discovers `.ps` test files, locates or generates reference images, and runs scripts.

#### Functions

- **`imageExtensions :: [String]`**: `[".jpg", ".jpeg", ".png", ".bmp"]`.
- **`discoverTestCases :: Maybe FilePath -> IO [TestCase]`**: If given a file path, builds a single `TestCase`; if given a directory, scans it non-recursively; if `Nothing`, recursively scans `test/working_graphics`, `test/Examples`, and their `Project/postscript-interpreter/...` equivalents, deduplicating by `testName` and sorting alphabetically.
- **`scanDirectory :: FilePath -> IO [TestCase]`** & **`scanDirectoryRecursively :: FilePath -> IO [TestCase]`**: Find `.ps` files (case-insensitive extension) and construct `TestCase`s via `makeTestCase`.
- **`makeTestCase :: FilePath -> IO TestCase`**: Builds a `TestCase` with `takeBaseName` and `findPageImages`.
- **`findPageImages :: FilePath -> IO [FilePath]`**: Searches for multi-page numbered images (`findNumberedImages`), then single-page `<base>.<ext>`, and falls back to `tryRenderWithGhostscript`.
- **`findNumberedImages :: FilePath -> Int -> IO [FilePath]`**: Probes page numbers `1..20` across separators `"_"`, `"-"`, `"."`, `"_page"`, `"-page"` and `imageExtensions`.
- **`tryRenderWithGhostscript :: FilePath -> IO [FilePath]`**: If `gs` is on `PATH`, renders the `.ps` file at 150 DPI JPEG into `/tmp/postscript_gui_cache/<name>/page_%d.jpg` and collects the generated pages.
- **`findFirstExisting :: [FilePath] -> IO (Maybe FilePath)`**: Returns the first existing file path from a list of candidates.
- **`loadTestExecution :: FilePath -> IO (Either String PSState)`**: Reads a `.ps` file (catching `SomeException`) and runs `executeScript`.

---

### `GlossRender` (`app-gui/GlossRender.hs`)

Converts `GraphicCommand`s into Gloss `Picture`s for interactive OpenGL display.

#### Functions

- **`pageDimensions :: (Float, Float)`**, **`pageWidth`**, **`pageHeight`**, **`pageHalfW`**, **`pageHalfH`**: US Letter dimensions `(612.0, 792.0)` and half-dimensions.
- **`toGlossPoint :: (Double, Double) -> Point`**: Translates PostScript bottom-left coordinates `(0..612, 0..792)` to Gloss center-origin coordinates.
- **`psColorToGloss :: (Double, Double, Double) -> Color`**: Clamps RGB `[0, 1]` to a Gloss `Color`.
- **`paperCanvas :: Picture -> Picture`**: Wraps a `Picture` with a drop shadow, white US Letter background, and grey border frame.
- **`compilePages :: [GraphicCommand] -> [Picture]`** & **`splitByDrawPage :: [GraphicCommand] -> [[GraphicCommand]]`**: Partition commands at `DrawPage` and compile each page into a `paperCanvas`-wrapped `Picture`.
- **`renderCommandsToPicture :: [GraphicCommand] -> Picture`**: Maps `DrawStroke`, `DrawFill`, `DrawText`, and `DrawPage` (`Blank`) into a combined `Pictures`.
- **`renderStroke :: GraphicsState -> [PathSegment] -> Picture`**: Extracts subpaths, applies CTM scaling to `lineWidth` and `dashArray`/`dashOffset`, subdivides dashed paths via `dashSubpath`, and renders each subpath/dot.
- **`renderSubpath :: Float -> Int -> Int -> [Point] -> Picture`**: Renders thin lines (`effW <= 1.0`) with `renderClippedLines` and optional cap dots (`renderPixelDot`), or thick lines via `renderThickSubpath`.
- **`renderPixelDot`**, **`renderDot`**, **`renderDisk`**: Render 4-vertex square quads or 12-sided `Polygon` disks clipped to page bounds.
- **`dashSubpath :: [Double] -> Double -> [(Double, Double)] -> ([[(Double, Double)]], [(Double, Double)])`**: Implements PostScript dash pattern walking (doubling odd-length patterns, normalizing `dashOffset`, splitting segments at dash/gap transitions, and recording zero-length `"on"` dashes as dots).
- **`renderClippedLines :: [Point] -> Picture`**: Clips line segments to the page rectangle using `clipSegmentToRect` and emits `Line` primitives.
- **`renderThickSubpath :: Float -> Int -> Int -> [Point] -> Picture`**: Clipped thick-line renderer emitting oriented `Polygon` quads per segment, 12-sided vertex disks for round joins (`join == 1`), and round end caps (`cap == 1`).
- **`renderFill :: GraphicsState -> [PathSegment] -> Picture`**: Fills subpaths as Gloss `Polygon`s clipped to page bounds with `clipPolygonToRect`; substitutes active `clipPath` when filling a large bounding-box rectangle.
- **`renderText :: GraphicsState -> (Double, Double) -> String -> Picture`**: Dispatches to `renderClippedText` if `clipPath` is active, otherwise to `renderOneChar`.
- **`renderClippedText :: GraphicsState -> (Double, Double) -> String -> [[Point]] -> Picture`**: Renders text using the normalized vector stroke font (`charStrokes`), clipped segment-by-segment against active clip polygons via `clipSegmentToPolys`.
- **`segmentIntersectT`**, **`clipSegmentToPolys`**, **`isPointInClipPolys`**: 2D segment-polygon clipping and ray-casting point-in-polygon tests.
- **`charStrokes :: Char -> (Double, [[(Double, Double)]])`**: Normalized stroke-font glyph table for clipped text rendering.
- **`renderOneChar :: GraphicsState -> Point -> Float -> String -> Picture`**: Renders unclipped text using Gloss `Text` with CTM scaling/rotation and multi-offset bold overdraw.
- **`clipPolygonToRect`**, **`clipSegmentToRect`**, **`isInsideRect`**: Sutherland-Hodgman polygon clipping, Liang-Barsky line clipping, and rectangle containment test.
- **`segmentsToSubpaths`**, **`sampleBezier`**, **`sampleArc`**: Convert `PathSegment`s into sampled coordinate polylines (20 samples per Bézier, 24 per arc).

---

### `Main` (`app-gui/Main.hs`)

Interactive Gloss GUI entry point, event loop, and async worker orchestration.

#### Functions

- **`windowWidth`**, **`windowHeight`**, **`windowDisplay`**, **`backgroundColor`**: Window configuration (`1400 x 900`, dark grey `0.15` background).
- **`testEstimatedDuration :: String -> Maybe String`**: Lookup table of known slow fractal/Mandelbrot tests and their estimated runtimes for UI warnings.
- **`main :: IO ()`**: Discovers test cases from CLI arg (or default directories), initializes `ViewerState`, and launches `Gloss.playIO` at 30 FPS.
- **`initViewerState :: [TestCase] -> IO ViewerState`**: Creates an empty `MVar`, builds the initial `ViewerState`, and starts the first background job.
- **`startBackgroundJob :: ViewerState -> Int -> IO ViewerState`**: Kills any running worker thread, drains stale `MVar` results, loads reference page images via `loadReferencePicture`, and forks a background thread (`forkIO`) running `loadTestExecution` + `compilePages` and posting results to `asyncMVar`.
- **`stepWorld :: Float -> ViewerState -> IO ViewerState`**: Polls `asyncMVar` with `tryTakeMVar`; if a result matching `currentJobId` has arrived, updates `currentTestPages` and `statusMessage`.
- **`renderWorld :: ViewerState -> IO Picture`**: Composes the active `ViewMode` (side-by-side, interpreted-only, or reference-only) with `panOffset` and `zoomScale`, plus header, footer, and optional menu overlays.
- **`renderHeaderOverlay`**, **`renderFooterOverlay`**, **`renderMenuOverlay`**: Render the top status bar, bottom shortcut bar, and scrollable `M`-toggled test selection overlay.
- **`handleEvent :: Event -> ViewerState -> IO ViewerState`**: Handles mouse drag panning, wheel/`+`/`-` zooming, `Space`/`0` view reset, `1`/`2`/`3` view mode switching, `n`/`p`/arrow test navigation, `PgDn`/`PgUp`/`Tab`/`<`/`>` page navigation, `r` reload, `m` menu toggle, and `q`/`Esc` exit.
- **`adjustZoom`**, **`quitApp`**, **`advancePageOrTest`**, **`retreatPageOrTest`**, **`nextPage`**, **`prevPage`**, **`getPageAt`**: View, lifecycle, and page-cycling helpers.
- **`currentInterpPicture`**, **`currentRefPicture`**, **`loadingPlaceholder`**, **`errorPlaceholder`**: Active page accessors and status placeholder `Picture`s.
- **`currentCase`**, **`currentTestName`**, **`currentImageLabel`**, **`currentInterpLabel`**: `ViewerState` label/metadata accessors.

---

## Test Suite (`test/`)

### `Spec` (`test/Spec.hs`)

Three-part automated test suite (`QuickCheck` properties, `.ps` program stack checks, and Ghostscript differential tests).

#### Helpers & Test Runners

- **`evalScript :: String -> IO (Either String (String, [Object]))`**: Parses `code` and executes it with a custom `printAction` backed by an `IORef [String]`, returning captured stdout and the final `operandStack`.
- **`evalFile :: FilePath -> IO (Either String (String, [Object]))`**: Reads a `.ps` file and calls `evalScript`.
- **`runsTo :: String -> [Object] -> Property`**: QuickCheck `ioProperty` asserting that `evalScript code` succeeds with `operandStack == expected`.
- **`testPrograms :: [(FilePath, [Object])]`** & **`runProgramTest`**: Integration suite running 8 `.ps` programs in `test/programs/` (`fibonacci.ps`, `primes.ps`, `collatz_poly.ps`, `dict_scope.ps`, `stack_ops.ps`, `math_advanced.ps`, `euclid_gcd.ps`, `control_flow.ps`) and verifying exact final operand stacks.
- **`runGhostscript :: FilePath -> IO (Either String String)`**: Runs `gs -q -dNODISPLAY <path>` and captures stdout.
- **`compareOutputs :: String -> String -> Bool`**: Compares token lists (exact or sorted) with a `0.005` tolerance on floating-point numbers.
- **`runDiffTest :: FilePath -> IO Bool`**: Runs a `.ps` file in both `evalFile` and `runGhostscript` and checks `compareOutputs`.
- **`runQC :: (Testable prop) => String -> prop -> IO Bool`**: Runs 100 QuickCheck tests per property.
- **`main :: IO ()`**: Executes all 108 QuickCheck properties (covering stack, math, control flow, arrays/dicts, bitwise/marks, `bind`/`exec`/`loop`, 2D transforms, paths, graphics state/SVG output, utility operators, and text/fonts), all 8 program integration tests, and all `.ps` files in `test/output_tests` against Ghostscript, exiting with `exitSuccess` or `exitFailure`.

#### QuickCheck Properties (`prop_*`)

- **Stack & Output**: `prop_dupPop`, `prop_exchTwice`, `prop_dupAddDoubles`, `prop_countMatches`, `prop_clearEmpties`, `prop_quitTerminates`, `prop_formatEq`, `prop_formatDebug`.
- **Math & Bitwise**: `prop_addCommutative`, `prop_mulCommutative`, `prop_subInverse`, `prop_negInvolution`, `prop_absNonNegative`, `prop_idivModIdentity`, `prop_bitwiseAnd`, `prop_bitwiseOr`, `prop_bitwiseXor`, `prop_bitshiftShiftsBits`, `prop_cviTruncatesReal`, `prop_randGeneratesPositiveInt`, `prop_usertimeReturnsInt`, `prop_trailingDotParsesReal`.
- **Control, Scoping & Containers**: `prop_ifelseTrue`, `prop_ifelseFalse`, `prop_repeatCount`, `prop_defLookup`, `prop_eqReflexive`, `prop_arrayAllocLength`, `prop_arrayPutGet`, `prop_bracketArray`, `prop_bracketArrayGet`, `prop_dictPutGet`, `prop_dictLiteral`, `prop_dictLength`, `prop_arrayCopy`, `prop_arrayForallSum`, `prop_nullEq`, `prop_arrayRefSemantics`, `prop_countToMark`, `prop_clearToMark`, `prop_execBlock`, `prop_execLiteral`, `prop_loopExit`, `prop_repeatExit`, `prop_forExit`, `prop_bindOperator`, `prop_bindPreservesLiterals`, `prop_bindExecutes`, `prop_aloadUnpacksArray`, `prop_cvxExecutesBlock`, `prop_loadAliasesBuiltin`, `prop_whereFindsDict`, `prop_astorePopsIntoArray`, `prop_cleardictstackResetsStack`, `prop_userdictPushesRootDict`.
- **Transforms**: `prop_matrixAllocLength`, `prop_identmatrixIdentity`, `prop_defaultmatrixIdentity`, `prop_translateMatrix`, `prop_scaleMatrix`, `prop_rotateMatrix`, `prop_rotateCTM`, `prop_concatCTM`, `prop_concatmatrixIdentity`, `prop_invertMatrixIdentity`, `prop_dtransformIgnoresTranslation`, `prop_dtransformIdtransformRoundTrip`, `prop_currentmatrixSetmatrix`, `prop_transformItransformRoundTrip`.
- **Paths**: `prop_movetoCurrentpoint`, `prop_rmovetoRel`, `prop_linetoCurrentpoint`, `prop_rlinetoRel`, `prop_closepathReturnsToStart`, `prop_newpathClearsCurrentpoint`, `prop_curvetoEndpoint`, `prop_rcurvetoEndpoint`, `prop_arcEndpoint`, `prop_arcnEndpoint`, `prop_arctoTangents`, `prop_pathbboxRect`, `prop_flattenpathPreservesBBox`, `prop_pathforallAccumulates`, `prop_currentpointWithCTM`, `prop_arcTransformsElliptically`.
- **Graphics State & SVG**: `prop_gsaveGrestoreRGB`, `prop_grestoreall`, `prop_setrgbcolor`, `prop_setgrayCurrentgray`, `prop_setcmykcolor`, `prop_sethsbcolor`, `prop_setlinewidth`, `prop_strokeClearsPath`, `prop_fillClearsPath`, `prop_eofillClearsPath`, `prop_clipPreservesCurrentpoint`, `prop_showpageClearsPath`, `prop_strokeEmitsSVG`, `prop_fillEmitsSVG`, `prop_eofillEmitsSVG`, `prop_saveRestorePreservesState`, `prop_clippathEmitsBoundary`.
- **Text**: `prop_showAdvancesCurrentpoint`, `prop_stringwidthMeasuresAdvance`, `prop_showEmitsDrawTextAndSVG`, `prop_ashowAdvancesWithSpacing`, `prop_kshowExecutesProcBetweenChars`, `prop_charpathAppendsSubpaths`.
